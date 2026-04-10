#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.11"
# dependencies = ["pyte>=0.8.2"]
# ///
"""
Diff the rendered grid of `cargo run --example embed FILE` against a real
`nvim FILE` TUI of the same size and inputs.

Runs each under a pseudo-terminal, feeds the raw output into a `pyte.Screen`,
waits for output to quiesce, and then walks every cell comparing text +
foreground + background + attributes. Reports the first N mismatches.

Usage:
    ./tools/compare_embed.py [FILE] [--cols N] [--rows N] [--settle MS]

Defaults: FILE=crates/extui-neovim/src/grid.rs, 100x30, 1500ms settle.
"""

from __future__ import annotations

import argparse
import fcntl
import os
import pty
import select
import signal
import struct
import subprocess
import sys
import termios
import time
from dataclasses import dataclass
from pathlib import Path
from typing import List, Tuple

import pyte
import pyte.graphics


_NAME_TO_INDEX = {
    "black": 0, "red": 1, "green": 2, "brown": 3, "blue": 4,
    "magenta": 5, "cyan": 6, "white": 7,
    "brightblack": 8, "brightred": 9, "brightgreen": 10, "brightbrown": 11,
    "brightblue": 12, "brightmagenta": 13, "brightcyan": 14, "brightwhite": 15,
    # pyte uses "brightyellow" in some tables and "brightbrown" in others;
    # accept both.
    "brightyellow": 11,
}


_CUBE_LEVELS = (0x00, 0x5f, 0x87, 0xaf, 0xd7, 0xff)
_GREY_LEVELS = tuple(range(0x08, 0xef, 0x0a))


def _quantize_rgb_index(rgb: int) -> int:
    """Nearest xterm 256-color index for a 24-bit color.

    Must match `quantize_rgb` in crates/extui-neovim/src/grid.rs so the
    "real" leg (which emits 24-bit RGB under termguicolors) compares
    equal to the embed leg (which emits the 256-color approximation).
    """
    r = (rgb >> 16) & 0xff
    g = (rgb >> 8) & 0xff
    b = rgb & 0xff

    def closest(v: int, table):
        return min(range(len(table)), key=lambda i: abs(v - table[i]))

    ci_r = closest(r, _CUBE_LEVELS)
    ci_g = closest(g, _CUBE_LEVELS)
    ci_b = closest(b, _CUBE_LEVELS)
    cube_idx = 16 + ci_r * 36 + ci_g * 6 + ci_b
    cube_r, cube_g, cube_b = _CUBE_LEVELS[ci_r], _CUBE_LEVELS[ci_g], _CUBE_LEVELS[ci_b]
    cube_dist = (r - cube_r) ** 2 + (g - cube_g) ** 2 + (b - cube_b) ** 2

    avg = (r + g + b) // 3
    gi = closest(avg, _GREY_LEVELS)
    grey = _GREY_LEVELS[gi]
    grey_dist = (r - grey) ** 2 + (g - grey) ** 2 + (b - grey) ** 2

    return 232 + gi if grey_dist < cube_dist else cube_idx


def normalize_color(c: str) -> str:
    """Collapse the different spellings pyte emits for the same palette slot.

    pyte reports ANSI SGR `\\x1b[94m` as the name `brightcyan` but reports the
    equivalent 256-color sequence `\\x1b[38;5;14m` as the hex string `00ffff`.
    It also reports 24-bit sequences `\\x1b[38;2;R;G;Bm` as the raw RGB hex,
    which would never match an embed leg that quantizes to the 256-color cube.
    We unify everything to `idxNNN` (a palette index, 0-255) or `default`.
    """
    if c == "default":
        return "default"
    idx = _NAME_TO_INDEX.get(c)
    if idx is None and len(c) == 6:
        try:
            rgb = int(c, 16)
        except ValueError:
            return c
        # If the RGB value is exactly a palette entry, recover its index.
        if c in pyte.graphics.FG_BG_256:
            idx = pyte.graphics.FG_BG_256.index(c)
        else:
            idx = _quantize_rgb_index(rgb)
    if idx is None:
        return c
    return f"idx{idx:03d}"


REPO = Path(__file__).resolve().parents[3]


def set_winsize(fd: int, rows: int, cols: int) -> None:
    winsize = struct.pack("HHHH", rows, cols, 0, 0)
    fcntl.ioctl(fd, termios.TIOCSWINSZ, winsize)


def capture(argv: List[str], cwd: Path, rows: int, cols: int, settle_ms: int) -> bytes:
    """Spawn `argv` under a pty, read until output is quiet for `settle_ms`."""
    env = os.environ.copy()
    env["TERM"] = "xterm-256color"
    env.pop("COLORTERM", None)
    # Suppress the "defaults.lua: Did not detect DSR response" hit-enter
    # prompt that would otherwise block both legs.
    env["NVIM_TEST"] = "1"
    # Avoid user config interference in the "real nvim" leg.
    env.pop("NVIM", None)
    env.pop("NVIM_LISTEN_ADDRESS", None)
    env["LINES"] = str(rows)
    env["COLUMNS"] = str(cols)

    pid, fd = pty.fork()
    if pid == 0:
        try:
            set_winsize(0, rows, cols)
        except Exception:
            pass
        os.chdir(cwd)
        os.execvpe(argv[0], argv, env)
        os._exit(127)

    set_winsize(fd, rows, cols)

    buf = bytearray()
    start = time.monotonic()
    quiet_deadline = start + settle_ms / 1000.0
    hard_deadline = start + 15.0
    while True:
        now = time.monotonic()
        if now >= hard_deadline or now >= quiet_deadline:
            break
        timeout = min(quiet_deadline - now, hard_deadline - now)
        try:
            r, _, _ = select.select([fd], [], [], timeout)
        except OSError:
            break
        if not r:
            continue
        try:
            chunk = os.read(fd, 65536)
        except OSError:
            break
        if not chunk:
            break
        buf.extend(chunk)
        quiet_deadline = time.monotonic() + settle_ms / 1000.0

    # Send Ctrl+Q so extui embed exits cleanly, then force-kill anything left.
    try:
        os.write(fd, b"\x11")
    except OSError:
        pass
    drain_until = time.monotonic() + 0.3
    while time.monotonic() < drain_until:
        try:
            r, _, _ = select.select([fd], [], [], drain_until - time.monotonic())
        except OSError:
            break
        if not r:
            break
        try:
            if not os.read(fd, 65536):
                break
        except OSError:
            break

    for sig in (signal.SIGTERM, signal.SIGKILL):
        try:
            os.killpg(os.getpgid(pid), sig)
        except (ProcessLookupError, PermissionError, OSError):
            try:
                os.kill(pid, sig)
            except ProcessLookupError:
                pass
        kill_deadline = time.monotonic() + 1.0
        while time.monotonic() < kill_deadline:
            try:
                rpid, _ = os.waitpid(pid, os.WNOHANG)
            except ChildProcessError:
                rpid = pid
                break
            if rpid == pid:
                break
            time.sleep(0.02)
        else:
            continue
        break
    try:
        os.close(fd)
    except OSError:
        pass

    return bytes(buf)


def feed(screen: pyte.Screen, stream: pyte.Stream, data: bytes) -> None:
    # pyte consumes unicode; decode permissively.
    stream.feed(data.decode("utf-8", errors="replace"))


@dataclass
class CellView:
    ch: str
    fg: str
    bg: str
    bold: bool
    italics: bool
    underscore: bool
    strikethrough: bool
    reverse: bool

    @classmethod
    def of(cls, c) -> "CellView":
        return cls(
            ch=c.data,
            fg=normalize_color(c.fg),
            bg=normalize_color(c.bg),
            bold=c.bold,
            italics=c.italics,
            underscore=c.underscore,
            strikethrough=c.strikethrough,
            reverse=c.reverse,
        )

    def brief(self) -> str:
        mods = []
        if self.bold: mods.append("bold")
        if self.italics: mods.append("italics")
        if self.underscore: mods.append("under")
        if self.strikethrough: mods.append("strike")
        if self.reverse: mods.append("rev")
        modstr = "+".join(mods) or "-"
        return f"{self.ch!r} fg={self.fg} bg={self.bg} {modstr}"


def grid(screen: pyte.Screen) -> List[List[CellView]]:
    out = []
    for y in range(screen.lines):
        row = []
        line = screen.buffer[y]
        for x in range(screen.columns):
            row.append(CellView.of(line[x]))
        out.append(row)
    return out


def render_row(row: List[CellView]) -> str:
    return "".join(c.ch for c in row).rstrip()


def diff_grids(a: List[List[CellView]], b: List[List[CellView]], max_mismatches: int = 40) -> Tuple[int, List[str]]:
    rows = min(len(a), len(b))
    cols = min(len(a[0]), len(b[0])) if rows else 0
    mismatches = 0
    report: List[str] = []
    for y in range(rows):
        for x in range(cols):
            if a[y][x] != b[y][x]:
                mismatches += 1
                if len(report) < max_mismatches:
                    report.append(f"  ({y:2},{x:3}) real={a[y][x].brief()}  embed={b[y][x].brief()}")
    return mismatches, report


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("file", nargs="?", default="crates/extui-neovim/src/grid.rs")
    ap.add_argument("--cols", type=int, default=100)
    ap.add_argument("--rows", type=int, default=30)
    ap.add_argument("--settle", type=int, default=1500)
    ap.add_argument("--max", type=int, default=40)
    ap.add_argument("--show-grids", action="store_true")
    ap.add_argument(
        "--init",
        help="path to an init.lua to source (passed via -u); bypasses the minimal default config",
    )
    args = ap.parse_args()

    file_path = (REPO / args.file).resolve()
    if not file_path.exists():
        print(f"file not found: {file_path}", file=sys.stderr)
        return 2

    # `-i NONE -n` skips shada and swap. `--cmd` bits suppress hit-enter
    # prompts and the defaults.lua DSR warning so both legs start from
    # an identical, deterministic state. `termguicolors` is left at
    # whatever the init file sets so we can validate both the cterm
    # path (e.g. a 256-colour urxvt with a custom palette) and the
    # rgb path (modern Lua themes) against the embed.
    clean_args = [
        "-i", "NONE", "-n",
        "--cmd", "set nomore shortmess+=saIFc",
    ]
    if args.init:
        clean_args += ["-u", str(Path(args.init).resolve())]
    else:
        clean_args += ["-u", "NONE", "--cmd", "set termguicolors", "+syntax on"]
    real_cmd = ["nvim", *clean_args, str(file_path)]
    embed_cmd = [
        "cargo", "run", "--quiet", "-p", "extui-neovim", "--example", "embed",
        "--",
        *clean_args, str(file_path),
    ]

    print(f"# file    : {file_path}")
    print(f"# size    : {args.cols}x{args.rows}")
    print(f"# real    : {' '.join(real_cmd)}")
    print(f"# embed   : {' '.join(embed_cmd)}")
    print()

    # Warm the cargo build first so its own output doesn't pollute the pty capture.
    subprocess.run(
        ["cargo", "build", "--quiet", "-p", "extui-neovim", "--example", "embed"],
        cwd=REPO, check=True,
    )

    real_bytes = capture(real_cmd, REPO, args.rows, args.cols, args.settle)
    embed_bytes = capture(embed_cmd, REPO, args.rows, args.cols, args.settle)

    def parse(data: bytes) -> pyte.Screen:
        screen = pyte.Screen(args.cols, args.rows)
        stream = pyte.Stream(screen)
        feed(screen, stream, data)
        return screen

    real_screen = parse(real_bytes)
    embed_screen = parse(embed_bytes)

    real_grid = grid(real_screen)
    embed_grid = grid(embed_screen)

    if args.show_grids:
        print("=== real ===")
        for row in real_grid:
            print(render_row(row))
        print("=== embed ===")
        for row in embed_grid:
            print(render_row(row))
        print()

    n, report = diff_grids(real_grid, embed_grid, max_mismatches=args.max)
    total = args.rows * args.cols
    print(f"mismatches: {n} / {total} cells")
    for line in report:
        print(line)
    return 0 if n == 0 else 1


if __name__ == "__main__":
    sys.exit(main())
