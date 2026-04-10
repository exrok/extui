//! Background thread that decodes MessagePack-RPC frames from Neovim stdout.
//!
//! The reader owns a [`std::process::ChildStdout`] and a shared handle to
//! the [`GridState`]. It reads chunks into a scratch buffer, detects whole
//! top-level frames with [`crate::msgpack::frame_len`], and applies
//! accumulated redraw events to the shared state when a `flush` event is
//! observed. Each batch is applied under a single mutex lock.

use std::io::Read;
use std::process::ChildStdout;
use std::sync::{Arc, Mutex};

use extui::event::polling::Waker;

use crate::grid::GridState;
use crate::msgpack::{self, Reader};

/// Runs the reader loop until stdout closes or an unrecoverable error occurs.
///
/// This is the thread entry point; it never returns until EOF. On any
/// terminal condition it marks the shared state as dead, clears the dirty
/// flag, and wakes the external waker so the main loop can observe it.
pub fn run(mut stdout: ChildStdout, state: Arc<Mutex<GridState>>, waker: &'static Waker) {
    let mut scratch: Vec<u8> = Vec::with_capacity(64 * 1024);
    let mut tmp = [0u8; 8192];
    loop {
        let n = match stdout.read(&mut tmp) {
            Ok(0) => {
                set_dead(&state, waker);
                return;
            }
            Ok(n) => n,
            Err(e) if e.kind() == std::io::ErrorKind::Interrupted => continue,
            Err(_) => {
                set_dead(&state, waker);
                return;
            }
        };
        scratch.extend_from_slice(&tmp[..n]);

        let mut consumed = 0;
        let mut made_visible = false;
        loop {
            let Some(len) = msgpack::frame_len(&scratch[consumed..]) else {
                break;
            };
            let frame = &scratch[consumed..consumed + len];
            match apply_frame(frame, &state) {
                Ok(true) => made_visible = true,
                Ok(false) => {}
                Err(()) => {
                    set_dead(&state, waker);
                    return;
                }
            }
            consumed += len;
        }
        if consumed > 0 {
            scratch.drain(..consumed);
        }
        if made_visible {
            let _ = waker.wake();
        }
    }
}

fn set_dead(state: &Arc<Mutex<GridState>>, waker: &'static Waker) {
    if let Ok(mut guard) = state.lock() {
        guard.mark_dead();
    }
    let _ = waker.wake();
}

fn apply_frame(bytes: &[u8], state: &Arc<Mutex<GridState>>) -> Result<bool, ()> {
    let mut r = Reader::new(bytes);
    let arr_len = r.read_array_len().map_err(|_| ())?;
    if arr_len < 3 {
        return Err(());
    }
    let msg_type = r.read_u64().map_err(|_| ())?;
    match msg_type {
        1 => {
            for _ in 1..arr_len {
                r.skip().map_err(|_| ())?;
            }
            Ok(false)
        }
        2 => {
            let method = r.read_str().map_err(|_| ())?;
            if method != "redraw" {
                return Ok(false);
            }
            let Ok(mut guard) = state.lock() else {
                return Err(());
            };
            guard.apply_redraw(&mut r).map_err(|_| ())?;
            Ok(guard.is_dirty())
        }
        _ => {
            for _ in 1..arr_len {
                r.skip().map_err(|_| ())?;
            }
            Ok(false)
        }
    }
}
