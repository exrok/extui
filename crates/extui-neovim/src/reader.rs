//! Background thread that decodes MessagePack-RPC frames from Neovim stdout.
//!
//! The reader owns a [`std::process::ChildStdout`] and a shared handle to
//! the [`GridState`]. It reads chunks into a scratch buffer, detects whole
//! top-level frames with [`crate::msgpack::frame_len`], and applies
//! accumulated redraw events to the shared state when a `flush` event is
//! observed. Each batch is applied under a single mutex lock.

use std::io::Read;
use std::process::ChildStdout;
use std::sync::mpsc::SyncSender;
use std::sync::{Arc, Mutex};

use std::collections::{HashMap, VecDeque};

use crate::NeovimWaker;
use crate::grid::GridState;
use crate::msgpack::{self, Reader};

type PendingRequests = Arc<Mutex<HashMap<u32, SyncSender<Vec<u8>>>>>;
type Notifications = Arc<Mutex<VecDeque<String>>>;

/// Runs the reader loop until stdout closes or an unrecoverable
/// error occurs.
///
/// Meant to be used as the entry point of a dedicated thread and does
/// not return until the child stream reaches EOF. On any terminal
/// condition the shared state is marked as dead and the waker is
/// pulsed so the host main loop observes the change on its next poll.
pub fn run(
    mut stdout: ChildStdout,
    state: Arc<Mutex<GridState>>,
    pending_requests: PendingRequests,
    notifications: Notifications,
    waker: NeovimWaker,
) {
    let mut scratch: Vec<u8> = Vec::with_capacity(64 * 1024);
    let mut tmp = [0u8; 8192];
    loop {
        let n = match stdout.read(&mut tmp) {
            Ok(0) => {
                set_dead(&state, &pending_requests, &waker);
                return;
            }
            Ok(n) => n,
            Err(e) if e.kind() == std::io::ErrorKind::Interrupted => continue,
            Err(_) => {
                set_dead(&state, &pending_requests, &waker);
                return;
            }
        };
        scratch.extend_from_slice(&tmp[..n]);

        let mut consumed = 0;
        let mut wake_needed = false;
        loop {
            let Some(len) = msgpack::frame_len(&scratch[consumed..]) else {
                break;
            };
            let frame = &scratch[consumed..consumed + len];
            match apply_frame(frame, &state, &pending_requests, &notifications) {
                Ok(true) => wake_needed = true,
                Ok(false) => {}
                Err(()) => {
                    set_dead(&state, &pending_requests, &waker);
                    return;
                }
            }
            consumed += len;
        }
        if consumed > 0 {
            scratch.drain(..consumed);
        }
        if wake_needed {
            waker.wake();
        }
    }
}

fn set_dead(
    state: &Arc<Mutex<GridState>>,
    pending_requests: &PendingRequests,
    waker: &NeovimWaker,
) {
    if let Ok(mut guard) = state.lock() {
        guard.mark_dead();
    }
    if let Ok(mut pending) = pending_requests.lock() {
        pending.clear();
    }
    waker.wake();
}

fn apply_frame(
    bytes: &[u8],
    state: &Arc<Mutex<GridState>>,
    pending_requests: &PendingRequests,
    notifications: &Notifications,
) -> Result<bool, ()> {
    let mut r = Reader::new(bytes);
    let arr_len = r.read_array_len().map_err(|_| ())?;
    if arr_len < 3 {
        return Err(());
    }
    let msg_type = r.read_u64().map_err(|_| ())?;
    match msg_type {
        1 => {
            let msgid = r.read_u64().map_err(|_| ())? as u32;
            let sender = pending_requests.lock().map_err(|_| ())?.remove(&msgid);
            if let Some(sender) = sender {
                let _ = sender.send(bytes.to_vec());
            } else {
                for _ in 2..arr_len {
                    r.skip().map_err(|_| ())?;
                }
            }
            Ok(false)
        }
        2 => {
            let method = r.read_str().map_err(|_| ())?;
            if method == "redraw" {
                let Ok(mut guard) = state.lock() else {
                    return Err(());
                };
                guard.apply_redraw(&mut r).map_err(|_| ())?;
                return Ok(guard.is_dirty());
            }
            let Ok(mut queue) = notifications.lock() else {
                return Err(());
            };
            queue.push_back(method.to_string());
            Ok(true)
        }
        _ => {
            for _ in 1..arr_len {
                r.skip().map_err(|_| ())?;
            }
            Ok(false)
        }
    }
}
