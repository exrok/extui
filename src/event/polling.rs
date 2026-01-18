use libc::SIGWINCH;
use libc::{POLLERR, POLLHUP, POLLIN, c_int, poll, pollfd};
use std::io;
use std::mem;
use std::os::unix::io::AsRawFd;
use std::sync::OnceLock;
use std::sync::atomic::{AtomicBool, AtomicU64};
use std::time::Duration;
pub use waker::Waker;

use crate::event::Polled;

static GLOBAL_WAKER: OnceLock<Waker> = OnceLock::new();
static RESIZE_COUNTER: AtomicU64 = AtomicU64::new(0);
static TERMINATION_REQUESTED: AtomicBool = AtomicBool::new(false);

/// Returns the number of resize events that have occurred.
///
/// The counter is incremented each time a `SIGWINCH` signal is received.
pub fn resize_count() -> u64 {
    RESIZE_COUNTER.load(std::sync::atomic::Ordering::Relaxed)
}

/// Returns a reference to the global waker if initialized.
pub fn global_waker() -> Option<&'static Waker> {
    GLOBAL_WAKER.get()
}

/// Returns `true` if a termination signal has been received.
///
/// Checks for `SIGTERM`, `SIGINT`, or `SIGHUP` signals.
pub fn termination_requested() -> bool {
    TERMINATION_REQUESTED.load(std::sync::atomic::Ordering::Relaxed)
}

/// Configuration for the global waker signal handlers.
#[derive(Default)]
pub struct GlobalWakerConfig {
    /// Enables `SIGWINCH` (resize) signal handling.
    pub resize: bool,
    /// Enables `SIGTERM`, `SIGINT`, and `SIGHUP` signal handling.
    pub termination: bool,
}

/// Initializes the global waker with the specified signal handlers.
///
/// # Errors
///
/// Returns an error if the waker cannot be created.
pub fn initialize_global_waker(config: GlobalWakerConfig) -> std::io::Result<&'static Waker> {
    fn termination_handler() {
        TERMINATION_REQUESTED.store(true, std::sync::atomic::Ordering::Relaxed);
        if let Some(waker) = GLOBAL_WAKER.get() {
            let _ = waker.wake();
        }
    }
    let waker = GLOBAL_WAKER.get_or_init(|| Waker::new().unwrap());
    if config.termination {
        unsafe {
            let _ = signal_hook_registry::register(libc::SIGTERM, termination_handler);
            let _ = signal_hook_registry::register(libc::SIGINT, termination_handler);
            let _ = signal_hook_registry::register(libc::SIGHUP, termination_handler);
        }
    }
    if config.resize {
        unsafe {
            let _ = signal_hook_registry::register(SIGWINCH, || {
                RESIZE_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                if let Some(waker) = GLOBAL_WAKER.get() {
                    let _ = waker.wake();
                }
            });
        }
    }

    Ok(waker)
}

/// Initializes the global waker with resize signal handling.
///
/// # Errors
///
/// Returns an error if the waker cannot be created.
pub fn resize_waker() -> std::io::Result<&'static Waker> {
    let waker = GLOBAL_WAKER.get_or_init(|| {
        let waker = Waker::new().expect("Waker to construct");
        unsafe {
            // todo fix:
            // OnceLock::get() may not awlays eb safe in signal uandler
            let _ = signal_hook_registry::register(SIGWINCH, || {
                if let Some(waker) = GLOBAL_WAKER.get() {
                    RESIZE_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                    let _ = waker.wake();
                }
            });
        }
        waker
    });
    Ok(waker)
}
// This module contains the public-facing Waker struct, but its internal
// implementation is chosen at compile time based on the target OS.
mod waker {
    use super::*;
    // --- Linux-Optimized Implementation using eventfd ---
    #[cfg(target_os = "linux")]
    pub use self::linux_impl::*;
    #[cfg(target_os = "linux")]
    mod linux_impl {
        use super::*;
        use libc::{EFD_CLOEXEC, EFD_NONBLOCK, eventfd};

        /// A thread-safe waker for Linux using the lightweight `eventfd` syscall.
        #[repr(transparent)]
        pub struct Waker {
            event_fd: c_int,
        }

        impl Waker {
            /// Creates a new waker.
            ///
            /// # Errors
            ///
            /// Returns an error if the underlying file descriptor cannot be created.
            pub fn new() -> io::Result<Self> {
                // unsafe: calling Linux-specific eventfd syscall.
                // EFD_CLOEXEC: close the fd on execve
                // EFD_NONBLOCK: reads/writes will not block
                let fd = unsafe { eventfd(0, EFD_CLOEXEC | EFD_NONBLOCK) };
                if fd == -1 {
                    Err(io::Error::last_os_error())
                } else {
                    Ok(Waker { event_fd: fd })
                }
            }

            /// Wakes up any thread waiting on this waker.
            ///
            /// # Errors
            ///
            /// Returns an error if the write operation fails.
            pub fn wake(&self) -> std::io::Result<()> {
                let value: u64 = 1;
                loop {
                    let res = unsafe {
                        libc::write(
                            self.event_fd,
                            &value as *const _ as *const libc::c_void,
                            mem::size_of::<u64>(),
                        )
                    };

                    if res == -1 {
                        let err = io::Error::last_os_error();
                        if err.kind() == io::ErrorKind::Interrupted {
                            continue;
                        }
                        return Err(err);
                    } else {
                        return Ok(());
                    }
                }
            }

            pub(crate) fn reset(&self) {
                let mut value: u64 = 0;
                // unsafe: reading from the eventfd to reset its counter.
                // Since it's non-blocking, this will either read the value or
                // return EAGAIN if it's already 0. In either case, the "woken"
                // state is cleared without blocking.
                let _ = unsafe {
                    libc::read(
                        self.event_fd,
                        &mut value as *mut _ as *mut libc::c_void,
                        mem::size_of::<u64>(),
                    )
                };
            }

            pub(crate) fn read_fd(&self) -> c_int {
                self.event_fd
            }
        }

        impl Drop for Waker {
            fn drop(&mut self) {
                // unsafe: closing the file descriptor.
                unsafe { libc::close(self.event_fd) };
            }
        }
    }

    // --- POSIX (macOS, BSD, etc.) Implementation using pipe ---
    #[cfg(not(target_os = "linux"))]
    pub use self::portable_impl::*;
    #[cfg(not(target_os = "linux"))]
    mod portable_impl {
        use super::*;
        use libc::{F_GETFL, F_SETFL, O_NONBLOCK, fcntl};

        /// A portable, thread-safe waker using a `pipe`.
        pub struct Waker {
            read_fd: c_int,
            write_fd: c_int,
        }

        impl Waker {
            pub fn new() -> io::Result<Self> {
                let mut fds: [c_int; 2] = [0; 2];
                // unsafe: calling C function from libc
                if unsafe { libc::pipe(fds.as_mut_ptr()) } == -1 {
                    return Err(io::Error::last_os_error());
                }

                let read_fd = fds[0];
                let write_fd = fds[1];

                // Set the read-end to non-blocking for the `reset()` method.
                let flags = unsafe { fcntl(read_fd, F_GETFL, 0) };
                if flags == -1 {
                    return Err(io::Error::last_os_error());
                }
                if unsafe { fcntl(read_fd, F_SETFL, flags | O_NONBLOCK) } == -1 {
                    return Err(io::Error::last_os_error());
                }

                Ok(Waker { read_fd, write_fd })
            }

            pub fn wake(&self) {
                let buf = [1u8];
                loop {
                    // unsafe: calling C function from libc
                    if unsafe { libc::write(self.write_fd, buf.as_ptr() as *const _, 1) } == -1 {
                        if io::Error::last_os_error().kind() == io::ErrorKind::Interrupted {
                            continue;
                        }
                        return;
                    } else {
                        return;
                    }
                }
            }

            pub(crate) fn reset(&self) {
                let mut buf = [0u8; 64];
                loop {
                    // unsafe: calling C function from libc
                    let res =
                        unsafe { libc::read(self.read_fd, buf.as_mut_ptr() as *mut _, buf.len()) };
                    if res <= 0 {
                        break;
                    }
                }
            }

            pub(crate) fn read_fd(&self) -> c_int {
                self.read_fd
            }
        }

        impl Drop for Waker {
            fn drop(&mut self) {
                unsafe {
                    libc::close(self.read_fd);
                    libc::close(self.write_fd);
                }
            }
        }
    }

    // Mark Waker as thread-safe. This is safe because the underlying OS calls
    // (pipe, eventfd, read, write, close) are thread-safe.
    unsafe impl Send for Waker {}
    unsafe impl Sync for Waker {}
}

// --- Polling Logic ---

pub(super) fn wait_on_fd_inner(
    fd: i32,
    waker: Option<&Waker>,
    timeout: Option<Duration>,
) -> io::Result<Polled> {
    // Use a stack-allocated array instead of a Vec to avoid heap allocation.
    // We know the maximum size is 2.
    let mut poll_fds: [pollfd; 2] = unsafe { mem::zeroed() };
    let mut nfds = 0;

    poll_fds[nfds] = pollfd {
        fd: fd.as_raw_fd(),
        events: POLLIN | POLLERR | POLLHUP,
        revents: 0,
    };
    nfds += 1;

    let waker_index = if let Some(w) = waker {
        poll_fds[nfds] = pollfd {
            fd: w.read_fd(),
            events: POLLIN | POLLERR | POLLHUP,
            revents: 0,
        };
        nfds += 1;
        Some(1)
    } else {
        None
    };

    let timeout_ms = match timeout {
        Some(d) => d.as_millis().try_into().unwrap_or(c_int::MAX),
        None => -1,
    };

    loop {
        let ret = unsafe { poll(poll_fds.as_mut_ptr(), nfds as _, timeout_ms) };

        if ret == -1 {
            let err = io::Error::last_os_error();
            if err.kind() != io::ErrorKind::Interrupted {
                return Err(err);
            }
        } else if ret == 0 {
            return Ok(Polled::TimedOut);
        } else {
            if let Some(idx) = waker_index
                && poll_fds[idx].revents != 0
            {
                if let Some(w) = waker {
                    w.reset();
                }
                return Ok(Polled::Woken);
            }

            if poll_fds[0].revents != 0 {
                return Ok(Polled::ReadReady);
            }
        }
    }
}
