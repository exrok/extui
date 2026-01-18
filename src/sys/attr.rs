use std::{
    io, mem,
    os::fd::{AsRawFd, BorrowedFd},
};

use super::{Termios, cvt};

pub fn get_terminal_attr(fd: BorrowedFd) -> io::Result<Termios> {
    unsafe {
        let mut termios = mem::zeroed();
        cvt(libc::tcgetattr(fd.as_raw_fd(), &mut termios))?;
        Ok(termios)
    }
}

pub fn set_terminal_attr(fd: BorrowedFd, termios: &Termios) -> io::Result<()> {
    // cvt(unsafe { libc::tcsetattr(fd.as_raw_fd(), libc::TCSANOW, termios) }).and(Ok(()))
    cvt(unsafe { libc::tcsetattr(fd.as_raw_fd(), libc::TCSAFLUSH, termios) }).and(Ok(()))
}

pub fn raw_terminal_attr(termios: &mut Termios) {
    unsafe { libc::cfmakeraw(termios) }
}
// pub fn make_raw_with_signals(termios: &mut Termios) {
//     // cfmakeraw does the hard work of turning off canonical mode, echo, etc.
//     unsafe { libc::cfmakeraw(termios) };

//     // But we want to re-enable the ISIG flag so that Ctrl-C, Ctrl-Z, etc.
//     // still generate signals.
//     termios.c_lflag |= libc::ISIG;
// }
