use std::cell::RefCell;
use std::io;
use std::io::Error;
use std::io::Read;
use std::io::Write;
use std::io::{stderr, stdin};
use std::mem;
use std::process::ExitCode;

const CTRL_C: u8 = 0x03;
const BS: u8 = 0x08;
const LF: u8 = 0x0a;
const CR: u8 = 0x0d;
const ESC: u8 = 0x1b;

const DEBUG: bool = true;

thread_local!(static SIGWINCH: RefCell<bool> = false.into());

#[derive(Debug)]
enum State {
    Normal,
    Esc,
    CSI,
}

struct Termline {
    input: Box<dyn Read>,
    output: Box<dyn Write>,
    prompt: Vec<u8>,

    buf: Vec<u8>,
    pos: usize,
    state: State,
    args: Vec<u8>,
    msg: String,
    cols: Option<u16>,
}

impl Termline {
    fn new(input: Box<dyn Read>, output: Box<dyn Write>, prompt: Vec<u8>) -> Result<Self, Error> {
        Ok(Self {
            input,
            output,
            prompt: prompt.into(),
            buf: Vec::new(),
            pos: 0,
            state: State::Normal,
            args: Vec::new(),
            msg: String::new(),
            cols: None,
        })
    }

    fn stdio(prompt: Vec<u8>) -> Result<Self, Error> {
        Self::new(Box::new(stdin()), Box::new(stderr()), prompt)
    }

    fn run(&mut self) -> Result<Vec<u8>, Error> {
        let termios_orig = Self::set_raw();
        let mut bufin = [0; 16];

        self.cols = Some(Self::get_width()?);
        Self::listen_for_window_resize()?;

        self.output.write_all(&self.prompt)?;
        self.output.flush()?;

        let mut run = true;
        let mut success = false;
        while run {
            if Self::has_window_resized() {
                self.cols = Some(Self::get_width()?);
            }

            if DEBUG {
                let mut debug = vec![];
                // ensure there's spare lines underneath
                debug.extend_from_slice(&[LF, LF, LF, LF, LF, ESC, b'[', b'5', b'A']); // add new lines below prompt, scroll back up
                debug.extend_from_slice(&[ESC, b'7']); // DECSC: DEC Save Cursor
                debug.extend_from_slice(&[ESC, b'[', b'J']); // Erase in Display (cursor to end)
                debug.extend_from_slice(&[ESC, b'[', b'2', b'm']); // dim text
                debug.extend(
                    format!(
                        "\r\nbuf: {}\r\n     {}^pos:{} len:{}",
                        String::from_utf8_lossy(&self.buf),
                        " ".repeat(self.pos),
                        self.pos,
                        self.buf.len(),
                    )
                    .bytes(),
                );
                match self.cols {
                    None => debug.extend(format!("\r\ncols: ?").bytes()),
                    Some(cols) => debug.extend(format!("\r\ncols: {cols}").bytes()),
                }
                debug.extend(
                    format!(
                        "\r\nstate: {:?}, args: [{}]",
                        self.state,
                        String::from_utf8_lossy(&self.args),
                    )
                    .bytes(),
                );
                debug.extend(format!("\r\nmsg: {}", self.msg).bytes());
                debug.extend_from_slice(&[ESC, b'8']); // DECRC: DEC Restore Cursor
                self.output.write_all(&debug)?;
                self.output.flush()?
            }

            match self.input.read(&mut bufin) {
                Ok(size) => {
                    for n in 0..size {
                        match bufin[n] {
                            CTRL_C => run = false,
                            CR | LF => (run, success) = (false, true),
                            n => match self.accept(n) {
                                Ok(r) => match r {
                                    Some(out) => {
                                        self.output.write_all(&out)?;
                                        self.output.flush()?;
                                    }
                                    None => (),
                                },

                                Err(err) => {
                                    panic!("{}", err);
                                }
                            },
                        }
                    }
                }
                Err(e) => self.msg.replace_range(.., &format!("read error: {e}")),
            };
        }

        unsafe {
            Self::check_c_err(libc::tcsetattr(
                libc::STDERR_FILENO,
                0,
                &mut termios_orig.unwrap(),
            ))?;
        }

        write!(self.output, "\x1b[J\n")?;

        if success {
            Ok(self.buf.to_owned())
        } else {
            Err(std::io::ErrorKind::Interrupted.into())
        }
    }

    fn accept(&mut self, b: u8) -> Result<Option<Vec<u8>>, Error> {
        Ok(match self.state {
            State::Normal => match b {
                0x20..=0x7e /* printable ASCII */ => {
                    self.buf.insert(self.pos, b);
                    self.pos += 1;
                    let tail = &self.buf[self.pos..];

                    let mut to_emit = vec![b];
                    if tail.len() > 0 {
                        to_emit.extend_from_slice(tail);
                        // CSI CUB tail.len()
                        to_emit.extend_from_slice(&[ESC, b'[']);
                        to_emit.extend_from_slice(&tail.len().to_string().as_bytes());
                        to_emit.extend_from_slice(&[b'D']);
                    }
                    Some(to_emit)
                }
                0x7f /* backspace */ => {
                    if self.pos >= 1 {
                        self.pos -= 1;
                        self.buf.remove(self.pos);

                        let tail = &self.buf[self.pos..];
                        let mut to_emit = vec![];
                        if tail.len() == 0 {
                            to_emit.extend_from_slice(&[0x08, b' ', 0x08]);
                        } else {
                            to_emit.push(0x08);
                            to_emit.extend_from_slice(&[ESC, b'[', b'K']);
                            to_emit.extend_from_slice(&tail);
                            // CSI CUB tail.len()
                            to_emit.extend_from_slice(&[ESC, b'[']);
                            to_emit.extend_from_slice(&tail.len().to_string().as_bytes());
                            to_emit.extend_from_slice(&[b'D']);
                        }
                        Some(to_emit)
                    } else {
                        None
                    }
                }
                ESC => {
                    self.transition(State::Esc);
                    None
                }
                _ => {
                    self.msg.replace_range(.., &format!("unhandled char: {b:#04x}"));
                    None
                }
            },
            State::Esc => match b {
                b'[' => {
                    self.transition(State::CSI);
                    None
                }
                _ => {
                    self.msg
                        .replace_range(.., &format!("unhandled escape: {b:#04x}"));
                    self.transition(State::Normal);
                    None
                }
            },
            State::CSI => match b {
                b'0'..=b'9' | b';' => {
                    self.args.push(b);
                    None
                }
                // CUF: Cursor Forward
                b'C' => {
                    self.transition(State::Normal);
                    if self.pos < self.buf.len() {
                        self.pos += 1;
                        Some(vec![ESC, b'[', b'C'])
                    } else {
                        self.msg.replace_range(.., &format!("CUF rejected"));
                        None
                    }
                }
                // CUB: Cursor Back
                b'D' => {
                    self.transition(State::Normal);
                    if self.pos > 0 {
                        self.pos -= 1;
                        Some(vec![BS]) // BS instead of CUB to handle wrapping
                    } else {
                        self.msg.replace_range(.., &format!("CUB rejected"));
                        None
                    }
                }
                // vt input sequences (home, insert, delete etc)
                b'~' => match self.args[..] {
                    // DEL (CSI 3 ~)
                    [b'3'] => {
                        if self.pos < self.buf.len() {
                            let x = self.buf.remove(self.pos);
                            self.msg.replace_range(.., &format!("DEL: {:#04X}", x));
                            let tail = &self.buf[self.pos..];
                            let mut emit = vec![];
                            if tail.len() > 0 {
                                emit.extend_from_slice(tail);
                                emit.extend_from_slice(&[ESC, b'[', b'K']);

                                // go back
                                emit.extend_from_slice(&[ESC, b'[']);
                                emit.extend_from_slice(&tail.len().to_string().as_bytes());
                                emit.extend_from_slice(&[b'D']);
                            } else {
                                emit.extend_from_slice(&[ESC, b'[', b'K']);
                            }

                            self.transition(State::Normal);
                            Some(emit)
                        } else {
                            self.transition(State::Normal);
                            None
                        }
                    }
                    _ => {
                        self.msg
                            .replace_range(.., &format!("unhandled VT input {:?}~", self.args));
                        self.transition(State::Normal);
                        None
                    }
                },
                _ => {
                    self.msg
                        .replace_range(.., &format!("unhandled CSI {b:#04x}"));
                    self.transition(State::Normal);
                    None
                }
            },
        })
    }

    fn transition(&mut self, s: State) {
        self.args.clear();
        self.state = s;
    }

    fn get_width() -> Result<u16, Error> {
        unsafe {
            let mut ws: libc::winsize = mem::zeroed();
            Self::check_c_err(libc::ioctl(libc::STDIN_FILENO, libc::TIOCGWINSZ, &mut ws))?;
            Ok(ws.ws_col)
        }
    }

    fn listen_for_window_resize() -> Result<(), Error> {
        extern "C" fn handle_sigwinch(_: libc::c_int) {
            SIGWINCH.with(|val| *val.borrow_mut() = true);
        }

        Self::check_c_err(unsafe {
            libc::sigaction(
                libc::SIGWINCH,
                &libc::sigaction {
                    sa_flags: 0,
                    sa_mask: 0,
                    sa_sigaction: handle_sigwinch as libc::sighandler_t,
                },
                std::ptr::null_mut(),
            )
        })
    }

    fn has_window_resized() -> bool {
        SIGWINCH.with(|val| {
            if *val.borrow() {
                *val.borrow_mut() = false;
                true
            } else {
                false
            }
        })
    }

    fn set_raw() -> Result<libc::termios, io::Error> {
        Ok(unsafe {
            let mut termios: libc::termios = mem::zeroed();
            Self::check_c_err(libc::tcgetattr(libc::STDERR_FILENO, &mut termios))?;
            let mut termios_raw = termios;
            libc::cfmakeraw(&mut termios_raw);
            Self::check_c_err(libc::tcsetattr(libc::STDERR_FILENO, 0, &termios_raw))?;
            termios
        })
    }

    fn check_c_err(x: libc::c_int) -> Result<(), Error> {
        if x == -1 {
            Err(io::Error::last_os_error())
        } else {
            Ok(())
        }
    }
}

fn main() -> ExitCode {
    let mut termline = Termline::stdio("termline> ".into()).unwrap();
    match termline.run() {
        Ok(result) => {
            println!("{}", String::from_utf8_lossy(&result));
            ExitCode::SUCCESS
        }
        Err(err) => {
            write!(stderr(), "Error: {}\n", err).unwrap();
            ExitCode::FAILURE
        }
    }
}
