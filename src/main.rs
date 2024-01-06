use std::cell::RefCell;
use std::env;
use std::io;
use std::io::Error;
use std::io::Read;
use std::io::Write;
use std::io::{stderr, stdin};
use std::mem;
use std::process::ExitCode;

const BS: u8 = 0x08;
const CR: u8 = 0x0d;
const CSI: u8 = b'[';
const CTRL_C: u8 = 0x03;
const ESC: u8 = 0x1b;
const LF: u8 = 0x0a;

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
    prompt_len: usize,

    buf: Vec<u8>,
    pos: usize,     // offset into virtual input buffer, copied after accept
    newpos: usize,  // offset into virtual input buffer, mutating during accept
    termpos: usize, // offset from start of first line, including prompt
    state: State,
    args: Vec<u8>,
    cols: usize,
    debug: bool,
    initial: Option<Vec<u8>>,

    log: std::fs::File,
}

impl Termline {
    fn new(input: Box<dyn Read>, output: Box<dyn Write>, prompt: Vec<u8>) -> Result<Self, Error> {
        let log = std::fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open("termline.log")
            .unwrap();

        let prompt_len = prompt.len();

        Ok(Self {
            input,
            output,
            prompt,
            prompt_len,
            buf: Vec::new(),
            pos: 0,
            newpos: 0,
            termpos: 0,
            state: State::Normal,
            args: Vec::new(),
            cols: Self::get_width().unwrap_or(80),
            debug: false,
            initial: None,
            log,
        })
    }

    fn stdio(prompt: Vec<u8>) -> Result<Self, Error> {
        Self::new(Box::new(stdin()), Box::new(stderr()), prompt)
    }

    fn set_debug(&mut self, debug: bool) {
        self.debug = debug;
    }

    fn set_initial(&mut self, initial: Vec<u8>) {
        self.initial = Some(initial);
    }

    fn run(&mut self) -> Result<Vec<u8>, Error> {
        write!(self.log, "\nrun() cols:{}\n", self.cols)?;

        let termios_orig = Self::set_raw();
        let mut bufin = [0; 16];

        Self::listen_for_window_resize()?;

        self.output.write_all(&[CR, ESC, b'[', b'J'])?;
        self.output.write_all(&self.prompt)?;
        self.output.flush()?;

        self.termpos += self.prompt_len;

        let mut run = true;
        let mut success = false;
        while run {
            if Self::has_window_resized() {
                self.cols = Self::get_width()?;
            }

            // Push the --initial input into the state machine.
            // TODO: don't duplicate so much logic from self.input.read(...)
            if let Some(init) = self.initial.take() {
                for x in init {
                    match self.accept(x) {
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
                    }
                }
            }

            match self.input.read(&mut bufin) {
                Ok(len) => {
                    write!(
                        self.log,
                        "read() → len:{len} {}\n",
                        fmt_bytes(&bufin[0..len])
                    )?;

                    for n in 0..len {
                        match bufin[n] {
                            CTRL_C => run = false,
                            CR | LF => (run, success) = (false, true),
                            byte => match self.accept(byte) {
                                Ok(result) => match result {
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
                Err(_) => (),
            };
        }

        unsafe {
            Self::check_c_err(libc::tcsetattr(
                libc::STDERR_FILENO,
                0,
                &mut termios_orig.unwrap(),
            ))?;
        }

        write!(self.output, "\x1b[J\n")?; // clear from cursor to end of screen, newline

        if success {
            Ok(self.buf.to_owned())
        } else {
            Err(std::io::ErrorKind::Interrupted.into())
        }
    }

    fn accept(&mut self, b: u8) -> Result<Option<Vec<u8>>, Error> {
        write!(self.log, "accept({})\n", fmt_byte(b))?;

        Ok(match self.state {
            State::Normal => match b {
                0x20..=0x7e /* printable ASCII */ => {
                    write!(
                        self.log,
                        "'{}' -> pos:{}/len:{}\n",
                        b as char,
                        self.pos,
                        self.buf.len(),

                    )?;
                    self.buf.insert(self.pos, b);
                    self.newpos += 1;

                    self.update_line()?

                    //let tail = &self.buf[self.pos..];

                    //let mut to_emit = vec![b];

                    //// wrap to the next line if we're at the end
                    //if (self.pos + self.prompt.len()) % self.cols == 0 {
                    //    write!(self.log, "wrap at (prompt:{} + pos:{} = term_pos:{}) % cols:{} = {}\n", self.prompt_len, self.pos, self.term_pos(), self.cols, self.term_pos() % self.cols)?;

                    //    // ensure we're at the start of the next line by spacing on further, then
                    //    // going to start of line.
                    //    // https://stackoverflow.com/a/31360700
                    //    to_emit.extend_from_slice(&[b' ', ESC, CSI, b'G']); // go to start of next line
                    //    to_emit.extend_from_slice(&[ESC, CSI, b'K']); // clear to end of line
                    //}

                    //if tail.len() > 0 {
                    //    to_emit.extend_from_slice(tail);
                    //    to_emit.extend_from_slice(&[ESC, b'[', b'K']); // CSI EL (erase in line)
                    //    // CSI CUB tail.len()
                    //    to_emit.extend_from_slice(&[ESC, b'[']);
                    //    to_emit.extend_from_slice(&tail.len().to_string().as_bytes());
                    //    to_emit.extend_from_slice(&[b'D']);
                    //}
                    //Some(to_emit)
                }
                0x7f /* backspace */ => {
                    if self.pos >= 1 {
                        let mut to_emit = vec![];

                        self.pos -= 1;
                        self.buf.remove(self.pos);

                        // check for wrap
                        if (self.pos + self.prompt.len()) % self.cols == self.cols - 1 {

                            // unwrap up to the end of the previous line...

                            // CUU: cursor up
                            to_emit.extend_from_slice(&[ESC, CSI, b'A']);

                            // CHA: cursor horizontal absolute
                            to_emit.extend_from_slice(&[ESC, CSI]);
                            to_emit.extend_from_slice(&(self.cols + 1).to_string().as_bytes());
                            to_emit.extend_from_slice(&[b'G']);
                        }

                        let tail = &self.buf[self.pos..];
                        if tail.len() == 0 {
                            // backspace, overwrite with space, backspace
                            to_emit.extend_from_slice(&[BS, b' ', BS]);
                        } else {
                            to_emit.push(BS);

                            to_emit.extend_from_slice(&[ESC, b'[', b'K']); // clear to end of line
                            to_emit.extend_from_slice(&[ESC, b'[', b'J']); // clear to end of screen

                            let remaining_cols = self.cols.saturating_sub((self.term_pos()) % self.cols);
                            let mut remaining_tail = tail;

                            // emit the remainder columns of the current row
                            if remaining_tail.len() > 0 && remaining_cols > 0 {
                                let max = remaining_cols.clamp(0, remaining_tail.len());
                                to_emit.extend_from_slice(&tail[0..max]);
                                remaining_tail = &remaining_tail[max..]
                            }

                            for chunk in remaining_tail.chunks(self.cols) {
                                if remaining_tail.len() > 0 {
                                    to_emit.extend_from_slice(&[CR, LF]);
                                    to_emit.extend_from_slice(&chunk);
                                }
                            }

                            // TODO: jump back/up to position

                            let plen = self.prompt.len();
                            let actual_x = (plen + self.buf.len()) % self.cols;
                            let actual_y = (plen + self.buf.len()) / self.cols;
                            let wanted_x = (plen + self.pos) % self.cols;
                            let wanted_y = (plen + self.pos) / self.cols;
                            let delta_x = wanted_x as i32 - actual_x as i32;
                            let delta_y = wanted_y as i32 - actual_y as i32;

                            // self.msg.replace_range(
                            //     ..,
                            //     &format!(
                            //         "prompt+buf:{} actual_x:{actual_x} actual_y:{actual_y} wanted_x:{wanted_x} wanted_y:{wanted_y} delta_x:{delta_x} delta_y:{delta_y}",
                            //         self.prompt_len + self.buf.len(),
                            //     )
                            // );

                            if delta_x != 0 {
                                // CHA: Cursor Horizontal Absolute
                                to_emit.extend_from_slice(&[ESC, b'[']);
                                to_emit.extend_from_slice((wanted_x + 1).to_string().as_bytes());
                                to_emit.extend_from_slice(&[b'G']);
                            }

                            if delta_y < 0 {
                                // CUU
                                to_emit.extend_from_slice(&[ESC, b'[']);
                                to_emit.extend_from_slice(delta_y.abs().to_string().as_bytes());
                                to_emit.extend_from_slice(&[b'A']);
                            } else if delta_y > 0 {
                                // CUD
                                to_emit.extend_from_slice(&[ESC, b'[']);
                                to_emit.extend_from_slice(delta_y.to_string().as_bytes());
                                to_emit.extend_from_slice(&[b'B']);
                            }

                            // // CSI CUB tail.len()
                            // to_emit.extend_from_slice(&[ESC, b'[']);
                            // to_emit.extend_from_slice(&tail.len().to_string().as_bytes());
                            // to_emit.extend_from_slice(&[b'D']);
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
                    // self.msg.replace_range(.., &format!("unhandled char: {b:#04x}"));
                    None
                }
            },
            State::Esc => match b {
                b'[' => {
                    self.transition(State::CSI);
                    None
                }
                _ => {
                    // self.msg
                    //     .replace_range(.., &format!("unhandled escape: {b:#04x}"));
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
                        if (self.term_pos()) % self.cols == 0 {
                            Some(vec![CR, LF])
                        } else {
                            Some(vec![ESC, b'[', b'C'])
                        }
                    } else {
                        // self.msg.replace_range(.., &format!("CUF rejected"));
                        None
                    }
                }
                // CUB: Cursor Back
                b'D' => {
                    self.transition(State::Normal);
                    if self.pos > 0 {
                        self.pos -= 1;
                        let mut to_emit = vec![];
                        if (self.term_pos()) % self.cols == self.cols - 1 {
                            // CUU
                            to_emit.extend_from_slice(&[ESC, b'[', b'A']);
                            // CHA to last column
                            to_emit.extend_from_slice(&[ESC, b'[']);
                            to_emit.extend_from_slice(&self.cols.to_string().as_bytes());
                            to_emit.extend_from_slice(&[b'G']);
                        } else {
                            to_emit.extend_from_slice(&[ESC, b'[', b'D']); // CUB
                        }
                        Some(to_emit)
                    } else {
                        // self.msg.replace_range(.., &format!("CUB rejected"));
                        None
                    }
                }
                // vt input sequences (home, insert, delete etc)
                b'~' => match self.args[..] {
                    // DEL (CSI 3 ~)
                    [b'3'] => {
                        if self.pos < self.buf.len() {
                            let _x = self.buf.remove(self.pos);
                            // self.msg.replace_range(.., &format!("DEL: {:#04X}", x));
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
                        // self.msg
                        //     .replace_range(.., &format!("unhandled VT input {:?}~", self.args));
                        self.transition(State::Normal);
                        None
                    }
                },
                _ => {
                    // self.msg
                    //     .replace_range(.., &format!("unhandled CSI {b:#04x}"));
                    self.transition(State::Normal);
                    None
                }
            },
        })
    }

    fn update_line(&mut self) -> Result<Option<Vec<u8>>, Error> {
        // pos_delta: where we are now in the input buffer vs where we were before
        let pos_delta = self.newpos - self.pos;

        let tail = &self.buf[self.pos..];

        write!(
            self.log,
            "update_line() pos:{} newpos:{} pos_delta:{} termpos:{} len:{} tail.len:{}\n",
            self.pos,
            self.newpos,
            pos_delta,
            self.term_pos(),
            self.buf.len(),
            tail.len(),
        )?;

        let mut to_emit = vec![];

        //// wrap to the next line if we're at the end
        //if (self.pos + self.prompt.len()) % self.cols == 0 {
        //    write!(
        //        self.log,
        //        "wrap at (prompt:{} + pos:{} = term_pos:{}) % cols:{} = {}\n",
        //        self.prompt_len,
        //        self.pos,
        //        self.term_pos(),
        //        self.cols,
        //        self.term_pos() % self.cols
        //    )?;

        //    // ensure we're at the start of the next line by spacing on further, then
        //    // going to start of line.
        //    // https://stackoverflow.com/a/31360700
        //    to_emit.extend_from_slice(&[b' ', ESC, CSI, b'G']); // go to start of next line
        //    to_emit.extend_from_slice(&[ESC, CSI, b'K']); // clear to end of line
        //}

        if tail.len() > 0 {
            to_emit.extend_from_slice(tail);
            to_emit.extend_from_slice(&[ESC, b'[', b'K']); // CSI EL (erase in line)

            // // CSI CUB tail.len()
            // to_emit.extend_from_slice(&[ESC, b'[']);
            // to_emit.extend_from_slice(&tail.len().to_string().as_bytes());
            // to_emit.extend_from_slice(&[b'D']);
        }

        let plen = self.prompt.len();
        let actual_x = (plen + self.buf.len()) % self.cols;
        let actual_y = (plen + self.buf.len()) / self.cols;
        let wanted_x = (plen + self.pos) % self.cols;
        let wanted_y = (plen + self.pos) / self.cols;
        let delta_x = wanted_x as i32 - actual_x as i32;
        let delta_y = wanted_y as i32 - actual_y as i32;

        if delta_x != 0 {
            // CHA: Cursor Horizontal Absolute
            to_emit.extend_from_slice(&[ESC, b'[']);
            to_emit.extend_from_slice((wanted_x + 1).to_string().as_bytes());
            to_emit.extend_from_slice(&[b'G']);
        }

        if delta_y < 0 {
            // CUU
            to_emit.extend_from_slice(&[ESC, b'[']);
            to_emit.extend_from_slice(delta_y.abs().to_string().as_bytes());
            to_emit.extend_from_slice(&[b'A']);
        } else if delta_y > 0 {
            // CUD
            to_emit.extend_from_slice(&[ESC, b'[']);
            to_emit.extend_from_slice(delta_y.to_string().as_bytes());
            to_emit.extend_from_slice(&[b'B']);
        }

        self.pos = self.newpos;

        write!(self.log, "to_emit:{}\n", fmt_bytes(&to_emit))?;

        Ok(Some(to_emit))
    }

    // Position including prompt length
    fn term_pos(&self) -> usize {
        self.prompt_len + self.pos
    }

    fn transition(&mut self, s: State) {
        self.args.clear();
        self.state = s;
    }

    fn get_width() -> Result<usize, Error> {
        unsafe {
            let mut ws: libc::winsize = mem::zeroed();
            Self::check_c_err(libc::ioctl(libc::STDIN_FILENO, libc::TIOCGWINSZ, &mut ws))?;
            Ok(ws.ws_col as usize)
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

    let mut errors: Vec<String> = Vec::new();
    let mut args = env::args().into_iter().skip(1);
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--debug" => termline.set_debug(true),
            "--initial" => {
                if let Some(val) = args.next() {
                    termline.set_initial(val.into())
                }
            }
            _ => {
                errors.push(format!("unknown: {}", arg));
            }
        }
    }
    if !errors.is_empty() {
        let mut out = stderr().lock();
        for e in errors {
            write!(out, "args: {}\n", e).unwrap();
        }
        return ExitCode::FAILURE;
    }

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

fn fmt_bytes(bytes: &[u8]) -> String {
    format!(
        "[{}]",
        bytes
            .iter()
            .map(|&byte| fmt_byte(byte))
            .collect::<Vec<String>>()
            .join(" ")
    )
}

fn fmt_byte(b: u8) -> String {
    let tmp: String;
    format!(
        "{:02X}:{}",
        b,
        match b {
            CTRL_C => "Ctrl-C",
            BS => "BS",
            LF => "LF",
            CR => "CR",
            ESC => "ESC",
            32..=126 => {
                tmp = format!("\"{}\"", b as char);
                &*tmp
            }
            _ => "?",
        }
        .to_string()
    )
}
