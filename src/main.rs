use std::io;
use std::io::Error;
use std::io::Read;
use std::io::Write;
use std::io::{stdin, stdout};
use std::mem;

const CTRL_C: u8 = 0x03;
const ESC: u8 = 0x1b;
const DEBUG: bool = true;

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
    log: std::fs::File,
}

impl Termline {
    fn new(input: Box<dyn Read>, output: Box<dyn Write>, prompt: Vec<u8>) -> Result<Self, Error> {
        let log = std::fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open("termline.log")
            .unwrap();

        Ok(Self {
            input,
            output,
            prompt: prompt.into(),
            buf: Vec::new(),
            pos: 0,
            state: State::Normal,
            log,
        })
    }

    fn stdio(prompt: Vec<u8>) -> Result<Self, Error> {
        Self::new(Box::new(stdin()), Box::new(stdout()), prompt)
    }

    fn run(&mut self) -> Result<Vec<u8>, Error> {
        let termios_orig = Self::set_raw();
        let mut bufin = [0; 16];

        self.output.write_all(&self.prompt)?;
        self.output.flush()?;

        let mut run = true;
        while run {
            if DEBUG {
                let mut debug = vec![];
                // ensure there's two spare lines underneath
                debug.extend_from_slice(&[0x0a, 0x0a]); // new lines
                debug.extend_from_slice(&[ESC, b'[', b'2', b'A']); // CUU: Cursor Up
                debug.extend_from_slice(&[ESC, b'7']); // DECSC: DEC Save Cursor
                debug.extend_from_slice(&[0x0d, 0x0a]); // CR,LF
                debug.extend(
                    format!(
                        "\x1b[2m\x1b[G\x1b[Kbuf: {}\n\x1b[G\x1b[K     {}^pos:{} len:{}",
                        String::from_utf8_lossy(&self.buf),
                        " ".repeat(self.pos),
                        self.pos,
                        self.buf.len(),
                    )
                    .bytes(),
                );
                debug.extend_from_slice(&[ESC, b'8']); // DECRC: DEC Restore Cursor
                self.emit(&debug)?;
            }

            match self.input.read(&mut bufin) {
                Ok(size) => {
                    write!(
                        self.log,
                        "### bytes read: {}: {}\n",
                        size,
                        String::from_utf8_lossy(
                            &bufin[..size]
                                .iter()
                                .map(|x| std::ascii::escape_default(*x).collect::<Vec<u8>>())
                                .flatten()
                                .collect::<Vec<u8>>()
                        )
                    )?;

                    for n in 0..size {
                        let b = bufin[n];
                        if b == CTRL_C {
                            run = false;
                        }

                        write!(self.log, "b: {b:#04x}\n")?;

                        match self.accept(b) {
                            Ok(r) => match r {
                                Some(out) => {
                                    self.emit(&out)?;

                                    write!(
                                        self.log,
                                        "buf: {}\n     {}^pos:{} len:{}\n",
                                        String::from_utf8_lossy(&self.buf),
                                        " ".repeat(self.pos),
                                        self.pos,
                                        self.buf.len(),
                                    )?;
                                }

                                None => {}
                            },

                            Err(err) => {
                                panic!("{}", err);
                            }
                        }
                    }
                }
                Err(e) => write!(self.log, "read error: {e}\n").unwrap(),
            };
        }

        let result: libc::c_int;
        unsafe {
            result = libc::tcsetattr(libc::STDOUT_FILENO, 0, &mut termios_orig.unwrap());
        }

        if result == -1 {
            panic!("{}", io::Error::last_os_error());
        }
        println!();

        Ok(self.buf.to_owned())
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
                0x7f /* backspace (del) */ => {
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
                    write!(self.log, "unhandled char: {b:#04x}\n")?;
                    write!(self.output, "<?>")?;
                    None
                }
            },
            State::Esc => match b {
                b'[' => {
                    self.transition(State::CSI);
                    None
                }
                _ => {
                    write!(self.log, "unhandled escape: {b:#04x}\n")?;
                    self.transition(State::Normal);
                    None
                }
            },
            State::CSI => match b {
                // TODO: handle Delete i.e. CSI 3 ~
                // TODO: handle multi-byte CSI (parameters)

                // CUF: Cursor Forward
                b'C' => {
                    self.transition(State::Normal);
                    if self.pos < self.buf.len() {
                        self.pos += 1;
                        Some(vec![ESC, b'[', b'C'])
                    } else {
                        write!(self.log, "CUF blocked\n")?;
                        None
                    }
                }
                // CUB: Cursor Back
                b'D' => {
                    self.transition(State::Normal);
                    if self.pos > 0 {
                        self.pos -= 1;
                        Some(vec![ESC, b'[', b'D'])
                    } else {
                        write!(self.log, "CUB blocked\n")?;
                        None
                    }
                }
                _ => {
                    write!(self.log, "unhandled CSI: {b:#04x}\n")?;
                    self.transition(State::Normal);
                    None
                }
            },
        })
    }

    fn transition(&mut self, s: State) {
        write!(self.log, "state: {:?} -> {:?}\n", self.state, s).unwrap();
        self.state = s;
    }

    fn emit(&mut self, x: &[u8]) -> Result<(), Error> {
        write!(self.log, "emit: {:?}\n", String::from_utf8_lossy(&x))?;
        self.output.write_all(&x)?;
        self.output.flush()
    }

    fn set_raw() -> Result<libc::termios, io::Error> {
        let mut termios: libc::termios;

        unsafe {
            termios = mem::zeroed();
            Self::check_c_err(libc::tcgetattr(libc::STDOUT_FILENO, &mut termios))?;
        }

        let mut termios_raw = termios;

        unsafe {
            libc::cfmakeraw(&mut termios_raw);
            Self::check_c_err(libc::tcsetattr(libc::STDOUT_FILENO, 0, &termios_raw))?;
        }

        Ok(termios)
    }

    fn check_c_err(x: libc::c_int) -> Result<(), Error> {
        if x == -1 {
            Err(io::Error::last_os_error())
        } else {
            Ok(())
        }
    }
}

fn main() {
    let mut termline = Termline::stdio("termline> ".into()).unwrap();
    let result = termline.run().unwrap();
    println!("{}", String::from_utf8_lossy(&result));
}
