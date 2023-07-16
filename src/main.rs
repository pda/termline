use std::io;
use std::io::Read;
use std::mem;

#[repr(C)]
#[derive(Debug)]
struct DebugTermios {
    pub c_iflag: Flag32,
    pub c_oflag: Flag32,
    pub c_cflag: Flag32,
    pub c_lflag: Flag32,
    pub c_line: u8,
    pub c_cc: [u8; 32],
    pub c_ispeed: u32,
    pub c_ospeed: u32,
}

// See https://github.com/nobomi/Arduino-ARMSID-configurator/pull/2#issuecomment-907168147
// for example dumping termios
struct Flag32(u32);
impl std::fmt::Debug for Flag32 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{:032b}", &self.0)
    }
}

fn main() {
    let termios_orig = termthings();

    println!("Hello, world!");

    let mut stdin = std::io::stdin().bytes();

    if let Some(x) = stdin.nth(0) {
        match x {
            Ok(x) => println!("ok: {}", x),
            Err(x) => println!("nope: {}", x),
        }
    }

    print!("restoring termios... ");
    let result: libc::c_int;
    unsafe {
        result = libc::tcsetattr(libc::STDOUT_FILENO, 0, &mut termios_orig.unwrap());
    }

    if result == -1 {
        panic!("{}", io::Error::last_os_error());
    }
    println!("termios restored");
}

fn termthings() -> Result<libc::termios, io::Error> {
    let mut termios: libc::termios;
    let mut result: libc::c_int;

    unsafe {
        termios = mem::zeroed();
    }

    unsafe {
        result = libc::tcgetattr(libc::STDOUT_FILENO, &mut termios);
    }

    if result == -1 {
        return Err(io::Error::last_os_error());
    }

    println!("before:");
    unsafe {
        println!("{:?}", mem::transmute_copy::<_, DebugTermios>(&termios));
    }

    let mut termios_raw = termios;

    unsafe {
        libc::cfmakeraw(&mut termios_raw);
    }

    unsafe {
        result = libc::tcsetattr(libc::STDOUT_FILENO, 0, &termios_raw);
    }

    if result == -1 {
        return Err(io::Error::last_os_error());
    }

    unsafe {
        result = libc::tcgetattr(libc::STDOUT_FILENO, &mut termios_raw);
    }

    if result == -1 {
        return Err(io::Error::last_os_error());
    }

    println!("after:");
    unsafe {
        println!("{:?}", mem::transmute_copy::<_, DebugTermios>(&termios));
    }

    Ok(termios)
}
