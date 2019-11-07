
extern {
    fn print(x: u32);
}

#[no_mangle]
pub fn fib(n: u32) -> u32 {
    if n <= 2 {
        n
    } else {
        fib(n - 1) + fib(n - 2)
    }
}

#[no_mangle]
pub fn main() {
    unsafe {
        print(fib(8));
    }
}

