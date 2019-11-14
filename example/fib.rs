
extern "C" {
    fn print(x: u32);
}

#[no_mangle]
pub fn fib(n: u32) -> u32 {
    if n <= 2 {
        1
    } else {
        fib(n - 1) + fib(n - 2)
    }
}

#[no_mangle]
pub fn main() {
    for i in 1..30 {
        println!("{}", fib(i));
    }
}

