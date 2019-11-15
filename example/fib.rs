
extern "C" {
    fn print(x: u32);
}

fn fib(n: u32) -> u32 {
    if n <= 2 {
        1
    } else {
        fib(n - 1) + fib(n - 2)
    }
}

#[no_mangle]
pub fn main() {
    for i in 1..=30 {
        unsafe {
            print(fib(i))
        }
    }
}

