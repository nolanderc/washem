
extern {
    fn print(x: i32);
}

fn squares(x: i32) -> Vec<i32> {
    let mut sums = vec![0];
    let mut sum = 0;
    let mut i = 1;

    while sum < x {
        i += 1;
        sum += i*i;
        sums.push(sum);
    }

    sums
}

#[no_mangle]
pub fn main() {
    for x in squares(100_000) {
        unsafe { print(x) }
    }
}

