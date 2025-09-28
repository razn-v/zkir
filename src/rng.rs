use std::arch::x86_64::_rdtsc;

pub struct Rng(u64);

impl Rng {
    pub fn new() -> Self {
        unsafe {
            Self(0x13371337 /*_rdtsc()*/)
        }
    }

    pub fn next(&mut self) -> usize {
        let val = self.0;
        self.0 ^= self.0 << 13;
        self.0 ^= self.0 >> 17;
        self.0 ^= self.0 << 43;
        val as usize
    }

    pub fn rand(&mut self, min: usize, max: usize) -> usize {
        assert!(min <= max, "Invalid range");

        if min == max {
            return min;
        }

        min + (self.next() as usize % (max - min + 1))
    }

    fn rand_char(&mut self) -> char {
        const CHARSET: &[u8] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";

        let idx = self.rand(0, CHARSET.len() - 1);
        CHARSET[idx] as char
    }

    pub fn rand_string(&mut self, len: usize) -> String {
        (0..len).map(|_| self.rand_char()).collect()
    }

    pub fn rand_vec<'a, T>(&mut self, vec: &'a Vec<T>) -> Option<&'a T> {
        if vec.is_empty() {
            return None;
        }
        Some(&vec[self.rand(0, vec.len() - 1)])
    }
}
