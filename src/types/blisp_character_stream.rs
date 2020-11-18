use std::iter::Peekable;

pub struct BLispCharacterStream<I>
where I: Iterator<Item = char> {
    iterator: Peekable<I>,
    line_num: i32,
    col_num: i32,
}

impl<I> BLispCharacterStream<I> 
where I: Iterator<Item = char> {
    pub fn new(iterator: I) -> BLispCharacterStream<I>
    where I: Iterator<Item = char> {
        BLispCharacterStream { iterator: iterator.peekable(), line_num: 1, col_num: 1 }
    }

    pub fn peek(&mut self) -> Option<&char> {
        self.iterator.peek()
    }

    pub fn next(&mut self) -> Option<char> {
        let next = self.iterator.next();
        match next {
            Some('\n') => { self.line_num += 1; self.col_num = 0; }
            _ => self.col_num += 1,
        }

        next
    }

    pub fn current_position(&self) -> (i32, i32) {
        (self.line_num, self.col_num)
    }
}

