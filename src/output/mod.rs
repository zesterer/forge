use std::fmt;
use crate::parser::SrcRef;

pub struct Repeat(pub char, pub usize);

impl std::fmt::Display for Repeat {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for _ in 0..self.1 {
            let _ = write!(f, "{}", self.0);
        }
        Ok(())
    }
}

pub fn fmt_ref(f: &mut fmt::Formatter, r: SrcRef, src: Option<&str>, depth: usize) -> fmt::Result {
    let pos_str = r.start().pos().map(|p| format!("{:>4}", p.0)).unwrap_or(String::new());
    if let (Some(src), Some((line, col))) = (src, r.start().pos()) {
        let line_str = src.lines().nth(line.saturating_sub(1)).unwrap_or("<none>").replace('\t', " ");
        Ok(())
            .and_then(|_| writeln!(f, "{}{}| {}", Repeat(' ', depth * 3), pos_str, line_str))
            .and_then(|_| writeln!(f, "{}{}|{}{}",
                Repeat(' ', depth * 3),
                Repeat(' ', pos_str.len()),
                Repeat(' ', col),
                Repeat('^', r.length_in(src).unwrap_or(1),
            )))
    } else if let Some(src) = src {
        let line_str = src.lines().filter(|l| l.trim().len() > 0).last().unwrap_or("<none>").replace('\t', " ");
        Ok(())
            .and_then(|_| writeln!(f, "{}|{}| {}", Repeat(' ', depth * 3), r.start(), line_str))
    } else {
        writeln!(f, "{}", r)
    }
}
