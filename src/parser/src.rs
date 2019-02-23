use std::{
    rc::Rc,
    cmp::{
        Ordering,
        PartialOrd,
    },
};

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum SrcLoc {
    At {
        line: usize,
        col: usize,
    },
    End,
    Nowhere,
}

impl SrcLoc {
    pub fn start() -> Self {
        SrcLoc::At {
            line: 1,
            col: 1,
        }
    }

    pub fn next_col(mut self) -> Self {
        match &mut self {
            SrcLoc::At { col, .. } => *col += 1,
            SrcLoc::End => {},
            SrcLoc::Nowhere => {},
        }
        self
    }

    pub fn next_line(mut self) -> Self {
        match &mut self {
            SrcLoc::At { line, col } => {
                *line += 1;
                *col = 1;
            },
            SrcLoc::End => {},
            SrcLoc::Nowhere => {},
        }
        self
    }

    pub fn end() -> Self {
        SrcLoc::End
    }
}

impl PartialOrd for SrcLoc {
    fn partial_cmp(&self, rhs: &Self) -> Option<Ordering> {
        match (self, rhs) {
            (
                SrcLoc::At { line: l0, col: c0 },
                SrcLoc::At { line: l1, col: c1 },
            ) => if l0 == l1 {
                Some(c0.cmp(c1))
            } else {
                Some(l0.cmp(l1))
            },
            (SrcLoc::End, SrcLoc::End) => Some(Ordering::Equal),
            (SrcLoc::End, SrcLoc::At { .. }) => Some(Ordering::Greater),
            (SrcLoc::At { .. }, SrcLoc::End) => Some(Ordering::Less),
            _ => None,
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum SrcRef {
    Range {
        start: SrcLoc,
        limit: SrcLoc,
    },
    Empty,
}

impl SrcRef {
    pub fn single(start: SrcLoc) -> Self {
        SrcRef::Range {
            start,
            limit: start.next_col(),
        }
    }

    pub fn double(start: SrcLoc) -> Self {
        SrcRef::Range {
            start,
            limit: start.next_col().next_col(),
        }
    }

    pub fn many(start: SrcLoc, limit: SrcLoc) -> Self {
        SrcRef::Range {
            start,
            limit,
        }
    }

    pub fn end() -> Self {
        SrcRef::Range {
            start: SrcLoc::end(),
            limit: SrcLoc::end(),
        }
    }

    pub fn empty() -> Self {
        SrcRef::Empty
    }

    pub fn limit(&self) -> SrcLoc {
        match self {
            SrcRef::Range { limit, .. } => *limit,
            SrcRef::Empty => SrcLoc::Nowhere,
        }
    }

    pub fn union(&self, other: &Self) -> Self {
        match (self, other) {
            (
                SrcRef::Range { start: s0, limit: e0 },
                SrcRef::Range { start: s1, limit: e1 },
            ) => SrcRef::Range {
                start: if s0 < s1 { *s0 } else { *s1 },
                limit: if e0 > e1 { *e0 } else { *e1 },
            },
            (SrcRef::Empty, SrcRef::Empty) => SrcRef::Empty,
            (this, SrcRef::Empty) => *this,
            (SrcRef::Empty, other) => *other,
        }
    }
}
