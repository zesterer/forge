use super::{
    ParseError,
    ParseResult,
    SrcRef,
    SrcLoc,
};

#[derive(Clone, Debug, PartialEq)]
pub enum Lexeme {
    // Single-character lexemes
    LParen, RParen,
    LBrace, RBrace,
    LBrack, RBrack,
    Comma,
    Dot,
    Semicolon,
    Pipe,

    // Multi-character lexemes
    Bang,    BangEq,
    Assign,  Eq,
    Greater, GreaterEq,
    Less,    LessEq,
    Plus,    PlusEq,
    Minus,   MinusEq,
    Star,    StarEq,
    Slash,   SlashEq,

    // Literals
    Ident(String),
    String(String),
    Number(f64),
    True, False,
    Null,

    // Keywords
    And, Or, Xor, In,
    If, Else,
    Break, Return,
    For, While,
    Fn,
    This,
    Var,
    Print,

    // Misc
    AnyIdent,
    AnyString,
    AnyNumber,
    Reserved,
    Eof,
}

const RESERVED_KEYWORDS: [&'static str; 34] = [
    "let",
    "self",
    "Self",
    "extern",
    "move",
    "mut",
    "enum",
    "const",
    "as",
    "loop",
    "pub",
    "priv",
    "ref",
    "match",
    "use",
    "where",
    "do",
    "clone",
    "type",
    "class",
    "base",
    "super",
    "struct",
    "trait",
    "impl",
    "of",
    "with",
    "when",
    "then",
    "await",
    "async",
    "continue",
    "yield",
    "mut",
];

#[derive(Clone, Debug)]
pub struct Token(pub Lexeme, pub SrcRef);

pub fn lex(code: &str) -> ParseResult<Vec<Token>> {
    let mut tokens = vec![];
    let mut errors = vec![];

    let mut chars = code.chars();
    let mut loc = SrcLoc::start();

    enum State {
        Default,
        Comment,
        String,
        Number,
        Ident,
    }
    let mut state = State::Default;
    let mut strbuf = String::new();
    let mut seen_dot = false;
    let mut start_loc = SrcLoc::start();

    loop {
        let c = chars.clone().next().unwrap_or('\0');
        let mut incr = 1;
        match state {
            State::Default => match c {
                ' ' | '\r' | '\t' | '\n' => {},
                '(' => tokens.push(Token(Lexeme::LParen, SrcRef::single(loc))),
                ')' => tokens.push(Token(Lexeme::RParen, SrcRef::single(loc))),
                '{' => tokens.push(Token(Lexeme::LBrace, SrcRef::single(loc))),
                '}' => tokens.push(Token(Lexeme::RBrace, SrcRef::single(loc))),
                '[' => tokens.push(Token(Lexeme::LBrack, SrcRef::single(loc))),
                ']' => tokens.push(Token(Lexeme::RBrack, SrcRef::single(loc))),
                ',' => tokens.push(Token(Lexeme::Comma, SrcRef::single(loc))),
                '.' => tokens.push(Token(Lexeme::Dot, SrcRef::single(loc))),
                '|' => tokens.push(Token(Lexeme::Pipe, SrcRef::single(loc))),
                ';' => tokens.push(Token(Lexeme::Semicolon, SrcRef::single(loc))),
                '!' => if chars.clone().nth(1) == Some('=') {
                    tokens.push(Token(Lexeme::BangEq, SrcRef::double(loc)));
                    incr = 2;
                } else {
                    tokens.push(Token(Lexeme::Bang, SrcRef::single(loc)));
                },
                '=' => if chars.clone().nth(1) == Some('=') {
                    tokens.push(Token(Lexeme::Eq, SrcRef::double(loc)));
                    incr = 2;
                } else {
                    tokens.push(Token(Lexeme::Assign, SrcRef::single(loc)));
                },
                '>' => if chars.clone().nth(1) == Some('=') {
                    tokens.push(Token(Lexeme::GreaterEq, SrcRef::double(loc)));
                    incr = 2;
                } else {
                    tokens.push(Token(Lexeme::Greater, SrcRef::single(loc)));
                },
                '<' => if chars.clone().nth(1) == Some('=') {
                    tokens.push(Token(Lexeme::LessEq, SrcRef::double(loc)));
                    incr = 2;
                } else {
                    tokens.push(Token(Lexeme::Less, SrcRef::single(loc)));
                },
                '+' => if chars.clone().nth(1) == Some('=') {
                    tokens.push(Token(Lexeme::PlusEq, SrcRef::double(loc)));
                    incr = 2;
                } else {
                    tokens.push(Token(Lexeme::Plus, SrcRef::single(loc)));
                },
                '-' => if chars.clone().nth(1) == Some('=') {
                    tokens.push(Token(Lexeme::MinusEq, SrcRef::double(loc)));
                    incr = 2;
                } else {
                    tokens.push(Token(Lexeme::Minus, SrcRef::single(loc)));
                },
                '*' => if chars.clone().nth(1) == Some('=') {
                    tokens.push(Token(Lexeme::StarEq, SrcRef::double(loc)));
                    incr = 2;
                } else {
                    tokens.push(Token(Lexeme::Star, SrcRef::single(loc)));
                },
                '/' => if chars.clone().nth(1) == Some('=') {
                    tokens.push(Token(Lexeme::SlashEq, SrcRef::double(loc)));
                    incr = 2;
                } else {
                    tokens.push(Token(Lexeme::Slash, SrcRef::single(loc)));
                },
                '#' => state = State::Comment,
                '"' => /*"*/ {
                    strbuf.clear();
                    start_loc = loc;
                    state = State::String;
                },
                '0' ... '9' => {
                    strbuf.clear();
                    start_loc = loc;
                    seen_dot = false;
                    state = State::Number;
                    incr = 0;
                },
                '\0' => break,
                c => if c.is_alphanumeric() || c == '_' {
                    strbuf.clear();
                    start_loc = loc;
                    state = State::Ident;
                    incr = 0;
                } else {
                    errors.push(ParseError::At(
                        SrcRef::single(loc),
                        Box::new(ParseError::UnexpectedChar(c)),
                    ));
                },
            },
            State::Comment => match c {
                '\n' | '\0' => state = State::Default,
                _ => {},
            },
            State::String => match c {
                '"' => /*"*/ {
                    tokens.push(Token(Lexeme::String(strbuf.clone()), SrcRef::many(start_loc, loc)));
                    state = State::Default;
                },
                '\0' => {
                    errors.push(ParseError::At(
                        SrcRef::end(),
                        Box::new(ParseError::ExpectedDelimiter('"')), /*"*/
                    ));
                    break;
                },
                c => strbuf.push(c),
            },
            State::Number => match c {
                '0' ... '9' => strbuf.push(c),
                '.' => if !seen_dot {
                    strbuf.push(c);
                    seen_dot = true;
                } else {
                    tokens.push(Token(Lexeme::Number(strbuf.parse().unwrap()), SrcRef::many(start_loc, loc)));
                    state = State::Default;
                },
                _ => {
                    tokens.push(Token(Lexeme::Number(strbuf.parse().unwrap()), SrcRef::many(start_loc, loc)));
                    state = State::Default;
                    incr = 0;
                },
            },
            State::Ident => if c.is_alphanumeric() || c == '_' {
                strbuf.push(c);
            } else {
                tokens.push(Token(match strbuf.as_str() {
                    "and" => Lexeme::And,
                    "or" => Lexeme::Or,
                    "xor" => Lexeme::Xor,
                    "in" => Lexeme::In,
                    "if" => Lexeme::If,
                    "else" => Lexeme::Else,
                    "break" => Lexeme::Break,
                    "return" => Lexeme::Return,
                    "for" => Lexeme::For,
                    "while" => Lexeme::While,
                    "fn" => Lexeme::Fn,
                    "this" => Lexeme::This,
                    "var" => Lexeme::Var,
                    "print" => Lexeme::Print,
                    "true" => Lexeme::True,
                    "false" => Lexeme::False,
                    "null" => Lexeme::Null,
                    ident => if RESERVED_KEYWORDS.contains(&ident) {
                        errors.push(ParseError::At(
                            SrcRef::many(start_loc, loc),
                            Box::new(ParseError::ReservedKeyword(strbuf.clone())),
                        ));
                        Lexeme::Reserved
                    } else {
                        Lexeme::Ident(strbuf.clone())
                    },
                }, SrcRef::many(start_loc, loc)));
                state = State::Default;
                incr = 0;
            },
        }

        for _ in 0..incr {
            if c == '\n' {
                loc = loc.next_line();
                chars.next();
            } else {
                loc = loc.next_col();
                chars.next();
            }
        }
    }

    if errors.len() == 0 {
        Ok(tokens)
    } else {
        Err(ParseError::Many(errors))
    }
}
