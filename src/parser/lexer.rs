use std::fmt;
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
    DotDot,
    Colon,
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
    Percent, PercentEq,

    // Literals
    Ident(String),
    String(String),
    Char(char),
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
    Print, Input,
    Clone,
    Mirror,

    // Misc
    Reserved,
    Eof,
}

impl fmt::Display for Lexeme {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Lexeme::LParen => write!(f, "("),
            Lexeme::RParen => write!(f, ")"),
            Lexeme::LBrace => write!(f, "{{"),
            Lexeme::RBrace => write!(f, "}}"),
            Lexeme::LBrack => write!(f, "["),
            Lexeme::RBrack => write!(f, "]"),
            Lexeme::Comma => write!(f, ","),
            Lexeme::Dot => write!(f, "."),
            Lexeme::DotDot => write!(f, ".."),
            Lexeme::Colon => write!(f, ":"),
            Lexeme::Semicolon => write!(f, ";"),
            Lexeme::Pipe => write!(f, "|"),

            Lexeme::Bang =>      write!(f, "!"),
            Lexeme::BangEq =>    write!(f, "!="),
            Lexeme::Assign =>    write!(f, "="),
            Lexeme::Eq =>        write!(f, "=="),
            Lexeme::Greater =>   write!(f, ">"),
            Lexeme::GreaterEq => write!(f, ">="),
            Lexeme::Less =>      write!(f, "<"),
            Lexeme::LessEq =>    write!(f, "<="),
            Lexeme::Plus =>      write!(f, "+"),
            Lexeme::PlusEq =>    write!(f, "+="),
            Lexeme::Minus =>     write!(f, "-"),
            Lexeme::MinusEq =>   write!(f, "-="),
            Lexeme::Star =>      write!(f, "*"),
            Lexeme::StarEq =>    write!(f, "*="),
            Lexeme::Slash =>     write!(f, "/"),
            Lexeme::SlashEq =>   write!(f, "/="),
            Lexeme::Percent =>   write!(f, "%"),
            Lexeme::PercentEq => write!(f, "%="),

            Lexeme::Ident(s) => write!(f, "{}", s),
            Lexeme::String(s) => write!(f, "\"{}\"", s),
            Lexeme::Char(c) => write!(f, "\"{}\"", c),
            Lexeme::Number(x) => write!(f, "{}", x),
            Lexeme::True => write!(f, "true"),
            Lexeme::False => write!(f, "false"),
            Lexeme::Null => write!(f, "null"),

            Lexeme::And => write!(f, "and"),
            Lexeme::Or => write!(f, "or"),
            Lexeme::Xor => write!(f, "xor"),
            Lexeme::In => write!(f, "in"),
            Lexeme::If => write!(f, "if"),
            Lexeme::Else => write!(f, "else"),
            Lexeme::Break => write!(f, "break"),
            Lexeme::Return => write!(f, "return"),
            Lexeme::For => write!(f, "for"),
            Lexeme::While => write!(f, "while"),
            Lexeme::Fn => write!(f, "fn"),
            Lexeme::This => write!(f, "this"),
            Lexeme::Var => write!(f, "var"),
            Lexeme::Print => write!(f, "print"),
            Lexeme::Input => write!(f, "input"),
            Lexeme::Clone => write!(f, "clone"),
            Lexeme::Mirror => write!(f, "mirror"),

            Lexeme::Reserved => write!(f, "<reserved>"),
            Lexeme::Eof => write!(f, "EOF"),
        }
    }
}

const RESERVED_KEYWORDS: [&'static str; 38] = [
    "let",      "self",   "Self",  "extern", "move",
    "mut",      "enum",   "num",   "string", "str",
    "bool",     "const",  "as",    "loop",   "pub",
    "priv",     "ref",    "match", "use",    "where",
    "do",       "clone",  "type",  "class",  "base",
    "super",    "struct", "trait", "impl",   "of",
    "with",     "when",   "then",  "await",  "async",
    "continue", "yield",  "mut",
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
        Char,
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
        let mut was_whitespace = false;
        match state {
            State::Default => match c {
                ' ' | '\r' | '\t' | '\n' => was_whitespace = true,
                '(' => tokens.push(Token(Lexeme::LParen, SrcRef::single(loc))),
                ')' => tokens.push(Token(Lexeme::RParen, SrcRef::single(loc))),
                '{' => tokens.push(Token(Lexeme::LBrace, SrcRef::single(loc))),
                '}' => tokens.push(Token(Lexeme::RBrace, SrcRef::single(loc))),
                '[' => tokens.push(Token(Lexeme::LBrack, SrcRef::single(loc))),
                ']' => tokens.push(Token(Lexeme::RBrack, SrcRef::single(loc))),
                ',' => tokens.push(Token(Lexeme::Comma, SrcRef::single(loc))),
                '|' => tokens.push(Token(Lexeme::Pipe, SrcRef::single(loc))),
                ':' => tokens.push(Token(Lexeme::Colon, SrcRef::single(loc))),
                ';' => tokens.push(Token(Lexeme::Semicolon, SrcRef::single(loc))),
                '.' => if chars.clone().nth(1) == Some('.') {
                    tokens.push(Token(Lexeme::DotDot, SrcRef::double(loc)));
                    incr = 2;
                } else {
                    tokens.push(Token(Lexeme::Dot, SrcRef::single(loc)));
                },
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
                '%' => if chars.clone().nth(1) == Some('=') {
                    tokens.push(Token(Lexeme::PercentEq, SrcRef::double(loc)));
                    incr = 2;
                } else {
                    tokens.push(Token(Lexeme::Percent, SrcRef::single(loc)));
                },
                '#' => state = State::Comment,
                '"' => /*"*/ {
                    strbuf.clear();
                    start_loc = loc;
                    state = State::String;
                },
                '\'' => {
                    strbuf.clear();
                    start_loc = loc;
                    state = State::Char;
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
                    tokens.push(Token(Lexeme::String(strbuf.clone()), SrcRef::many(start_loc, loc.next_col(true))));
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
            State::Char => match c {
                '\'' => {
                    if strbuf.len() == 1 {
                        tokens.push(Token(Lexeme::Char(strbuf.char_indices().next().unwrap().1), SrcRef::many(start_loc, loc.next_col(true))));
                    } else {
                        errors.push(ParseError::At(
                            SrcRef::many(start_loc, loc.next_col(true)),
                            Box::new(ParseError::CharTooLong),
                        ));
                    }
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
                '.' => if !seen_dot && chars.clone().nth(1).map(|c| c.is_ascii_digit()).unwrap_or(false) {
                    strbuf.push(c);
                    seen_dot = true;
                } else {
                    tokens.push(Token(Lexeme::Number(strbuf.parse().unwrap()), SrcRef::many(start_loc, loc)));
                    state = State::Default;
                    incr = 0;
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
                    "input" => Lexeme::Input,
                    "clone" => Lexeme::Clone,
                    "mirror" => Lexeme::Mirror,
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
                loc = loc.next_col(!was_whitespace);
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
