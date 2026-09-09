//! Hand-written lexer for L1
//!
//! Replaces the old `rustlex` compiler-plugin lexer (which relied on the now
//! removed `#![plugin(...)]` machinery) with a plain scanner. It produces the
//! same `Marked<Token>` stream that `parse::parse` already consumes, so nothing
//! downstream needed to change: `Lexer::new(reader)`, `next()` and the
//! `comment_depth` field all keep their previous meaning.

use util::{Marked, Mark};
use super::{intern, parser_panic};
use super::token::Token;
use std::io;

pub type MarkedToken = Marked<Token>;

pub struct Lexer {
    input: Vec<u8>,
    pos: usize,
    pub comment_depth: usize,
}

impl Lexer {
    /// Reads the entire input up front and prepares to tokenize it. Byte
    /// offsets into the input are used as `Mark` positions, matching what the
    /// `CodeMap` expects.
    pub fn new<R: io::Read>(mut reader: R) -> Lexer {
        let mut contents = String::new();
        reader.read_to_string(&mut contents)
            .expect("failed to read source for lexing");
        Lexer { input: contents.into_bytes(), pos: 0, comment_depth: 0 }
    }

    fn peek(&self) -> Option<u8> { self.input.get(self.pos).cloned() }
    fn peek2(&self) -> Option<u8> { self.input.get(self.pos + 1).cloned() }

    /// The input came from a `String` and L1 source is ASCII, so this slice is
    /// always valid UTF-8.
    fn slice(&self, lo: usize, hi: usize) -> &str {
        std::str::from_utf8(&self.input[lo..hi]).unwrap()
    }

    /// Skips whitespace, line comments, and nested block comments, leaving
    /// `pos` at the next significant character (or at EOF). An unterminated
    /// block comment leaves `comment_depth > 0`, which `parse::parse` checks.
    fn skip_trivia(&mut self) {
        loop {
            match self.peek() {
                Some(b' ') | Some(b'\t') | Some(b'\r') | Some(b'\n')
                | Some(0x0B) | Some(0x0C) => self.pos += 1,
                Some(b'/') if self.peek2() == Some(b'/') => {
                    self.pos += 2;
                    while let Some(c) = self.peek() {
                        if c == b'\n' { break; }
                        self.pos += 1;
                    }
                }
                Some(b'/') if self.peek2() == Some(b'*') => self.skip_block_comment(),
                _ => break,
            }
        }
    }

    fn skip_block_comment(&mut self) {
        self.pos += 2; // consume the opening "/*"
        self.comment_depth += 1;
        while self.comment_depth > 0 {
            match self.peek() {
                None => return, // unterminated; reported by the caller
                Some(b'/') if self.peek2() == Some(b'*') => {
                    self.pos += 2;
                    self.comment_depth += 1;
                }
                Some(b'*') if self.peek2() == Some(b'/') => {
                    self.pos += 2;
                    self.comment_depth -= 1;
                }
                _ => self.pos += 1,
            }
        }
    }

    /// Produces the next token, or `None` at end of input.
    pub fn next(&mut self) -> Option<MarkedToken> {
        self.skip_trivia();
        let lo = self.pos;
        let c = match self.peek() {
            Some(c) => c,
            None => return None,
        };

        if is_ident_start(c) {
            self.pos += 1;
            while let Some(c) = self.peek() {
                if is_ident_continue(c) { self.pos += 1; } else { break; }
            }
            let hi = self.pos;
            let word = self.slice(lo, hi);
            let tok = keyword(word).unwrap_or_else(|| Token::Ident(intern(word)));
            return Some(Marked::new(tok, Mark::new(lo, hi)));
        }

        if c.is_ascii_digit() {
            return Some(self.number(lo));
        }

        self.operator(lo)
    }

    /// Lexes a decimal or hexadecimal integer constant starting at `lo`.
    fn number(&mut self, lo: usize) -> MarkedToken {
        // Hexadecimal: 0[xX][0-9a-fA-F]+
        if self.peek() == Some(b'0')
            && (self.peek2() == Some(b'x') || self.peek2() == Some(b'X')) {
            self.pos += 2;
            let ds = self.pos;
            while let Some(c) = self.peek() {
                if c.is_ascii_hexdigit() { self.pos += 1; } else { break; }
            }
            let hi = self.pos;
            if self.pos == ds {
                parser_panic(String::from("Invalid hexadecimal constant"),
                             Mark::new(lo, hi));
            }
            let s = self.slice(ds, hi);
            let v = u32::from_str_radix(s, 16).unwrap_or_else(|_| {
                parser_panic(format!("Hexadecimal constant 0x{} is too large", s),
                             Mark::new(lo, hi))
            });
            return Marked::new(Token::Intconst(v), Mark::new(lo, hi));
        }

        // Decimal: 0 | [1-9][0-9]*  (a lone '0' is its own token)
        if self.peek() == Some(b'0') {
            self.pos += 1;
            return Marked::new(Token::Intconst(0), Mark::new(lo, self.pos));
        }

        self.pos += 1;
        while let Some(c) = self.peek() {
            if c.is_ascii_digit() { self.pos += 1; } else { break; }
        }
        let hi = self.pos;
        let s = self.slice(lo, hi);
        let n = s.parse::<u32>().unwrap_or_else(|_| {
            parser_panic(format!("Constant {} is too large", s), Mark::new(lo, hi))
        });
        if n > 2u32.pow(31) {
            parser_panic(format!("Constant {} is too large", n), Mark::new(lo, hi));
        }
        Marked::new(Token::Intconst(n), Mark::new(lo, hi))
    }

    /// Lexes an operator or punctuation token, preferring the longest match.
    fn operator(&mut self, lo: usize) -> Option<MarkedToken> {
        let (tok, len) = match (self.peek(), self.peek2()) {
            (Some(b'+'), Some(b'=')) => (Token::Pluseq, 2),
            (Some(b'-'), Some(b'=')) => (Token::Minuseq, 2),
            (Some(b'*'), Some(b'=')) => (Token::Stareq, 2),
            (Some(b'/'), Some(b'=')) => (Token::Slasheq, 2),
            (Some(b'%'), Some(b'=')) => (Token::Percenteq, 2),
            (Some(b'-'), Some(b'-')) => (Token::Decrement, 2),
            (Some(c), _) => {
                let t = match c {
                    b'(' => Token::Lparen,
                    b')' => Token::Rparen,
                    b'{' => Token::Lbrace,
                    b'}' => Token::Rbrace,
                    b';' => Token::Semi,
                    b'=' => Token::Assign,
                    b'+' => Token::Plus,
                    b'-' => Token::Minus,
                    b'*' => Token::Star,
                    b'/' => Token::Slash,
                    b'%' => Token::Percent,
                    _ => parser_panic(format!("Illegal character {:?}", c as char),
                                      Mark::new(lo, lo + 1)),
                };
                (t, 1)
            }
            (None, _) => return None,
        };
        self.pos += len;
        Some(Marked::new(tok, Mark::new(lo, lo + len)))
    }
}

fn is_ident_start(c: u8) -> bool {
    (c >= b'A' && c <= b'Z') || (c >= b'a' && c <= b'z') || c == b'_'
}

fn is_ident_continue(c: u8) -> bool {
    is_ident_start(c) || (c >= b'0' && c <= b'9')
}

fn keyword(s: &str) -> Option<Token> {
    Some(match s {
        "struct" => Token::Struct,
        "typedef" => Token::Typedef,
        "if" => Token::If,
        "else" => Token::Else,
        "while" => Token::While,
        "for" => Token::For,
        "continue" => Token::Continue,
        "break" => Token::Break,
        "assert" => Token::Assert,
        "true" => Token::True,
        "false" => Token::False,
        "NULL" => Token::Null,
        "alloc" => Token::Alloc,
        "alloc_array" => Token::Allocarray,
        "bool" => Token::Bool,
        "void" => Token::Void,
        "char" => Token::Char,
        "string" => Token::String,
        "return" => Token::Return,
        "int" => Token::Int,
        "main" => Token::Main,
        _ => return None,
    })
}
