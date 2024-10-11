use std::collections::hash_map::HashMap;
use std::error::Error;
use std::fmt::Display;
use std::str::Chars;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Keyword {
    DoubleQuote,        // "        beginning and end of strings
    SingleQuote,        // '        beginning and end of a characters
    LeftParen,          // (        left of grouping expressions
    RightParen,         // )        right of grouping expressions
    LeftSquareBracket,  // [        left of matricies
    RightSquareBracket, // ]        right of matricies
    Strand,             // _        used in strand notation for lists / vectors / matricies
    Lambda,             // \        used to denote the head of lambdas
    Dot,                // .        used to denote the body of lambdas and decimals
    Assign,             // =        for assignment
    Pipe,               // |        used in algebraic data type constructors or set builder notation
    Optional,           // ?        shorthand for the maybe monad
    Either,             // ^        shorthand for the either monad
    TypeDeclaration,    // :        for type annotation (variables)
    Plus,               // +        adds 2 numbers
    Minus,              // -        subtracts 2 numbers
    Negate,             // ~        negates a number
    Star,               // *        multiplies 2 numbers
    Slash,              // /        divides 2 numbers
    Modulo,             // %        remainder of division of two numbers
    Concat,             // ++       concatenates two lists
    HasType,            // ::       has type
    Arrow,              // ->       for type annotation (functions)
    DoubleArrow,        // =>       for trait contexts
    In,                 // <-       for list comprehensions
    Bottom,             // _|_      for functions that can never return
    Class,              // class    used to declare type classes
    Data,               // data     used to declare algebraic data types
    Module,             // module   used to include an external .hum file
    Type,               // type     used to declare a type alias
}

pub fn keyword_strings() -> HashMap<Keyword, &'static str> {
    HashMap::from([
        (Keyword::DoubleQuote, "\""),
        (Keyword::SingleQuote, "\'"),
        (Keyword::LeftParen, "("),
        (Keyword::RightParen, ")"),
        (Keyword::LeftSquareBracket, "["),
        (Keyword::RightSquareBracket, "]"),
        (Keyword::Strand, "_"),
        (Keyword::Lambda, "\\"),
        (Keyword::Dot, "."),
        (Keyword::Assign, "="),
        (Keyword::Pipe, "|"),
        (Keyword::Optional, "?"),
        (Keyword::Either, "^"),
        (Keyword::TypeDeclaration, ":"),
        (Keyword::Plus, "+"),
        (Keyword::Minus, "-"),
        (Keyword::Negate, "~"),
        (Keyword::Star, "*"),
        (Keyword::Slash, "/"),
        (Keyword::Modulo, "%"),
        (Keyword::Concat, "++"),
        (Keyword::HasType, "::"),
        (Keyword::Arrow, "->"),
        (Keyword::DoubleArrow, "=>"),
        (Keyword::In, "<-"),
        (Keyword::Bottom, "_|_"),
        (Keyword::Class, "class"),
        (Keyword::Data, "data"),
        (Keyword::Module, "module"),
        (Keyword::Type, "type"),
    ])
}

pub fn escape_sequences() -> HashMap<char, char> {
    HashMap::from([
        ('\\', '\\'),
        ('\"', '\"'),
        ('\'', '\''),
        ('n', '\n'),
        ('t', '\t'),
        ('r', '\r'),
    ])
}

pub fn is_valid_hex(string: &str) -> bool {
    string.starts_with("0x")
        && string.len() > 2
        && string[2..].chars().all(|c| c.is_ascii_hexdigit())
}

pub fn is_valid_binary(string: &str) -> bool {
    string.starts_with("0b")
        && string.len() > 2
        && string[2..].chars().all(|c| c == '0' || c == '1')
}

pub fn is_valid_integer(string: &str) -> bool {
    if string.starts_with('-') {
        string.len() > 1 && string[1..].chars().all(|c| c.is_ascii_digit())
    } else {
        !string.is_empty() && string.chars().all(|c| c.is_ascii_digit())
    }
}

pub fn is_valid_decimal(string: &str) -> bool {
    let number_fragment: Vec<&str> = string.split('E').collect();

    if number_fragment.len() > 2 || number_fragment.len() == 0 {
        false
    } else if number_fragment.iter().any(|&fragment| fragment == "") {
        false
    } else {
        number_fragment
            .iter()
            .all(|&fragment| is_valid_integer(fragment))
    }
}

pub fn is_valid_number(string: &str) -> bool {
    is_valid_binary(string)
        || is_valid_hex(string)
        || is_valid_decimal(string)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum LiteralType {
    StringLiteral,
    NumberLiteral,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Literal {
    value: String,
    literal_type: LiteralType,
}

impl Literal {
    pub fn new(value: String, literal_type: LiteralType) -> Literal {
        Literal {
            value,
            literal_type
        }
    }
}

#[derive(Debug, Clone)]
pub enum TokenType {
    Literal(Literal),
    Identifier(String),
    Keyword(Keyword),
    EOF,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

impl Position {
    pub fn new(line: usize, column: usize) -> Position {
        Position { line, column }
    }
}

impl ToString for Position {
    fn to_string(&self) -> String {
        format!("line: {} column: {}", self.line, self.column)
    }
}

#[derive(Debug)]
pub struct Token {
    token_type: TokenType,
    position: Position,
}

impl Token {
    pub fn new(token_type: TokenType, position: Position) -> Token {
        Token {
            token_type,
            position,
        }
    }

    pub fn get_token_type(&self) -> TokenType {
        self.token_type.clone()
    }

    pub fn get_postition(&self) -> Position {
        self.position
    }
}

impl ToString for Token {
    fn to_string(&self) -> String {
        format!("Token `{:?}` is at {:?}", self.token_type, self.position)
    }
}

#[derive(Debug)]
pub enum LexingError {
    InvalidName(String),
    InvalidChar(char),
}

impl Display for LexingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InvalidName(name) => write!(f, "Invalid Name: `{name}`"),
            Self::InvalidChar(c) => write!(f, "Invalid Char: `{c}`")
        }
    }
}

impl Error for LexingError {}

pub struct Scanner<'a> {
    source: &'a str,
    tokens: Vec<Token>,
    in_progress: bool,
    current_token: String,
    chars: Chars<'a>,
    current_position: Position
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Scanner {
        Scanner {
            source,
            tokens: vec![],
            in_progress: true,
            current_token: String::new(),
            chars: source.chars(),
            current_position: Position::new(1, 0)
        }
    }

    fn next_char(&mut self) -> Option<char> {
        if let Some(c) = self.chars.next() {
            self.current_position.column += 1;
            if c == '\n' {
                self.current_position.line += 1;
                self.current_position.column = 0;
            }
            Some(c)
        } else {
            None
        }
    }

    fn scan_keyword(&mut self) -> Result<(), LexingError> {
        let mut possible_keywords = keyword_strings();

        while possible_keywords.len() > 1 {
            if let Some(c) = self.next_char() {
                self.current_token.push(c);
                for (&entry, &possibility) in possible_keywords.clone().iter() {
                    if possibility.starts_with(&self.current_token) {
                        possible_keywords.remove(&entry);
                    }
                }
            } else {
                return Err(LexingError::InvalidName(self.current_token.clone()));
            }
        }

        if possible_keywords.is_empty() {
            Err(LexingError::InvalidName(self.current_token.clone()))
        } else {
            let &keyword = possible_keywords
                .keys()
                .next()
                .expect("Oops! Something when wrong! I couldn't parse your file...");
            self.tokens.push(Token::new(
                TokenType::Keyword(keyword),
                self.current_position
            ));
            Ok(())
        }
    }

    fn scan_number(&mut self) -> Result<(), LexingError> {
        while is_valid_number(&self.current_token) {
            if let Some(c) = self.next_char() {
                self.current_token.push(c);
            } else {
                let new_literal = Literal::new(self.current_token.clone(), LiteralType::NumberLiteral);
                let new_token = Token::new(TokenType::Literal(new_literal), self.current_position);
                self.tokens.push(new_token);
            }
        }

        let literal_value = self.current_token[..self.current_token.len()-1].to_string();
        let new_literal = Literal::new(literal_value, LiteralType::NumberLiteral);
        let new_token = Token::new(TokenType::Literal(new_literal), self.current_position);
        self.tokens.push(new_token);

        self.current_token = self.current_token
            .chars()
            .nth(self.current_token.len() - 1)
            .expect("Oops! Could not reset current token!")
            .to_string();

        Ok(())
    }

    fn scan_string(&mut self) -> Result<(), LexingError> {
        todo!("Can not currently scan strings");
    }
}
