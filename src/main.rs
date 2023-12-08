mod grammar;

use grammar::ParseResult;

enum MyParseResult {
    Token(MySymbolKind),
    Parsed(MySymbolKind),
}

impl grammar::ParseResult<MySymbolKind> for MyParseResult {
    fn kind(&self) -> MySymbolKind {
        match self {
            MyParseResult::Token(kind) => *kind,
            MyParseResult::Parsed(kind) => *kind,
        }
    }
}

grammar::grammar! {
    grammar MyGrammar {
        symbol_kind: MySymbolKind,
        tokens: [A, B],
        parse_result: MyParseResult,
        rules: {
            ParsedA: A:a => MyParseResult::Parsed(a.kind()),
            ParsedB: B:b => MyParseResult::Parsed(b.kind()),
        }
    }
}

fn main() {
    let grammar = MyGrammar::new();
    println!("{:?}", grammar);
}
