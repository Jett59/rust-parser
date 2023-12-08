pub trait ParseResult<SymbolKind> {
    fn kind(&self) -> SymbolKind;
}

pub struct GrammarRule<ParseResult: self::ParseResult<SymbolKind>, SymbolKind> {
    pub input: Vec<SymbolKind>,
    pub reduce: fn(Vec<ParseResult>) -> ParseResult,
}

macro_rules! append_grammar_rules {
    ($rules_list:ident, $SymbolKind:ty, $ParseResult:ty, $result_symbol_kind:ident, $($symbol_kind:ident$(:$name:ident)?)* => $reduce:expr) => {{
        fn reduce(symbols: Vec<$ParseResult>) -> $ParseResult {
            let mut symbol_iterator = symbols.into_iter();
            $($(let $name =)? symbol_iterator.next().unwrap();)*
            let result = $reduce;
            assert_eq!(result.kind(), <$SymbolKind>::$result_symbol_kind, "Grammar rule returned wrong kind");
            result
        }
        $rules_list.push(GrammarRule::<$ParseResult, $SymbolKind> {
            input: vec![$(<$SymbolKind>::$symbol_kind),*],
            reduce,
        });
    }};
    ($rules_list:ident, $SymbolKind:ty, $ParseResult:ty, $result_symbol_kind:ident, { $($($symbol_kind:ident$(:$name:ident)?)* => $reduce:expr),* }) => {
        $(
            append_grammar_rules!($rules_list, $SymbolKind, $ParseResult, $result_symbol_kind, $($symbol_kind$(:$name)?)* => $reduce);
        )*
    };
}

macro_rules! grammar {
    ($visibility:vis grammar $Grammar:ident {
        symbol_kind: $SymbolKind:ident,
        tokens: [$($token:ident),*],
        parse_result: $ParseResult:ident,
        rules: {$(
            $NonTerminal:ident: $first:tt $($($optional_rule_tokens:ident)* => $optional_rule_reduce:expr)?
    ),*$(,)?}
    }) => {
        #[derive(Clone, Copy, Debug, PartialEq)]
        $visibility enum $SymbolKind {
            $($token,)*
            $($NonTerminal,)*
        }

        $visibility struct $Grammar {
            pub rules: Vec<GrammarRule<$ParseResult, $SymbolKind>>,
        }

        impl $Grammar {
            $visibility fn new() -> Self {
                let mut rules = Vec::new();
                $(
                    append_grammar_rules!(rules, $SymbolKind, $ParseResult, $NonTerminal, $first $($($optional_rule_tokens)* => $optional_rule_reduce)?);
                )*
                $Grammar { rules }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn append_grammar_rules_single() {
        #[derive(Clone, Copy, Debug, PartialEq)]
        enum SymbolKind {
            A,
            B,
        }
        impl ParseResult<SymbolKind> for SymbolKind {
            fn kind(&self) -> SymbolKind {
                *self
            }
        }
        let mut rules = Vec::new();
        append_grammar_rules!(rules, SymbolKind, SymbolKind, A, B A:a => a);
        assert_eq!(rules.len(), 1);
        assert_eq!(rules[0].input, vec![SymbolKind::B, SymbolKind::A]);
        assert_eq!(
            (rules[0].reduce)(vec![SymbolKind::B, SymbolKind::A]),
            SymbolKind::A
        );
    }

    #[test]
    fn append_grammar_rules_multiple() {
        #[derive(Clone, Copy, Debug, PartialEq)]
        enum SymbolKind {
            A,
            B,
            C,
        }
        impl ParseResult<SymbolKind> for SymbolKind {
            fn kind(&self) -> SymbolKind {
                *self
            }
        }
        let mut rules = Vec::new();
        append_grammar_rules!(rules, SymbolKind, SymbolKind, B, {A B:b => b, B:b C => b});
        assert_eq!(rules.len(), 2);
        assert_eq!(rules[0].input, vec![SymbolKind::A, SymbolKind::B]);
        assert_eq!(
            (rules[0].reduce)(vec![SymbolKind::A, SymbolKind::B]),
            SymbolKind::B
        );
        assert_eq!(rules[1].input, vec![SymbolKind::B, SymbolKind::C]);
        assert_eq!(
            (rules[1].reduce)(vec![SymbolKind::B, SymbolKind::C]),
            SymbolKind::B
        );
    }

    #[derive(Debug, PartialEq)]
    enum TestParseResult1 {
        A,
        B,
        C,
        D,
        E,
        F,
    }

    grammar! {
        grammar TestGrammar1 {
            symbol_kind: TestSymbolKind1,
            tokens: [A, B, C],
            parse_result: TestParseResult1,
            rules: {
                D: {A B => TestParseResult1::D, B A => TestParseResult1::D},
                E: {B C => TestParseResult1::E, C B => TestParseResult1::E},
                F: A C => TestParseResult1::F,
            }
        }
    }

    impl ParseResult<TestSymbolKind1> for TestParseResult1 {
        fn kind(&self) -> TestSymbolKind1 {
            match self {
                TestParseResult1::A => TestSymbolKind1::A,
                TestParseResult1::B => TestSymbolKind1::B,
                TestParseResult1::C => TestSymbolKind1::C,
                TestParseResult1::D => TestSymbolKind1::D,
                TestParseResult1::E => TestSymbolKind1::E,
                TestParseResult1::F => TestSymbolKind1::F,
            }
        }
    }

    #[test]
    fn grammar_macro() {
        let grammar = TestGrammar1::new();
        assert_eq!(grammar.rules.len(), 5);
        assert_eq!(
            grammar.rules[0].input,
            vec![TestSymbolKind1::A, TestSymbolKind1::B]
        );
        assert_eq!(
            (grammar.rules[0].reduce)(vec![TestParseResult1::A, TestParseResult1::B]),
            TestParseResult1::D
        );
        assert_eq!(
            grammar.rules[1].input,
            vec![TestSymbolKind1::B, TestSymbolKind1::A]
        );
        assert_eq!(
            (grammar.rules[1].reduce)(vec![TestParseResult1::B, TestParseResult1::A]),
            TestParseResult1::D
        );
        assert_eq!(
            grammar.rules[2].input,
            vec![TestSymbolKind1::B, TestSymbolKind1::C]
        );
        assert_eq!(
            (grammar.rules[2].reduce)(vec![TestParseResult1::B, TestParseResult1::C]),
            TestParseResult1::E
        );
        assert_eq!(
            grammar.rules[3].input,
            vec![TestSymbolKind1::C, TestSymbolKind1::B]
        );
        assert_eq!(
            (grammar.rules[3].reduce)(vec![TestParseResult1::C, TestParseResult1::B]),
            TestParseResult1::E
        );
        assert_eq!(
            grammar.rules[4].input,
            vec![TestSymbolKind1::A, TestSymbolKind1::C]
        );
        assert_eq!(
            (grammar.rules[4].reduce)(vec![TestParseResult1::A, TestParseResult1::C]),
            TestParseResult1::F
        );
    }
}
