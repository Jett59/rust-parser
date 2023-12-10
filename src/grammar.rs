use std::fmt::{self, Debug, Formatter};

/// Represents a symbol (either a token or a non-terminal) in a grammar.
/// Symbols are generally nodes in an AST or parse tree.
///
/// # Example
/// ```
/// # use parser::grammar::Symbol;
/// # #[derive(Clone, Copy, Debug, PartialEq)]
/// # enum MySymbolKind { A, B, C, Node1, Node2 }
/// enum MySymbol  {
///     Token(MySymbolKind),
///     Node1(String),
///     Node2(Vec<MySymbol>),
/// }
///
/// impl Symbol<MySymbolKind> for MySymbol {
///     fn kind(&self) -> MySymbolKind {
///         match self {
///             MySymbol::Token(kind) => *kind,
///             MySymbol::Node1(_) => MySymbolKind::Node1,
///             MySymbol::Node2(_) => MySymbolKind::Node2,
///         }
///     }
/// }
pub trait Symbol<SymbolKind> {
    fn kind(&self) -> SymbolKind;
}

impl<T: Symbol<S>, S> Symbol<S> for Box<T> {
    fn kind(&self) -> S {
        (**self).kind()
    }
}

/// A rule to reduce a sequence of input symbols to a single output symbol.
///
/// # Example
/// ```
/// # use parser::grammar::{GrammarRule, Symbol};
/// # #[derive(Clone, Copy, Debug, PartialEq)]
/// # enum MySymbolKind { A, B, C }
/// # struct MySymbol (MySymbolKind);
/// # impl Symbol<MySymbolKind> for MySymbol {
/// #     fn kind(&self) -> MySymbolKind { self.0 }
/// # }
/// fn my_reduce_function(_symbols: Vec<MySymbol>) -> MySymbol {
///     MySymbol(MySymbolKind::C)
/// }
/// let rule = GrammarRule {
///    input: vec![MySymbolKind::A, MySymbolKind::B],
///    result: MySymbolKind::C,
///   reduce: my_reduce_function,
/// };
/// ```
pub struct GrammarRule<Symbol: self::Symbol<SymbolKind>, SymbolKind> {
    pub input: Vec<SymbolKind>,
    pub result: SymbolKind,
    pub reduce: fn(Vec<Symbol>) -> Symbol,
}

impl<Symbol: self::Symbol<SymbolKind>, SymbolKind: Debug> Debug
    for GrammarRule<Symbol, SymbolKind>
{
    fn fmt(&self, fmt: &mut Formatter<'_>) -> fmt::Result {
        fmt.debug_struct("GrammarRule")
            .field("input", &self.input)
            .field("result", &self.result)
            .finish()
    }
}

#[macro_export]
#[doc(hidden)]
macro_rules! append_grammar_rules {
    ($rules_list:ident, $SymbolKind:ty, $Symbol:ty, $result_symbol_kind:ident, $($symbol_kind:ident$(:$name:ident)?)* => $reduce:expr) => {{
        fn reduce(symbols: Vec<$Symbol>) -> $Symbol {
            let mut symbol_iterator = symbols.into_iter();
            $($(let $name =)? symbol_iterator.next().unwrap();)*
            let result = $reduce;
            assert_eq!($crate::grammar::Symbol::kind(&result), <$SymbolKind>::$result_symbol_kind, "Grammar rule returned wrong kind");
            result
        }
        $rules_list.push($crate::grammar::GrammarRule::<$Symbol, $SymbolKind> {
            input: vec![$(<$SymbolKind>::$symbol_kind),*],
            result: <$SymbolKind>::$result_symbol_kind,
            reduce,
        });
    }};
    ($rules_list:ident, $SymbolKind:ty, $Symbol:ty, $result_symbol_kind:ident, { $($($symbol_kind:ident$(:$name:ident)?)* => $reduce:expr),* $(,)? }) => {
        $(
            $crate::grammar::append_grammar_rules!($rules_list, $SymbolKind, $Symbol, $result_symbol_kind, $($symbol_kind$(:$name)?)* => $reduce);
        )*
    };
}

#[doc(hidden)]
pub use append_grammar_rules;

/// Defines a grammar based on a set of rules.
/// Each rule in the grammar is defined by a sequence of input symbols and a
/// function to reduce the input symbols to a single output symbol.
/// 
/// # Example
/// ```
/// # use parser::grammar::{Symbol, grammar};
/// # #[derive(Clone, Debug, PartialEq)]
/// # enum MySymbol { Token(MySymbolKind), Number(f64), Addition(Box<MySymbol>, Box<MySymbol>), Result(Box<MySymbol>) }
/// # impl Symbol<MySymbolKind> for MySymbol {
/// #     fn kind(&self) -> MySymbolKind {
/// #         match self {
/// #             MySymbol::Token(kind) => *kind,
/// #             MySymbol::Number(_) => MySymbolKind::Number,
/// #             MySymbol::Addition(_, _) => MySymbolKind::Addition,
/// #             MySymbol::Result(_) => MySymbolKind::Result,
/// #         }
/// #     }
/// # }
/// grammar! {
///     grammar MyGrammar {
///         symbol_kind: MySymbolKind,
///         tokens: [Plus, Semicolon, Number],
///         symbol: Box<MySymbol>,
///         rules: {
///             Addition: {
///                 Number:a Plus Number:b => Box::new(MySymbol::Addition(a, b)),
///                 Addition:a Plus Number:b => Box::new(MySymbol::Addition(a, b)),
///             },
///             Result: Addition:a Semicolon => Box::new(MySymbol::Result(a)),
///         }
///     }
/// }
#[macro_export]
macro_rules! grammar {
    ($visibility:vis grammar $Grammar:ident {
        symbol_kind: $SymbolKind:ident,
        tokens: [$($token:ident),*],
        symbol: $Symbol:ty,
        rules: {$(
            $NonTerminal:ident: $({$($grouped_rule_tokens:tt)*})? $($($optional_rule_symbol_kinds:ident$(:$optional_rule_symbol_names:ident)?)* => $optional_rule_reduce:expr)?
    ),*$(,)?}
    }) => {
        #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
        $visibility enum $SymbolKind {
            $($token,)*
            $($NonTerminal,)*
        }

        #[derive(Debug)]
        $visibility struct $Grammar {
            rules: Vec<$crate::grammar::GrammarRule<$Symbol, $SymbolKind>>,
        }

        impl $Grammar {
            $visibility fn new() -> Self {
                let mut rules = Vec::new();
                $(
                    $crate::grammar::append_grammar_rules!(rules, $SymbolKind, $Symbol, $NonTerminal, $({$($grouped_rule_tokens)*})? $($($optional_rule_symbol_kinds$(:$optional_rule_symbol_names)?)* => $optional_rule_reduce)?);
                )*
                $Grammar { rules }
            }
        }
    }
}

pub use grammar;

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
        impl Symbol<SymbolKind> for SymbolKind {
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
        impl Symbol<SymbolKind> for SymbolKind {
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
    enum TestSymbol1 {
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
            symbol: TestSymbol1,
            rules: {
                D: {A B => TestSymbol1::D, B A => TestSymbol1::D},
                E: {B C => TestSymbol1::E, C B => TestSymbol1::E},
                F: A C => TestSymbol1::F,
            }
        }
    }

    impl Symbol<TestSymbolKind1> for TestSymbol1 {
        fn kind(&self) -> TestSymbolKind1 {
            match self {
                TestSymbol1::A => TestSymbolKind1::A,
                TestSymbol1::B => TestSymbolKind1::B,
                TestSymbol1::C => TestSymbolKind1::C,
                TestSymbol1::D => TestSymbolKind1::D,
                TestSymbol1::E => TestSymbolKind1::E,
                TestSymbol1::F => TestSymbolKind1::F,
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
            (grammar.rules[0].reduce)(vec![TestSymbol1::A, TestSymbol1::B]),
            TestSymbol1::D
        );
        assert_eq!(
            grammar.rules[1].input,
            vec![TestSymbolKind1::B, TestSymbolKind1::A]
        );
        assert_eq!(
            (grammar.rules[1].reduce)(vec![TestSymbol1::B, TestSymbol1::A]),
            TestSymbol1::D
        );
        assert_eq!(
            grammar.rules[2].input,
            vec![TestSymbolKind1::B, TestSymbolKind1::C]
        );
        assert_eq!(
            (grammar.rules[2].reduce)(vec![TestSymbol1::B, TestSymbol1::C]),
            TestSymbol1::E
        );
        assert_eq!(
            grammar.rules[3].input,
            vec![TestSymbolKind1::C, TestSymbolKind1::B]
        );
        assert_eq!(
            (grammar.rules[3].reduce)(vec![TestSymbol1::C, TestSymbol1::B]),
            TestSymbol1::E
        );
        assert_eq!(
            grammar.rules[4].input,
            vec![TestSymbolKind1::A, TestSymbolKind1::C]
        );
        assert_eq!(
            (grammar.rules[4].reduce)(vec![TestSymbol1::A, TestSymbol1::C]),
            TestSymbol1::F
        );
    }

    grammar! {
        grammar TestGrammar2 {
            symbol_kind: TestSymbolKind2,
            tokens: [A],
            symbol: TestSymbol2,
            rules: {
                ParsedA: A:a => TestSymbol2::Parsed(a.kind()),
            }
        }
    }

    #[derive(Debug, PartialEq)]
    enum TestSymbol2 {
        Token(TestSymbolKind2),
        Parsed(TestSymbolKind2),
    }

    impl Symbol<TestSymbolKind2> for TestSymbol2 {
        fn kind(&self) -> TestSymbolKind2 {
            match self {
                TestSymbol2::Token(TestSymbolKind2::A) => TestSymbolKind2::A,
                TestSymbol2::Parsed(TestSymbolKind2::A) => TestSymbolKind2::ParsedA,
                _ => panic!("Unexpected parse result: {:?}", self),
            }
        }
    }

    #[test]
    fn grammar_macro_captured_input() {
        let grammar = TestGrammar2::new();
        assert_eq!(grammar.rules.len(), 1);
        assert_eq!(grammar.rules[0].input, vec![TestSymbolKind2::A]);
        assert_eq!(
            (grammar.rules[0].reduce)(vec![TestSymbol2::Token(TestSymbolKind2::A)]),
            TestSymbol2::Parsed(TestSymbolKind2::A)
        );
    }
}
