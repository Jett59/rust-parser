use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
    hash::Hash,
};

use crate::grammar::{self, GrammarRule};

type State = u32;

#[derive(Clone, Debug)]
enum Action<Symbol: grammar::Symbol<SymbolKind>, SymbolKind: Hash> {
    Shift(HashMap<SymbolKind, State>),
    Reduce(usize, fn(Vec<Symbol>) -> Symbol, State),
    Accept,
}

impl<Symbol: grammar::Symbol<SymbolKind>, SymbolKind: Hash> Action<Symbol, SymbolKind> {
    fn is_shift(&self) -> bool {
        matches!(self, Action::Shift(_))
    }

    fn is_reduce(&self) -> bool {
        matches!(self, Action::Reduce(_, _, _))
    }

    fn is_accept(&self) -> bool {
        matches!(self, Action::Accept)
    }
}

pub struct Parser<Symbol: grammar::Symbol<SymbolKind>, SymbolKind: Eq + Clone + Hash> {
    parse_table: HashMap<State, Action<Symbol, SymbolKind>>,
}

impl<Symbol: grammar::Symbol<SymbolKind>, SymbolKind> Parser<Symbol, SymbolKind>
where
    SymbolKind: Eq + Clone + Hash + Debug,
{
    pub fn new(
        rules: &[grammar::GrammarRule<Symbol, SymbolKind>],
        root_symbol: SymbolKind,
        tokens: &[SymbolKind],
    ) -> Self {
        let mut states = (0 as State)..;
        macro_rules! next_state {
            () => {
                states.next().unwrap()
            };
        }
        let mut parse_table = HashMap::new();
        let mut stack_to_state = HashMap::new();
        let mut incomplete_rules: Vec<(State, Vec<SymbolKind>)> = vec![(next_state!(), vec![])];
        while !incomplete_rules.is_empty() {
            let mut new_incomplete_rules = Vec::new();
            for (state, stack) in incomplete_rules {
                dbg!(state);
                dbg!(&stack);
                if stack.len() == 1 && stack[0] == root_symbol {
                    parse_table.insert(state, Action::Accept);
                    continue;
                }
                let mut valid_shifts = Vec::new();
                for token in tokens {
                    dbg!(token);
                    let mut new_stack = stack.clone();
                    new_stack.push(token.clone());
                    if Self::potentially_valid_stack(new_stack, &root_symbol, rules) {
                        valid_shifts.push(token.clone());
                    }
                }
                let mut valid_reductions = Vec::new();
                for rule in rules {
                    if stack.len() >= rule.input.len()
                        && stack[stack.len() - rule.input.len()..]
                            .iter()
                            .enumerate()
                            .all(|(i, symbol)| symbol == &rule.input[i])
                    {
                        let mut new_stack = stack[..stack.len() - rule.input.len()].to_vec();
                        new_stack.push(rule.result.clone());
                        if Self::potentially_valid_stack(new_stack, &root_symbol, rules) {
                            valid_reductions.push(rule);
                        }
                    }
                }
                dbg!(&valid_shifts);
                dbg!(&valid_reductions);
                if valid_shifts.is_empty() && valid_reductions.is_empty() {
                    panic!("No valid actions for state {:?}", state);
                } else if !valid_shifts.is_empty() && !valid_reductions.is_empty() {
                    panic!(
                        "Shift/reduce conflict: {:?}: shift {:?}, reduce {:?}",
                        stack, valid_shifts, valid_reductions
                    );
                } else if !valid_shifts.is_empty() {
                    let mut shift_map = HashMap::new();
                    for token in valid_shifts {
                        let new_state = next_state!();
                        shift_map.insert(token.clone(), new_state);
                        let mut new_stack = stack.clone();
                        new_stack.push(token);
                        stack_to_state.insert(new_stack.clone(), new_state);
                        new_incomplete_rules.push((new_state, new_stack));
                    }
                    parse_table.insert(state, Action::Shift(shift_map));
                } else if !valid_reductions.is_empty() {
                    if valid_reductions.len() > 1 {
                        panic!(
                            "Reduce/reduce conflict: {:?}: reduce {:?}",
                            stack, valid_reductions
                        );
                    }
                    let rule = &valid_reductions[0];
                    let mut new_stack = stack[..stack.len() - rule.input.len()].to_vec();
                    new_stack.push(rule.result.clone());
                    if let Some(next_state) = stack_to_state.get(&new_stack) {
                        parse_table.insert(
                            state,
                            Action::Reduce(rule.input.len(), rule.reduce, *next_state),
                        );
                    } else {
                        let new_state = next_state!();
                        parse_table.insert(
                            state,
                            Action::Reduce(rule.input.len(), rule.reduce, new_state),
                        );
                        stack_to_state.insert(new_stack.clone(), new_state);
                        new_incomplete_rules.push((new_state, new_stack));
                    }
                }
            }
            incomplete_rules = new_incomplete_rules;
        }
        Self { parse_table }
    }

    fn potentially_valid_stack(
        stack: Vec<SymbolKind>,
        root_symbol: &SymbolKind,
        rules: &[GrammarRule<Symbol, SymbolKind>],
    ) -> bool {
        let mut tried_stacks = HashSet::new();
        let mut latest_stack_attempts = vec![stack];
        while !latest_stack_attempts.is_empty() {
            let mut new_latest_stack_attempts = Vec::new();
            for stack in latest_stack_attempts {
                println!("Attempt begin");
                dbg!(&stack);
                if stack.len() == 1 && stack[0] == *root_symbol {
                    println!("Accept");
                    return true;
                }
                for rule in rules {
                    dbg!(rule);
                    for symbol_count in (1..=rule.input.len())
                        .filter(|symbol_count| &stack.len() >= symbol_count)
                        .filter(|symbol_count| {
                            &stack[stack.len() - symbol_count..] == &rule.input[..*symbol_count]
                        })
                    {
                        dbg!(symbol_count);
                        let mut new_stack = stack[..stack.len() - symbol_count].to_vec();
                        new_stack.push(rule.result.clone());
                        if !tried_stacks.contains(&new_stack) {
                            dbg!("New stack");
                            tried_stacks.insert(new_stack.clone());
                            new_latest_stack_attempts.push(new_stack);
                        }
                    }
                }
            }
            latest_stack_attempts = new_latest_stack_attempts;
        }
        false
    }

    fn parse(&self, tokens: &mut dyn Iterator<Item = Symbol>) -> Symbol {
        let mut tokens = tokens.peekable();
        let mut state = 0;
        let mut stack = Vec::new();
        loop {
            let action = self
                .parse_table
                .get(&state)
                .expect("Missing state from parse table");
            match action {
                Action::Shift(shift_table) => {
                    let lookahead = tokens.peek().expect(
                        format!(
                            "Unexpected end of input, expecting one of {:?}",
                            shift_table.keys()
                        )
                        .as_str(),
                    );
                    let new_state = shift_table.get(&lookahead.kind()).expect(
                        format!(
                            "Invalid token {:?}, expecting {:?}",
                            lookahead.kind(),
                            shift_table.keys()
                        )
                        .as_str(),
                    );
                    stack.push(tokens.next().unwrap());
                    state = *new_state;
                }
                Action::Reduce(input_length, reduce, new_state) => {
                    let mut reduce_stack = Vec::new();
                    for _ in 0..*input_length {
                        reduce_stack.push(stack.pop().unwrap());
                    }
                    reduce_stack.reverse();
                    stack.push(reduce(reduce_stack));
                    state = *new_state;
                }
                Action::Accept => {
                    assert!(tokens.next().is_none(), "Unexpected trailing tokens");
                    return stack.pop().unwrap();
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_create_parser_basic() {
        #[derive(Clone, Debug, Eq, Hash, PartialEq)]
        enum Symbol {
            Token(SymbolKind),
            Addition(Box<Symbol>, Box<Symbol>),
            Result(Box<Symbol>),
        }
        impl grammar::Symbol<SymbolKind> for Symbol {
            fn kind(&self) -> SymbolKind {
                match self {
                    Symbol::Token(kind) => *kind,
                    Symbol::Addition(_, _) => SymbolKind::Addition,
                    Symbol::Result(_) => SymbolKind::Result,
                }
            }
        }
        grammar::grammar! {
            grammar Grammar {
                symbol_kind: SymbolKind,
                tokens: [Plus, Semicolon, Number],
                symbol: Box<Symbol>,
                rules: {
                                 Addition: {
                 Number:a Plus Number:b => Box::new(Symbol::Addition(a, b)),
                 Addition:a Plus Number:b => Box::new(Symbol::Addition(a, b)),
             },
             Result: Addition:a Semicolon => Box::new(Symbol::Result(a)),
                }
            }
        }

        let grammar = Grammar::new();

        let parser = Parser::new(
            &grammar.rules,
            SymbolKind::Result,
            &[SymbolKind::Number, SymbolKind::Plus, SymbolKind::Semicolon],
        );
        let input = vec![
            Box::new(Symbol::Token(SymbolKind::Number)),
            Box::new(Symbol::Token(SymbolKind::Plus)),
            Box::new(Symbol::Token(SymbolKind::Number)),
            Box::new(Symbol::Token(SymbolKind::Semicolon)),
        ];
        let result = parser.parse(&mut input.into_iter());
        assert_eq!(
            result,
            Box::new(Symbol::Result(Box::new(Symbol::Addition(
                Box::new(Symbol::Token(SymbolKind::Number)),
                Box::new(Symbol::Token(SymbolKind::Number))
            ))))
        );
        let input = vec![
            Box::new(Symbol::Token(SymbolKind::Number)),
            Box::new(Symbol::Token(SymbolKind::Plus)),
            Box::new(Symbol::Token(SymbolKind::Number)),
            Box::new(Symbol::Token(SymbolKind::Plus)),
            Box::new(Symbol::Token(SymbolKind::Number)),
            Box::new(Symbol::Token(SymbolKind::Semicolon)),
        ];
        let result = parser.parse(&mut input.into_iter());
        assert_eq!(
            result,
            Box::new(Symbol::Result(Box::new(Symbol::Addition(
                Box::new(Symbol::Addition(
                    Box::new(Symbol::Token(SymbolKind::Number)),
                    Box::new(Symbol::Token(SymbolKind::Number))
                )),
                Box::new(Symbol::Token(SymbolKind::Number))
            ))))
        );
    }
}
