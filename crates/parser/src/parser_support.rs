use crate::ast::{self, ListRef, Span, Symbol};

#[inline(always)]
pub fn span1(pos: usize) -> Span {
    Span::new(pos, pos + 1)
}

#[inline(always)]
pub fn span2(pos: usize) -> Span {
    Span::new(pos, pos + 2)
}

#[inline(always)]
pub fn span3(pos: usize) -> Span {
    Span::new(pos, pos + 3)
}

#[allow(dead_code)]
#[derive(Clone, Debug)]
pub enum _NamedTypeTail {
    Unqual {
        args: (
            Option<Span>,
            ListRef<ast::TypeId>,
            Option<Span>,
            Option<Span>,
        ),
    },
    Qual {
        dot_pos: Span,
        name: (Symbol, Span),
        args: (
            Option<Span>,
            ListRef<ast::TypeId>,
            Option<Span>,
            Option<Span>,
        ),
    },
}
