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

#[derive(Clone, Debug)]
pub struct ParamDecl {
    pub names: Vec<ast::IdentName>,
    pub ellipsis_pos: Option<Span>,
    pub typ: Option<ast::TypeId>,
    pub span: Span,
}

pub fn resolve_param_list(
    arena: &mut ast::AstArena,
    params: Vec<ParamDecl>,
) -> Vec<ast::FieldId> {
    let mut out = Vec::new();
    let mut pending_names: Vec<ast::IdentName> = Vec::new();
    let mut pending_start: Option<u32> = None;

    for param in params {
        if let Some(typ) = param.typ {
            let mut names = Vec::new();
            if !pending_names.is_empty() {
                names.append(&mut pending_names);
            }
            names.extend(param.names);

            let names_ref = if names.is_empty() {
                ast::ListRef::EMPTY
            } else {
                arena.list_ident_names(names)
            };
            let start = pending_start.unwrap_or(param.span.start);
            let span = Span {
                start,
                end: param.span.end,
            };
            let field = ast::Field {
                names: names_ref,
                ellipsis_pos: param.ellipsis_pos,
                typ,
                tag: None,
                is_embed: false,
                comma: None,
                doc: None,
                comment: None,
            };
            out.push(arena.fields.alloc(field, span));
            pending_start = None;
        } else {
            if pending_names.is_empty() {
                pending_start = Some(param.span.start);
            }
            pending_names.extend(param.names);
        }
    }

    if !pending_names.is_empty() {
        for name in pending_names {
            let typ = named_type_from_ident(arena, name);
            let span = name.pos;
            let field = ast::Field {
                names: ast::ListRef::EMPTY,
                ellipsis_pos: None,
                typ,
                tag: None,
                is_embed: false,
                comma: None,
                doc: None,
                comment: None,
            };
            out.push(arena.fields.alloc(field, span));
        }
    }

    out
}

fn named_type_from_ident(arena: &mut ast::AstArena, name: ast::IdentName) -> ast::TypeId {
    arena.types.alloc(
        ast::Type::Named {
            pkg: None,
            pkg_pos: None,
            dot_pos: None,
            name: name.sym,
            name_pos: name.pos,
            args_lbrack: None,
            args: ast::ListRef::EMPTY,
            args_trailing_comma: None,
            args_rbrack: None,
        },
        name.pos,
    )
}
