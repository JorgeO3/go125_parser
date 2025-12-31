use crate::ast::*;

// Core traits
pub trait Walk<'ast> {
    fn walk<V: Visitor<'ast> + ?Sized>(&self, a: &'ast AstArena, v: &mut V);
}

pub trait Visitor<'ast> {
    #[inline(always)]
    fn visit_source_file(&mut self, a: &'ast AstArena, f: &'ast SourceFile) {
        f.walk(a, self);
    }

    #[inline(always)]
    fn visit_decl(&mut self, a: &'ast AstArena, id: DeclId) {
        a.decls[id].walk(a, self);
    }

    #[inline(always)]
    fn visit_func_decl(&mut self, a: &'ast AstArena, id: FuncDeclId) {
        a.funcs[id].walk(a, self);
    }

    #[inline(always)]
    fn visit_stmt(&mut self, a: &'ast AstArena, id: StmtId) {
        a.stmts[id].walk(a, self);
    }

    #[inline(always)]
    fn visit_simple_stmt(&mut self, a: &'ast AstArena, id: SimpleStmtId) {
        a.simple_stmts[id].walk(a, self);
    }

    #[inline(always)]
    fn visit_expr(&mut self, a: &'ast AstArena, id: ExprId) {
        a.exprs[id].walk(a, self);
    }

    #[inline(always)]
    fn visit_type(&mut self, a: &'ast AstArena, id: TypeId) {
        a.types[id].walk(a, self);
    }

    #[inline(always)]
    fn visit_field(&mut self, a: &'ast AstArena, id: FieldId) {
        a.fields[id].walk(a, self);
    }

    #[inline(always)]
    fn visit_signature(&mut self, a: &'ast AstArena, id: SignatureId) {
        a.signatures[id].walk(a, self);
    }

    #[inline(always)]
    fn visit_switch_clause(&mut self, a: &'ast AstArena, id: SwitchClauseId) {
        a.switch_clauses[id].walk(a, self);
    }

    #[inline(always)]
    fn visit_comm_clause(&mut self, a: &'ast AstArena, id: CommClauseId) {
        a.comm_clauses[id].walk(a, self);
    }

    #[inline(always)]
    fn visit_type_params(&mut self, a: &'ast AstArena, id: TypeParamsId) {
        a.type_params[id].walk(a, self);
    }

    #[inline(always)]
    fn visit_type_param_decl(&mut self, a: &'ast AstArena, id: TypeParamDeclId) {
        a.type_param_decls[id].walk(a, self);
    }

    #[inline(always)]
    fn visit_comment(&mut self, a: &'ast AstArena, id: CommentId) {
        a.comments[id].walk(a, self);
    }

    #[inline(always)]
    fn visit_comment_group(&mut self, a: &'ast AstArena, id: CommentGroupId) {
        a.comment_groups[id].walk(a, self);
    }
}

// Macro para reducir boilerplate de IDs
macro_rules! impl_walk_for_ids {
    ($($id:ty => $visit:ident),* $(,)?) => {
        $(
            impl<'ast> Walk<'ast> for $id {
                #[inline(always)]
                fn walk<V: Visitor<'ast> + ?Sized>(&self, a: &'ast AstArena, v: &mut V) {
                    v.$visit(a, *self);
                }
            }
        )*
    };
}

impl_walk_for_ids! {
    DeclId => visit_decl,
    FuncDeclId => visit_func_decl,
    StmtId => visit_stmt,
    SimpleStmtId => visit_simple_stmt,
    ExprId => visit_expr,
    TypeId => visit_type,
    FieldId => visit_field,
    SignatureId => visit_signature,
    SwitchClauseId => visit_switch_clause,
    CommClauseId => visit_comm_clause,
    TypeParamsId => visit_type_params,
    TypeParamDeclId => visit_type_param_decl,
    CommentId => visit_comment,
    CommentGroupId => visit_comment_group,
}

// ListSlice trait - versión mejorada con mejor ergonomía
pub trait ListSlice<T> {
    fn slice(&self, r: ListRef<T>) -> &[T];
}

// Implementación genérica para ListRef
impl<'ast, T> Walk<'ast> for ListRef<T>
where
    AstArena: ListSlice<T>,
    T: Walk<'ast>,
{
    #[inline(always)]
    fn walk<V: Visitor<'ast> + ?Sized>(&self, a: &'ast AstArena, v: &mut V) {
        for item in a.slice(*self) {
            item.walk(a, v);
        }
    }
}

// Caso especial: no recorrer identifiers
impl<'ast> Walk<'ast> for ListRef<Ident> {
    #[inline(always)]
    fn walk<V: Visitor<'ast> + ?Sized>(&self, _: &'ast AstArena, _: &mut V) {}
}

// Macro mejorada para ListSlice
macro_rules! impl_list_slice {
    ($($t:ty => $getter:ident),* $(,)?) => {
        $(
            impl ListSlice<$t> for AstArena {
                #[inline(always)]
                fn slice(&self, r: ListRef<$t>) -> &[$t] {
                    self.$getter(r)
                }
            }
        )*
    };
}

impl_list_slice! {
    ExprId => exprs_list,
    StmtId => stmts_list,
    TypeId => types_list,
    FieldId => fields_list,
    KeyedElement => keyed_elems_list,
    Spec => specs_list,
    TopLevelDecl => top_decls,
    SwitchClauseId => switch_clause_ids,
    CommClauseId => comm_clause_ids,
    TypeCaseElem => type_case_elems,
    TypeTerm => type_terms,
    InterfaceElem => interface_elems,
    IdentName => ident_names,
    TypeParamDeclId => type_param_decl_ids,
    CommentId => comment_ids,
    CommentGroupId => comment_group_ids,
    ExprOrType => expr_or_types,
}

// Utilities
impl<'ast, T: Walk<'ast>> Walk<'ast> for Option<T> {
    #[inline(always)]
    fn walk<V: Visitor<'ast> + ?Sized>(&self, a: &'ast AstArena, v: &mut V) {
        if let Some(x) = self {
            x.walk(a, v);
        }
    }
}

impl<'ast> Walk<'ast> for Block {
    #[inline(always)]
    fn walk<V: Visitor<'ast> + ?Sized>(&self, a: &'ast AstArena, v: &mut V) {
        self.stmts.walk(a, v);
    }
}

// No-ops para tipos hoja
macro_rules! impl_walk_noop {
    ($($ty:ty),* $(,)?) => {
        $(
            impl<'ast> Walk<'ast> for $ty {
                #[inline(always)]
                fn walk<V: Visitor<'ast> + ?Sized>(&self, _: &'ast AstArena, _: &mut V) {}
            }
        )*
    };
}

impl_walk_noop! {
    Span,
    Symbol,
    BasicLit,
    StringLit,
    bool,
    GenDeclKind,
    BasicLitKind,
    ChanDir,
    UnaryOp,
    BinaryOp,
    AssignOp,
    IncDecOp,
    CommentKind,
}
