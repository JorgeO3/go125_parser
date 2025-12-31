//! AST (Abstract Syntax Tree) module for a Go language parser.
//!
//! Production goals:
//! - Low allocation / low latency parsing (avoid per-node Vec allocations).
//! - Compact memory layout and good cache locality.
//! - Type-safe IDs (no raw indices in the public API).
//! - Clear, readable data structures.
//!
//! Key optimizations implemented here (inspired by Zig/Clang/rustc-style layouts):
//! 1) **Side-table spans**: spans are stored in parallel vectors, not inside each node.
//!    This keeps the arena element "stride" smaller and improves cache locality.
//! 2) **Extra-data lists (Zig-style)**: variable-length lists are stored in centralized
//!    "extra" buffers and referenced by `ListRef` (start,len) instead of `Vec` per node.
//!    This massively reduces heap churn in parsing hot paths.
//! 3) **No string cloning in interner map**: the interner hashes strings (FNV-1a) and
//!    stores only hash -> candidate symbols, with collision checks against the canonical storage.
//!
//! Notes:
//! - This module is *syntax-level*. For semantic stages, you may build a separate HIR
//!   with different tradeoffs (e.g. interned symbols, resolved names, types, etc.).
//! - `Ident` is an interned `Symbol` here. If you want even lower parse-time overhead,
//!   you can store identifier *spans* or *token IDs* during parsing and intern lazily later.

use core::marker::PhantomData;
use core::ops::{Index, IndexMut};
use smallvec::SmallVec;
use std::collections::HashMap;

// =============================================================================
// Core: IDs, Span, ListRef
// =============================================================================

/// Compact source span (byte offsets in the source file).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

impl Span {
    #[inline]
    pub fn new(start: usize, end: usize) -> Self {
        // If you parse files >4GiB, widen to u64.
        Self {
            start: start as u32,
            end: end as u32,
        }
    }

    #[inline]
    pub fn len(self) -> u32 {
        self.end.saturating_sub(self.start)
    }

    #[inline]
    pub fn is_empty(self) -> bool {
        self.start >= self.end
    }
}

/// A compact handle into an arena for a specific node type.
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
#[repr(transparent)]
pub struct Id<T> {
    raw: u32,
    _marker: PhantomData<fn() -> T>,
}

impl<T> Id<T> {
    #[inline]
    pub const fn from_raw(raw: u32) -> Self {
        Self {
            raw,
            _marker: PhantomData,
        }
    }

    #[inline]
    pub const fn raw(&self) -> u32 {
        self.raw
    }

    #[inline]
    pub const fn to_usize(&self) -> usize {
        self.raw as usize
    }
}

/// A reference to a list stored in an "extra data" buffer.
///
/// This replaces `Vec<T>` stored inside nodes, avoiding per-node allocations.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct ListRef {
    pub start: u32,
    pub len: u32,
}

impl ListRef {
    pub const EMPTY: ListRef = ListRef { start: 0, len: 0 };

    #[inline]
    pub const fn is_empty(self) -> bool {
        self.len == 0
    }

    #[inline]
    pub const fn end(self) -> u32 {
        self.start + self.len
    }
}

// =============================================================================
// Symbol + Interner (fast, allocation-light)
// =============================================================================

/// An interned string handle.
///
/// `u32` is enough for most compilers. If you expect >4B unique strings, widen.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Symbol(u32);

impl Symbol {
    #[inline]
    pub const fn as_u32(self) -> u32 {
        self.0
    }

    #[inline]
    pub const fn from_u32(v: u32) -> Self {
        Symbol(v)
    }
}

/// Identifiers are interned symbols.
pub type Ident = Symbol;

/// Very fast, deterministic 64-bit FNV-1a hash (good enough for an interner).
#[inline]
fn fnv1a64(bytes: &[u8]) -> u64 {
    const OFFSET: u64 = 0xcbf29ce484222325;
    const PRIME: u64 = 0x100000001b3;

    let mut h = OFFSET;
    for &b in bytes {
        h ^= b as u64;
        h = h.wrapping_mul(PRIME);
    }
    h
}

/// Simple string interner optimized to avoid double-allocations/clones.
///
/// Layout:
/// - `strings`: canonical storage of each interned string (Box<str>) by Symbol index.
/// - `buckets`: hash -> candidate Symbol list (collision chain).
///
/// On lookup, we hash + compare against the canonical strings (only for collisions).
#[derive(Debug, Default)]
pub struct Interner {
    strings: Vec<Box<str>>,
    buckets: HashMap<u64, SmallVec<[Symbol; 1]>>,
}

impl Interner {
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.strings.len()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.strings.is_empty()
    }

    /// Interns `s` and returns its `Symbol`.
    ///
    /// Hot path: no allocations on hits; on misses, only one allocation for the canonical string.
    pub fn intern(&mut self, s: &str) -> Symbol {
        let h = fnv1a64(s.as_bytes());

        if let Some(cands) = self.buckets.get(&h) {
            // Collision check: usually 1 candidate.
            for &sym in cands {
                if self.strings[sym.as_u32() as usize].as_ref() == s {
                    return sym;
                }
            }
        }

        // Miss: allocate exactly once for canonical storage.
        let sym = Symbol::from_u32(self.strings.len() as u32);
        self.strings.push(s.into());

        self.buckets.entry(h).or_default().push(sym);

        sym
    }

    #[inline]
    pub fn resolve(&self, sym: Symbol) -> &str {
        &self.strings[sym.as_u32() as usize]
    }
}

// =============================================================================
// Arena implementation + spans side tables
// =============================================================================

/// A compact arena storing `T` densely in a Vec.
///
/// This is intentionally simple:
/// - IDs are stable (indices).
/// - No deallocation (bump-like).
#[derive(Debug)]
pub struct Arena<T> {
    data: Vec<T>,
}

impl<T> Default for Arena<T> {
    #[inline]
    fn default() -> Self {
        Self { data: Vec::new() }
    }
}

impl<T> Arena<T> {
    #[inline]
    pub fn new() -> Self {
        Self { data: Vec::new() }
    }

    #[inline]
    pub fn with_capacity(cap: usize) -> Self {
        Self {
            data: Vec::with_capacity(cap),
        }
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.data.len()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    #[inline]
    pub fn reserve(&mut self, additional: usize) {
        self.data.reserve(additional);
    }

    #[inline]
    pub fn alloc(&mut self, value: T) -> Id<T> {
        let id = Id::<T>::from_raw(self.data.len() as u32);
        self.data.push(value);
        id
    }

    #[inline]
    pub fn get(&self, id: Id<T>) -> &T {
        &self.data[id.to_usize()]
    }

    #[inline]
    pub fn get_mut(&mut self, id: Id<T>) -> &mut T {
        &mut self.data[id.to_usize()]
    }

    #[inline]
    pub fn iter(&self) -> core::slice::Iter<'_, T> {
        self.data.iter()
    }
}

impl<T> Index<Id<T>> for Arena<T> {
    type Output = T;
    #[inline]
    fn index(&self, id: Id<T>) -> &Self::Output {
        self.get(id)
    }
}

impl<T> IndexMut<Id<T>> for Arena<T> {
    #[inline]
    fn index_mut(&mut self, id: Id<T>) -> &mut Self::Output {
        self.get_mut(id)
    }
}

/// An arena plus a side table of spans.
///
/// This keeps node payload small and makes span access explicit.
#[derive(Debug)]
pub struct SpannedArena<T> {
    nodes: Arena<T>,
    spans: Vec<Span>,
}

impl<T> Default for SpannedArena<T> {
    #[inline]
    fn default() -> Self {
        Self {
            nodes: Arena::default(),
            spans: Vec::new(),
        }
    }
}

impl<T> SpannedArena<T> {
    #[inline]
    pub fn new() -> Self {
        Self {
            nodes: Arena::new(),
            spans: Vec::new(),
        }
    }

    #[inline]
    pub fn with_capacity(cap: usize) -> Self {
        Self {
            nodes: Arena::with_capacity(cap),
            spans: Vec::with_capacity(cap),
        }
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.nodes.len()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.nodes.is_empty()
    }

    #[inline]
    pub fn reserve(&mut self, additional: usize) {
        self.nodes.reserve(additional);
        self.spans.reserve(additional);
    }

    #[inline]
    pub fn alloc(&mut self, node: T, span: Span) -> Id<T> {
        let id = self.nodes.alloc(node);
        debug_assert_eq!(self.spans.len(), id.to_usize());
        self.spans.push(span);
        id
    }

    #[inline]
    pub fn get(&self, id: Id<T>) -> &T {
        self.nodes.get(id)
    }

    #[inline]
    pub fn get_mut(&mut self, id: Id<T>) -> &mut T {
        self.nodes.get_mut(id)
    }

    #[inline]
    pub fn span(&self, id: Id<T>) -> Span {
        self.spans[id.to_usize()]
    }
}

impl<T> Index<Id<T>> for SpannedArena<T> {
    type Output = T;
    #[inline]
    fn index(&self, id: Id<T>) -> &Self::Output {
        self.get(id)
    }
}

impl<T> IndexMut<Id<T>> for SpannedArena<T> {
    #[inline]
    fn index_mut(&mut self, id: Id<T>) -> &mut Self::Output {
        self.get_mut(id)
    }
}

// =============================================================================
// Extra data buffers (Zig-style list storage)
// =============================================================================

#[derive(Debug, Default)]
pub struct Extras {
    pub idents: Vec<Ident>,
    pub exprs: Vec<ExprId>,
    pub stmts: Vec<StmtId>,
    pub types: Vec<TypeId>,
    pub elements: Vec<Element>,
    pub fields: Vec<FieldId>,
    pub switch_clauses: Vec<SwitchClauseId>,
    pub comm_clauses: Vec<CommClauseId>,
}

impl Extras {
    #[inline]
    fn push_list<T>(buf: &mut Vec<T>, items: impl IntoIterator<Item = T>) -> ListRef {
        let start = buf.len();
        buf.extend(items);
        let len = buf.len() - start;

        debug_assert!(start <= u32::MAX as usize);
        debug_assert!(len <= u32::MAX as usize);

        ListRef {
            start: start as u32,
            len: len as u32,
        }
    }

    #[inline]
    fn slice<T>(buf: &[T], list: ListRef) -> &[T] {
        let s = list.start as usize;
        let e = (list.start + list.len) as usize;
        &buf[s..e]
    }
}

// =============================================================================
// Public IDs
// =============================================================================

pub type DeclId = Id<Decl>;
pub type StmtId = Id<Stmt>;
pub type ExprId = Id<Expr>;
pub type TypeId = Id<Type>;
pub type SignatureId = Id<Signature>;
pub type FuncDeclId = Id<FuncDecl>;
pub type FieldId = Id<Field>;
pub type SwitchClauseId = Id<SwitchClause>;
pub type CommClauseId = Id<CommClause>;

// =============================================================================
// Central AST arena
// =============================================================================

#[derive(Debug, Default)]
pub struct AstArena {
    pub decls: SpannedArena<Decl>,
    pub stmts: SpannedArena<Stmt>,
    pub exprs: SpannedArena<Expr>,
    pub types: SpannedArena<Type>,
    pub signatures: SpannedArena<Signature>,
    pub funcs: SpannedArena<FuncDecl>,
    pub fields: SpannedArena<Field>,
    pub switch_clauses: SpannedArena<SwitchClause>,
    pub comm_clauses: SpannedArena<CommClause>,

    pub extras: Extras,
}

impl AstArena {
    #[inline]
    pub fn new() -> Self {
        Default::default()
    }

    // -------------------------
    // Allocation helpers
    // -------------------------

    #[inline]
    pub fn alloc_decl(&mut self, node: Decl, span: Span) -> DeclId {
        self.decls.alloc(node, span)
    }

    #[inline]
    pub fn alloc_stmt(&mut self, node: Stmt, span: Span) -> StmtId {
        self.stmts.alloc(node, span)
    }

    #[inline]
    pub fn alloc_expr(&mut self, node: Expr, span: Span) -> ExprId {
        self.exprs.alloc(node, span)
    }

    #[inline]
    pub fn alloc_type(&mut self, node: Type, span: Span) -> TypeId {
        self.types.alloc(node, span)
    }

    #[inline]
    pub fn alloc_signature(&mut self, node: Signature, span: Span) -> SignatureId {
        self.signatures.alloc(node, span)
    }

    #[inline]
    pub fn alloc_func(&mut self, node: FuncDecl, span: Span) -> FuncDeclId {
        self.funcs.alloc(node, span)
    }

    #[inline]
    pub fn alloc_field(&mut self, node: Field, span: Span) -> FieldId {
        self.fields.alloc(node, span)
    }

    #[inline]
    pub fn alloc_switch_clause(&mut self, node: SwitchClause, span: Span) -> SwitchClauseId {
        self.switch_clauses.alloc(node, span)
    }

    #[inline]
    pub fn alloc_comm_clause(&mut self, node: CommClause, span: Span) -> CommClauseId {
        self.comm_clauses.alloc(node, span)
    }

    // -------------------------
    // Span access (side tables)
    // -------------------------

    #[inline]
    pub fn decl_span(&self, id: DeclId) -> Span {
        self.decls.span(id)
    }

    #[inline]
    pub fn stmt_span(&self, id: StmtId) -> Span {
        self.stmts.span(id)
    }

    #[inline]
    pub fn expr_span(&self, id: ExprId) -> Span {
        self.exprs.span(id)
    }

    #[inline]
    pub fn type_span(&self, id: TypeId) -> Span {
        self.types.span(id)
    }

    #[inline]
    pub fn signature_span(&self, id: SignatureId) -> Span {
        self.signatures.span(id)
    }

    #[inline]
    pub fn func_span(&self, id: FuncDeclId) -> Span {
        self.funcs.span(id)
    }

    // -------------------------
    // Extra-data builders
    // -------------------------

    #[inline]
    pub fn list_idents(&mut self, items: impl IntoIterator<Item = Ident>) -> ListRef {
        Extras::push_list(&mut self.extras.idents, items)
    }

    #[inline]
    pub fn list_exprs(&mut self, items: impl IntoIterator<Item = ExprId>) -> ListRef {
        Extras::push_list(&mut self.extras.exprs, items)
    }

    #[inline]
    pub fn list_stmts(&mut self, items: impl IntoIterator<Item = StmtId>) -> ListRef {
        Extras::push_list(&mut self.extras.stmts, items)
    }

    #[inline]
    pub fn list_types(&mut self, items: impl IntoIterator<Item = TypeId>) -> ListRef {
        Extras::push_list(&mut self.extras.types, items)
    }

    #[inline]
    pub fn list_elements(&mut self, items: impl IntoIterator<Item = Element>) -> ListRef {
        Extras::push_list(&mut self.extras.elements, items)
    }

    #[inline]
    pub fn list_fields(&mut self, items: impl IntoIterator<Item = FieldId>) -> ListRef {
        Extras::push_list(&mut self.extras.fields, items)
    }

    #[inline]
    pub fn list_switch_clauses(
        &mut self,
        items: impl IntoIterator<Item = SwitchClauseId>,
    ) -> ListRef {
        Extras::push_list(&mut self.extras.switch_clauses, items)
    }

    #[inline]
    pub fn list_comm_clauses(&mut self, items: impl IntoIterator<Item = CommClauseId>) -> ListRef {
        Extras::push_list(&mut self.extras.comm_clauses, items)
    }

    // -------------------------
    // Extra-data slice accessors
    // -------------------------

    #[inline]
    pub fn idents(&self, list: ListRef) -> &[Ident] {
        Extras::slice(&self.extras.idents, list)
    }

    #[inline]
    pub fn exprs_list(&self, list: ListRef) -> &[ExprId] {
        Extras::slice(&self.extras.exprs, list)
    }

    #[inline]
    pub fn stmts_list(&self, list: ListRef) -> &[StmtId] {
        Extras::slice(&self.extras.stmts, list)
    }

    #[inline]
    pub fn types_list(&self, list: ListRef) -> &[TypeId] {
        Extras::slice(&self.extras.types, list)
    }

    #[inline]
    pub fn elements_list(&self, list: ListRef) -> &[Element] {
        Extras::slice(&self.extras.elements, list)
    }

    #[inline]
    pub fn fields_list(&self, list: ListRef) -> &[FieldId] {
        Extras::slice(&self.extras.fields, list)
    }

    #[inline]
    pub fn switch_clauses_list(&self, list: ListRef) -> &[SwitchClauseId] {
        Extras::slice(&self.extras.switch_clauses, list)
    }

    #[inline]
    pub fn comm_clauses_list(&self, list: ListRef) -> &[CommClauseId] {
        Extras::slice(&self.extras.comm_clauses, list)
    }
}

// =============================================================================
// Root structure
// =============================================================================

/// Root node representing a complete Go source file.
///
/// Note:
/// - `arena` owns all nodes and extra data.
/// - Top-level lists are kept as `Vec` because there's typically only a few of them.
///   If you want *fully* centralized lists, migrate these to `ListRef` too.
#[derive(Debug)]
pub struct SourceFile {
    pub package_name: Ident,
    pub arena: AstArena,
    pub imports: Vec<ImportSpec>,
    pub decls: Vec<TopLevelDecl>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TopLevelDecl {
    Decl(DeclId),
    Func(FuncDeclId),
}

// =============================================================================
// Declarations
// =============================================================================

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Decl {
    Gen(GenDecl),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GenDecl {
    pub kind: GenDeclKind,
    pub specs: Vec<Spec>, // kept as Vec: usually small; can move to ListRef if needed
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum GenDeclKind {
    Import,
    Const,
    Type,
    Var,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Spec {
    Import(ImportSpec),
    Value(ValueSpec),
    Type(TypeSpec),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImportSpec {
    pub name: Option<Ident>,
    pub path: StringLit,
}

/// Value specification: moved dynamic lists into `ListRef` to avoid per-node Vec allocations.
///
/// The parser should build these lists using `arena.list_idents(...)` and `arena.list_exprs(...)`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ValueSpec {
    pub names: ListRef, // Ident[]
    pub typ: Option<TypeId>,
    pub values: ListRef, // ExprId[]
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeSpec {
    pub name: Ident,
    pub type_params: Option<TypeParamList>,
    pub typ: TypeId,
    pub alias: bool,
}

// =============================================================================
// Functions / Signatures / Fields
// =============================================================================

/// Functions are flattened into their own arena (stable ID, spans in side table).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FuncDecl {
    pub recv: Option<FieldList>,
    pub name: Ident,
    pub type_params: Option<TypeParamList>,
    pub signature: SignatureId,
    pub body: Option<Block>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Signature {
    pub params: FieldList,
    pub results: Option<Results>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Results {
    Params(FieldList),
    Type(TypeId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeParamList {
    pub params: Vec<TypeParam>, // usually small; can flatten later if needed
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeParam {
    pub names: ListRef, // Ident[]
    pub constraint: TypeId,
}

/// Field list uses `ListRef` into extras.fields (FieldId[]).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FieldList {
    pub fields: ListRef, // FieldId[]
}

#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    pub names: ListRef, // Ident[]
    pub typ: TypeId,
}

// =============================================================================
// Statements
// =============================================================================

/// Block of statements, stored as a `ListRef` into extras.stmts (StmtId[]).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Block {
    pub stmts: ListRef,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Decl(DeclId),
    Empty,

    Labeled {
        label: Ident,
        stmt: StmtId,
    },
    Expr(ExprId),

    Send {
        chan: ExprId,
        value: ExprId,
    },

    IncDec {
        expr: ExprId,
        op: IncDecOp,
    },

    Assign {
        lhs: ListRef, // ExprId[]
        op: AssignOp,
        rhs: ListRef, // ExprId[]
    },

    ShortVarDecl {
        names: ListRef,  // Ident[]
        values: ListRef, // ExprId[]
    },

    Go(ExprId),
    Defer(ExprId),

    Return(ListRef), // ExprId[]

    Break(Option<Ident>),
    Continue(Option<Ident>),
    Goto(Ident),
    Fallthrough,

    Block(Block),
    If(IfStmt),
    For(ForStmt),
    Switch(SwitchStmt),
    Select(SelectStmt),
}

#[derive(Debug, Clone, PartialEq)]
pub enum StmtSuffix {
    Assign(AssignOp, ListRef), // rhs ExprId[]
    Define(ListRef),           // rhs ExprId[]
    Send(ExprId),
    Inc,
    Dec,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum IncDecOp {
    Inc,
    Dec,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum AssignOp {
    Assign,
    Define,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
    AndAssign,
    OrAssign,
    XorAssign,
    ShlAssign,
    ShrAssign,
    AndNotAssign,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfStmt {
    pub init: Option<StmtId>,
    pub cond: ExprId,
    pub then_block: Block,
    pub else_stmt: Option<StmtId>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ForStmt {
    pub kind: ForKind,
    pub block: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ForKind {
    Infinite,
    Cond(ExprId),
    ForClause {
        init: Option<StmtId>,
        cond: Option<ExprId>,
        post: Option<StmtId>,
    },
    Range {
        lhs: Option<RangeLhs>,
        expr: ExprId,
        define: bool,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RangeLhs {
    pub exprs: ListRef, // ExprId[]
}

// =============================================================================
// Switch / Select (clauses flattened + referenced via ListRef)
// =============================================================================

#[derive(Debug, Clone, PartialEq)]
pub struct SwitchStmt {
    pub init: Option<StmtId>,
    pub tag: Option<ExprId>,
    pub clauses: ListRef, // SwitchClauseId[]
}

#[derive(Debug, Clone, PartialEq)]
pub enum SwitchClause {
    Expr {
        exprs: ListRef, // ExprId[]
        stmts: ListRef, // StmtId[]
    },
    Default {
        stmts: ListRef, // StmtId[]
    },
    Type {
        types: ListRef, // TypeId[]
        bind: Option<Ident>,
        stmts: ListRef, // StmtId[]
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct SelectStmt {
    pub clauses: ListRef, // CommClauseId[]
}

#[derive(Debug, Clone, PartialEq)]
pub enum CommClause {
    Comm {
        comm: CommStmt,
        stmts: ListRef, // StmtId[]
    },
    Default {
        stmts: ListRef, // StmtId[]
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum CommStmt {
    Send {
        chan: ExprId,
        value: ExprId,
    },
    Recv {
        lhs: Option<ListRef>, // ExprId[]
        define: bool,
        recv: ExprId,
    },
}

// =============================================================================
// Expressions
// =============================================================================

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Ident(Ident),
    BasicLit(BasicLit),

    CompositeLit {
        typ: TypeId,
        elements: ListRef, // Element[]
    },

    FuncLit {
        signature: SignatureId,
        body: Block,
    },

    Paren(ExprId),

    Selector {
        expr: ExprId,
        ident: Ident,
    },

    Index {
        expr: ExprId,
        index: ExprId,
    },

    Slice {
        expr: ExprId,
        lo: Option<ExprId>,
        hi: Option<ExprId>,
        max: Option<ExprId>,
    },

    TypeAssert {
        expr: ExprId,
        typ: TypeId,
    },

    Call {
        fun: ExprId,
        args: ListRef, // ExprId[]
        variadic: bool,
    },

    Unary {
        op: UnaryOp,
        expr: ExprId,
    },

    Binary {
        left: ExprId,
        op: BinaryOp,
        right: ExprId,
    },

    Star(ExprId),

    Starred {
        value: ExprId,
        ctx: ExprContext,
    },

    Receive(ExprId),

    TypeWrapper(TypeId),

    Bad,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum ExprContext {
    Load,
    Store,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Element {
    Expr(ExprId),
    KeyValue { key: ExprId, value: ExprId },
}

/// Basic literal: store only kind + raw span (no owned string).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BasicLit {
    pub kind: BasicLitKind,
    pub raw: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum BasicLitKind {
    Int,
    Float,
    Imag,
    Rune,
    String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum UnaryOp {
    Add,
    Sub,
    Not,
    Xor,
    Deref,
    Addr,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    Xor,
    Shl,
    Shr,
    AndNot,
    LAnd,
    LOr,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

// =============================================================================
// Types
// =============================================================================

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Named(NamedType),

    Pointer(TypeId),

    Array { len: Option<ExprId>, elem: TypeId },

    Slice(TypeId),

    Map { key: TypeId, value: TypeId },

    Chan { dir: ChanDir, elem: TypeId },

    Struct { fields: FieldList },

    Interface { methods: ListRef }, // InterfaceElem[] in extras? kept simple: see below

    Func { signature: SignatureId },

    Union(ListRef), // TypeId[] (type constraints)

    Tilde(TypeId),

    Paren(TypeId),

    ExprFallback(ExprId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NamedType {
    pub pkg: Option<Ident>,
    pub name: Ident,
    pub type_args: Option<ListRef>, // TypeId[]
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum ChanDir {
    Both,
    Send,
    Recv,
}

/// Interface element (method or embedded type).
///
/// If you want to eliminate `Vec` here too, store these in an arena and reference by ListRef.
/// For now, we keep them as a standalone enum for readability (usually small counts).
#[derive(Debug, Clone, PartialEq)]
pub enum InterfaceElem {
    Method {
        name: Ident,
        type_params: Option<TypeParamList>,
        sig: SignatureId,
    },
    Embed(TypeId),
}

// =============================================================================
// String literal
// =============================================================================

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct StringLit {
    pub raw: Span,
}

impl StringLit {
    #[inline]
    pub const fn new(raw: Span) -> Self {
        Self { raw }
    }
}

// =============================================================================
// Minimal sanity tests (fast, no heavy fixtures)
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn interner_deduplicates() {
        let mut i = Interner::new();
        let a = i.intern("hello");
        let b = i.intern("hello");
        let c = i.intern("world");
        assert_eq!(a, b);
        assert_ne!(a, c);
        assert_eq!(i.resolve(a), "hello");
        assert_eq!(i.resolve(c), "world");
    }

    #[test]
    fn listref_slices_correctly() {
        let mut arena = AstArena::new();
        let s1 = arena.list_idents([
            Symbol::from_u32(1),
            Symbol::from_u32(2),
            Symbol::from_u32(3),
        ]);
        assert_eq!(arena.idents(s1).len(), 3);
        assert_eq!(arena.idents(s1)[0].as_u32(), 1);

        let s2 = arena.list_idents([Symbol::from_u32(9)]);
        assert_eq!(arena.idents(s2).len(), 1);
        assert_eq!(arena.idents(s2)[0].as_u32(), 9);
    }

    #[test]
    fn spanned_arena_keeps_spans_aligned() {
        let mut a: SpannedArena<u32> = SpannedArena::new();
        let id0 = a.alloc(10, Span::new(0, 1));
        let id1 = a.alloc(20, Span::new(2, 5));
        assert_eq!(a.get(id0), &10);
        assert_eq!(a.get(id1), &20);
        assert_eq!(a.span(id0), Span::new(0, 1));
        assert_eq!(a.span(id1), Span::new(2, 5));
    }
}
