//! # Go Language Production AST
//!
//! This module implements a production-ready Abstract Syntax Tree (AST) for the Go programming language.
//!
//! ## Design Goals
//!
//! - **Target**: Go language specification (https://go.dev/ref/spec)
//! - **Memory Efficiency**: Arena-allocated nodes to minimize per-node allocations
//! - **Performance**: Side-table spans and interned symbols for fast lookups
//! - **Type Safety**: Typed list references (`ListRef<T>`) instead of raw vectors
//!
//! ## Architecture
//!
//! - **Nodes**: All AST nodes are allocated in typed arenas (`SpannedArena<T>`)
//! - **Lists**: Centralized buffers avoid per-node `Vec` allocations
//! - **Spans**: Location information stored separately in side tables
//! - **Symbols**: String interning for efficient identifier handling
//!
//! ## Important Notes
//!
//! - This module models Go's **syntax only** (AST level)
//! - Some syntactic forms require type-checking for semantic validation
//! - Walk/Visitor infrastructure lives in `walk.rs` (see `crate::walk`)
//! - `#[derive(WalkAst)]` generates `impl crate::walk::Walk`
//!
//! ## Specification Compliance
//!
//! This implementation includes several spec-driven improvements:
//!
//! - Variadic parameter support via `Field::ellipsis_pos`
//! - Generic function instantiation: `OperandName[TypeArgs]` via `Expr::Instantiate`
//! - Accurate composite literal modeling per spec
//! - Function vs method type parameter distinction
//! - Precise channel type directionality (`<-chan T` vs `chan<- T`)
//! - Builtin call support for `make(T, ...)`, `new(T)` patterns

use ast_derive::WalkAst;
use core::marker::PhantomData;
use core::ops::{Index, IndexMut};
use smallvec::SmallVec;
use std::collections::HashMap;
use std::hash::{BuildHasher, BuildHasherDefault, Hasher, RandomState};

// =============================================================================
// Core Foundation Types
// =============================================================================

/// Represents a source code span with start and end positions.
///
/// Positions are stored as `u32` to save memory, limiting file size to 4GB.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

impl Span {
    /// Creates a new span from usize positions.
    ///
    /// # Panics
    /// In debug builds, panics if positions exceed `u32::MAX`.
    #[inline]
    pub fn new(start: usize, end: usize) -> Self {
        debug_assert!(start <= u32::MAX as usize);
        debug_assert!(end <= u32::MAX as usize);
        Self {
            start: start as u32,
            end: end as u32,
        }
    }

    /// Returns the length of the span.
    #[inline]
    pub const fn len(&self) -> u32 {
        self.end.saturating_sub(self.start)
    }

    /// Checks if the span is empty.
    #[inline]
    pub const fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

/// Type-safe identifier for arena-allocated nodes.
///
/// The phantom marker ensures type safety at compile time.
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
#[repr(transparent)]
pub struct Id<T> {
    raw: u32,
    _marker: PhantomData<fn() -> T>,
}

impl<T> Id<T> {
    /// Creates an ID from a raw u32 value.
    #[inline]
    pub const fn from_raw(raw: u32) -> Self {
        Self {
            raw,
            _marker: PhantomData,
        }
    }

    /// Converts the ID to usize for indexing.
    #[inline]
    pub const fn to_usize(&self) -> usize {
        self.raw as usize
    }

    /// Returns the raw u32 value.
    #[inline]
    pub const fn raw(&self) -> u32 {
        self.raw
    }
}

/// Typed reference into a centralized list buffer.
///
/// Lists are stored in `AstArena::extras` to avoid per-node allocations.
/// Each `ListRef` points to a contiguous slice in the appropriate buffer.
#[derive(Debug, PartialEq, Eq)]
pub struct ListRef<T> {
    start: u32,
    len: u32,
    _marker: PhantomData<fn() -> T>,
}

impl<T> Copy for ListRef<T> {}

impl<T> Clone for ListRef<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Default for ListRef<T> {
    fn default() -> Self {
        Self::EMPTY
    }
}

impl<T> ListRef<T> {
    /// Empty list constant.
    pub const EMPTY: Self = Self {
        start: 0,
        len: 0,
        _marker: PhantomData,
    };

    /// Creates a new list reference.
    #[inline]
    pub const fn new(start: u32, len: u32) -> Self {
        Self {
            start,
            len,
            _marker: PhantomData,
        }
    }

    /// Checks if the list is empty.
    #[inline]
    pub const fn is_empty(&self) -> bool {
        self.len == 0
    }

    /// Returns the start index.
    #[inline]
    pub const fn start(&self) -> u32 {
        self.start
    }

    /// Returns the length.
    #[inline]
    pub const fn len(&self) -> u32 {
        self.len
    }

    /// Returns the end index (exclusive).
    #[inline]
    pub const fn end(&self) -> u32 {
        self.start + self.len
    }
}

// =============================================================================
// Symbol Interning System
// =============================================================================

/// Interned string symbol.
///
/// Symbols are immutable identifiers stored once and referenced by ID.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Symbol(u32);

/// Type alias for identifier symbols.
pub type Ident = Symbol;

impl Symbol {
    /// Creates a symbol from a raw u32 value.
    #[inline]
    pub const fn from_raw(v: u32) -> Self {
        Self(v)
    }

    /// Returns the raw u32 value.
    #[inline]
    pub const fn as_u32(self) -> u32 {
        self.0
    }
}

/// Identifier occurrence (interned symbol + source position).
///
/// Use this for every `IdentifierList` in the spec so per-identifier spans are preserved.
#[derive(Debug, Clone, Copy, PartialEq, Eq, WalkAst)]
pub struct IdentName {
    pub sym: Ident,
    pub pos: Span,
}

/// Identity hasher for u64 values (used for symbol hash buckets).
#[derive(Default)]
struct U64IdentityHasher(u64);

impl Hasher for U64IdentityHasher {
    fn write(&mut self, _b: &[u8]) {
        unreachable!("U64IdentityHasher only supports write_u64")
    }

    #[inline]
    fn write_u64(&mut self, i: u64) {
        self.0 = i;
    }

    #[inline]
    fn finish(&self) -> u64 {
        self.0
    }
}

type U64IdentityBuild = BuildHasherDefault<U64IdentityHasher>;

/// String interner for efficient symbol storage.
///
/// Symbols are stored once and looked up via hash buckets.
/// Uses a seeded random hasher for security against hash collision attacks.
#[derive(Debug, Default)]
pub struct Interner {
    strings: Vec<Box<str>>,
    buckets: HashMap<u64, SmallVec<[Symbol; 1]>, U64IdentityBuild>,
    state: RandomState,
}

impl Interner {
    /// Creates a new empty interner.
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    /// Reserves capacity for additional symbols.
    #[inline]
    pub fn reserve(&mut self, additional: usize) {
        self.strings.reserve(additional);
        self.buckets.reserve(additional);
    }

    /// Computes the hash of a string using the interner's hash function.
    #[inline(always)]
    pub fn hash_str(&self, s: &str) -> u64 {
        self.state.hash_one(s)
    }

    /// Interns a string and returns its symbol.
    ///
    /// If the string already exists, returns the existing symbol.
    #[inline]
    pub fn intern(&mut self, s: &str) -> Symbol {
        let h = self.hash_str(s);
        self.intern_with_hash(s, h)
    }

    /// Interns a string with a pre-computed hash.
    #[inline]
    pub fn intern_with_hash(&mut self, s: &str, h: u64) -> Symbol {
        let entry = self.buckets.entry(h).or_default();

        // Check if string already exists in this bucket
        for &sym in entry.iter() {
            if self.strings[sym.0 as usize].as_ref() == s {
                return sym;
            }
        }

        // Add new string
        let sym = Symbol(self.strings.len() as u32);
        self.strings.push(s.into());
        entry.push(sym);
        sym
    }

    /// Resolves a symbol back to its string.
    ///
    /// # Panics
    /// Panics if the symbol is invalid (debug assertion in bounds check).
    #[inline]
    pub fn resolve(&self, sym: Symbol) -> &str {
        debug_assert!((sym.0 as usize) < self.strings.len());
        self.strings
            .get(sym.0 as usize)
            .expect("invalid symbol")
            .as_ref()
    }
}

// =============================================================================
// Arena Allocation
// =============================================================================

/// Arena for nodes with associated spans.
///
/// Stores nodes and their spans in parallel vectors for cache efficiency.
#[derive(Debug)]
pub struct SpannedArena<T> {
    data: Vec<T>,
    spans: Vec<Span>,
}

impl<T> Default for SpannedArena<T> {
    fn default() -> Self {
        Self {
            data: Vec::new(),
            spans: Vec::new(),
        }
    }
}

impl<T> SpannedArena<T> {
    /// Creates a new empty arena.
    pub fn new() -> Self {
        Self::default()
    }

    /// Allocates a node with its span and returns an ID.
    #[inline]
    pub fn alloc(&mut self, node: T, span: Span) -> Id<T> {
        let id = Id::from_raw(self.data.len() as u32);
        self.data.push(node);
        self.spans.push(span);
        id
    }

    /// Gets an immutable reference to a node.
    #[inline]
    pub fn get(&self, id: Id<T>) -> &T {
        &self.data[id.to_usize()]
    }

    /// Gets a mutable reference to a node.
    #[inline]
    pub fn get_mut(&mut self, id: Id<T>) -> &mut T {
        &mut self.data[id.to_usize()]
    }

    /// Gets the span for a node.
    #[inline]
    pub fn span(&self, id: Id<T>) -> Span {
        self.spans[id.to_usize()]
    }

    /// Returns the number of nodes in the arena.
    #[inline]
    pub fn len(&self) -> usize {
        self.data.len()
    }

    /// Checks if the arena is empty.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }
}

impl<T> Index<Id<T>> for SpannedArena<T> {
    type Output = T;
    fn index(&self, id: Id<T>) -> &T {
        self.get(id)
    }
}

impl<T> IndexMut<Id<T>> for SpannedArena<T> {
    fn index_mut(&mut self, id: Id<T>) -> &mut T {
        self.get_mut(id)
    }
}

// =============================================================================
// Type Aliases for Node IDs
// =============================================================================

pub type DeclId = Id<Decl>;
pub type StmtId = Id<Stmt>;
pub type SimpleStmtId = Id<SimpleStmt>;
pub type ExprId = Id<Expr>;
pub type TypeId = Id<Type>;
pub type FieldId = Id<Field>;
pub type SignatureId = Id<Signature>;
pub type FuncDeclId = Id<FuncDecl>;
pub type SwitchClauseId = Id<SwitchClause>;
pub type CommClauseId = Id<CommClause>;
pub type TypeParamsId = Id<TypeParams>;
pub type TypeParamDeclId = Id<TypeParamDecl>;
pub type CommentId = Id<Comment>;
pub type CommentGroupId = Id<CommentGroup>;

// =============================================================================
// Centralized List Storage
// =============================================================================

/// Extra data storage for all list types.
///
/// This structure holds all the centralized buffers that `ListRef<T>` points into.
/// Keeping lists here instead of in individual nodes reduces memory overhead.
#[derive(Debug, Default)]
pub struct ExtraData {
    pub idents: Vec<Ident>,
    pub ident_names: Vec<IdentName>,
    pub exprs: Vec<ExprId>,
    pub stmts: Vec<StmtId>,
    pub types: Vec<TypeId>,
    pub fields: Vec<FieldId>,
    pub specs: Vec<Spec>,
    pub keyed_elems: Vec<KeyedElement>,
    pub top_decls: Vec<TopLevelDecl>,
    pub switch_clause_ids: Vec<SwitchClauseId>,
    pub comm_clause_ids: Vec<CommClauseId>,
    pub type_case_elems: Vec<TypeCaseElem>,
    pub type_terms: Vec<TypeTerm>,
    pub interface_elems: Vec<InterfaceElem>,
    pub type_param_decl_ids: Vec<TypeParamDeclId>,
    pub comment_ids: Vec<CommentId>,
    pub comment_group_ids: Vec<CommentGroupId>,
    pub expr_or_types: Vec<ExprOrType>,
}

// =============================================================================
// Main AST Arena
// =============================================================================

/// Central arena holding all AST nodes and list buffers.
///
/// This is the main entry point for AST allocation and access.
#[derive(Debug, Default)]
pub struct AstArena {
    pub decls: SpannedArena<Decl>,
    pub stmts: SpannedArena<Stmt>,
    pub simple_stmts: SpannedArena<SimpleStmt>,
    pub exprs: SpannedArena<Expr>,
    pub types: SpannedArena<Type>,
    pub signatures: SpannedArena<Signature>,
    pub funcs: SpannedArena<FuncDecl>,
    pub fields: SpannedArena<Field>,
    pub switch_clauses: SpannedArena<SwitchClause>,
    pub comm_clauses: SpannedArena<CommClause>,
    pub type_params: SpannedArena<TypeParams>,
    pub type_param_decls: SpannedArena<TypeParamDecl>,
    pub comments: SpannedArena<Comment>,
    pub comment_groups: SpannedArena<CommentGroup>,
    pub extras: ExtraData,
}

impl AstArena {
    /// Creates a new empty AST arena.
    pub fn new() -> Self {
        Self::default()
    }

    /// Helper to push items into a buffer and return a typed list reference.
    #[inline]
    fn push_list<T>(buf: &mut Vec<T>, items: impl IntoIterator<Item = T>) -> ListRef<T> {
        let start = buf.len();
        buf.extend(items);
        let len = buf.len() - start;
        debug_assert!(start <= u32::MAX as usize);
        debug_assert!(len <= u32::MAX as usize);
        ListRef::new(start as u32, len as u32)
    }

    /// Helper to get a slice from a buffer using a list reference.
    #[inline]
    fn slice<T>(buf: &[T], r: ListRef<T>) -> &[T] {
        let s = r.start() as usize;
        let e = r.end() as usize;
        &buf[s..e]
    }

    // List Builders

    pub fn list_idents(&mut self, i: impl IntoIterator<Item = Ident>) -> ListRef<Ident> {
        Self::push_list(&mut self.extras.idents, i)
    }

    pub fn list_ident_names(
        &mut self,
        i: impl IntoIterator<Item = IdentName>,
    ) -> ListRef<IdentName> {
        Self::push_list(&mut self.extras.ident_names, i)
    }

    pub fn list_exprs(&mut self, i: impl IntoIterator<Item = ExprId>) -> ListRef<ExprId> {
        Self::push_list(&mut self.extras.exprs, i)
    }

    pub fn list_stmts(&mut self, i: impl IntoIterator<Item = StmtId>) -> ListRef<StmtId> {
        Self::push_list(&mut self.extras.stmts, i)
    }

    pub fn list_types(&mut self, i: impl IntoIterator<Item = TypeId>) -> ListRef<TypeId> {
        Self::push_list(&mut self.extras.types, i)
    }

    pub fn list_fields(&mut self, i: impl IntoIterator<Item = FieldId>) -> ListRef<FieldId> {
        Self::push_list(&mut self.extras.fields, i)
    }

    pub fn list_keyed_elems(
        &mut self,
        i: impl IntoIterator<Item = KeyedElement>,
    ) -> ListRef<KeyedElement> {
        Self::push_list(&mut self.extras.keyed_elems, i)
    }

    pub fn list_specs(&mut self, i: impl IntoIterator<Item = Spec>) -> ListRef<Spec> {
        Self::push_list(&mut self.extras.specs, i)
    }

    pub fn list_top_decls(
        &mut self,
        i: impl IntoIterator<Item = TopLevelDecl>,
    ) -> ListRef<TopLevelDecl> {
        Self::push_list(&mut self.extras.top_decls, i)
    }

    pub fn list_switch_clause_ids(
        &mut self,
        i: impl IntoIterator<Item = SwitchClauseId>,
    ) -> ListRef<SwitchClauseId> {
        Self::push_list(&mut self.extras.switch_clause_ids, i)
    }

    pub fn list_comm_clause_ids(
        &mut self,
        i: impl IntoIterator<Item = CommClauseId>,
    ) -> ListRef<CommClauseId> {
        Self::push_list(&mut self.extras.comm_clause_ids, i)
    }

    pub fn list_type_cases(
        &mut self,
        i: impl IntoIterator<Item = TypeCaseElem>,
    ) -> ListRef<TypeCaseElem> {
        Self::push_list(&mut self.extras.type_case_elems, i)
    }

    pub fn list_type_terms(&mut self, i: impl IntoIterator<Item = TypeTerm>) -> ListRef<TypeTerm> {
        Self::push_list(&mut self.extras.type_terms, i)
    }

    pub fn list_interface_elems(
        &mut self,
        i: impl IntoIterator<Item = InterfaceElem>,
    ) -> ListRef<InterfaceElem> {
        Self::push_list(&mut self.extras.interface_elems, i)
    }

    pub fn list_type_param_decl_ids(
        &mut self,
        i: impl IntoIterator<Item = TypeParamDeclId>,
    ) -> ListRef<TypeParamDeclId> {
        Self::push_list(&mut self.extras.type_param_decl_ids, i)
    }

    pub fn list_comment_ids(
        &mut self,
        i: impl IntoIterator<Item = CommentId>,
    ) -> ListRef<CommentId> {
        Self::push_list(&mut self.extras.comment_ids, i)
    }

    pub fn list_comment_group_ids(
        &mut self,
        i: impl IntoIterator<Item = CommentGroupId>,
    ) -> ListRef<CommentGroupId> {
        Self::push_list(&mut self.extras.comment_group_ids, i)
    }

    pub fn list_expr_or_types(
        &mut self,
        i: impl IntoIterator<Item = ExprOrType>,
    ) -> ListRef<ExprOrType> {
        Self::push_list(&mut self.extras.expr_or_types, i)
    }

    // List Accessors

    pub fn idents(&self, r: ListRef<Ident>) -> &[Ident] {
        Self::slice(&self.extras.idents, r)
    }

    pub fn ident_names(&self, r: ListRef<IdentName>) -> &[IdentName] {
        Self::slice(&self.extras.ident_names, r)
    }

    pub fn exprs_list(&self, r: ListRef<ExprId>) -> &[ExprId] {
        Self::slice(&self.extras.exprs, r)
    }

    pub fn stmts_list(&self, r: ListRef<StmtId>) -> &[StmtId] {
        Self::slice(&self.extras.stmts, r)
    }

    pub fn types_list(&self, r: ListRef<TypeId>) -> &[TypeId] {
        Self::slice(&self.extras.types, r)
    }

    pub fn fields_list(&self, r: ListRef<FieldId>) -> &[FieldId] {
        Self::slice(&self.extras.fields, r)
    }

    pub fn keyed_elems_list(&self, r: ListRef<KeyedElement>) -> &[KeyedElement] {
        Self::slice(&self.extras.keyed_elems, r)
    }

    pub fn specs_list(&self, r: ListRef<Spec>) -> &[Spec] {
        Self::slice(&self.extras.specs, r)
    }

    pub fn top_decls(&self, r: ListRef<TopLevelDecl>) -> &[TopLevelDecl] {
        Self::slice(&self.extras.top_decls, r)
    }

    pub fn switch_clause_ids(&self, r: ListRef<SwitchClauseId>) -> &[SwitchClauseId] {
        Self::slice(&self.extras.switch_clause_ids, r)
    }

    pub fn comm_clause_ids(&self, r: ListRef<CommClauseId>) -> &[CommClauseId] {
        Self::slice(&self.extras.comm_clause_ids, r)
    }

    pub fn type_case_elems(&self, r: ListRef<TypeCaseElem>) -> &[TypeCaseElem] {
        Self::slice(&self.extras.type_case_elems, r)
    }

    pub fn type_terms(&self, r: ListRef<TypeTerm>) -> &[TypeTerm] {
        Self::slice(&self.extras.type_terms, r)
    }

    pub fn interface_elems(&self, r: ListRef<InterfaceElem>) -> &[InterfaceElem] {
        Self::slice(&self.extras.interface_elems, r)
    }

    pub fn type_param_decl_ids(&self, r: ListRef<TypeParamDeclId>) -> &[TypeParamDeclId] {
        Self::slice(&self.extras.type_param_decl_ids, r)
    }

    pub fn comment_ids(&self, r: ListRef<CommentId>) -> &[CommentId] {
        Self::slice(&self.extras.comment_ids, r)
    }

    pub fn comment_group_ids(&self, r: ListRef<CommentGroupId>) -> &[CommentGroupId] {
        Self::slice(&self.extras.comment_group_ids, r)
    }

    pub fn expr_or_types(&self, r: ListRef<ExprOrType>) -> &[ExprOrType] {
        Self::slice(&self.extras.expr_or_types, r)
    }
}

// =============================================================================
// Comments / Directives
// =============================================================================

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CommentKind {
    Line,
    Block,
}

/// Comment token (text is recovered from source via span).
#[derive(Debug, Clone, Copy, PartialEq, Eq, WalkAst)]
pub struct Comment {
    pub kind: CommentKind,
}

/// Group of adjacent comments (doc, directives, etc.).
#[derive(Debug, Clone, Copy, PartialEq, Eq, WalkAst)]
pub struct CommentGroup {
    pub comments: ListRef<CommentId>,
}

// =============================================================================
// Source File (Root Node)
// =============================================================================

/// Represents a complete Go source file.
///
/// Corresponds to `SourceFile` in the Go spec:
/// ```text
/// SourceFile = PackageClause ";" { ImportDecl ";" } { TopLevelDecl ";" }
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, WalkAst)]
pub struct SourceFile {
    /// Position of the `package` keyword
    pub package_pos: Span,
    /// Package name
    pub name: Ident,
    /// All top-level declarations in source order
    pub decls: ListRef<TopLevelDecl>,
    /// All comment groups in this file (directives/build tags/doc/etc.)
    pub comments: ListRef<CommentGroupId>,
    /// Optional file/package doc comment group
    pub doc: Option<CommentGroupId>,
}

// =============================================================================
// Declarations
// =============================================================================

/// Top-level declaration (can be a general declaration or function).
#[derive(Debug, Clone, Copy, PartialEq, Eq, WalkAst)]
pub enum TopLevelDecl {
    Decl(DeclId),
    Func(FuncDeclId),
}

/// General declaration node.
#[derive(Debug, Clone, Copy, PartialEq, Eq, WalkAst)]
pub enum Decl {
    Gen(GenDecl),
    Bad,
}

/// Generic declaration (import, const, type, var).
///
/// Corresponds to spec forms:
/// ```text
/// Declaration   = ConstDecl | TypeDecl | VarDecl
/// TopLevelDecl  = Declaration | FunctionDecl | MethodDecl
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, WalkAst)]
pub struct GenDecl {
    /// Leading doc comment group (optional)
    pub doc: Option<CommentGroupId>,
    /// Position of keyword (import/const/type/var)
    pub kw_pos: Span,
    /// Kind of declaration
    pub kind: GenDeclKind,
    /// Opening parenthesis for grouped declarations
    pub l_paren: Option<Span>,
    /// Specification list
    pub specs: ListRef<Spec>,
    /// Closing parenthesis
    pub r_paren: Option<Span>,
}

/// Kind of generic declaration.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum GenDeclKind {
    Import,
    Const,
    Type,
    Var,
}

/// Specification within a declaration.
#[derive(Debug, Clone, Copy, PartialEq, Eq, WalkAst)]
pub enum Spec {
    Import(ImportSpec),
    Value(ValueSpec),
    Type(TypeSpec),
}

/// Import specification.
///
/// Spec: `ImportSpec = [ "." | PackageName ] ImportPath`
#[derive(Debug, Clone, Copy, PartialEq, Eq, WalkAst)]
pub struct ImportSpec {
    /// Leading doc comment group (optional)
    pub doc: Option<CommentGroupId>,
    /// Optional import name (dot import, blank, or alias)
    pub name: Option<ImportName>,
    /// Import path string
    pub path: StringLit,
}

/// Import name variants.
#[derive(Debug, Clone, Copy, PartialEq, Eq, WalkAst)]
pub enum ImportName {
    /// Dot import: `import . "pkg"`
    Dot(Span),
    /// Blank import: `import _ "pkg"`
    Blank(Span),
    /// Named import: `import name "pkg"`
    Name(Ident, Span),
}

/// Value specification (const or var).
///
/// Spec: `ConstSpec = IdentifierList [ [ Type ] "=" ExpressionList ]`
/// Spec: `VarSpec = IdentifierList ( Type [ "=" ExpressionList ] | "=" ExpressionList )`
#[derive(Debug, Clone, Copy, PartialEq, Eq, WalkAst)]
pub struct ValueSpec {
    /// Leading doc comment group (optional)
    pub doc: Option<CommentGroupId>,
    /// Variable/constant names
    pub names: ListRef<IdentName>,
    /// Optional type
    pub typ: Option<TypeId>,
    /// Initial values
    pub values: ListRef<ExprId>,
}

/// Type specification.
///
/// Spec: `TypeSpec = AliasDecl | TypeDef`
/// Type parameters represented as empty list when absent.
#[derive(Debug, Clone, Copy, PartialEq, Eq, WalkAst)]
pub struct TypeSpec {
    /// Leading doc comment group (optional)
    pub doc: Option<CommentGroupId>,
    /// Type name
    pub name: Ident,
    /// Position of name
    pub name_pos: Span,
    /// Type parameters (absent if none)
    pub type_params: Option<TypeParamsId>,
    /// Position of '=' for type alias
    pub assign_pos: Option<Span>,
    /// Underlying type
    pub typ: TypeId,
    /// True if this is an alias (`=`), false if definition
    pub alias: bool,
}

/// Function or method declaration.
///
/// Spec:
/// ```text
/// FunctionDecl = "func" FunctionName [ TypeParameters ] Signature [ FunctionBody ]
/// MethodDecl   = "func" Receiver MethodName Signature [ FunctionBody ]
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, WalkAst)]
pub struct FuncDecl {
    /// Leading doc comment group (optional)
    pub doc: Option<CommentGroupId>,
    /// Position of `func` keyword
    pub func_pos: Span,
    /// Receiver (present only for methods)
    pub recv: Option<Receiver>,
    /// Function name
    pub name: Ident,
    /// Position of name
    pub name_pos: Span,
    /// Type parameters (only for functions, not methods)
    pub func_type_params: Option<TypeParamsId>,
    /// Function signature
    pub signature: SignatureId,
    /// Function body (None for forward declarations)
    pub body: Option<Block>,
}

// =============================================================================
// Generics: Type Parameters and Constraints
// =============================================================================

#[derive(Debug, Clone, Copy, PartialEq, Eq, WalkAst)]
pub struct TypeParams {
    pub l_brack: Span,
    pub params: ListRef<TypeParamDeclId>,
    pub trailing_comma: Option<Span>,
    pub r_brack: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, WalkAst)]
pub struct TypeParamDecl {
    pub names: ListRef<IdentName>,
    pub constraint: TypeConstraint,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, WalkAst)]
pub enum TypeConstraint {
    Any { any_pos: Span },
    TypeElem(TypeElem),
    Interface(TypeId),
}

/// Syntactic type element: `TypeTerm { "|" TypeTerm }`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, WalkAst)]
pub struct TypeElem {
    pub terms: ListRef<TypeTerm>,
}

/// Method receiver with optional receiver-declared type parameter names.
///
/// The bracket list here is a binder of *names* (identifiers), not general type arguments.
#[derive(Debug, Clone, Copy, PartialEq, Eq, WalkAst)]
pub struct Receiver {
    pub l_paren: Span,
    pub name: Option<IdentName>,
    pub typ: TypeId,
    pub type_params: Option<ReceiverTypeParams>,
    pub r_paren: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, WalkAst)]
pub struct ReceiverTypeParams {
    pub l_brack: Span,
    pub names: ListRef<IdentName>,
    pub trailing_comma: Option<Span>,
    pub r_brack: Span,
}

// =============================================================================
// Signatures and Fields
// =============================================================================

/// Function signature.
///
/// Spec: `Signature = Parameters [ Result ]`
#[derive(Debug, Clone, Copy, PartialEq, Eq, WalkAst)]
pub struct Signature {
    /// Parameter list
    pub params: FieldList,
    /// Optional result (return type)
    pub results: Option<Results>,
}

/// Function result specification.
#[derive(Debug, Clone, Copy, PartialEq, Eq, WalkAst)]
pub enum Results {
    /// Named results: `(x int, y string)`
    Params(FieldList),
    /// Single unnamed result: `int`
    Type(TypeId),
}

/// Field list (parameters, results, struct fields).
///
/// Spec: `Parameters = "(" [ ParameterList [ "," ] ] ")"`
#[derive(Debug, Clone, Copy, PartialEq, Eq, WalkAst)]
pub struct FieldList {
    /// Opening parenthesis
    pub l_paren: Span,
    /// Fields
    pub fields: ListRef<FieldId>,
    /// Closing parenthesis
    pub r_paren: Span,
}

/// Field in a parameter list, struct, or interface.
///
/// Spec: `ParameterDecl = [ IdentifierList ] [ "..." ] Type`
#[derive(Debug, Clone, Copy, PartialEq, Eq, WalkAst)]
pub struct Field {
    /// Field names (empty for anonymous fields)
    pub names: ListRef<IdentName>,
    /// Position of `...` for variadic parameters
    pub ellipsis_pos: Option<Span>,
    /// Field type
    pub typ: TypeId,
    /// Struct tag (struct fields only)
    pub tag: Option<StringLit>,
    /// True if this is an embedded field
    pub is_embed: bool,
    /// Position of comma (for formatting/recovery)
    pub comma: Option<Span>,
    /// Leading doc comment group (optional)
    pub doc: Option<CommentGroupId>,
    /// Trailing line comment group (optional)
    pub comment: Option<CommentGroupId>,
}

// =============================================================================
// Statements
// =============================================================================

/// Simple statement (used in for/if/switch headers).
///
/// Spec: `SimpleStmt = EmptyStmt | ExpressionStmt | SendStmt | IncDecStmt | Assignment | ShortVarDecl`
#[derive(Debug, Clone, Copy, PartialEq, Eq, WalkAst)]
pub enum SimpleStmt {
    Empty(Span),
    Expr(ExprId),

    /// Send statement: `chan <- value`
    Send {
        chan: ExprId,
        op_pos: Span,
        value: ExprId,
    },

    /// Increment/decrement: `x++` or `x--`
    IncDec {
        expr: ExprId,
        op: IncDecOp,
        op_pos: Span,
    },

    /// Assignment: `x = y` or `x += y`
    Assign {
        lhs: ListRef<ExprId>,
        op: AssignOp,
        op_pos: Span,
        rhs: ListRef<ExprId>,
    },

    /// Short variable declaration: `x := y`
    ShortVarDecl {
        names: ListRef<IdentName>,
        op_pos: Span,
        values: ListRef<ExprId>,
    },
}

/// Statement node.
#[derive(Debug, Clone, Copy, PartialEq, Eq, WalkAst)]
pub enum Stmt {
    Simple(SimpleStmtId),
    Decl(DeclId),

    /// Labeled statement: `label: stmt`
    Labeled {
        label: Ident,
        label_pos: Span,
        colon_pos: Span,
        stmt: StmtId,
    },

    /// Go statement: `go f()`
    Go {
        go_pos: Span,
        call: ExprId,
    },

    /// Defer statement: `defer f()`
    Defer {
        defer_pos: Span,
        call: ExprId,
    },

    /// Return statement: `return [expr, ...]`
    Return {
        return_pos: Span,
        results: ListRef<ExprId>,
    },

    /// Branch statement (break, continue, goto, fallthrough)
    Branch(BranchStmt),

    /// Block statement: `{ ... }`
    Block(Block),

    /// If statement
    If {
        if_pos: Span,
        init: Option<SimpleStmtId>,
        semi_pos: Option<Span>,
        cond: ExprId,
        then_block: Block,
        else_stmt: Option<StmtId>,
    },

    /// For loop
    For {
        for_pos: Span,
        kind: ForKind,
        block: Block,
    },

    /// Switch statement
    Switch {
        switch_pos: Span,
        init: Option<SimpleStmtId>,
        semi_pos: Option<Span>,
        tag: Option<ExprId>,
        l_brace: Span,
        clauses: ListRef<SwitchClauseId>,
        r_brace: Span,
    },

    /// Type switch statement
    TypeSwitch {
        switch_pos: Span,
        init: Option<SimpleStmtId>,
        semi_pos: Option<Span>,
        guard: TypeSwitchGuard,
        l_brace: Span,
        clauses: ListRef<SwitchClauseId>,
        r_brace: Span,
    },

    /// Select statement
    Select {
        select_pos: Span,
        l_brace: Span,
        clauses: ListRef<CommClauseId>,
        r_brace: Span,
    },

    /// Bad statement (error recovery)
    Bad(Span),
}

/// For loop variants.
#[derive(Debug, Clone, Copy, PartialEq, Eq, WalkAst)]
pub enum ForKind {
    /// Infinite loop: `for { ... }`
    Infinite,

    /// Condition loop: `for cond { ... }`
    Cond(ExprId),

    /// Three-clause loop: `for init; cond; post { ... }`
    ForClause {
        init: Option<SimpleStmtId>,
        semi1_pos: Span,
        cond: Option<ExprId>,
        semi2_pos: Span,
        post: Option<SimpleStmtId>,
    },

    /// Range loop: `for k, v := range x { ... }`
    Range {
        lhs: Option<RangeLhs>,
        range_pos: Span,
        expr: ExprId,
    },
}

/// Left-hand side of range loop.
#[derive(Debug, Clone, Copy, PartialEq, Eq, WalkAst)]
pub enum RangeLhs {
    /// Short variable declaration: `k, v :=`
    Def {
        idents: ListRef<IdentName>,
        op_pos: Span,
    },
    /// Assignment: `k, v =`
    Assign {
        exprs: ListRef<ExprId>,
        op_pos: Span,
    },
}

/// Switch or type switch case clause.
#[derive(Debug, Clone, Copy, PartialEq, Eq, WalkAst)]
pub enum SwitchClause {
    /// Expression case: `case x, y:` or `default:`
    ExprCase {
        case_pos: Span,
        items: ListRef<ExprId>,
        colon_pos: Span,
        stmts: ListRef<StmtId>,
    },

    /// Type case: `case int, string:` or `default:`
    TypeCase {
        case_pos: Span,
        items: ListRef<TypeCaseElem>,
        colon_pos: Span,
        stmts: ListRef<StmtId>,
    },
}

/// Select case clause.
#[derive(Debug, Clone, Copy, PartialEq, Eq, WalkAst)]
pub enum CommClause {
    /// Communication case: `case <-ch:` or `case ch <- x:`
    Case {
        case_pos: Span,
        comm: CommStmt,
        colon_pos: Span,
        stmts: ListRef<StmtId>,
    },

    /// Default case: `default:`
    Default {
        default_pos: Span,
        colon_pos: Span,
        stmts: ListRef<StmtId>,
    },
}

/// Communication statement (send or receive).
#[derive(Debug, Clone, Copy, PartialEq, Eq, WalkAst)]
pub enum CommStmt {
    /// Send: `ch <- x`
    Send {
        chan: ExprId,
        op_pos: Span,
        value: ExprId,
    },

    /// Receive with optional assignment: `x := <-ch` or `<-ch`
    Recv { lhs: Option<RangeLhs>, expr: ExprId },
}

/// Branch statement (break, continue, goto, fallthrough).
#[derive(Debug, Clone, Copy, PartialEq, Eq, WalkAst)]
pub enum BranchStmt {
    Break {
        break_pos: Span,
        label: Option<Ident>,
        label_pos: Option<Span>,
    },
    Continue {
        cont_pos: Span,
        label: Option<Ident>,
        label_pos: Option<Span>,
    },
    Goto {
        goto_pos: Span,
        label: Ident,
        label_pos: Span,
    },
    Fallthrough {
        fall_pos: Span,
    },
}

/// Block of statements.
///
/// Spec: `Block = "{" StatementList "}"`
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Block {
    pub l_brace: Span,
    pub stmts: ListRef<StmtId>,
    pub r_brace: Span,
}

/// Type switch guard.
///
/// Spec: `TypeSwitchGuard = [ identifier ":=" ] PrimaryExpr "." "(" "type" ")"`
#[derive(Debug, Clone, Copy, PartialEq, Eq, WalkAst)]
pub struct TypeSwitchGuard {
    pub bind: Option<TypeSwitchBind>,
    pub x: ExprId,
    pub dot_pos: Span,
    pub l_paren: Span,
    pub type_pos: Span,
    pub r_paren: Span,
}

/// Type switch binding.
#[derive(Debug, Clone, Copy, PartialEq, Eq, WalkAst)]
pub enum TypeSwitchBind {
    /// Short variable declaration: `x :=`
    ShortVar {
        ident: Ident,
        ident_pos: Span,
        op_pos: Span,
    },
    /// Assignment: `x =`
    Assign { lhs: ListRef<ExprId>, op_pos: Span },
}

// =============================================================================
// Expressions
// =============================================================================

/// Literal value (composite literal element list).
///
/// Spec: `LiteralValue = "{" [ ElementList [ "," ] ] "}"`
#[derive(Debug, Clone, Copy, PartialEq, Eq, WalkAst)]
pub struct LiteralValue {
    pub l_brace: Span,
    pub elements: ListRef<KeyedElement>,
    pub trailing_comma: Option<Span>,
    pub r_brace: Span,
}

/// Keyed element in a literal value.
///
/// Spec: `KeyedElement = [ Key ":" ] Element`
#[derive(Debug, Clone, Copy, PartialEq, Eq, WalkAst)]
pub struct KeyedElement {
    pub key: Option<Key>,
    pub colon_pos: Option<Span>,
    pub value: Element,
}

/// Key in a keyed element.
///
/// Spec: `Key = FieldName | Expression | LiteralValue`
#[derive(Debug, Clone, Copy, PartialEq, Eq, WalkAst)]
pub enum Key {
    /// Field name in struct literal: `Person{Name: "Alice"}`
    FieldName { ident: Ident, ident_pos: Span },
    /// Expression key: `map[string]int{"key": 1}`
    Expr(ExprId),
    /// Nested literal: `[][]int{{1, 2}}`
    Literal(LiteralValue),
}

/// Element value in a literal.
///
/// Spec: `Element = Expression | LiteralValue`
#[derive(Debug, Clone, Copy, PartialEq, Eq, WalkAst)]
pub enum Element {
    Expr(ExprId),
    Literal(LiteralValue),
}

/// Bracket list item that may be an expression (index) or a type (type args).
#[derive(Debug, Clone, Copy, PartialEq, Eq, WalkAst)]
pub enum ExprOrType {
    Expr(ExprId),
    Type(TypeId),
}

/// Call callee may be an expression or a type (conversion).
#[derive(Debug, Clone, Copy, PartialEq, Eq, WalkAst)]
pub enum CallCallee {
    Expr(ExprId),
    Type(TypeId),
}

/// Expression node.
#[derive(Debug, Clone, Copy, PartialEq, Eq, WalkAst)]
pub enum Expr {
    /// Identifier (type arguments handled via `Instantiate`)
    Ident(Ident, Span),

    /// Basic literal (int, float, string, etc.)
    BasicLit(BasicLit),

    /// Function literal: `func(x int) int { return x }`
    FuncLit {
        func_pos: Span,
        sig: SignatureId,
        body: Block,
    },

    /// Composite literal: `T{...}`
    CompositeLit { typ: TypeId, lit: LiteralValue },

    /// Bracket list: either indexing (`a[i]`) or generic instantiation (`F[T]`),
    /// deferred for later disambiguation.
    IndexOrInstantiate {
        base: ExprId,
        l_brack: Span,
        args: ListRef<ExprOrType>,
        trailing_comma: Option<Span>,
        r_brack: Span,
    },

    /// Parenthesized expression: `(x)`
    Paren {
        l_paren: Span,
        expr: ExprId,
        r_paren: Span,
    },

    /// Selector: `x.y`
    Selector {
        expr: ExprId,
        dot_pos: Span,
        ident: Ident,
        ident_pos: Span,
    },

    /// Slice expression: `a[lo:hi]` or `a[lo:hi:max]`
    Slice {
        expr: ExprId,
        l_brack: Span,
        lo: Option<ExprId>,
        colon1: Span,
        hi: Option<ExprId>,
        colon2: Option<Span>,
        max: Option<ExprId>,
        r_brack: Span,
    },

    /// Type assertion: `x.(T)` or `x.(type)` (in type switch)
    TypeAssert {
        expr: ExprId,
        dot_pos: Span,
        l_paren: Span,
        typ: Option<TypeId>,
        r_paren: Span,
    },

    /// Function call or conversion: `f(args...)` or `T(x)`
    ///
    /// Supports builtin form: `make(T, args)` via `type_arg`.
    Call {
        callee: CallCallee,
        l_paren: Span,
        /// Type argument for builtins: `make(T, ...)`, `new(T)`
        type_arg: Option<TypeId>,
        /// Comma after type_arg when followed by expressions
        type_comma: Option<Span>,
        /// Expression arguments
        args: ListRef<ExprId>,
        trailing_comma: Option<Span>,
        /// Ellipsis for variadic call: `f(x...)`
        ellipsis: Option<Span>,
        r_paren: Span,
    },

    /// Unary expression: `+x`, `-x`, `!x`, `^x`, `*x`, `&x`, `<-x`
    Unary {
        op: UnaryOp,
        op_pos: Span,
        expr: ExprId,
    },

    /// Binary expression: `x + y`, `x && y`, etc.
    Binary {
        left: ExprId,
        op: BinaryOp,
        op_pos: Span,
        right: ExprId,
    },

    /// Method expression: `T.Method`
    MethodExpr {
        recv: TypeId,
        dot_pos: Span,
        name: Ident,
        name_pos: Span,
    },

    /// Bad expression (error recovery)
    Bad(Span),
}

/// Basic literal value.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BasicLit {
    pub kind: BasicLitKind,
    pub raw: Span,
}

/// Basic literal kind.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum BasicLitKind {
    Int,
    Float,
    Imag,
    Rune,
    String,
}

/// String literal.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct StringLit {
    pub raw: Span,
}

// =============================================================================
// Types
// =============================================================================

/// Type node.
#[derive(Debug, Clone, Copy, PartialEq, Eq, WalkAst)]
pub enum Type {
    /// Named type: `pkg.Name` or `Name[T]`
    Named {
        pkg: Option<Ident>,
        pkg_pos: Option<Span>,
        dot_pos: Option<Span>,
        name: Ident,
        name_pos: Span,
        args_lbrack: Option<Span>,
        args: ListRef<TypeId>,
        args_trailing_comma: Option<Span>,
        args_rbrack: Option<Span>,
    },

    /// Pointer type: `*T`
    Pointer { star_pos: Span, elem: TypeId },

    /// Array type: `[N]T`
    Array {
        l_brack: Span,
        len: ArrayLen,
        r_brack: Span,
        elem: TypeId,
    },

    /// Slice type: `[]T`
    Slice {
        l_brack: Span,
        r_brack: Span,
        elem: TypeId,
    },

    /// Map type: `map[K]V`
    Map {
        map_pos: Span,
        l_brack: Span,
        key: TypeId,
        r_brack: Span,
        val: TypeId,
    },

    /// Channel type: `chan T`, `<-chan T`, or `chan<- T`
    Chan {
        dir: ChanDir,
        chan_pos: Span,
        /// Position of `<-` in `<-chan T`
        recv_arrow_pos: Option<Span>,
        /// Position of `<-` in `chan<- T`
        send_arrow_pos: Option<Span>,
        elem: TypeId,
    },

    /// Struct type: `struct { ... }`
    Struct {
        struct_pos: Span,
        l_brace: Span,
        fields: ListRef<FieldId>,
        r_brace: Span,
    },

    /// Interface type: `interface { ... }`
    Interface {
        interface_pos: Span,
        l_brace: Span,
        elems: ListRef<InterfaceElem>,
        r_brace: Span,
    },

    /// Function type: `func(...) ...`
    Func { func_pos: Span, sig: SignatureId },

    /// Parenthesized type: `(T)`
    Paren {
        l_paren: Span,
        typ: TypeId,
        r_paren: Span,
    },

    /// Bad type (error recovery)
    Bad(Span),
}

/// Array length specification.
#[derive(Debug, Clone, Copy, PartialEq, Eq, WalkAst)]
pub enum ArrayLen {
    /// Fixed length: `[10]int`
    Expr(ExprId),
    /// Inferred length: `[...]int`
    Ellipsis(Span),
}

/// Channel direction.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ChanDir {
    /// Bidirectional: `chan T`
    Both,
    /// Send-only: `chan<- T`
    Send,
    /// Receive-only: `<-chan T`
    Recv,
}

/// Type case element (in type switch).
#[derive(Debug, Clone, Copy, PartialEq, Eq, WalkAst)]
pub enum TypeCaseElem {
    Type(TypeId),
    Nil(Span),
}

/// Type term in union (interface).
#[derive(Debug, Clone, Copy, PartialEq, Eq, WalkAst)]
pub enum TypeTerm {
    /// Approximate type: `~T`
    Tilde { tilde_pos: Span, typ: TypeId },
    /// Exact type: `T`
    Type { typ: TypeId },
}

/// Interface element.
#[derive(Debug, Clone, Copy, PartialEq, Eq, WalkAst)]
pub enum InterfaceElem {
    /// Method specification: `Method(...) ...`
    Method {
        name: Ident,
        name_pos: Span,
        sig: SignatureId,
    },

    /// Type element: embedded type (`io.Reader`) or union (`int | ~float64`)
    TypeElem(TypeElem),
}

// =============================================================================
// Operators
// =============================================================================

/// Unary operator.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Add,   // +
    Sub,   // -
    Not,   // !
    Xor,   // ^
    Deref, // *
    Addr,  // &
    Recv,  // <-
}

/// Binary operator.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Add,    // +
    Sub,    // -
    Mul,    // *
    Div,    // /
    Mod,    // %
    And,    // &
    Or,     // |
    Xor,    // ^
    Shl,    // <<
    Shr,    // >>
    AndNot, // &^
    LAnd,   // &&
    LOr,    // ||
    Eq,     // ==
    Ne,     // !=
    Lt,     // <
    Le,     // <=
    Gt,     // >
    Ge,     // >=
}

/// Assignment operator.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AssignOp {
    Assign,       // =
    AddAssign,    // +=
    SubAssign,    // -=
    MulAssign,    // *=
    DivAssign,    // /=
    ModAssign,    // %=
    AndAssign,    // &=
    OrAssign,     // |=
    XorAssign,    // ^=
    ShlAssign,    // <<=
    ShrAssign,    // >>=
    AndNotAssign, // &^=
}

/// Increment/decrement operator.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IncDecOp {
    Inc, // ++
    Dec, // --
}
