//! # Abstract Syntax Tree (AST) Module for Go Language Parser
//!
//! This module implements a production-grade AST with the following design principles:
//!
//! ## Performance Characteristics
//!
//! - **Low allocation overhead**: Variable-length lists are stored in centralized buffers
//!   referenced by compact `ListRef` handles (start + length), eliminating per-node `Vec` allocations
//! - **Cache-friendly layout**: Spans are stored in separate side tables rather than inline,
//!   reducing node stride and improving locality during traversal
//! - **Type-safe IDs**: All node references use strongly-typed `Id<T>` wrappers to prevent
//!   cross-arena indexing bugs at compile time
//! - **Zero-copy interning**: String interner uses FNV-1a hashing with collision chains,
//!   avoiding string clones during lookup
//!
//! ## Architecture
//!
//! The design follows patterns from production compilers (Zig, Clang, rustc):
//!
//! 1. **Arena allocation**: All nodes are bump-allocated into typed arenas with stable IDs
//! 2. **Extra data buffers**: Lists (parameters, statements, etc.) are stored in centralized
//!    `Vec<T>` buffers and referenced by `ListRef { start: u32, len: u32 }`
//! 3. **Side-table spans**: Source locations are kept separate from node data for better packing
//!
//! ## Usage Example
//!
//! ```rust
//! use ast::{AstArena, Expr, Span, Interner};
//!
//! let mut arena = AstArena::new();
//! let mut interner = Interner::new();
//!
//! // Allocate an identifier expression
//! let name = interner.intern("foo");
//! let expr_id = arena.alloc_expr(
//!     Expr::Ident(name),
//!     Span::new(0, 3)
//! );
//!
//! // Access the node and its span
//! let expr = &arena.exprs[expr_id];
//! let span = arena.expr_span(expr_id);
//! ```

use core::marker::PhantomData;
use core::ops::{Index, IndexMut};
use smallvec::SmallVec;
use std::collections::HashMap;
use std::hash::{BuildHasherDefault, Hasher};

// =============================================================================
// SECTION: Core Types (IDs, Spans, List References)
// =============================================================================

/// Compact source location encoded as byte offsets into the source file.
///
/// Uses `u32` to support files up to 4GiB. For larger files, widen to `u64`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

impl Span {
    /// Creates a new span from byte offsets.
    ///
    /// # Panics
    ///
    /// In debug builds, panics if offsets exceed `u32::MAX`.
    #[inline]
    pub fn new(start: usize, end: usize) -> Self {
        debug_assert!(start <= u32::MAX as usize);
        debug_assert!(end <= u32::MAX as usize);

        Self {
            start: start as u32,
            end: end as u32,
        }
    }

    /// Returns the length of this span in bytes.
    #[inline]
    pub const fn len(self) -> u32 {
        self.end.saturating_sub(self.start)
    }

    /// Returns `true` if this span has zero length.
    #[inline]
    pub const fn is_empty(self) -> bool {
        self.start >= self.end
    }
}

/// Type-safe handle into an arena for a specific node type `T`.
///
/// The phantom type parameter prevents accidentally using an `Id<Expr>` to
/// index into an arena of `Stmt` nodes.
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
#[repr(transparent)]
pub struct Id<T> {
    raw: u32,
    _marker: PhantomData<fn() -> T>,
}

impl<T> Id<T> {
    /// Creates an ID from a raw index.
    ///
    /// # Safety
    ///
    /// Caller must ensure the index is valid for the target arena.
    #[inline]
    pub const fn from_raw(raw: u32) -> Self {
        Self {
            raw,
            _marker: PhantomData,
        }
    }

    /// Returns the raw index value.
    #[inline]
    pub const fn raw(&self) -> u32 {
        self.raw
    }

    /// Converts this ID to a `usize` for indexing.
    #[inline]
    pub const fn to_usize(&self) -> usize {
        self.raw as usize
    }
}

/// Reference to a contiguous list stored in an extra data buffer.
///
/// This replaces per-node `Vec<T>` allocations with a compact `(start, len)` pair
/// that indexes into a centralized buffer, dramatically reducing allocation overhead.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct ListRef {
    pub start: u32,
    pub len: u32,
}

impl ListRef {
    /// Empty list constant.
    pub const EMPTY: Self = Self { start: 0, len: 0 };

    /// Returns `true` if this list contains no elements.
    #[inline]
    pub const fn is_empty(self) -> bool {
        self.len == 0
    }

    /// Returns the exclusive end index of this list.
    #[inline]
    pub const fn end(self) -> u32 {
        self.start + self.len
    }
}

// =============================================================================
// SECTION: String Interning
// =============================================================================

/// Handle to an interned string.
///
/// `u32` supports up to 4 billion unique strings. For larger symbol tables,
/// widen to `u64`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Symbol(u32);

impl Symbol {
    /// Extracts the raw numeric value.
    #[inline]
    pub const fn as_u32(self) -> u32 {
        self.0
    }

    /// Creates a symbol from a raw value (internal use).
    #[inline]
    pub const fn from_u32(v: u32) -> Self {
        Self(v)
    }
}

/// Type alias for identifiers (interned names).
pub type Ident = Symbol;

/// Computes FNV-1a 64-bit hash of a byte sequence.
///
/// This is a fast, non-cryptographic hash suitable for hash table keying.
/// The implementation is const-compatible for potential compile-time evaluation.
const fn fnv1a64(bytes: &[u8]) -> u64 {
    const OFFSET: u64 = 0xcbf29ce484222325;
    const PRIME: u64 = 0x100000001b3;

    let mut hash = OFFSET;
    let mut i = 0;

    while i < bytes.len() {
        hash ^= bytes[i] as u64;
        hash = hash.wrapping_mul(PRIME);
        i += 1;
    }

    hash
}

/// Identity hasher for pre-hashed `u64` keys.
///
/// Since we hash strings ourselves with FNV-1a, we can use the hash directly
/// as the HashMap key, avoiding rehashing overhead.
#[derive(Default)]
struct U64IdentityHasher(u64);

impl Hasher for U64IdentityHasher {
    #[inline]
    fn write(&mut self, bytes: &[u8]) {
        // Fallback for non-u64 types (rarely used in our code)
        let mut hash = 0u64;
        for &b in bytes {
            hash = hash.wrapping_mul(131).wrapping_add(b as u64);
        }
        self.0 = hash;
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

type U64NoHash = BuildHasherDefault<U64IdentityHasher>;

/// String interner with zero-copy lookups.
///
/// ## Memory Layout
///
/// - `strings`: Canonical storage of each unique string as `Box<str>`, indexed by `Symbol`
/// - `buckets`: Hash table mapping FNV-1a hashes to symbol collision chains
///
/// ## Performance
///
/// - **Intern hit**: O(1) hash lookup + equality check (no allocation)
/// - **Intern miss**: O(1) hash insert + one allocation for the canonical string
/// - **Resolve**: O(1) array indexing
#[derive(Debug, Default)]
pub struct Interner {
    strings: Vec<Box<str>>,
    buckets: HashMap<u64, SmallVec<[Symbol; 1]>, U64NoHash>,
}

impl Interner {
    /// Creates an empty interner.
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    /// Returns the number of unique interned strings.
    #[inline]
    pub fn len(&self) -> usize {
        self.strings.len()
    }

    /// Returns `true` if no strings have been interned.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.strings.is_empty()
    }

    /// Interns a string and returns its unique symbol.
    ///
    /// If the string was previously interned, returns the existing symbol.
    /// Otherwise, allocates a new entry and returns a fresh symbol.
    pub fn intern(&mut self, s: &str) -> Symbol {
        let hash = fnv1a64(s.as_bytes());
        let candidates = self.buckets.entry(hash).or_default();

        // Check collision chain for existing symbol
        for &sym in candidates.iter() {
            if self.strings[sym.as_u32() as usize].as_ref() == s {
                return sym;
            }
        }

        // Not found - allocate new symbol
        let sym = Symbol::from_u32(self.strings.len() as u32);
        self.strings.push(s.into());
        candidates.push(sym);
        sym
    }

    /// Resolves a symbol back to its string slice.
    ///
    /// # Panics
    ///
    /// Panics if the symbol is invalid (out of bounds).
    #[inline]
    pub fn resolve(&self, sym: Symbol) -> &str {
        &self.strings[sym.as_u32() as usize]
    }
}

// =============================================================================
// SECTION: Arena Allocators
// =============================================================================

/// Simple bump allocator for homogeneous node types.
///
/// Nodes are allocated densely in a `Vec` with stable IDs. No deallocation
/// is supported (arena lifetime model).
#[derive(Debug)]
pub struct Arena<T> {
    data: Vec<T>,
}

impl<T> Default for Arena<T> {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

impl<T> Arena<T> {
    /// Creates an empty arena.
    #[inline]
    pub fn new() -> Self {
        Self { data: Vec::new() }
    }

    /// Creates an arena with preallocated capacity.
    #[inline]
    pub fn with_capacity(cap: usize) -> Self {
        Self {
            data: Vec::with_capacity(cap),
        }
    }

    /// Returns the number of allocated nodes.
    #[inline]
    pub fn len(&self) -> usize {
        self.data.len()
    }

    /// Returns `true` if no nodes have been allocated.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    /// Reserves capacity for at least `additional` more nodes.
    #[inline]
    pub fn reserve(&mut self, additional: usize) {
        self.data.reserve(additional);
    }

    /// Allocates a node and returns its stable ID.
    #[inline]
    pub fn alloc(&mut self, value: T) -> Id<T> {
        let id = Id::from_raw(self.data.len() as u32);
        self.data.push(value);
        id
    }

    /// Returns a reference to the node with the given ID.
    #[inline]
    pub fn get(&self, id: Id<T>) -> &T {
        &self.data[id.to_usize()]
    }

    /// Returns a mutable reference to the node with the given ID.
    #[inline]
    pub fn get_mut(&mut self, id: Id<T>) -> &mut T {
        &mut self.data[id.to_usize()]
    }

    /// Returns an iterator over all nodes.
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

/// Arena with separate span storage (side table pattern).
///
/// Keeping spans out-of-line reduces node size and improves cache locality
/// when traversing the AST without needing location information.
#[derive(Debug)]
pub struct SpannedArena<T> {
    nodes: Arena<T>,
    spans: Vec<Span>,
}

impl<T> Default for SpannedArena<T> {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

impl<T> SpannedArena<T> {
    /// Creates an empty spanned arena.
    #[inline]
    pub fn new() -> Self {
        Self {
            nodes: Arena::new(),
            spans: Vec::new(),
        }
    }

    /// Creates a spanned arena with preallocated capacity.
    #[inline]
    pub fn with_capacity(cap: usize) -> Self {
        Self {
            nodes: Arena::with_capacity(cap),
            spans: Vec::with_capacity(cap),
        }
    }

    /// Returns the number of allocated nodes.
    #[inline]
    pub fn len(&self) -> usize {
        self.nodes.len()
    }

    /// Returns `true` if no nodes have been allocated.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.nodes.is_empty()
    }

    /// Reserves capacity for at least `additional` more nodes.
    #[inline]
    pub fn reserve(&mut self, additional: usize) {
        self.nodes.reserve(additional);
        self.spans.reserve(additional);
    }

    /// Allocates a node with an associated span.
    #[inline]
    pub fn alloc(&mut self, node: T, span: Span) -> Id<T> {
        let id = self.nodes.alloc(node);
        debug_assert_eq!(self.spans.len(), id.to_usize());
        self.spans.push(span);
        id
    }

    /// Returns a reference to the node with the given ID.
    #[inline]
    pub fn get(&self, id: Id<T>) -> &T {
        self.nodes.get(id)
    }

    /// Returns a mutable reference to the node with the given ID.
    #[inline]
    pub fn get_mut(&mut self, id: Id<T>) -> &mut T {
        self.nodes.get_mut(id)
    }

    /// Returns the span associated with a node ID.
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
// SECTION: Extra Data Buffers (Zig-style list storage)
// =============================================================================

/// Centralized storage for variable-length lists.
///
/// Instead of storing `Vec<T>` inside each node (expensive), nodes store
/// `ListRef { start, len }` that indexes into these buffers.
#[derive(Debug, Default)]
pub struct ExtraData {
    pub idents: Vec<Ident>,
    pub exprs: Vec<ExprId>,
    pub stmts: Vec<StmtId>,
    pub types: Vec<TypeId>,
    pub elements: Vec<Element>,
    pub fields: Vec<FieldId>,
    pub switch_clauses: Vec<SwitchClauseId>,
    pub comm_clauses: Vec<CommClauseId>,
}

impl ExtraData {
    /// Appends items to a buffer and returns a `ListRef` to the range.
    #[inline]
    fn push_list<T>(buffer: &mut Vec<T>, items: impl IntoIterator<Item = T>) -> ListRef {
        let start = buffer.len();
        buffer.extend(items);
        let len = buffer.len() - start;

        debug_assert!(
            start <= u32::MAX as usize,
            "Buffer overflow: too many elements"
        );
        debug_assert!(len <= u32::MAX as usize, "List too long");

        ListRef {
            start: start as u32,
            len: len as u32,
        }
    }

    /// Returns a slice view into a buffer for a given list reference.
    #[inline]
    fn slice<T>(buffer: &[T], list: ListRef) -> &[T] {
        let start = list.start as usize;
        let end = list.end() as usize;
        &buffer[start..end]
    }
}

// =============================================================================
// SECTION: Type Aliases for Node IDs
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
// SECTION: Central AST Arena
// =============================================================================

/// Main AST data structure owning all nodes and extra data.
///
/// This is the single source of truth for an AST. All nodes are allocated
/// here, and all IDs are stable throughout the arena's lifetime.
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

    pub extras: ExtraData,
}

impl AstArena {
    /// Creates a new empty AST arena.
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    // -------------------------
    // Node Allocation
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
    // Span Access
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
    // List Building
    // -------------------------

    #[inline]
    pub fn list_idents(&mut self, items: impl IntoIterator<Item = Ident>) -> ListRef {
        ExtraData::push_list(&mut self.extras.idents, items)
    }

    #[inline]
    pub fn list_exprs(&mut self, items: impl IntoIterator<Item = ExprId>) -> ListRef {
        ExtraData::push_list(&mut self.extras.exprs, items)
    }

    #[inline]
    pub fn list_stmts(&mut self, items: impl IntoIterator<Item = StmtId>) -> ListRef {
        ExtraData::push_list(&mut self.extras.stmts, items)
    }

    #[inline]
    pub fn list_types(&mut self, items: impl IntoIterator<Item = TypeId>) -> ListRef {
        ExtraData::push_list(&mut self.extras.types, items)
    }

    #[inline]
    pub fn list_elements(&mut self, items: impl IntoIterator<Item = Element>) -> ListRef {
        ExtraData::push_list(&mut self.extras.elements, items)
    }

    #[inline]
    pub fn list_fields(&mut self, items: impl IntoIterator<Item = FieldId>) -> ListRef {
        ExtraData::push_list(&mut self.extras.fields, items)
    }

    #[inline]
    pub fn list_switch_clauses(
        &mut self,
        items: impl IntoIterator<Item = SwitchClauseId>,
    ) -> ListRef {
        ExtraData::push_list(&mut self.extras.switch_clauses, items)
    }

    #[inline]
    pub fn list_comm_clauses(&mut self, items: impl IntoIterator<Item = CommClauseId>) -> ListRef {
        ExtraData::push_list(&mut self.extras.comm_clauses, items)
    }

    // -------------------------
    // List Access
    // -------------------------

    #[inline]
    pub fn idents(&self, list: ListRef) -> &[Ident] {
        ExtraData::slice(&self.extras.idents, list)
    }

    #[inline]
    pub fn exprs_list(&self, list: ListRef) -> &[ExprId] {
        ExtraData::slice(&self.extras.exprs, list)
    }

    #[inline]
    pub fn stmts_list(&self, list: ListRef) -> &[StmtId] {
        ExtraData::slice(&self.extras.stmts, list)
    }

    #[inline]
    pub fn types_list(&self, list: ListRef) -> &[TypeId] {
        ExtraData::slice(&self.extras.types, list)
    }

    #[inline]
    pub fn elements_list(&self, list: ListRef) -> &[Element] {
        ExtraData::slice(&self.extras.elements, list)
    }

    #[inline]
    pub fn fields_list(&self, list: ListRef) -> &[FieldId] {
        ExtraData::slice(&self.extras.fields, list)
    }

    #[inline]
    pub fn switch_clauses_list(&self, list: ListRef) -> &[SwitchClauseId] {
        ExtraData::slice(&self.extras.switch_clauses, list)
    }

    #[inline]
    pub fn comm_clauses_list(&self, list: ListRef) -> &[CommClauseId] {
        ExtraData::slice(&self.extras.comm_clauses, list)
    }
}

// =============================================================================
// SECTION: Root Node
// =============================================================================

/// Root of a parsed Go source file.
///
/// This owns the arena and provides top-level declarations. The parser
/// constructs this structure and returns it to callers.
#[derive(Debug)]
pub struct SourceFile {
    pub package_name: Ident,
    pub arena: AstArena,
    pub imports: Vec<ImportSpec>,
    pub decls: Vec<TopLevelDecl>,
}

/// Top-level declaration (function or other declaration).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TopLevelDecl {
    Decl(DeclId),
    Func(FuncDeclId),
}

// =============================================================================
// SECTION: Declaration Nodes
// =============================================================================

/// General declaration node.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Decl {
    Gen(GenDecl),
}

/// Generic declaration (import, const, type, var).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GenDecl {
    pub kind: GenDeclKind,
    pub specs: Vec<Spec>, // Small count typically; can move to ListRef if needed
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

/// Specification within a generic declaration.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Spec {
    Import(ImportSpec),
    Value(ValueSpec),
    Type(TypeSpec),
}

/// Import specification (name and path).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ImportSpec {
    pub name: Option<Ident>,
    pub path: StringLit,
}

/// Value specification (const or var declaration).
///
/// Uses `ListRef` for names and values to avoid per-spec allocations.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ValueSpec {
    pub names: ListRef, // Ident[]
    pub typ: Option<TypeId>,
    pub values: ListRef, // ExprId[]
}

/// Type specification (type declaration).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeSpec {
    pub name: Ident,
    pub type_params: Option<TypeParamList>,
    pub typ: TypeId,
    pub alias: bool,
}

// =============================================================================
// SECTION: Function Declarations
// =============================================================================

/// Function declaration.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FuncDecl {
    pub recv: Option<FieldList>,
    pub name: Ident,
    pub type_params: Option<TypeParamList>,
    pub signature: SignatureId,
    pub body: Option<Block>,
}

/// Function signature (parameters and results).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Signature {
    pub params: FieldList,
    pub results: Option<Results>,
}

/// Return type specification.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Results {
    Params(FieldList),
    Type(TypeId),
}

/// Type parameter list (generics).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeParamList {
    pub params: Vec<TypeParam>, // Small count; could flatten if needed
}

/// Single type parameter with constraint.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeParam {
    pub names: ListRef, // Ident[]
    pub constraint: TypeId,
}

/// List of fields (parameters, struct fields, etc.).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FieldList {
    pub fields: ListRef, // FieldId[]
}

/// Single field with optional names and a type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Field {
    pub names: ListRef, // Ident[]
    pub typ: TypeId,
}

// =============================================================================
// SECTION: Statement Nodes
// =============================================================================

/// Block of statements.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Block {
    pub stmts: ListRef, // StmtId[]
}

/// Statement node.
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

/// Statement suffix for parsing assignment/define/send operations.
#[derive(Debug, Clone, PartialEq)]
pub enum StmtSuffix {
    Assign(AssignOp, ListRef), // rhs ExprId[]
    Define(ListRef),           // rhs ExprId[]
    Send(ExprId),
    Inc,
    Dec,
}

/// Increment/decrement operator.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum IncDecOp {
    Inc,
    Dec,
}

/// Assignment operator.
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

/// If statement.
#[derive(Debug, Clone, PartialEq)]
pub struct IfStmt {
    pub init: Option<StmtId>,
    pub cond: ExprId,
    pub then_block: Block,
    pub else_stmt: Option<StmtId>,
}

/// For statement (infinite, conditional, for-clause, or range).
#[derive(Debug, Clone, PartialEq)]
pub struct ForStmt {
    pub kind: ForKind,
    pub block: Block,
}

/// Kind of for loop.
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

/// Left-hand side of range expression.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RangeLhs {
    pub exprs: ListRef, // ExprId[]
}

// =============================================================================
// SECTION: Switch and Select Statements
// =============================================================================

/// Switch statement.
#[derive(Debug, Clone, PartialEq)]
pub struct SwitchStmt {
    pub init: Option<StmtId>,
    pub tag: Option<ExprId>,
    pub clauses: ListRef, // SwitchClauseId[]
}

/// Switch case clause.
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

/// Select statement.
#[derive(Debug, Clone, PartialEq)]
pub struct SelectStmt {
    pub clauses: ListRef, // CommClauseId[]
}

/// Communication clause in select statement.
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

/// Communication statement (send or receive).
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
// SECTION: Expression Nodes
// =============================================================================

/// Expression node.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

/// Expression context (load or store).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum ExprContext {
    Load,
    Store,
}

/// Composite literal element.
#[derive(Debug, Clone, PartialEq)]
pub enum Element {
    Expr(ExprId),
    KeyValue { key: ExprId, value: ExprId },
}

/// Basic literal token (stores span, not value).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BasicLit {
    pub kind: BasicLitKind,
    pub raw: Span,
}

/// Kind of basic literal.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum BasicLitKind {
    Int,
    Float,
    Imag,
    Rune,
    String,
}

/// Unary operator.
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

/// Binary operator.
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
// SECTION: Type Nodes
// =============================================================================

/// Type expression.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Named(NamedType),
    Pointer(TypeId),
    Array {
        len: Option<ExprId>,
        elem: TypeId,
    },
    Slice(TypeId),
    Map {
        key: TypeId,
        value: TypeId,
    },
    Chan {
        dir: ChanDir,
        elem: TypeId,
    },
    Struct {
        fields: FieldList,
    },
    Interface {
        methods: ListRef, // InterfaceElem[] (kept small, not in extras)
    },
    Func {
        signature: SignatureId,
    },
    Union(ListRef), // TypeId[]
    Tilde(TypeId),
    Paren(TypeId),
    ExprFallback(ExprId),
}

/// Named type with optional package qualifier and type arguments.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct NamedType {
    pub pkg: Option<Ident>,
    pub name: Ident,
    pub type_args: Option<ListRef>, // TypeId[]
}

/// Channel direction.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum ChanDir {
    Both,
    Send,
    Recv,
}

/// Interface element (method or embedded type).
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
// SECTION: Literals
// =============================================================================

/// String literal (stores span only, not the actual string value).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct StringLit {
    pub raw: Span,
}

impl StringLit {
    /// Creates a string literal from a span.
    #[inline]
    pub const fn new(raw: Span) -> Self {
        Self { raw }
    }
}

// =============================================================================
// SECTION: Unit Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_span_operations() {
        let span = Span::new(10, 20);
        assert_eq!(span.len(), 10);
        assert!(!span.is_empty());

        let empty = Span::new(5, 5);
        assert_eq!(empty.len(), 0);
        assert!(empty.is_empty());
    }

    #[test]
    fn test_interner_deduplication() {
        let mut interner = Interner::new();

        let sym1 = interner.intern("hello");
        let sym2 = interner.intern("hello");
        let sym3 = interner.intern("world");

        assert_eq!(sym1, sym2, "Same string should return same symbol");
        assert_ne!(
            sym1, sym3,
            "Different strings should return different symbols"
        );

        assert_eq!(interner.resolve(sym1), "hello");
        assert_eq!(interner.resolve(sym3), "world");
        assert_eq!(interner.len(), 2);
    }

    #[test]
    fn test_interner_collision_handling() {
        let mut interner = Interner::new();

        // Intern many strings to test collision chains
        let symbols: Vec<_> = (0..100)
            .map(|i| interner.intern(&format!("ident_{}", i)))
            .collect();

        // Verify all are unique
        for i in 0..symbols.len() {
            for j in (i + 1)..symbols.len() {
                assert_ne!(symbols[i], symbols[j]);
            }
        }

        assert_eq!(interner.len(), 100);
    }

    #[test]
    fn test_listref_operations() {
        let mut arena = AstArena::new();

        let list1 = arena.list_idents([
            Symbol::from_u32(1),
            Symbol::from_u32(2),
            Symbol::from_u32(3),
        ]);

        assert_eq!(list1.len, 3);
        assert!(!list1.is_empty());

        let idents = arena.idents(list1);
        assert_eq!(idents.len(), 3);
        assert_eq!(idents[0].as_u32(), 1);
        assert_eq!(idents[1].as_u32(), 2);
        assert_eq!(idents[2].as_u32(), 3);

        let list2 = arena.list_idents([Symbol::from_u32(9)]);
        assert_eq!(arena.idents(list2).len(), 1);
        assert_eq!(arena.idents(list2)[0].as_u32(), 9);

        // Verify first list is unchanged
        let idents1 = arena.idents(list1);
        assert_eq!(idents1.len(), 3);
        assert_eq!(idents1[0].as_u32(), 1);
    }

    #[test]
    fn test_empty_listref() {
        let arena = AstArena::new();
        let empty = ListRef::EMPTY;

        assert!(empty.is_empty());
        assert_eq!(empty.len, 0);
        assert_eq!(arena.idents(empty).len(), 0);
    }

    #[test]
    fn test_spanned_arena_alignment() {
        let mut arena: SpannedArena<u32> = SpannedArena::new();

        let id1 = arena.alloc(42, Span::new(0, 2));
        let id2 = arena.alloc(100, Span::new(5, 10));
        let id3 = arena.alloc(999, Span::new(15, 20));

        assert_eq!(arena.get(id1), &42);
        assert_eq!(arena.get(id2), &100);
        assert_eq!(arena.get(id3), &999);

        assert_eq!(arena.span(id1), Span::new(0, 2));
        assert_eq!(arena.span(id2), Span::new(5, 10));
        assert_eq!(arena.span(id3), Span::new(15, 20));

        assert_eq!(arena.len(), 3);
    }

    #[test]
    fn test_arena_indexing() {
        let mut arena = AstArena::new();

        let expr_id = arena.alloc_expr(Expr::Bad, Span::new(0, 1));

        // Test both direct access and indexing trait
        assert_eq!(arena.exprs.get(expr_id), &Expr::Bad);
        assert_eq!(&arena.exprs[expr_id], &Expr::Bad);

        // Test mutable access
        arena.exprs[expr_id] = Expr::Ident(Symbol::from_u32(0));
        assert_eq!(&arena.exprs[expr_id], &Expr::Ident(Symbol::from_u32(0)));
    }

    #[test]
    fn test_expr_allocation_and_retrieval() {
        let mut arena = AstArena::new();

        let ident_sym = Symbol::from_u32(42);
        let expr_id = arena.alloc_expr(Expr::Ident(ident_sym), Span::new(10, 15));

        match arena.exprs[expr_id] {
            Expr::Ident(sym) => assert_eq!(sym, ident_sym),
            _ => panic!("Expected Ident expression"),
        }

        assert_eq!(arena.expr_span(expr_id), Span::new(10, 15));
    }

    #[test]
    fn test_multiple_list_types() {
        let mut arena = AstArena::new();

        // Create lists of different types
        let idents = arena.list_idents([Symbol::from_u32(1), Symbol::from_u32(2)]);

        let expr1 = arena.alloc_expr(Expr::Bad, Span::new(0, 1));
        let expr2 = arena.alloc_expr(Expr::Bad, Span::new(1, 2));
        let exprs = arena.list_exprs([expr1, expr2]);

        // Verify they don't interfere with each other
        assert_eq!(arena.idents(idents).len(), 2);
        assert_eq!(arena.exprs_list(exprs).len(), 2);

        assert_eq!(arena.idents(idents)[0].as_u32(), 1);
        assert_eq!(arena.exprs_list(exprs)[0], expr1);
    }

    #[test]
    fn test_fnv1a_hash_deterministic() {
        let hash1 = fnv1a64(b"test");
        let hash2 = fnv1a64(b"test");
        let hash3 = fnv1a64(b"different");

        assert_eq!(hash1, hash2, "Same input should produce same hash");
        assert_ne!(
            hash1, hash3,
            "Different inputs should produce different hashes"
        );
    }
}
