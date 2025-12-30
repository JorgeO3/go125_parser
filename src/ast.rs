//! AST (Abstract Syntax Tree) module for Go language parser.
//!
//! This module implements a flattened AST using arena allocation for memory efficiency
//! and cache locality. All AST nodes are stored in typed arenas and referenced via
//! type-safe indices (IDs).
//!
//! ## Architecture
//!
//! - **Arena-based allocation**: All nodes live in `AstArena`, avoiding deep nesting
//! - **Typed indices**: `DeclId`, `ExprId`, etc. provide type safety over raw pointers
//! - **Span tracking**: Every node tracks its source location via `Spanned<T>`
//! - **Optimized collections**: Uses `SmallVec` for small lists, `SmolStr` for strings

use std::collections::HashMap;

use la_arena::{Arena, Idx};
use smallvec::SmallVec;

// =============================================================================
// Core Types, IDs and Arena
// =============================================================================

/// Interned identifier string.
///
/// Uses `SmolStr` which stores short strings inline without heap allocation.
pub type Ident = Symbol;

/// Type-safe index into the declarations arena.
pub type DeclId = Idx<Spanned<Decl>>;

/// Type-safe index into the statements arena.
pub type StmtId = Idx<Spanned<Stmt>>;

/// Type-safe index into the expressions arena.
pub type ExprId = Idx<Spanned<Expr>>;

/// Type-safe index into the types arena.
pub type TypeId = Idx<Spanned<Type>>;

/// Type-safe index into the function signatures arena.
pub type SignatureId = Idx<Spanned<Signature>>;

/// An interned string handle (compact identifier).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Symbol(u32);

impl Symbol {
    #[inline]
    pub const fn as_u32(self) -> u32 {
        self.0
    }
}

/// Source code location range (byte offsets).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Span {
    /// Start byte offset in source.
    pub start: u32,
    /// End byte offset in source (exclusive).
    pub end: u32,
}

impl Span {
    /// Creates a new span from byte offsets.
    pub fn new(start: usize, end: usize) -> Self {
        Self {
            start: start as u32,
            end: end as u32,
        }
    }
}

/// Wrapper that associates an AST node with its source location.
#[derive(Debug, Clone, PartialEq)]
pub struct Spanned<T> {
    /// The actual AST node.
    pub node: T,
    /// Source location of this node.
    pub span: Span,
}

/// Simple string interner (no external deps).
///
/// - `intern(&str) -> Symbol` deduplicates identifiers.
/// - `resolve(Symbol) -> &str` retrieves the original text.
#[derive(Debug, Default)]
pub struct Interner {
    map: HashMap<Box<str>, Symbol>,
    vec: Vec<Box<str>>,
}

impl Interner {
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    #[inline]
    pub fn intern(&mut self, s: &str) -> Symbol {
        if let Some(&sym) = self.map.get(s) {
            return sym;
        }
        let boxed: Box<str> = s.into();
        let sym = Symbol(self.vec.len() as u32);
        // Important: insert using the same boxed key we store.
        self.map.insert(boxed.clone(), sym);
        self.vec.push(boxed);
        sym
    }

    #[inline]
    pub fn resolve(&self, sym: Symbol) -> &str {
        &self.vec[sym.0 as usize]
    }
}

/// Central arena that owns all AST node memory.
///
/// This flattened structure improves cache locality and simplifies memory management.
/// All nodes are allocated here and referenced via typed IDs.
#[derive(Default, Debug, PartialEq)]
pub struct AstArena {
    /// Storage for all declaration nodes.
    pub decls: Arena<Spanned<Decl>>,
    /// Storage for all statement nodes.
    pub stmts: Arena<Spanned<Stmt>>,
    /// Storage for all expression nodes.
    pub exprs: Arena<Spanned<Expr>>,
    /// Storage for all type nodes.
    pub types: Arena<Spanned<Type>>,
    /// Storage for all function signature nodes.
    pub signatures: Arena<Spanned<Signature>>,
}

impl AstArena {
    /// Creates a new empty arena.
    pub fn new() -> Self {
        Self::default()
    }

    // --- Allocation helpers ---

    /// Allocates a declaration node in the arena.
    #[inline]
    pub fn alloc_decl(&mut self, decl: Decl, span: Span) -> DeclId {
        self.decls.alloc(Spanned { node: decl, span })
    }

    /// Allocates a statement node in the arena.
    #[inline]
    pub fn alloc_stmt(&mut self, stmt: Stmt, span: Span) -> StmtId {
        self.stmts.alloc(Spanned { node: stmt, span })
    }

    /// Allocates an expression node in the arena.
    #[inline]
    pub fn alloc_expr(&mut self, expr: Expr, span: Span) -> ExprId {
        self.exprs.alloc(Spanned { node: expr, span })
    }

    /// Allocates a type node in the arena.
    #[inline]
    pub fn alloc_type(&mut self, typ: Type, span: Span) -> TypeId {
        self.types.alloc(Spanned { node: typ, span })
    }

    /// Allocates a function signature node in the arena.
    #[inline]
    pub fn alloc_signature(&mut self, sig: Signature, span: Span) -> SignatureId {
        self.signatures.alloc(Spanned { node: sig, span })
    }

    // --- Access helpers ---

    /// Retrieves a declaration node by ID.
    #[inline]
    pub fn get_decl(&self, id: DeclId) -> &Spanned<Decl> {
        &self.decls[id]
    }

    /// Gets the span of a declaration.
    #[inline]
    pub fn decl_span(&self, id: DeclId) -> Span {
        self.decls[id].span
    }

    /// Retrieves a statement node by ID.
    #[inline]
    pub fn get_stmt(&self, id: StmtId) -> &Spanned<Stmt> {
        &self.stmts[id]
    }

    /// Gets the span of a statement.
    #[inline]
    pub fn stmt_span(&self, id: StmtId) -> Span {
        self.stmts[id].span
    }

    /// Retrieves an expression node by ID.
    #[inline]
    pub fn get_expr(&self, id: ExprId) -> &Spanned<Expr> {
        &self.exprs[id]
    }

    /// Gets the span of an expression.
    #[inline]
    pub fn expr_span(&self, id: ExprId) -> Span {
        self.exprs[id].span
    }

    /// Retrieves a type node by ID.
    #[inline]
    pub fn get_type(&self, id: TypeId) -> &Spanned<Type> {
        &self.types[id]
    }

    /// Gets the span of a type.
    #[inline]
    pub fn type_span(&self, id: TypeId) -> Span {
        self.types[id].span
    }

    /// Retrieves a signature node by ID.
    #[inline]
    pub fn get_signature(&self, id: SignatureId) -> &Spanned<Signature> {
        &self.signatures[id]
    }
}

// =============================================================================
// Root Structure and Declarations
// =============================================================================

/// Root node representing a complete Go source file.
#[derive(Debug, PartialEq)]
pub struct SourceFile {
    /// Package name from the `package` declaration.
    pub package_name: Ident,
    /// The arena that owns all AST nodes.
    pub arena: AstArena,
    /// Import declarations.
    pub imports: Vec<ImportSpec>,
    /// Top-level declarations in the file.
    pub decls: Vec<TopLevelDecl>,
}

/// Top-level declaration (can be a general declaration or function).
#[derive(Debug, Clone, PartialEq)]
pub enum TopLevelDecl {
    /// General declaration (const, var, type, import).
    Decl(DeclId),
    /// Function declaration (stored as struct due to complexity).
    Func(FuncDecl),
}

/// Declaration node.
#[derive(Debug, Clone, PartialEq)]
pub enum Decl {
    /// Generic declaration (const, var, type, import).
    Gen(GenDecl),
}

/// Generic declaration with specifications.
#[derive(Debug, Clone, PartialEq)]
pub struct GenDecl {
    /// Kind of declaration (import, const, type, var).
    pub kind: GenDeclKind,
    /// List of specifications for this declaration.
    pub specs: Vec<Spec>,
}

/// Kind of generic declaration.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GenDeclKind {
    /// Import declaration.
    Import,
    /// Constant declaration.
    Const,
    /// Type declaration.
    Type,
    /// Variable declaration.
    Var,
}

/// Specification within a generic declaration.
#[derive(Debug, Clone, PartialEq)]
pub enum Spec {
    /// Import specification.
    Import(ImportSpec),
    /// Value specification (const or var).
    Value(ValueSpec),
    /// Type specification.
    Type(TypeSpec),
}

/// Import specification (`import "path"` or `import name "path"`).
#[derive(Debug, Clone, PartialEq)]
pub struct ImportSpec {
    /// Optional import name (alias or `.` for dot import).
    pub name: Option<Ident>,
    /// Import path string literal.
    pub path: StringLit,
}

/// Value specification for const or var declarations.
#[derive(Debug, Clone, PartialEq)]
pub struct ValueSpec {
    /// List of identifier names being declared.
    pub names: SmallVec<[Ident; 2]>,
    /// Optional type annotation.
    pub typ: Option<TypeId>,
    /// Optional initialization values.
    pub values: SmallVec<[ExprId; 2]>,
}

/// Type specification for type declarations.
#[derive(Debug, Clone, PartialEq)]
pub struct TypeSpec {
    /// Name of the type being declared.
    pub name: Ident,
    /// Optional type parameters (for generic types).
    pub type_params: Option<TypeParamList>,
    /// The actual type definition.
    pub typ: TypeId,
    /// Whether this is a type alias (`=`) vs definition.
    pub alias: bool,
}

/// Function declaration.
///
/// Kept as a struct (not flattened to ID) due to complexity and frequent access patterns.
#[derive(Debug, Clone, PartialEq)]
pub struct FuncDecl {
    /// Optional receiver (makes this a method).
    pub recv: Option<FieldList>,
    /// Function name.
    pub name: Ident,
    /// Optional type parameters (for generic functions).
    pub type_params: Option<TypeParamList>,
    /// Function signature (parameters and return types).
    pub signature: Signature,
    /// Optional function body (None for declarations without implementation).
    pub body: Option<Block>,
}

/// Function signature (parameters and return types).
#[derive(Debug, Clone, PartialEq)]
pub struct Signature {
    /// Input parameters.
    pub params: FieldList,
    /// Optional return values.
    pub results: Option<Results>,
}

/// Return value specification.
#[derive(Debug, Clone, PartialEq)]
pub enum Results {
    /// Named or typed return parameters.
    Params(FieldList),
    /// Single unnamed return type.
    Type(TypeId),
}

/// Type parameter list for generic types/functions.
#[derive(Debug, Clone, PartialEq)]
pub struct TypeParamList {
    /// List of type parameters.
    pub params: Vec<TypeParam>,
}

/// Single type parameter with constraint.
#[derive(Debug, Clone, PartialEq)]
pub struct TypeParam {
    /// Type parameter names (can be multiple with same constraint).
    pub names: SmallVec<[Ident; 2]>,
    /// Type constraint (interface type).
    pub constraint: TypeId,
}

/// List of fields (used in parameters, struct fields, etc.).
#[derive(Debug, Clone, PartialEq)]
pub struct FieldList {
    /// The fields.
    pub fields: Vec<Field>,
}

/// Single field with names and type.
#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    /// Field names (can be multiple with same type, or empty for anonymous).
    pub names: SmallVec<[Ident; 2]>,
    /// Field type.
    pub typ: TypeId,
}

// =============================================================================
// Statements (Flattened)
// =============================================================================

/// Block of statements.
#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    /// List of statement IDs in this block.
    pub stmts: Vec<StmtId>,
}

/// Statement node.
#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    /// Declaration statement.
    Decl(DeclId),

    /// Empty statement (`;`).
    Empty,

    /// Labeled statement.
    Labeled {
        /// Label name.
        label: Ident,
        /// Statement being labeled.
        stmt: StmtId,
    },

    /// Expression statement.
    Expr(ExprId),

    /// Channel send statement (`chan <- value`).
    Send {
        /// Channel expression.
        chan: ExprId,
        /// Value to send.
        value: ExprId,
    },

    /// Increment/decrement statement (`x++` or `x--`).
    IncDec {
        /// Expression to modify.
        expr: ExprId,
        /// Operation (increment or decrement).
        op: IncDecOp,
    },

    /// Assignment statement.
    Assign {
        /// Left-hand side expressions.
        lhs: SmallVec<[ExprId; 2]>,
        /// Assignment operator.
        op: AssignOp,
        /// Right-hand side expressions.
        rhs: SmallVec<[ExprId; 2]>,
    },

    /// Short variable declaration (`:=`).
    ShortVarDecl {
        /// Variable names.
        names: SmallVec<[Ident; 2]>,
        /// Initialization values.
        values: SmallVec<[ExprId; 2]>,
    },

    /// Go statement (goroutine launch).
    Go(ExprId),

    /// Defer statement.
    Defer(ExprId),

    /// Return statement.
    Return(SmallVec<[ExprId; 2]>),

    /// Break statement.
    Break(Option<Ident>),

    /// Continue statement.
    Continue(Option<Ident>),

    /// Goto statement.
    Goto(Ident),

    /// Fallthrough statement (in switch).
    Fallthrough,

    /// Block statement.
    Block(Block),

    /// If statement.
    If(IfStmt),

    /// For/range loop.
    For(ForStmt),

    /// Switch statement.
    Switch(SwitchStmt),

    /// Select statement (channel multiplexing).
    Select(SelectStmt),
}

/// Statement suffix for parsing disambiguation.
#[derive(Debug, Clone, PartialEq)]
pub enum StmtSuffix {
    /// Assignment suffix.
    Assign(AssignOp, SmallVec<[ExprId; 2]>),
    /// Definition suffix (`:=`).
    Define(SmallVec<[ExprId; 2]>),
    /// Channel send suffix.
    Send(ExprId),
    /// Increment suffix.
    Inc,
    /// Decrement suffix.
    Dec,
}

/// Increment or decrement operation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IncDecOp {
    /// Increment (`++`).
    Inc,
    /// Decrement (`--`).
    Dec,
}

/// Assignment operator.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AssignOp {
    /// Simple assignment (`=`).
    Assign,
    /// Short variable declaration (`:=`).
    Define,
    /// Add and assign (`+=`).
    AddAssign,
    /// Subtract and assign (`-=`).
    SubAssign,
    /// Multiply and assign (`*=`).
    MulAssign,
    /// Divide and assign (`/=`).
    DivAssign,
    /// Modulo and assign (`%=`).
    ModAssign,
    /// Bitwise AND and assign (`&=`).
    AndAssign,
    /// Bitwise OR and assign (`|=`).
    OrAssign,
    /// Bitwise XOR and assign (`^=`).
    XorAssign,
    /// Left shift and assign (`<<=`).
    ShlAssign,
    /// Right shift and assign (`>>=`).
    ShrAssign,
    /// Bit clear and assign (`&^=`).
    AndNotAssign,
}

/// If statement.
#[derive(Debug, Clone, PartialEq)]
pub struct IfStmt {
    /// Optional initialization statement.
    pub init: Option<StmtId>,
    /// Condition expression.
    pub cond: ExprId,
    /// Then branch block.
    pub then_block: Block,
    /// Optional else branch (can be another if or block).
    pub else_stmt: Option<StmtId>,
}

/// For loop statement.
#[derive(Debug, Clone, PartialEq)]
pub struct ForStmt {
    /// Kind of for loop.
    pub kind: ForKind,
    /// Loop body.
    pub block: Block,
}

/// For loop kind/condition.
#[derive(Debug, Clone, PartialEq)]
pub enum ForKind {
    /// Infinite loop (`for { }`).
    Infinite,

    /// Conditional loop (`for cond { }`).
    Cond(ExprId),

    /// C-style for loop (`for init; cond; post { }`).
    ForClause {
        /// Initialization statement.
        init: Option<StmtId>,
        /// Condition expression.
        cond: Option<ExprId>,
        /// Post iteration statement.
        post: Option<StmtId>,
    },

    /// Range loop (`for k, v := range expr { }`).
    Range {
        /// Left-hand side (variables being assigned).
        lhs: Option<RangeLhs>,
        /// Expression being ranged over.
        expr: ExprId,
        /// Whether using `:=` (true) or `=` (false).
        define: bool,
    },
}

/// Left-hand side of a range statement.
#[derive(Debug, Clone, PartialEq)]
pub struct RangeLhs {
    /// Expressions on left side (typically 1-2: key/index and value).
    pub exprs: SmallVec<[ExprId; 2]>,
}

/// Switch statement (expression or type switch).
#[derive(Debug, Clone, PartialEq)]
pub struct SwitchStmt {
    /// Optional initialization statement.
    pub init: Option<StmtId>,
    /// Optional tag expression (None for type switch).
    pub tag: Option<ExprId>,
    /// Case clauses.
    pub clauses: Vec<SwitchClause>,
}

/// Single case in a switch statement.
#[derive(Debug, Clone, PartialEq)]
pub enum SwitchClause {
    /// Expression case (`case expr1, expr2:`).
    Expr {
        /// Case expressions.
        exprs: SmallVec<[ExprId; 2]>,
        /// Statements in this case.
        stmts: Vec<StmtId>,
    },

    /// Default case.
    Default {
        /// Statements in default case.
        stmts: Vec<StmtId>,
    },

    /// Type case (`case Type1, Type2:`).
    Type {
        /// Case types.
        types: Vec<TypeId>,
        /// Optional variable binding.
        bind: Option<Ident>,
        /// Statements in this case.
        stmts: Vec<StmtId>,
    },
}

/// Select statement for channel operations.
#[derive(Debug, Clone, PartialEq)]
pub struct SelectStmt {
    /// Communication clauses.
    pub clauses: Vec<CommClause>,
}

/// Single case in a select statement.
#[derive(Debug, Clone, PartialEq)]
pub enum CommClause {
    /// Communication case (send or receive).
    Comm {
        /// The communication operation.
        comm: CommStmt,
        /// Statements to execute.
        stmts: Vec<StmtId>,
    },

    /// Default case.
    Default {
        /// Statements to execute.
        stmts: Vec<StmtId>,
    },
}

/// Communication statement in select.
#[derive(Debug, Clone, PartialEq)]
pub enum CommStmt {
    /// Channel send operation.
    Send {
        /// Channel to send to.
        chan: ExprId,
        /// Value to send.
        value: ExprId,
    },

    /// Channel receive operation.
    Recv {
        /// Optional left-hand side for assignment.
        lhs: Option<SmallVec<[ExprId; 2]>>,
        /// Whether using `:=` (true) or `=` (false).
        define: bool,
        /// Receive expression.
        recv: ExprId,
    },
}

// =============================================================================
// Expressions (Flattened)
// =============================================================================

/// Expression node.
#[rustfmt::skip]
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    /// Identifier reference.
    Ident(Ident),
    
    /// Basic literal (number, string, rune).
    BasicLit(BasicLit),
    
    /// Composite literal (`Type{elements}`).
    CompositeLit {
        /// Type being constructed.
        typ: TypeId,
        /// Elements in the literal.
        elements: Vec<Element>,
    },
    
    /// Function literal (lambda/closure).
    FuncLit {
        /// Function signature.
        signature: SignatureId,
        /// Function body.
        body: Block,
    },
    
    /// Parenthesized expression.
    Paren(ExprId),
    
    /// Selector expression (`expr.ident`).
    Selector {
        /// Base expression.
        expr: ExprId,
        /// Selected identifier.
        ident: Ident,
    },
    
    /// Index expression (`expr[index]`).
    Index {
        /// Base expression.
        expr: ExprId,
        /// Index expression.
        index: ExprId,
    },
    
    /// Slice expression (`expr[lo:hi:max]`).
    Slice {
        /// Base expression.
        expr: ExprId,
        /// Low bound (optional).
        lo: Option<ExprId>,
        /// High bound (optional).
        hi: Option<ExprId>,
        /// Max capacity (optional, for 3-index slices).
        max: Option<ExprId>,
    },
    
    /// Type assertion (`expr.(Type)`).
    TypeAssert {
        /// Expression being asserted.
        expr: ExprId,
        /// Type to assert.
        typ: TypeId,
    },
    
    /// Function call.
    Call {
        /// Function being called.
        fun: ExprId,
        /// Arguments.
        args: SmallVec<[ExprId; 4]>,
        /// Whether last argument is variadic (`...`).
        variadic: bool,
    },
    
    /// Unary operation (`+x`, `-x`, `!x`, etc.).
    Unary {
        /// Unary operator.
        op: UnaryOp,
        /// Operand expression.
        expr: ExprId,
    },
    
    /// Binary operation (`x + y`, `x == y`, etc.).
    Binary {
        /// Left operand.
        left: ExprId,
        /// Binary operator.
        op: BinaryOp,
        /// Right operand.
        right: ExprId,
    },
    
    /// Star expression (dereference or pointer type - context-dependent).
    Star(ExprId),
    
    /// Starred expression with explicit context.
    Starred {
        /// Value being starred.
        value: ExprId,
        /// Expression context (load or store).
        ctx: ExprContext,
    },
    
    /// Channel receive expression (`<-chan`).
    Receive(ExprId),
    
    /// Type wrapper (stores TypeId instead of Type by value).
    /// Used when a type appears in expression context.
    TypeWrapper(TypeId),
    
    /// Invalid/error expression (for error recovery during parsing).
    Bad,
}

/// Expression context (load vs store).
#[derive(Debug, Clone, PartialEq)]
pub enum ExprContext {
    /// Load context (reading value).
    Load,
    /// Store context (writing value).
    Store,
}

/// Element in composite literal.
#[derive(Debug, Clone, PartialEq)]
pub enum Element {
    /// Simple value element.
    Expr(ExprId),
    /// Key-value element (`key: value`).
    KeyValue {
        /// Key expression.
        key: ExprId,
        /// Value expression.
        value: ExprId,
    },
}

/// Basic literal value.
#[derive(Debug, Clone, PartialEq)]
pub struct BasicLit {
    /// Kind of literal.
    pub kind: BasicLitKind,
    /// Raw source text.
    pub raw: Span,
}

/// Kind of basic literal.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BasicLitKind {
    /// Integer literal.
    Int,
    /// Floating-point literal.
    Float,
    /// Imaginary number literal.
    Imag,
    /// Rune (character) literal.
    Rune,
    /// String literal.
    String,
}

/// Unary operator.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryOp {
    /// Unary plus (`+`).
    Add,
    /// Unary minus (`-`).
    Sub,
    /// Logical NOT (`!`).
    Not,
    /// Bitwise NOT (`^`).
    Xor,
    /// Dereference (`*`).
    Deref,
    /// Address-of (`&`).
    Addr,
}

/// Binary operator.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinaryOp {
    /// Addition (`+`).
    Add,
    /// Subtraction (`-`).
    Sub,
    /// Multiplication (`*`).
    Mul,
    /// Division (`/`).
    Div,
    /// Modulo (`%`).
    Mod,
    /// Bitwise AND (`&`).
    And,
    /// Bitwise OR (`|`).
    Or,
    /// Bitwise XOR (`^`).
    Xor,
    /// Left shift (`<<`).
    Shl,
    /// Right shift (`>>`).
    Shr,
    /// Bit clear (`&^`).
    AndNot,
    /// Logical AND (`&&`).
    LAnd,
    /// Logical OR (`||`).
    LOr,
    /// Equal (`==`).
    Eq,
    /// Not equal (`!=`).
    Ne,
    /// Less than (`<`).
    Lt,
    /// Less than or equal (`<=`).
    Le,
    /// Greater than (`>`).
    Gt,
    /// Greater than or equal (`>=`).
    Ge,
}

// =============================================================================
// Types (Flattened)
// =============================================================================

/// Type node.
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    /// Named type (possibly qualified, possibly generic).
    Named(NamedType),

    /// Pointer type (`*T`).
    Pointer(TypeId),

    /// Array type (`[len]T` or `[...]T`).
    Array {
        /// Array length (None for `[...]` syntax).
        len: Option<ExprId>,
        /// Element type.
        elem: TypeId,
    },

    /// Slice type (`[]T`).
    Slice(TypeId),

    /// Map type (`map[K]V`).
    Map {
        /// Key type.
        key: TypeId,
        /// Value type.
        value: TypeId,
    },

    /// Channel type (`chan T`, `chan<- T`, `<-chan T`).
    Chan {
        /// Channel direction.
        dir: ChanDir,
        /// Element type.
        elem: TypeId,
    },

    /// Struct type.
    Struct {
        /// Struct fields.
        fields: Vec<Field>,
    },

    /// Interface type.
    Interface {
        /// Interface methods and embedded types.
        methods: Vec<InterfaceElem>,
    },

    /// Function type.
    Func {
        /// Function signature.
        signature: SignatureId,
    },

    /// Union type (for type constraints, e.g., `int | string`).
    Union(Vec<TypeId>),

    /// Approximate type (`~T` in type constraints).
    Tilde(TypeId),

    /// Parenthesized type.
    Paren(TypeId),

    /// Expression fallback for parsing ambiguity.
    /// Used when we can't determine if something is a type or expression yet.
    ExprFallback(ExprId),
}

/// Named type reference.
#[derive(Debug, Clone, PartialEq)]
pub struct NamedType {
    /// Optional package qualifier.
    pub pkg: Option<Ident>,
    /// Type name.
    pub name: Ident,
    /// Optional type arguments (for generic instantiation).
    pub type_args: Option<Vec<TypeId>>,
}

/// Channel direction.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ChanDir {
    /// Bidirectional channel (`chan T`).
    Both,
    /// Send-only channel (`chan<- T`).
    Send,
    /// Receive-only channel (`<-chan T`).
    Recv,
}

/// Interface element (method or embedded type).
#[derive(Debug, Clone, PartialEq)]
pub enum InterfaceElem {
    /// Method specification.
    Method {
        /// Method name.
        name: Ident,
        /// Optional type parameters (for generic methods).
        type_params: Option<TypeParamList>,
        /// Method signature.
        sig: SignatureId,
    },

    /// Embedded type (interface embedding).
    Embed(TypeId),
}

/// String literal.
#[derive(Debug, Clone, PartialEq)]
pub struct StringLit {
    /// Raw string content (including quotes).
    pub raw: Span,
}

impl StringLit {
    /// Creates a new string literal.
    pub const fn new(raw: Span) -> Self {
        Self { raw }
    }
}
