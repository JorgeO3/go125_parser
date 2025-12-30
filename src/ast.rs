use smallvec::SmallVec;

pub type Ident = String;

#[derive(Debug, Clone, PartialEq)]
pub struct SourceFile {
    pub package_name: Ident,
    pub imports: Vec<ImportSpec>,
    pub decls: Vec<TopLevelDecl>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TopLevelDecl {
    Decl(Decl),
    Func(FuncDecl),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Decl {
    Gen(GenDecl),
}

#[derive(Debug, Clone, PartialEq)]
pub struct GenDecl {
    pub kind: GenDeclKind,
    pub specs: Vec<Spec>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GenDeclKind {
    Import,
    Const,
    Type,
    Var,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Spec {
    Import(ImportSpec),
    Value(ValueSpec),
    Type(TypeSpec),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImportSpec {
    pub name: Option<Ident>,
    pub path: StringLit,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ValueSpec {
    pub names: Vec<Ident>,
    pub typ: Option<Type>,
    pub values: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeSpec {
    pub name: Ident,
    pub type_params: Option<TypeParamList>,
    pub typ: Type,
    pub alias: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncDecl {
    pub recv: Option<FieldList>,
    pub name: Ident,
    pub type_params: Option<TypeParamList>,
    pub signature: Signature,
    pub body: Option<Block>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Signature {
    pub params: FieldList,
    pub results: Option<Results>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Results {
    Params(FieldList),
    Type(Type),
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeParamList {
    pub params: Vec<TypeParam>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeParam {
    pub names: Vec<Ident>,
    pub constraint: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldList {
    pub fields: Vec<Field>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    pub names: Vec<Ident>,
    pub typ: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Decl(Decl),
    Empty,
    Labeled {
        label: Ident,
        stmt: Box<Stmt>,
    },
    Expr(Expr),
    Send {
        chan: Expr,
        value: Expr,
    },
    IncDec {
        expr: Expr,
        op: IncDecOp,
    },
    Assign {
        lhs: Vec<Expr>,
        op: AssignOp,
        rhs: Vec<Expr>,
    },
    ShortVarDecl {
        names: Vec<Ident>,
        values: Vec<Expr>,
    },
    Go(Expr),
    Defer(Expr),
    Return(Vec<Expr>),
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

// Este enum es necesario para la regla SimpleStmt del parser
#[derive(Debug, Clone, PartialEq)]
pub enum StmtSuffix {
    Assign(AssignOp, Vec<Expr>),
    Define(Vec<Expr>),
    Send(Expr),
    Inc,
    Dec,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IncDecOp {
    Inc,
    Dec,
}

#[derive(Debug, Clone, PartialEq, Eq)]
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
    pub init: Option<Box<Stmt>>,
    pub cond: Expr,
    pub then_block: Block,
    pub else_stmt: Option<Box<Stmt>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ForStmt {
    pub kind: ForKind,
    pub block: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ForKind {
    Infinite,
    Cond(Expr),
    ForClause {
        init: Option<Box<Stmt>>,
        cond: Option<Expr>,
        post: Option<Box<Stmt>>,
    },
    Range {
        lhs: Option<RangeLhs>,
        expr: Expr,
        define: bool,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct RangeLhs {
    pub exprs: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SwitchStmt {
    pub init: Option<Box<Stmt>>,
    pub tag: Option<Expr>,
    pub clauses: Vec<SwitchClause>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum SwitchClause {
    Expr {
        exprs: Vec<Expr>,
        stmts: Vec<Stmt>,
    },
    Default {
        stmts: Vec<Stmt>,
    },
    Type {
        types: Vec<Type>,
        bind: Option<Ident>,
        stmts: Vec<Stmt>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct SelectStmt {
    pub clauses: Vec<CommClause>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum CommClause {
    Comm { comm: CommStmt, stmts: Vec<Stmt> },
    Default { stmts: Vec<Stmt> },
}

#[derive(Debug, Clone, PartialEq)]
pub enum CommStmt {
    Send {
        chan: Expr,
        value: Expr,
    },
    Recv {
        lhs: Option<Vec<Expr>>,
        define: bool,
        recv: Expr,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Ident(Ident),
    BasicLit(BasicLit),
    CompositeLit {
        typ: Type,
        elements: Vec<Element>,
    },
    FuncLit {
        signature: Signature,
        body: Block,
    },
    Paren(Box<Expr>),
    Selector {
        expr: Box<Expr>,
        ident: Ident, // Renombrado de 'sel' a 'ident' para coincidir con la gramática
    },
    Index {
        expr: Box<Expr>,
        index: Box<Expr>,
    },
    Slice {
        expr: Box<Expr>,
        lo: Option<Box<Expr>>,
        hi: Option<Box<Expr>>,
        max: Option<Box<Expr>>,
    },
    TypeAssert {
        expr: Box<Expr>,
        typ: Type,
    },
    Call {
        fun: Box<Expr>, // En la gramática usamos `func`, pero `fun` evita keyword de Rust
        args: Vec<Expr>,
        variadic: bool,
    },
    Unary {
        op: UnaryOp,
        expr: Box<Expr>,
    },
    Binary {
        left: Box<Expr>,
        op: BinaryOp,
        right: Box<Expr>,
    },
    Star(Box<Expr>),
    Starred {
        // Usado para "..." en argumentos o arrays
        value: Box<Expr>,
        ctx: ExprContext,
    },
    Receive(Box<Expr>),

    // Representa un tipo envuelto como expresión (usado en literales compuestos anónimos)
    TypeWrapper(Type),

    // Para recuperación de errores
    Bad,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprContext {
    Load,
    Store,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Element {
    Expr(Expr),
    KeyValue { key: Expr, value: Expr },
}

#[derive(Debug, Clone, PartialEq)]
pub struct BasicLit {
    pub kind: BasicLitKind,
    pub raw: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BasicLitKind {
    Int,
    Float,
    Imag,
    Rune,
    String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryOp {
    Add,
    Sub,
    Not,
    Xor,
    Deref,
    Addr,
}

#[derive(Debug, Clone, PartialEq, Eq)]
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

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Named(NamedType),
    Pointer(Box<Type>),
    Array {
        len: Option<Box<Expr>>,
        elem: Box<Type>,
    },
    Slice(Box<Type>),
    Map {
        key: Box<Type>,
        value: Box<Type>,
    },
    Chan {
        dir: ChanDir,
        elem: Box<Type>,
    },
    Struct {
        fields: Vec<Field>,
    },
    Interface {
        methods: Vec<InterfaceElem>,
    },
    Func {
        signature: Signature,
    },
    Union(Vec<Type>),
    Tilde(Box<Type>),
    Paren(Box<Type>),

    // CRUCIAL: Permite que el parser guarde una expresión temporalmente
    // cuando está parseando `MiStruct{...}` antes de saber si es un tipo.
    ExprFallback(Box<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct NamedType {
    pub pkg: Option<Ident>,
    pub name: Ident,
    pub type_args: Option<Vec<Type>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ChanDir {
    Both,
    Send,
    Recv,
}

#[derive(Debug, Clone, PartialEq)]
pub enum InterfaceElem {
    Method {
        name: Ident,
        type_params: Option<TypeParamList>,
        sig: Signature,
    },
    Embed(Type),
}

#[derive(Debug, Clone, PartialEq)]
pub struct StringLit {
    pub raw: String,
}

impl StringLit {
    pub fn new(raw: String) -> Self {
        Self { raw }
    }
}

pub type ExprList = SmallVec<[Expr; 4]>;
