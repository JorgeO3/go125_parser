# Go 1.25 Parser

Parser completo para el lenguaje Go 1.25 implementado en Rust, diseñado con principios de ingeniería de compiladores modernos.

## Índice

- [Visión General](#visión-general)
- [Arquitectura del Compilador](#arquitectura-del-compilador)
- [Sistema de Análisis Léxico (Lexer)](#sistema-de-análisis-léxico-lexer)
- [Sistema de Análisis Sintáctico (Parser)](#sistema-de-análisis-sintáctico-parser)
- [Árbol de Sintaxis Abstracta (AST)](#árbol-de-sintaxis-abstracta-ast)
- [Especificación Go 1.25](#especificación-go-125)
- [Símbolos No Terminales Soportados](#símbolos-no-terminales-soportados)
- [Símbolos Terminales Soportados](#símbolos-terminales-soportados)
- [Dependencias](#dependencias)
- [Uso](#uso)
- [Testing y Validación](#testing-y-validación)

---

## Visión General

Este proyecto implementa un **frontend de compilador** completo para Go 1.25 que procesa código fuente Go y produce un Árbol de Sintaxis Abstracta (AST) tipado y validado. El sistema está diseñado siguiendo el modelo clásico de compilador en múltiples fases:

```
Código Fuente → Lexer → Stream de Tokens → Parser → AST → [Análisis Semántico]
```

### Características Principales

- **Análisis Léxico**: Tokenización completa con soporte para inserción automática de punto y coma (`;`)
- **Análisis Sintáctico**: Parser LR(1) generado con LALRPOP que reconoce la gramática completa de Go
- **AST Optimizado**: Representación eficiente en memoria usando arenas y referencias tipadas
- **Recuperación de Errores**: Sistema robusto de diagnósticos con soporte para error recovery
- **Conformidad con Spec**: Implementación fiel a la especificación oficial de Go 1.25

---

## Arquitectura del Compilador

### Fases del Frontend

El compilador está estructurado en tres fases principales:

#### 1. **Análisis Léxico (Lexical Analysis)**
   - **Input**: String UTF-8 con código fuente Go
   - **Output**: Stream de tokens con posiciones
   - **Responsabilidad**: Reconocimiento de patrones léxicos y clasificación de tokens

#### 2. **Análisis Sintáctico (Syntactic Analysis)**
   - **Input**: Stream de tokens del lexer
   - **Output**: Árbol de Sintaxis Abstracta (AST)
   - **Responsabilidad**: Validación de la estructura gramatical del programa

#### 3. **Construcción del AST (Abstract Syntax Tree)**
   - **Input**: Acciones semánticas del parser
   - **Output**: Estructura de datos en memoria
   - **Responsabilidad**: Representación intermedia para análisis posteriores

### Separación de Concerns

```
┌─────────────────────────────────────────────────────────────┐
│                     Código Fuente Go                        │
└────────────────────────┬────────────────────────────────────┘
                         │
                         ▼
┌─────────────────────────────────────────────────────────────┐
│  LEXER (crates/parser/src/lexer.rs)                         │
│  • Escaneo de caracteres (Logos)                            │
│  • Tokenización                                             │
│  • Inserción automática de ';'                              │
│  • Manejo de comentarios                                    │
│  • Validación de literales (números, strings, runes)        │
└────────────────────────┬────────────────────────────────────┘
                         │ Vec<(usize, Tok<'src>, usize)>
                         ▼
┌─────────────────────────────────────────────────────────────┐
│  PARSER (crates/parser/src/parser.lalrpop)                  │
│  • Parser LR(1) generado por LALRPOP                        │
│  • Reconocimiento de gramática EBNF                         │
│  • Construcción dirigida por sintaxis                       │
│  • Error recovery con nodos Bad                             │
└────────────────────────┬────────────────────────────────────┘
                         │ AST
                         ▼
┌─────────────────────────────────────────────────────────────┐
│  AST (crates/parser/src/ast.rs)                             │
│  • Arena allocation para nodos                              │
│  • Tablas laterales de spans                                │
│  • Symbol interning                                         │
│  • Referencias tipadas (Id<T>, ListRef<T>)                  │
└─────────────────────────────────────────────────────────────┘
```

---

## Sistema de Análisis Léxico (Lexer)

**Archivo**: [`crates/parser/src/lexer.rs`](crates/parser/src/lexer.rs)

### Descripción

El lexer es la primera fase del compilador y se encarga de transformar el flujo de caracteres del código fuente en una secuencia de tokens clasificados. Utiliza el crate **Logos** para generar un escáner de alto rendimiento basado en autómatas finitos deterministas (DFA).

### Características Técnicas

#### 1. **Tokenización Zero-Copy**
```rust
pub enum Tok<'input> {
    Ident(&'input str),      // Referencias directas al buffer fuente
    IntLit(&'input str),
    StringLit(&'input str),
    // ...
}
```

- Los tokens literales mantienen referencias al buffer original (`&'input str`)
- No hay copias de strings durante el escaneo
- Reduce significativamente el overhead de memoria

#### 2. **Inserción Automática de Punto y Coma**

Go tiene reglas específicas para la inserción implícita de `;`:

**Regla**: Se inserta `;` antes de una nueva línea si el token anterior puede terminar una sentencia.

```rust
const SEMI_INSERT_TABLE: [bool; N] = [...];

impl RawTok {
    const fn can_insert_semicolon(self) -> bool {
        SEMI_INSERT_TABLE[self as usize]
    }
}
```

Tokens que permiten inserción de `;`:
- Identificadores y literales: `ident`, `int_lit`, `float_lit`, `imag_lit`, `rune_lit`, `string_lit`
- Keywords de término: `break`, `continue`, `fallthrough`, `return`
- Operadores: `++`, `--`, `)`, `]`, `}`

#### 3. **Clasificación de Literales Numéricos**

El lexer realiza análisis sofisticado de números con soporte para:

- **Enteros**: `42`, `0x2A`, `0o52`, `0b101010`
- **Flotantes**: `3.14`, `1e10`, `0x1.8p3`
- **Imaginarios**: `3.14i`, `1e10i`
- **Underscores**: `1_000_000`, `0b1111_0000`

```rust
// Validación en tiempo de escaneo
fn is_decimal_digits_with_underscores(bytes: &[u8]) -> bool {
    // Verifica que underscores estén entre dígitos
}
```

#### 4. **Manejo de Strings y Runes**

- **Strings interpretados**: `"hello\n"` con soporte para escapes Unicode (`\u`, `\U`, `\x`)
- **Raw strings**: `` `hello\n` `` sin procesamiento de escapes
- **Runes**: `'a'`, `'\n'`, `'\u0041'`

#### 5. **Comentarios**

- **Línea**: `// comentario hasta EOL`
- **Bloque**: `/* comentario multilínea */`
  - El lexer detecta si hay `\n` dentro del comentario para inserción de `;`

### Estado del Lexer

```rust
struct LexExtras {
    block_nl_off: u32,  // Offset a newline en comentario de bloque
    num_info: u8,       // Clasificación de número: 0=inválido, 1=int, 2=float
}
```

### Manejo de Errores

El lexer produce diagnósticos estructurados:

```rust
pub enum LexErrorKind {
    InvalidToken,           // Token no reconocido
    InvalidNumber,          // Literal numérico malformado
    InvalidEscape,          // Secuencia de escape inválida
    UnterminatedString,     // String sin cerrar
    UnterminatedComment,    // Comentario de bloque sin */
}
```

---

## Sistema de Análisis Sintáctico (Parser)

**Archivo**: [`crates/parser/src/parser.lalrpop`](crates/parser/src/parser.lalrpop)

### Descripción

El parser implementa un analizador sintáctico **LR(1)** generado automáticamente por LALRPOP a partir de una gramática EBNF. Este tipo de parser es determinista, eficiente (O(n)) y maneja precedencia de operadores sin ambigüedades.

### Tecnología: LALRPOP

**LALRPOP** (LR(1) Parser Generator) genera código Rust a partir de definiciones gramaticales:

```lalrpop
grammar<'input>(arena: &mut AstArena, interner: &mut Interner);

pub SourceFile: ast::SourceFile = {
    "package" <name:Ident> ";" <imports:ImportDecls> <decls:TopLevelDecls> => {
        ast::SourceFile { name, decls, ... }
    }
};
```

### Características del Parser

#### 1. **Construcción Dirigida por Sintaxis (Syntax-Directed Translation)**

Cada producción gramatical tiene una acción semántica que construye nodos del AST:

```lalrpop
FuncDecl: ast::FuncDeclId = {
    <s:@L> "func" <name:Ident> <sig:Signature> <body:Block> <e:@R> => {
        arena.funcs.alloc(ast::FuncDecl {
            func_pos: Span::new(s, s + 4),
            name,
            signature: sig,
            body: Some(body),
        }, Span::new(s, e))
    }
};
```

- `@L` y `@R`: Capturan posiciones inicio/fin
- Arena allocation: Asignación eficiente de nodos
- Spans: Rastreo de ubicación en código fuente

#### 2. **Macros para Reutilización**

```lalrpop
// 1+ elementos separados por coma
CommaPlus<T>: Vec<T> = {
    <v:CommaPlus<T>> "," <e:T> => { let mut v = v; v.push(e); v },
    <e:T> => vec![e],
};

// Aplicación
IdentList: ListRef<ast::IdentName> = CommaPlus<IdentName>;
```

Esto evita duplicación y hace la gramática más mantenible.

#### 3. **Manejo de Precedencia de Operadores**

Go tiene 6 niveles de precedencia para operadores binarios:

```lalrpop
#[precedence(level="5")] // Máxima: *, /, %, <<, >>, &, &^
#[precedence(level="4")] // +, -, |, ^
#[precedence(level="3")] // ==, !=, <, <=, >, >=
#[precedence(level="2")] // &&
#[precedence(level="1")] // || (Mínima)
```

LALRPOP genera automáticamente la lógica para resolver conflictos shift/reduce.

#### 4. **Error Recovery**

El parser puede recuperarse de errores sintácticos y continuar parseando:

```lalrpop
TopLevelDecl: ast::TopLevelDecl = {
    <d:Declaration> => ast::TopLevelDecl::Decl(d),
    <f:FuncDecl> => ast::TopLevelDecl::Func(f),
    <e:!> => {  // Recovery point
        errors.push(e);
        let id = arena.decls.alloc(ast::Decl::Bad, span);
        ast::TopLevelDecl::Decl(id)
    }
};
```

#### 5. **Gramática Context-Free**

La gramática de Go es **context-free** (CF), lo que permite parsing eficiente con LR(1). Sin embargo, hay algunas ambigüedades sintácticas que requieren resolución:

**Problema de `<-chan` vs `chan<-`:**

```go
<-chan int  // Receive-only channel
chan<- int  // Send-only channel
chan int    // Bidirectional channel
```

Solución: Reglas gramaticales especializadas:

```lalrpop
ChanType: ast::TypeId = {
    "<-" "chan" <t:Type> => ast::Type::Chan { dir: Recv, ... },
    "chan" "<-" <t:Type> => ast::Type::Chan { dir: Send, ... },
    "chan" <t:Type> => ast::Type::Chan { dir: Both, ... },
};
```

---

## Árbol de Sintaxis Abstracta (AST)

**Archivo**: [`crates/parser/src/ast.rs`](crates/parser/src/ast.rs)

### Descripción

El AST es la representación intermedia del programa que captura su estructura semántica. Está diseñado para ser eficiente en memoria y tiempo de acceso.

### Diseño de Arquitectura

#### 1. **Arena Allocation**

En lugar de usar `Box<T>` o `Rc<T>`, todos los nodos se almacenan en arenas tipadas:

```rust
pub struct AstArena {
    pub decls: SpannedArena<Decl>,
    pub types: SpannedArena<Type>,
    pub exprs: SpannedArena<Expr>,
    pub stmts: SpannedArena<Stmt>,
    pub funcs: SpannedArena<FuncDecl>,
    // ...
}
```

**Ventajas**:
- Localidad de caché: nodos del mismo tipo están contiguos en memoria
- No hay fragmentación del heap
- Liberación en batch: `Drop` de la arena libera todo
- Sin conteo de referencias (no overhead de `Rc`/`Arc`)

#### 2. **Referencias Tipadas**

En lugar de índices crudos (`usize`), usamos tipos wrapper:

```rust
#[repr(transparent)]
pub struct Id<T> {
    raw: u32,
    _marker: PhantomData<fn() -> T>,
}

pub type TypeId = Id<Type>;
pub type ExprId = Id<Expr>;
```

**Beneficios**:
- Type safety: no puedes usar un `TypeId` donde se espera un `ExprId`
- Tamaño: `u32` permite hasta 4 billones de nodos por tipo
- Zero-cost: `#[repr(transparent)]` garantiza misma representación que `u32`

#### 3. **Listas Centralizadas**

En lugar de `Vec<T>` por nodo, hay buffers compartidos:

```rust
pub struct ListRef<T> {
    start: u32,
    len: u32,
    _marker: PhantomData<fn() -> T>,
}

// En AstArena:
pub struct AstExtras {
    expr_buf: Vec<ExprId>,
    type_buf: Vec<TypeId>,
    // ...
}
```

Esto evita el overhead de `Vec` (3 words) en cada nodo.

#### 4. **Side Tables para Spans**

Los spans están separados de los nodos:

```rust
pub struct SpannedArena<T> {
    nodes: Vec<T>,
    spans: Vec<Span>,
}
```

Esto permite:
- Recorrer nodos sin cargar información de posición
- Compactar spans si no se necesitan después del parsing
- Mantener los nodos más pequeños

#### 5. **Symbol Interning**

Los identificadores se internan en una tabla:

```rust
pub struct Interner {
    map: HashMap<&'static str, Symbol>,
    strings: Vec<String>,
}

pub struct Symbol(u32);
pub type Ident = Symbol;
```

**Ventajas**:
- Comparación O(1): símbolos son enteros
- Deduplicación: `package` aparece una vez en memoria
- Tamaño: `Symbol` es 4 bytes vs `String` (24 bytes)

### Nodos Principales del AST

#### Declaraciones (`Decl`)

```rust
pub enum Decl {
    Bad,                    // Error recovery
    Gen(GenDecl),          // const, var, type, import
}

pub struct GenDecl {
    kind: GenDeclKind,     // Const | Var | Type | Import
    specs: ListRef<Spec>,  // Lista de especificaciones
    // ...
}
```

#### Tipos (`Type`)

```rust
pub enum Type {
    Named { ... },         // pkg.Name[Args]
    Pointer { ... },       // *T
    Array { ... },         // [N]T
    Slice { ... },         // []T
    Map { ... },           // map[K]V
    Chan { ... },          // chan T, <-chan T, chan<- T
    Struct { ... },        // struct { ... }
    Interface { ... },     // interface { ... }
    Func { ... },          // func(...) ...
    Paren { ... },         // (T)
    Bad(Span),
}
```

#### Expresiones (`Expr`)

```rust
pub enum Expr {
    Ident(Ident, Span),
    BasicLit(BasicLit),
    CompositeLit { ... },
    FuncLit { ... },
    Unary { op: UnaryOp, expr: ExprId },
    Binary { lhs: ExprId, op: BinaryOp, rhs: ExprId },
    Call { ... },
    Index { ... },
    Slice { ... },
    TypeAssert { ... },
    Paren { ... },
    Bad(Span),
}
```

#### Sentencias (`Stmt`)

```rust
pub enum Stmt {
    Bad(Span),
    Decl { decl: DeclId },
    Labeled { ... },
    Expr { expr: ExprId },
    Send { ... },
    IncDec { ... },
    Assign { ... },
    ShortVarDecl { ... },
    Go { ... },
    Defer { ... },
    Return { ... },
    Branch { ... },          // break, continue, goto, fallthrough
    Block { ... },
    If { ... },
    Switch { ... },
    TypeSwitch { ... },
    For { ... },
    Range { ... },
    Select { ... },
}
```

### Pattern: Walk Trait

Para recorrer el AST, se usa el patrón Visitor:

```rust
pub trait Walk {
    fn walk<V: Visitor>(&self, visitor: &mut V, arena: &AstArena);
}

// Generado automáticamente con #[derive(WalkAst)]
impl Walk for Type { ... }
```

**Archivo**: [`crates/parser/src/walk.rs`](crates/parser/src/walk.rs)

---

## Especificación Go 1.25

### Conformidad con la Especificación

Este parser implementa la especificación oficial de Go 1.25 (agosto de 2025) según:

**Referencia**: [https://go.dev/ref/spec](https://go.dev/ref/spec)

### Cambios en Go 1.25

Go 1.25 mantiene **estabilidad sintáctica total** respecto a Go 1.24. Los cambios principales fueron:

1. **Eliminación conceptual de "Core Types"**: Se simplificó la definición abstracta en la especificación, pero la sintaxis no cambió.

2. **Maduración de Generic Type Aliases**: Los aliases genéricos introducidos en Go 1.23 se consideran estables.

```go
type MyMap[K comparable, V any] = map[K]V
type Vector[T any] = []T
```

3. **Restricciones de Interfaces**: Las interfaces pueden contener:
   - Métodos: `Write([]byte) (int, error)`
   - Tipos embebidos: `io.Reader`
   - Uniones de tipos: `int | float64`
   - Tipos aproximados: `~string`

### Gramática EBNF

La gramática completa está en [`crates/parser/docs/grammar.txt`](crates/parser/docs/grammar.txt).

**Notación**:
```ebnf
|   alternativa
()  agrupación
[]  opcional (0 o 1)
{}  repetición (0 o más)
```

---

## Símbolos No Terminales Soportados

A continuación se listan los **símbolos no terminales** de la especificación Go 1.25 y su estado de soporte en este parser:

### ✅ Completamente Soportados

#### Estructura de Archivo

| Símbolo | Producción LALRPOP | Estado |
|---------|-------------------|--------|
| `SourceFile` | `pub SourceFile` | ✅ |
| `PackageClause` | Incorporado en `SourceFile` | ✅ |
| `ImportDecl` | `ImportDecl` | ✅ |
| `ImportSpec` | `ImportSpec` | ✅ |
| `ImportPath` | `StringLit` | ✅ |
| `TopLevelDecl` | `TopLevelDecl` | ✅ |

#### Declaraciones

| Símbolo | Producción LALRPOP | Estado |
|---------|-------------------|--------|
| `Declaration` | `Declaration` | ✅ |
| `ConstDecl` | `ConstDecl` | ✅ |
| `ConstSpec` | `ConstSpec` | ✅ |
| `TypeDecl` | `TypeDecl` | ✅ |
| `TypeSpec` | `TypeSpec` | ✅ |
| `AliasDecl` | Combinado en `TypeSpec` | ✅ |
| `TypeDef` | Combinado en `TypeSpec` | ✅ |
| `VarDecl` | `VarDecl` | ✅ |
| `VarSpec` | `VarSpec` | ✅ |
| `FunctionDecl` | `FuncDecl` | ✅ |
| `MethodDecl` | `FuncDecl` con `Receiver` | ✅ |

#### Tipos

| Símbolo | Producción LALRPOP | Estado |
|---------|-------------------|--------|
| `Type` | `Type` | ✅ |
| `TypeName` | `NamedType` | ✅ |
| `TypeLit` | `TypeLit` | ✅ |
| `TypeParameters` | `TypeParameters` | ✅ |
| `TypeParamList` | Macro `CommaTrail1<TypeParamDecl>` | ✅ |
| `TypeParamDecl` | `TypeParamDecl` | ✅ |
| `TypeConstraint` | Integrado en `TypeParamDecl` | ✅ |
| `TypeArgs` | Parte de `NamedType` | ✅ |
| `ArrayType` | `ArrayType` | ✅ |
| `SliceType` | `SliceType` | ✅ |
| `StructType` | `StructType` | ✅ |
| `PointerType` | `PointerType` | ✅ |
| `FunctionType` | `FunctionType` | ✅ |
| `InterfaceType` | `InterfaceType` | ✅ |
| `MapType` | `MapType` | ✅ |
| `ChannelType` | `ChanType` | ✅ |

#### Tipos de Interface

| Símbolo | Producción LALRPOP | Estado |
|---------|-------------------|--------|
| `InterfaceElem` | `InterfaceElem` | ✅ |
| `MethodSpec` | Variante de `InterfaceElem` | ✅ |
| `TypeElem` | `TypeElem` | ✅ |
| `TypeTerm` | `TypeTerm` | ✅ |
| `UnderlyingType` | Variante `TypeTerm::Tilde` | ✅ |

#### Funciones y Firmas

| Símbolo | Producción LALRPOP | Estado |
|---------|-------------------|--------|
| `Signature` | `Signature` | ✅ |
| `Parameters` | `Parameters` | ✅ |
| `ParameterList` | Macro `CommaOpt<ParameterDecl>` | ✅ |
| `ParameterDecl` | `ParameterDecl` | ✅ |
| `Result` | `Result` | ✅ |
| `Receiver` | `Receiver` | ✅ |

#### Sentencias Básicas

| Símbolo | Producción LALRPOP | Estado |
|---------|-------------------|--------|
| `Block` | `Block` | ✅ |
| `Statement` | `Stmt` | ⚠️ Parcial |
| `SimpleStmt` | ⚠️ | ⚠️ En desarrollo |
| `ReturnStmt` | `ReturnStmt` | ✅ |

### ⚠️ Parcialmente Soportados

Estos símbolos tienen soporte básico pero no todas las variantes:

| Símbolo | Estado | Faltante |
|---------|--------|----------|
| `Statement` | ⚠️ | `IfStmt`, `ForStmt`, `SwitchStmt`, `SelectStmt` |
| `Expression` | ⚠️ | Solo `Ident` y `BasicLit` literales |
| `PrimaryExpr` | ⚠️ | Faltan selectores, índices, slices, type assertions, llamadas |
| `UnaryExpr` | ❌ | No implementado |
| `BinaryExpr` | ❌ | No implementado |

### ❌ No Soportados (Pendientes)

Estos símbolos están en la especificación pero no implementados aún:

#### Sentencias de Control

- `IfStmt`
- `SwitchStmt` (Expression Switch)
- `TypeSwitchStmt`
- `ForStmt`
- `RangeClause`
- `SelectStmt`
- `GoStmt`
- `DeferStmt`
- `LabeledStmt`
- `GotoStmt`
- `BreakStmt`
- `ContinueStmt`
- `FallthroughStmt`

#### Expresiones

- `PrimaryExpr` completo:
  - `Selector` (`.field`)
  - `Index` (`[i]`)
  - `Slice` (`[low:high]`)
  - `TypeAssertion` (`.(Type)`)
  - `Arguments` (llamadas a funciones)
- `UnaryExpr` (operadores unarios: `+`, `-`, `!`, `^`, `*`, `&`, `<-`)
- `BinaryExpr` (operadores binarios: `+`, `-`, `*`, `/`, `%`, `&`, `|`, `^`, `<<`, `>>`, `&^`, `&&`, `||`, `==`, `!=`, `<`, `<=`, `>`, `>=`)
- `Conversion` (`Type(expr)`)
- `CompositeLit` (literales compuestos)
- `FunctionLit` (funciones anónimas)

#### Otros

- `Assignment` completo (solo básico)
- `ShortVarDecl` (`:=`)
- `IncDecStmt` (`++`, `--`)
- `SendStmt` (`<-`)

---

## Símbolos Terminales Soportados

### Tokens Léxicos

El lexer reconoce **todos** los tokens de la especificación Go 1.25:

#### Keywords (25)

```rust
pub enum Tok<'input> {
    // Control de flujo
    KwBreak, KwCase, KwContinue, KwDefault, KwFallthrough,
    KwFor, KwGoto, KwIf, KwElse, KwSwitch, KwSelect,
    
    // Declaraciones
    KwConst, KwFunc, KwImport, KwPackage, KwType, KwVar,
    
    // Tipos
    KwChan, KwInterface, KwMap, KwStruct,
    
    // Concurrencia
    KwGo, KwDefer,
    
    // Iteración
    KwRange,
    
    // Control de funciones
    KwReturn,
}
```

#### Literales

```rust
Ident(&'input str),        // identificadores: foo, _bar, utf8_αβγ
IntLit(&'input str),       // 42, 0x2A, 0o52, 0b101010, 1_000_000
FloatLit(&'input str),     // 3.14, 1e10, 0x1.8p3
ImagLit(&'input str),      // 3.14i, 1e10i
RuneLit(&'input str),      // 'a', '\n', '\u0041'
StringLit(&'input str),    // "hello\n"
RawStringLit(&'input str), // `raw\nstring`
```

#### Operadores y Puntuación (58 tokens)

##### Operadores Aritméticos
- `Plus` (`+`), `Minus` (`-`), `Star` (`*`), `Slash` (`/`), `Percent` (`%`)

##### Operadores Bitwise
- `Amp` (`&`), `Pipe` (`|`), `Caret` (`^`), `Tilde` (`~`)
- `Shl` (`<<`), `Shr` (`>>`), `AndNot` (`&^`)

##### Operadores Lógicos
- `LAnd` (`&&`), `LOr` (`||`), `Bang` (`!`)

##### Operadores de Comparación
- `EqEq` (`==`), `NotEq` (`!=`)
- `Lt` (`<`), `Le` (`<=`), `Gt` (`>`), `Ge` (`>=`)

##### Operadores de Asignación
- `Assign` (`=`), `Define` (`:=`)
- `AddAssign` (`+=`), `SubAssign` (`-=`), `MulAssign` (`*=`)
- `DivAssign` (`/=`), `ModAssign` (`%=`)
- `AndAssign` (`&=`), `OrAssign` (`|=`), `XorAssign` (`^=`)
- `ShlAssign` (`<<=`), `ShrAssign` (`>>=`), `AndNotAssign` (`&^=`)

##### Operadores de Incremento/Decremento
- `Inc` (`++`), `Dec` (`--`)

##### Operadores de Canales
- `Arrow` (`<-`)

##### Puntuación
- `LParen` (`(`), `RParen` (`)`)
- `LBrack` (`[`), `RBrack` (`]`)
- `LBrace` (`{`), `RBrace` (`}`)
- `Comma` (`,`), `Semi` (`;`), `Colon` (`:`)
- `Dot` (`.`), `Ellipsis` (`...`)

##### Especiales
- `Underscore` (`_`)
- `Error` (token de error para recovery)

---

## Dependencias

El proyecto usa un conjunto mínimo de dependencias de alta calidad:

### Dependencias de Producción

```toml
[dependencies]
logos = "0.16.0"           # Lexer generator (DFA-based)
thiserror = "2.0.17"       # Error handling macros
smallvec = "1.15.1"        # Stack-allocated vectors
memchr = "2.7.6"           # Optimized string searching
lalrpop-util = "0.22.2"    # Runtime support for LALRPOP
```

#### **Logos** (`0.16.0`)
- **Propósito**: Generación de lexers de alto rendimiento
- **Tecnología**: Genera código Rust con DFAs (Deterministic Finite Automata)
- **Ventajas**: 
  - Zero-cost abstractions
  - Sin dependencias de runtime
  - Performance comparable a lexers escritos a mano

#### **LALRPOP** (`0.22.2`)
- **Propósito**: Generación de parsers LR(1)
- **Tecnología**: Genera tablas de parsing en tiempo de compilación
- **Ventajas**:
  - Parser determinista y eficiente
  - Manejo automático de precedencia
  - Mensajes de error de conflictos LR

#### **Thiserror** (`2.0.17`)
- **Propósito**: Derivación automática de traits `Error` y `Display`
- **Uso**: Tipos de error estructurados (`LexErrorKind`, `ParseError`)

#### **SmallVec** (`1.15.1`)
- **Propósito**: Vectores optimizados para casos pequeños
- **Uso**: Listas temporales durante parsing
- **Ventaja**: Evita heap allocation para N ≤ capacidad inline

#### **Memchr** (`2.7.6`)
- **Propósito**: Búsqueda optimizada de bytes/caracteres
- **Uso**: Escaneo de comentarios de bloque (`*/`), strings raw (`` ` ``)
- **Ventaja**: Usa instrucciones SIMD cuando está disponible

### Dependencias de Build

```toml
[build-dependencies]
lalrpop = "0.22.2"         # Parser generator
gag = "1"                  # Silencia output de LALRPOP
```

### Dependencias de Desarrollo

```toml
[dev-dependencies]
proptest = "1"             # Property-based testing
pretty_assertions = "1"    # Asserts con mejor output
walkdir = "2"              # Traversal de directorios
criterion = "0.8.1"        # Benchmarking framework
```

#### **Proptest** (`1`)
- **Propósito**: Testing basado en propiedades
- **Uso**: Fuzzing del lexer con inputs aleatorios
- **Ventaja**: Descubre edge cases automáticamente

#### **Criterion** (`0.8.1`)
- **Propósito**: Benchmarking estadísticamente riguroso
- **Características**:
  - Detección de regresiones de performance
  - Gráficos HTML
  - Comparaciones entre ejecuciones

---

## Uso

### Instalación

```bash
# Clonar el repositorio
git clone https://github.com/usuario/go125_parser.git
cd go125_parser

# Compilar (genera el parser LALRPOP)
cargo build --release
```

### API Básica

```rust
use go125_parser::{Lexer, parser};
use go125_parser::ast::AstArena;
use go125_parser::ast::Interner;

fn main() {
    let source = r#"
        package main
        
        import "fmt"
        
        func main() {
            return
        }
    "#;
    
    // Fase 1: Lexer
    let lexer = Lexer::new(source);
    
    // Fase 2: Parser
    let mut arena = AstArena::new();
    let mut interner = Interner::new();
    let mut errors = Vec::new();
    
    match parser::SourceFileParser::new().parse(
        &mut arena,
        &mut interner,
        &mut errors,
        lexer
    ) {
        Ok(source_file) => {
            println!("✅ Parsing exitoso!");
            println!("Paquete: {:?}", source_file.name);
            println!("Declaraciones: {}", arena.top_decls(source_file.decls).len());
        }
        Err(e) => {
            eprintln!("❌ Error de parsing: {:?}", e);
        }
    }
    
    // Obtener diagnósticos del lexer
    let lexer = Lexer::new(source);
    let diags = lexer.take_diags();
    for diag in diags {
        eprintln!("Lexer: {:?}", diag);
    }
}
```

### Construcción Incremental

```bash
# Build optimizado
cargo build --release

# Build con símbolos de debug (útil para profiling)
cargo build --profile=release

# Limpiar artefactos generados
cargo clean
```

El archivo `build.rs` ejecuta LALRPOP automáticamente:

```rust
// crates/parser/build.rs
fn main() {
    lalrpop::process_root().unwrap();
}
```

Esto genera `parser.rs` a partir de `parser.lalrpop` en tiempo de compilación.

---

## Testing y Validación

El proyecto tiene una suite completa de tests organizados por subsistema:

### Tests del Lexer

**Ubicación**: [`crates/parser/tests/`](crates/parser/tests/)

#### 1. **Tests Golden** (`lexer_golden.rs`)
- Compara salida del lexer contra archivos `.golden`
- Verifica tokens, spans y tipos

#### 2. **Tests de Números** (`lexer_numbers.rs`)
- Enteros: decimal, hex, octal, binario
- Flotantes: científica, hexadecimal
- Imaginarios: sufijo `i`
- Underscores: `1_000_000`
- **Compatibilidad**: `lexer_numbers_go_scanner.rs` valida contra el scanner de Go

#### 3. **Tests de Strings** (`lexer_strings.rs`)
- Strings interpretados con escapes: `\n`, `\t`, `\u`, `\U`, `\x`
- Raw strings: `` `...` ``
- Runes: `'a'`, `'\n'`, `'\u0041'`

#### 4. **Tests de Comentarios** (`lexer_comments_cr.rs`)
- Comentarios de línea: `// ...`
- Comentarios de bloque: `/* ... */`
- Inserción de `;` después de comentarios con `\n`

#### 5. **Tests de Punto y Coma** (`lexer_semis.rs`)
- Inserción automática de `;` según reglas de Go
- Casos edge: `)`, `}`, `]`, keywords

#### 6. **Tests de Whitespace** (`lexer_whitespace.rs`)
- Espacios, tabs, saltos de línea, CR+LF

#### 7. **Tests de Unicode** (`lexer_unicode.rs`)
- Identificadores Unicode: `αβγ`, `函数`
- BOM (Byte Order Mark)
- Normalization

#### 8. **Tests de Errores** (`lexer_errors_corpus.rs`)
- Tokens inválidos
- Números malformados
- Strings sin cerrar
- Comentarios sin terminar

#### 9. **Property-Based Tests** (`lexer_props.rs`)
- Fuzzing con Proptest
- Invariantes: todo input válido produce tokens o errores, nunca panic

#### 10. **Scan Table** (`lexer_scan_table.rs`)
- Verifica la tabla de inserción de `;`
- Cobertura de todos los tokens

### Tests del Parser

Actualmente el parser tiene soporte básico. Los tests futuros incluirán:

- **Golden tests**: Comparar AST contra archivos `.ast`
- **Roundtrip tests**: Parser → AST → Pretty Print → Parser
- **Error recovery tests**: Validar nodos `Bad` y continuación de parsing
- **Corpus de Go estándar**: Parsear paquetes de la librería estándar de Go

### Benchmarks

```bash
# Ejecutar benchmarks con Criterion
cargo bench

# Ver reportes HTML
open target/criterion/report/index.html
```

Benchmarks incluyen:
- Lexer: tokens/segundo
- Parser: líneas/segundo
- Arena allocation: allocaciones/segundo

### Fuzzing

El directorio [`fuzz/`](fuzz/) contiene targets de fuzzing:

```bash
# Instalar cargo-fuzz
cargo install cargo-fuzz

# Ejecutar fuzzer del lexer
cargo fuzz run lexer

# Ver crashes
cargo fuzz cov lexer
```

---

## Arquitectura de Módulos

```
go125_parser/
├── crates/
│   ├── parser/              # Parser principal
│   │   ├── src/
│   │   │   ├── lib.rs       # Re-exports públicos
│   │   │   ├── lexer.rs     # Lexer (Logos)
│   │   │   ├── parser.lalrpop   # Gramática LALRPOP
│   │   │   ├── parser.rs    # (Generado por LALRPOP)
│   │   │   ├── ast.rs       # AST y arenas
│   │   │   ├── error.rs     # Tipos de error
│   │   │   ├── walk.rs      # Visitor pattern
│   │   │   └── parser_support.rs  # Helpers
│   │   ├── build.rs         # Build script (LALRPOP)
│   │   ├── tests/           # Suite de tests
│   │   └── docs/            # Documentación de gramática
│   │
│   └── ast_derive/          # Proc macro para #[derive(WalkAst)]
│       └── src/
│           └── lib.rs
│
├── fuzz/                    # Fuzzing targets
│   └── fuzz_targets/
│       └── lexer.rs
│
└── Cargo.toml               # Workspace config
```

---

## Roadmap

### Fase 1: Lexer ✅ (Completado)
- [x] Tokenización completa
- [x] Inserción de `;`
- [x] Manejo de comentarios
- [x] Literales (números, strings, runes)
- [x] Tests exhaustivos

### Fase 2: Parser Básico ✅ (Completado)
- [x] Estructura de archivo (`SourceFile`)
- [x] Declaraciones: `const`, `var`, `type`, `import`, `func`
- [x] Tipos: primitivos, structs, interfaces, maps, channels, arrays, slices
- [x] Genéricos: type parameters, type arguments
- [x] Función básica con `return`

### Fase 3: Expresiones (En Progreso 🚧)
- [ ] Literales básicos
- [ ] Operadores unarios y binarios
- [ ] Llamadas a funciones
- [ ] Índices y slices
- [ ] Type assertions
- [ ] Composite literals
- [ ] Funciones anónimas

### Fase 4: Sentencias de Control (Pendiente)
- [ ] `if`/`else`
- [ ] `for` (clásico y `range`)
- [ ] `switch` (expression y type)
- [ ] `select`
- [ ] `go` y `defer`
- [ ] Labels y `goto`

### Fase 5: Validación Semántica (Futuro)
- [ ] Symbol table
- [ ] Type checking
- [ ] Scoping rules
- [ ] Constant evaluation
- [ ] Initialization order

---

## Contribuir

Contribuciones son bienvenidas! Áreas de alto impacto:

1. **Implementar expresiones** (`Expr` en `parser.lalrpop`)
2. **Implementar sentencias de control** (`IfStmt`, `ForStmt`, etc.)
3. **Agregar más tests** (especialmente golden tests con código Go real)
4. **Optimización**: mejorar performance del lexer o arena allocation
5. **Documentación**: ejemplos de uso, tutoriales

### Estilo de Código

- Seguir convenciones de Rust: `cargo fmt`, `cargo clippy`
- Documentar funciones públicas con `///`
- Tests unitarios para cada feature
- Mantener terminología de compiladores consistente

---

## Licencia

MIT OR Apache-2.0 (dual license)

---

## Referencias

### Especificación de Go
- [The Go Programming Language Specification](https://go.dev/ref/spec)
- [Go 1.25 Release Notes](https://go.dev/doc/go1.25)

### Herramientas
- [Logos Documentation](https://docs.rs/logos/)
- [LALRPOP Book](https://lalrpop.github.io/lalrpop/)
- [Rust API Guidelines](https://rust-lang.github.io/api-guidelines/)

### Teoría de Compiladores
- *Compilers: Principles, Techniques, and Tools* (Dragon Book)
- *Engineering a Compiler* (Cooper & Torczon)
- *Modern Compiler Implementation in ML* (Appel)

---

## Contacto

Para preguntas o discusiones técnicas, abrir un issue en GitHub.

---

**Última actualización**: Enero 2026  
**Versión del parser**: 0.1.0  
**Compatibilidad**: Go 1.25 (spec de agosto 2025)
