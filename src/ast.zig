fn AstNode(comptime Type: type) type {
    return struct {
        sorucepos: SourcePos,
        value: Type,
    };
}

pub const SourcePos = struct { file: u64, line: u64, char: u64 };

pub const Binding = AstNode(union(enum) {
    binding: struct {
        name: []const u8,
        value: *Expression,
    }
});

pub const Expression = AstNode(union(enum) {
    /// ('widget'|'fn') identifier '(' ')'
    fndecl: struct {
        kind: enum { widget, function },
        body: *Expression,
    },
    /// component variables:
    /// state   :: a variable that is stored with a component. when changed, anything depending on it updates.
    /// trigger :: todo figure out exactly what this means and why this is better than state in certain situations
    /// bind    :: a memoized function with no arguments (when any variables this depends on update, this will update too)
    /// standard variables:
    /// const   :: a variable that is used within standard function bodies
    /// var     :: a variable that is used within standard function bodies
    /// ('var'|'const'|'state'|'trigger') identifier ':' expression '=' expression
    vardecl: struct {
        kind: enum { state, trigger, constant, variable, binding },
        @"type": *Expression,
        value: *Expression,
    },
    /// 'return' expression
    returndecl: struct {
        value: *Expression,
    },
    /// '.' identifier '(' [',']expression ')'
    htmlexpression: struct {
        node: []const u8,
        body: []Expression,
        bindings: []Binding,
    },
    /// ':' identifier '=' expression
    attributeexpression: struct {
        attrname: []const u8,
        attrvalue: *Expression,
    },
    /// '|' '|' expression
    inlinefnexpression: struct {
        expr: *Expression,
    },
    /// a block expression can return a value with a return statement
    /// '{' [';']expression '}'
    blockexpression: struct {
        body: []Expression
    },
    /// a parens expression does not support control flow like 'return'. the result of the last expression is returned.
    /// '(' [',']expression ')'
    parensexpression: struct {
        body: []Expression,
    },
});
