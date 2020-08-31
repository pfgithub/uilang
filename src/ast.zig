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
    variable: struct {
        name: []const u8,
    },
    parens: struct {},
});
