pub const Ast = struct {
    pub const SourcePos = struct { file: u64, line: u64, char: u64 };
    sorucepos: SourcePos,
    value: union(enum) {},
};
