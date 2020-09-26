pub const Type = struct {
    /// if this value is watchable
    watchable: bool,
    tkind: union(enum) {
        empty, // there is no value. like `never` in typescript.
        unit, // there is one value.
        t_type, // represents a type. only available at comptime.
        func, // any function. will include args and return type in the future.
        int: u8, // an integer, max i53
        uint: u8, // a uint, max u53 (higher requires some >>> mess)
        float, // a f64.
        ct_number, // a string number. must be casted into a runtime value to use.
        html, // a html element.
        attr, // a html attribute. maybe include more information in the future incl the type of attribute.
        string, // a string
    },
};
