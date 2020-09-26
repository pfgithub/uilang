const std = @import("std");
const Alloc = std.mem.Allocator;

pub fn range(max: usize) []const void {
    return @as([]const void, &[_]void{}).ptr[0..max];
}

pub fn allocDupe(alloc: *Alloc, a: anytype) !*@TypeOf(a) {
    const c = try alloc.create(@TypeOf(a));
    c.* = a;
    return c;
}

const ID = usize;

// in case this is ever made multithreaded, each thread will need a new starting point for this global_id (c pthread_self() * like std.math.maxInt(u50))
pub threadlocal var global_id: ID = 0;

pub fn getNewID() usize {
    defer global_id += 1;
    return global_id;
}
