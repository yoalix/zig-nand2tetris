const std = @import("std");
const parser = @import("parser.zig");
const Parser = parser.Parser;
const Command = parser.Command;
const CodeGen = @import("codegen.zig").CodeGen;

pub fn main() !void {
    //read file into parser
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    var allocator = arena.allocator();
    var args = try std.process.argsWithAllocator(allocator);
    defer args.deinit();
    if (args.inner.count < 2) {
        std.debug.print("usage: ./main.zig <file>\n", .{});
        return;
    }
    // get this filename as the first arg
    _ = args.next();
    // filename should be in second arg
    const file = args.next().?;
    var parsed = try Parser.parse(file, allocator);
    defer parsed.deinit();
    std.debug.print("parser: {any}\n", .{parsed.operations});
    //output parser vm commands to assembly code
    var codegen = CodeGen.init(file, allocator);
    defer codegen.deinit();
    try codegen.generate(&parsed);
    std.debug.print("{s}", .{codegen.fileContents.items});
    const writeFilename = try std.fmt.allocPrint(allocator, "{s}asm", .{file[0 .. file.len - 2]});
    try codegen.write(writeFilename);
}
