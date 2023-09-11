const std = @import("std");
const parser = @import("parser.zig");
const Parser = parser.Parser;
const Command = parser.Command;
const CodeGen = @import("codegen.zig").CodeGen;

pub fn main() !void {
    //read file into parser
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    var allocator = arena.allocator();
    var args = try std.process.argsWithAllocator(allocator);
    // defer args.deinit();
    if (args.inner.count < 2) {
        std.debug.print("usage: ./main.zig <file>|<dir>\n", .{});
        return;
    }
    // get this filename as the first arg
    _ = args.next();
    // filename should be in second arg
    const fileOrDir = args.next().?;

    var dir = std.fs.cwd().openIterableDir(fileOrDir, .{}) catch null;
    defer if (dir != null) dir.?.close();
    var codegen = CodeGen.init(allocator);
    // defer codegen.deinit();
    if (dir == null) {
        try codegen.setFilename(fileOrDir);
        try codegen.generate();
    } else {
        var dirWalk = try dir.?.walk(allocator);
        // defer dirWalk.deinit();

        try codegen.writeInit();
        while (try dirWalk.next()) |file| {
            const filename = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ fileOrDir, file.basename });
            const isVM = std.mem.indexOf(u8, filename, ".vm") != null;
            if (isVM) {
                try codegen.setFilename(filename);
                try codegen.generate();
            }
            // allocator.free(filename);
        }
    }

    std.debug.print("{s}", .{codegen.fileContents.items});
    const writeFilename = filename_if: {
        if (dir == null) {
            break :filename_if try std.fmt.allocPrint(allocator, "{s}asm", .{fileOrDir[0 .. fileOrDir.len - 2]});
        } else {
            const slashIndex = std.mem.lastIndexOf(u8, fileOrDir, "/");
            const asmFilename = fileOrDir[slashIndex.? + 1 .. fileOrDir.len];
            std.debug.print("asmFilename: {s}\n", .{asmFilename});
            break :filename_if try std.fmt.allocPrint(allocator, "{s}/{s}.asm", .{ fileOrDir, asmFilename });
        }
    };
    // defer allocator.free(writeFilename);
    try codegen.write(writeFilename);
}
