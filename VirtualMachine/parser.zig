const std = @import("std");

pub const Command = enum { ADD, SUB, NEG, EQ, GT, LT, AND, OR, NOT, PUSH, POP, LABEL, GOTO, IF, FUNCTION, RETURN, CALL, ARITHMETIC, NOOP };

pub const Memory = enum {
    LOCAL,
    ARGUMENT,
    THIS,
    THAT,
    CONSTANT,
    STATIC,
    POINTER,
    TEMP,
};

const Arg1 = union(enum) {
    memory: Memory,
    label: []const u8,
};

pub const Operation = struct {
    command: Command,
    arg1: ?Arg1,
    arg2: ?u32,
};

pub fn instructionToCommand(instruction: []const u8) Command {
    if (std.mem.eql(u8, instruction, "add")) {
        return Command.ADD;
    } else if (std.mem.eql(u8, instruction, "sub")) {
        return Command.SUB;
    } else if (std.mem.eql(u8, instruction, "neg")) {
        return Command.NEG;
    } else if (std.mem.eql(u8, instruction, "eq")) {
        return Command.EQ;
    } else if (std.mem.eql(u8, instruction, "gt")) {
        return Command.GT;
    } else if (std.mem.eql(u8, instruction, "lt")) {
        return Command.LT;
    } else if (std.mem.eql(u8, instruction, "and")) {
        return Command.AND;
    } else if (std.mem.eql(u8, instruction, "or")) {
        return Command.OR;
    } else if (std.mem.eql(u8, instruction, "not")) {
        return Command.NOT;
    } else if (std.mem.eql(u8, instruction, "push")) {
        return Command.PUSH;
    } else if (std.mem.eql(u8, instruction, "pop")) {
        return Command.POP;
    } else if (std.mem.eql(u8, instruction, "label")) {
        return Command.LABEL;
    } else if (std.mem.eql(u8, instruction, "goto")) {
        return Command.GOTO;
    } else if (std.mem.eql(u8, instruction, "if-goto")) {
        return Command.IF;
    } else if (std.mem.eql(u8, instruction, "function")) {
        return Command.FUNCTION;
    } else if (std.mem.eql(u8, instruction, "return")) {
        return Command.RETURN;
    } else if (std.mem.eql(u8, instruction, "call")) {
        return Command.CALL;
    } else {
        return Command.NOOP;
    }
}

fn instructionToLabelMemory(instruction: []const u8) Arg1 {
    std.debug.print("instructionToLabelMemory: {s}\n", .{instruction});
    if (std.mem.eql(u8, instruction, "local")) {
        return Arg1{ .memory = Memory.LOCAL };
    } else if (std.mem.eql(u8, instruction, "argument")) {
        return Arg1{ .memory = Memory.ARGUMENT };
    } else if (std.mem.eql(u8, instruction, "this")) {
        return Arg1{ .memory = Memory.THIS };
    } else if (std.mem.eql(u8, instruction, "that")) {
        return Arg1{ .memory = Memory.THAT };
    } else if (std.mem.eql(u8, instruction, "constant")) {
        return Arg1{ .memory = Memory.CONSTANT };
    } else if (std.mem.eql(u8, instruction, "static")) {
        return Arg1{ .memory = Memory.STATIC };
    } else if (std.mem.eql(u8, instruction, "pointer")) {
        return Arg1{ .memory = Memory.POINTER };
    } else if (std.mem.eql(u8, instruction, "temp")) {
        return Arg1{ .memory = Memory.TEMP };
    } else {
        return Arg1{ .label = instruction };
    }
}

fn instructionToInt32(instruction: []const u8) ?u32 {
    return std.fmt.parseInt(u32, removeCommentsWhitespace(instruction), 10) catch null;
}

fn removeCommentsWhitespace(instruction: []const u8) []const u8 {
    return std.mem.trim(u8, std.mem.trimRight(u8, instruction, "//"), &std.ascii.whitespace);
}

pub const Parser = struct {
    const Self = @This();
    alloc: std.mem.Allocator,
    operations: std.ArrayList(Operation),
    current: u8 = 0,
    file: []const u8,

    //parse file
    pub fn parse(fileName: []const u8, alloc: std.mem.Allocator) !Self {
        const file = try std.fs.cwd().readFileAlloc(alloc, fileName, 1_000_000);
        var instructions = std.mem.split(u8, file, "\n");
        var operations = std.ArrayList(Operation).init(alloc);
        while (instructions.next()) |ins| {
            // we dont use short commands, yet
            std.debug.print("ins: {s}\n", .{ins});
            if (ins.len <= 1) {
                continue;
            }
            // remove extra whitespace char
            const instruction = removeCommentsWhitespace(ins[0 .. ins.len - 1]);
            if (instruction.len < 1) {
                continue;
            }
            const isComment = instruction[0] == '/' and instruction[1] == '/';
            if (isComment) {
                continue;
            }
            var commands = std.mem.split(u8, instruction, " ");
            const command = commands.next().?;
            var opCommand = instructionToCommand(command);

            var opArg1 = if (commands.peek() != null) instructionToLabelMemory(commands.next().?) else null;
            var opArg2 = if (commands.peek() != null) instructionToInt32(commands.next().?) else null;

            var operation: Operation = Operation{
                .command = opCommand,
                .arg1 = opArg1,
                .arg2 = opArg2,
            };
            std.debug.print("operation: {any} ", .{operation.command});
            if (operation.arg1 != null) switch (operation.arg1.?) {
                .label => std.debug.print("arg1: {s} ", .{operation.arg1.?.label}),
                // null => std.debug.print("arg1: {any} ", .{operation.arg1}),
                else => {
                    std.debug.print("arg1: {any} ", .{operation.arg1.?.memory});
                },
            };
            std.debug.print("arg1: {any} ", .{operation.arg1});
            std.debug.print("arg2: {any}\n", .{operation.arg2});
            try operations.append(operation);
        }

        return Self{
            .alloc = alloc,
            .operations = operations,
            .file = file,
        };
    }

    pub fn deinit(self: *Self) void {
        self.alloc.free(self.file);
        self.operations.deinit();
    }
};

test "parser" {
    var allocator = std.testing.allocator;
    var parsed = try Parser.parse("/Users/yoali/Desktop/nand2tetris/projects/08/FunctionCalls/SimpleFunction/SimpleFunction.vm", allocator);
    defer parsed.deinit();
    std.debug.print("parsed: {any}\n", .{parsed.operations.items});
}
