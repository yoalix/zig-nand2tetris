const std = @import("std");

pub const Command = enum {
    ADD,
    SUB,
    NEG,
    EQ,
    GT,
    LT,
    AND,
    OR,
    NOT,
    PUSH,
    POP,
    LABEL,
    GOTO,
    IF,
    FUNCTION,
    RETURN,
    CALL,
    ARITHMETIC,
};

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

pub const Arithmetic = enum {
    ADD,
    SUB,
    NEG,
    EQ,
    GT,
    LT,
    AND,
    OR,
    NOT,
};

pub const Operation = struct {
    command: Command,
    arg1: ?Memory,
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
        return Command.ARITHMETIC;
    }
}

fn instructionToMemory(instruction: []const u8) Memory {
    if (std.mem.eql(u8, instruction, "local")) {
        return Memory.LOCAL;
    } else if (std.mem.eql(u8, instruction, "argument")) {
        return Memory.ARGUMENT;
    } else if (std.mem.eql(u8, instruction, "this")) {
        return Memory.THIS;
    } else if (std.mem.eql(u8, instruction, "that")) {
        return Memory.THAT;
    } else if (std.mem.eql(u8, instruction, "constant")) {
        return Memory.CONSTANT;
    } else if (std.mem.eql(u8, instruction, "static")) {
        return Memory.STATIC;
    } else if (std.mem.eql(u8, instruction, "pointer")) {
        return Memory.POINTER;
    } else if (std.mem.eql(u8, instruction, "temp")) {
        return Memory.TEMP;
    } else {
        return Memory.LOCAL;
    }
}

fn instructionToInt32(instruction: []const u8) !u32 {
    return std.fmt.parseInt(u32, instruction, 10) catch |e| {
        std.debug.print("error parsing arg2 {}\n", .{e});
        return e;
    };
}

pub const Parser = struct {
    const Self = @This();
    alloc: std.mem.Allocator,
    operations: []Operation,
    current: u8 = 0,

    //parse file
    pub fn parse(fileName: []const u8, alloc: std.mem.Allocator) !Self {
        const file = try std.fs.cwd().readFileAlloc(alloc, fileName, 1_000_000);
        var instructions = std.mem.split(u8, file, "\n");
        var operations = std.ArrayList(Operation).init(alloc);
        errdefer operations.deinit();
        while (instructions.next()) |ins| {
            // we dont use short commands, yet
            if (ins.len <= 1) {
                continue;
            }
            // remove extra whitespace char
            const instruction = ins[0 .. ins.len - 1];
            const isComment = instruction[0] == '/' and instruction[1] == '/';
            if (isComment) {
                std.debug.print("comment found\n", .{});
                continue;
            }
            var commands = std.mem.split(u8, instruction, " ");
            std.debug.print("commands: {any}\n", .{commands});
            const command = commands.next().?;
            var opCommand = instructionToCommand(command);

            std.debug.print("command: {any}\n", .{opCommand});
            var opArg1 = if (commands.peek() != null) instructionToMemory(commands.next().?) else null;
            std.debug.print("arg1: {any}\n", .{opArg1});
            var opArg2 = if (commands.peek() != null) try instructionToInt32(commands.next().?) else null;
            std.debug.print("arg2: {any}\n", .{opArg2});

            var operation: Operation = Operation{
                .command = opCommand,
                .arg1 = opArg1,
                .arg2 = opArg2,
            };
            std.debug.print("operation: {}\n", .{operation});
            try operations.append(operation);
        }
        return Self{
            .alloc = alloc,
            .operations = operations.items,
        };
    }
    pub fn hasMoreCommands(self: *Self) bool {
        //return true if there are more commands
        return self.current < self.operations.len;
    }
    pub fn advance(self: *Self) void {
        self.current += 1;
        //advance to next command

    }
    pub fn commandType(self: *Self) Command {
        //return command type
        return self.operations[self.current].command;
    }
    //return commands
    pub fn arg1(self: *Self) []u8 {
        return self.operations[self.current].arg1;
    }
    pub fn arg2(self: *Self) u32 {
        return self.operations[self.current].arg2;
    }

    pub fn deinit(self: *Self) void {
        self.alloc.free(self.operations);
    }
};
