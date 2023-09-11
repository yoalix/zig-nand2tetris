const std = @import("std");
const parser = @import("./parser.zig");

const HackCode = enum {
    SP,
    LCL,
    ARG,
    THIS,
    THAT,
    R0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
    SCREEN,
    KBD,
    M,
    D,
    A,
    AM,
    AD,
    AMD,
    Zero,
    One,
    Negative1,
    NotD,
    NotA,
    NotM,
    NegativeD,
    NegativeA,
    NegativeM,
    DPlusOne,
    APlusOne,
    MPlusOne,
    DMinusOne,
    AMinusOne,
    MMinusOne,
    DPlusA,
    DPlusM,
    DMinusA,
    DMinusM,
    AMinusD,
    MMinusD,
    DAndA,
    DAndM,
    DOrA,
    DOrM,
    JGT,
    JEQ,
    JGE,
    JLT,
    JNE,
    JLE,
    JMP,
    pub fn getHackInstruction(hackCode: HackCode) []const u8 {
        return switch (hackCode) {
            .SP => "@SP",
            .LCL => "@LCL",
            .ARG => "@ARG",
            .THIS => "@THIS",
            .THAT => "@THAT",
            .R0 => "@R0",
            .R1 => "@R1",
            .R2 => "@R2",
            .R3 => "@R3",
            .R4 => "@R4",
            .R5 => "@R5",
            .R6 => "@R6",
            .R7 => "@R7",
            .R8 => "@R8",
            .R9 => "@R9",
            .R10 => "@R10",
            .R11 => "@R11",
            .R12 => "@R12",
            .R13 => "@R13",
            .R14 => "@R14",
            .R15 => "@R15",
            .SCREEN => "@SCREEN",
            .KBD => "@KBD",
            .M => "M",
            .D => "D",
            .A => "A",
            .AM => "AM",
            .AD => "AD",
            .AMD => "AMD",
            .Zero => "0",
            .One => "1",
            .Negative1 => "-1",
            .NotD => "!D",
            .NotA => "!A",
            .NotM => "!M",
            .NegativeD => "-D",
            .NegativeA => "-A",
            .NegativeM => "-M",
            .DPlusOne => "D+1",
            .APlusOne => "A+1",
            .MPlusOne => "M+1",
            .DMinusOne => "D-1",
            .AMinusOne => "A-1",
            .MMinusOne => "M-1",
            .DPlusA => "D+A",
            .DPlusM => "D+M",
            .DMinusA => "D-A",
            .DMinusM => "D-M",
            .AMinusD => "A-D",
            .MMinusD => "M-D",
            .DAndA => "D&A",
            .DAndM => "D&M",
            .DOrA => "D|A",
            .DOrM => "D|M",
            .JGT => "JGT",
            .JEQ => "JEQ",
            .JGE => "JGE",
            .JLT => "JLT",
            .JNE => "JNE",
            .JLE => "JLE",
            .JMP => "JMP",
        };
    }
};

const Segment = enum {
    LCL,
    ARG,
    THIS,
    THAT,
    pub fn getSegmentPointer(segment: Segment) []const u8 {
        return switch (segment) {
            .LCL => "@LCL",
            .ARG => "@ARG",
            .THIS => "@THIS",
            .THAT => "@THAT",
        };
    }
};

pub const CodeGen = struct {
    const Self = @This();
    alloc: std.mem.Allocator,
    filename: ?[]const u8,
    fileContents: std.ArrayList(u8),
    labelCount: u32 = 0,
    parsed: ?*parser.Parser,

    pub fn init(alloc: std.mem.Allocator) Self {
        return Self{
            .alloc = alloc,
            .fileContents = std.ArrayList(u8).init(alloc),
            .filename = null,
            .parsed = null,
        };
    }

    pub fn deinit(self: *Self) void {
        self.fileContents.deinit();
        if (self.parsed != null) self.parsed.?.deinit();
    }

    pub fn setFilename(self: *Self, filename: []const u8) !void {
        // if (self.parsed != null) {
        //     self.parsed.?.deinit();
        //     self.parsed = null;
        // }
        var parsed = try parser.Parser.parse(filename, self.alloc);
        var it = std.mem.tokenize(u8, filename, "/.\n");

        const splitFilename = while (it.next()) |token| {
            if (std.mem.eql(u8, it.peek().?, "vm")) {
                break token;
            }
        } else filename;
        self.filename = splitFilename;
        self.parsed = &parsed;
    }

    pub fn generate(self: *Self) !void {
        // std.debug.print("{any}", .{self.parsed.?.operations});
        for (self.parsed.?.operations.items) |op| {
            std.debug.print("{any}\n", .{op});
            switch (op.command) {
                parser.Command.FUNCTION => {
                    try self.writeFunction(op.arg1.?.label, op.arg2.?);
                },
                parser.Command.CALL => {
                    try self.writeCall(op.arg1.?.label, op.arg2.?);
                },
                parser.Command.RETURN => {
                    try self.writeReturn();
                },
                parser.Command.LABEL => {
                    try self.writeLabel(op.arg1.?.label);
                },
                parser.Command.GOTO => {
                    try self.writeGoto(op.arg1.?.label);
                },
                parser.Command.IF => {
                    try self.writeIf(op.arg1.?.label);
                },

                parser.Command.PUSH, parser.Command.POP => {
                    try self.writePushPop(op.command, op.arg1.?.memory, op.arg2.?);
                },
                else => {
                    try self.writeArithmetic(op.command);
                },
            }
        }
    }

    pub fn write(self: *Self, filename: []const u8) !void {
        var file = try std.fs.cwd().createFile(
            filename,
            .{ .read = true },
        );
        defer file.close();
        try file.writeAll(self.fileContents.items);
    }

    pub fn writeInit(self: *Self) !void {
        const setSp =
            \\@256
            \\D=A
            \\@SP
            \\M=D
            \\
        ;
        try self.fileContents.appendSlice(setSp);
        try self.writeCall("Sys.init", 0);
        const returnSysInit =
            \\@return$Sys.init.0
            \\0;JMP
            \\
        ;
        try self.fileContents.writer().print(returnSysInit, .{});
    }

    pub fn writeLabel(self: *Self, label: []const u8) !void {
        std.debug.print("label: {s}\n", .{label});
        try self.fileContents.writer().print("//label {s}\n", .{label});
        try self.fileContents.writer().print("({s})\n", .{label});
    }

    pub fn writeGoto(self: *Self, label: []const u8) !void {
        std.debug.print("goto: {s}\n", .{label});
        try self.fileContents.writer().print("//goto {s}\n", .{label});
        try self.fileContents.writer().print("@{s}\n0;JMP\n", .{label});
    }

    pub fn writeIf(self: *Self, label: []const u8) !void {
        std.debug.print("if-goto: {s}\n", .{label});
        try self.fileContents.writer().print("//if-goto {s}\n", .{label});
        try self.writeDecrementSP();
        const ifGoto =
            \\A=M
            \\D=M
            \\M=0
            \\@{s}
            \\D;JNE
            \\
        ;
        try self.fileContents.writer().print(ifGoto, .{label});
    }

    pub fn writeFunction(self: *Self, functionName: []const u8, numLocals: u32) !void {
        try self.fileContents.writer().print("//function {s} {d}\n", .{ functionName, numLocals });
        try self.fileContents.writer().print("({s})\n", .{functionName});
        for (0..numLocals) |i| {
            const pushLocal =
                \\@LCL
                \\D=M
                \\@{}
                \\A=D+A
                \\M=0
                \\@SP
                \\M=M+1
                \\
            ;
            try self.fileContents.writer().print(pushLocal, .{i});
        }
    }

    pub fn writeCall(self: *Self, functionName: []const u8, numArgs: u32) !void {
        try self.fileContents.writer().print("//call {s} {d}\n", .{ functionName, numArgs });

        const returnAddress = try std.fmt.allocPrint(self.alloc, "return${s}.{}", .{ functionName, self.labelCount });
        self.labelCount += 1;
        defer self.alloc.free(returnAddress);

        _ = try self.fileContents.writer().print("@{s}\nD=A\n", .{returnAddress});
        try self.writeUpdateStack();
        try self.writeIncrementSP();

        try self.writePushSegmentMemory(Segment.LCL);
        try self.writePushSegmentMemory(Segment.ARG);
        try self.writePushSegmentMemory(Segment.THIS);
        try self.writePushSegmentMemory(Segment.THAT);

        const call =
            \\@SP
            \\D=M
            \\@5
            \\D=D-A
            \\@{d}
            \\D=D-A
            \\@ARG
            \\M=D
            \\@SP
            \\D=M
            \\@LCL
            \\M=D
            \\
        ;
        try self.fileContents.writer().print(call, .{numArgs});

        try self.writeGoto(functionName);
        try self.writeLabel(returnAddress);
    }

    pub fn writeReturn(self: *Self) !void {
        try self.fileContents.writer().print("//return\n", .{});

        // endFrame = LCL
        // retAddr = *(endFrame - 5)
        // *ARG = pop()
        // SP = ARG + 1
        // THAT = *(endFrame - 1)
        // THIS = *(endFrame - 2)
        // ARG = *(endFrame - 3)
        // LCL = *(endFrame - 4)
        // goto retAddr
        const returnFrame =
            \\@LCL
            \\D=M
            \\@R13
            \\M=D
            \\@5
            \\A=D-A
            \\D=M
            \\@R14
            \\M=D
            \\@SP
            \\AM=M-1
            \\D=M
            \\@ARG
            \\A=M
            \\M=D
            \\@ARG
            \\D=M+1
            \\@SP
            \\M=D
            \\{s}
            \\@THAT
            \\M=D
            \\{s}
            \\@THIS
            \\M=D
            \\{s}
            \\@ARG
            \\M=D
            \\{s}
            \\@LCL
            \\M=D
            \\@R14
            \\A=M
            \\0;JMP
            \\
        ;
        //*(endFrame - 1)
        const endFrame =
            \\@R13
            \\AM=M-1
            \\D=M
            \\
        ;
        try self.fileContents.writer().print(returnFrame, .{ endFrame, endFrame, endFrame, endFrame });
    }

    //SP--
    pub fn writeDecrementSP(self: *Self) !void {
        const decrementSP =
            \\@SP
            \\M=M-1
            \\
        ;
        try self.fileContents.appendSlice(decrementSP);
    }
    // SP++
    pub fn writeIncrementSP(self: *Self) !void {
        const incrementSP =
            \\@SP
            \\M=M+1
            \\
        ;
        try self.fileContents.appendSlice(incrementSP);
    }

    // *SP = D
    pub fn writeUpdateStack(self: *Self) !void {
        const updateStack =
            \\@SP
            \\A=M
            \\M=D
            \\
        ;
        try self.fileContents.appendSlice(updateStack);
    }

    //@index
    pub fn writeAddressIndex(self: *Self, index: u32) !void {
        try self.fileContents.writer().print("@{}\n", .{index});
    }
    //*SP = index; SP++
    pub fn writePushConstant(self: *Self, index: u32) !void {
        try self.writeAddressIndex(index);
        try self.fileContents.appendSlice("D=A\n");
        try self.writeUpdateStack();
        try self.writeIncrementSP();
    }

    //addr = segmentPointer + i, *SP=*addr, SP++
    pub fn writePushSegment(self: *Self, segment: Segment, index: u32) !void {
        try self.writeAddressIndex(index);
        try self.fileContents.appendSlice("D=A\n");
        try self.fileContents.appendSlice(Segment.getSegmentPointer(segment));
        try self.fileContents.appendSlice("\nD=D+M\n");
        try self.fileContents.appendSlice("@addr\n");
        try self.fileContents.appendSlice("M=D\n");
        try self.fileContents.appendSlice("A=M\n");
        try self.fileContents.appendSlice("D=M\n");
        try self.writeUpdateStack();
        try self.writeIncrementSP();
    }
    //*SP = segmentAddr, SP++
    pub fn writePushSegmentMemory(self: *Self, segment: Segment) !void {
        try self.fileContents.appendSlice(Segment.getSegmentPointer(segment));
        try self.fileContents.appendSlice("\nD=M\n");
        try self.writeUpdateStack();
        try self.writeIncrementSP();
    }

    //*SP=THIS/THAT,SP++
    pub fn writePushPointer(self: *Self, index: u32) !void {
        const thisOrThat = if (index == 0) HackCode.THIS else HackCode.THAT;
        var strThisOrThat = HackCode.getHackInstruction(thisOrThat);
        try self.fileContents.appendSlice(strThisOrThat);
        try self.fileContents.appendSlice("\nD=M\n");
        try self.writeUpdateStack();
        try self.writeIncrementSP();
    }

    //*SP=*@file.index, SP++
    pub fn writePushStatic(self: *Self, index: u32) !void {
        try self.fileContents.writer().print("@{s}.{}", .{ self.filename.?, index });
        try self.fileContents.appendSlice("\nD=M\n");
        try self.writeUpdateStack();
        try self.writeIncrementSP();
    }

    // addr=5+i; *SP=*addr, SP++
    pub fn writePushTemp(self: *Self, index: u32) !void {
        try self.writeAddressIndex(index);
        const pushTemp =
            \\D=A
            \\@5
            \\D=D+A
            \\@addr
            \\M=D
            \\A=M
            \\D=M
            \\
        ;
        try self.fileContents.appendSlice(pushTemp);
        try self.writeUpdateStack();
        try self.writeIncrementSP();
    }

    //addr = segmentPointer + i, SP--, *addr = *SP
    pub fn writePopSegment(self: *Self, segment: Segment, index: u32) !void {
        try self.writeAddressIndex(index);

        const popSegment =
            \\D=A
            \\{s}
            \\D=D+M
            \\@addr
            \\M=D
            \\@SP
            \\AM=M-1
            \\D=M
            \\@addr
            \\A=M
            \\M=D
            \\
        ;
        try self.fileContents.writer().print(popSegment, .{Segment.getSegmentPointer(segment)});
    }

    //SP--; THIS/THAT=*SP
    fn writePopPointer(self: *Self, index: u32) !void {
        const thisOrThat = if (index == 0) HackCode.THIS else HackCode.THAT;
        var strThisOrThat = HackCode.getHackInstruction(thisOrThat);
        try self.writeDecrementSP();
        const popPointer =
            \\@SP
            \\A=M
            \\D=M
            \\{s}
            \\M=D
            \\
        ;
        try self.fileContents.writer().print(popPointer, .{strThisOrThat});
    }

    //addr=5+i; SP--; *addr=*SP
    fn writePopTemp(self: *Self, index: u32) !void {
        try self.writeAddressIndex(index);
        const popTemp =
            \\D=A
            \\@5
            \\D=D+A
            \\@addr
            \\M=D
            \\@SP
            \\AM=M-1
            \\D=M
            \\@addr
            \\A=M
            \\M=D
            \\
        ;
        try self.fileContents.appendSlice(popTemp);
    }

    fn writePopStatic(self: *Self, index: u32) !void {
        const popStatic =
            \\@SP
            \\AM=M-1
            \\D=M
            \\@{s}.{}
            \\M=D
            \\
        ;
        try self.fileContents.writer().print(popStatic, .{ self.filename.?, index });
    }

    pub fn writePushPop(self: *Self, command: parser.Command, segment: parser.Memory, index: u32) !void {
        try self.fileContents.writer().print("//{} {} {}\n", .{ command, segment, index });
        switch (command) {
            parser.Command.PUSH => {
                switch (segment) {
                    parser.Memory.CONSTANT => {
                        try self.writePushConstant(index);
                    },
                    parser.Memory.LOCAL => {
                        try self.writePushSegment(Segment.LCL, index);
                    },
                    parser.Memory.ARGUMENT => {
                        try self.writePushSegment(Segment.ARG, index);
                    },
                    parser.Memory.THIS => {
                        try self.writePushSegment(Segment.THIS, index);
                    },
                    parser.Memory.THAT => {
                        try self.writePushSegment(Segment.THAT, index);
                    },
                    parser.Memory.POINTER => {
                        try self.writePushPointer(index);
                    },
                    parser.Memory.TEMP => {
                        try self.writePushTemp(index);
                    },
                    parser.Memory.STATIC => {
                        try self.writePushStatic(index);
                    },
                }
            },
            parser.Command.POP => {
                switch (segment) {
                    parser.Memory.LOCAL => {
                        try self.writePopSegment(Segment.LCL, index);
                    },
                    parser.Memory.ARGUMENT => {
                        try self.writePopSegment(Segment.ARG, index);
                    },
                    parser.Memory.THIS => {
                        try self.writePopSegment(Segment.THIS, index);
                    },
                    parser.Memory.THAT => {
                        try self.writePopSegment(Segment.THAT, index);
                    },
                    parser.Memory.POINTER => {
                        try self.writePopPointer(index);
                    },
                    parser.Memory.TEMP => {
                        try self.writePopTemp(index);
                    },
                    parser.Memory.STATIC => {
                        try self.writePopStatic(index);
                    },
                    else => unreachable,
                }
            },
            else => unreachable,
        }
    }
    fn writeAddSub(self: *Self, sign: []const u8) !void {
        const addSub =
            \\@SP
            \\AM=M-1
            \\D=M
            \\M=0
            \\A=A-1
            \\M=M{s}D
            \\
        ;
        try self.fileContents.writer().print(addSub, .{sign});
    }
    fn writeNegNot(self: *Self, sign: []const u8) !void {
        const negNot =
            \\@SP
            \\A=M-1
            \\M={s}M
            \\
        ;
        try self.fileContents.writer().print(negNot, .{sign});
    }
    fn writeAndOr(self: *Self, sign: []const u8) !void {
        const andOr =
            \\@SP
            \\AM=M-1
            \\D=M
            \\M=0
            \\A=A-1
            \\M=M{s}D
            \\
        ;
        try self.fileContents.writer().print(andOr, .{sign});
    }
    fn writeEqGtLt(self: *Self, sign: []const u8) !void {
        const labelCount = self.labelCount;
        self.labelCount += 1;
        const labelCount2 = self.labelCount;
        self.labelCount += 1;
        const eqGtLt =
            \\@SP
            \\AM=M-1
            \\D=M
            \\M=0
            \\A=A-1
            \\D=M-D
            \\@TRUE{}
            \\D;J{s}
            \\@SP
            \\A=M-1
            \\M=0
            \\@CONTINUE{}
            \\0;JEQ
            \\(TRUE{})
            \\@SP
            \\A=M
            \\A=A-1
            \\M=-1
            \\(CONTINUE{})
            \\
        ;

        try self.fileContents.writer().print(eqGtLt, .{ labelCount, sign, labelCount2, labelCount, labelCount2 });
    }

    pub fn writeArithmetic(self: *Self, aritmethic: parser.Command) !void {
        try self.fileContents.writer().print("//{}\n", .{aritmethic});
        switch (aritmethic) {
            .ADD => {
                try self.writeAddSub("+");
            },
            .SUB => {
                try self.writeAddSub("-");
            },
            .NEG => {
                try self.writeNegNot("-");
            },
            .NOT => {
                try self.writeNegNot("!");
            },
            .AND => {
                try self.writeAndOr("&");
            },
            .OR => {
                try self.writeAndOr("|");
            },
            .EQ => {
                try self.writeEqGtLt("EQ");
            },
            .GT => {
                try self.writeEqGtLt("GT");
            },
            .LT => {
                try self.writeEqGtLt("LT");
            },
            else => unreachable,
        }
    }
};

const testing = std.testing;

// test "codeGen parse" {
//     var allocator = std.testing.allocator;
//     var codeGen = CodeGen.init(allocator);
//
//     try codeGen.setFilename("/Users/yoali/Desktop/nand2tetris/projects/08/FunctionCalls/SimpleFunction/SimpleFunction.vm");
//     defer codeGen.parsed.?.deinit();
//     std.debug.print("parsed: {any}\n", .{codeGen.parsed});
//     // try codeGen.generate();
//     //
//     // std.debug.print("fileContents: {any}\n", .{codeGen.fileContents.items});
// }

// test "codeGen generate" {
//     var allocator = std.testing.allocator;
//     var codeGen = CodeGen.init(allocator);
//     defer codeGen.deinit();
//     var parsed = try parser.Parser.parse("/Users/yoali/Desktop/nand2tetris/projects/08/FunctionCalls/SimpleFunction/SimpleFunction.vm", allocator);
//     codeGen.parsed = &parsed;
//     try codeGen.generate();
// }

test "codegen push constant" {
    var allocator = std.testing.allocator;
    var codeGen = CodeGen.init(allocator);
    defer codeGen.deinit();
    codeGen.filename = "test.vm";
    try codeGen.writePushPop(parser.Command.PUSH, parser.Memory.CONSTANT, 10);
    const expectPushConstant =
        \\//parser.Command.PUSH parser.Memory.CONSTANT 10
        \\@10
        \\D=A
        \\@SP
        \\A=M
        \\M=D
        \\@SP
        \\M=M+1
        \\
    ;

    const pushConst = codeGen.fileContents.items;
    try testing.expectEqualStrings(expectPushConstant, pushConst);
}
test "codegen push local" {
    var allocator = std.testing.allocator;
    var codeGen = CodeGen.init(allocator);
    defer codeGen.deinit();
    codeGen.filename = "test.vm";
    const expectPushLocal =
        \\//parser.Command.PUSH parser.Memory.LOCAL 20
        \\@20
        \\D=A
        \\@LCL
        \\D=D+M
        \\@addr
        \\M=D
        \\A=M
        \\D=M
        \\@SP
        \\A=M
        \\M=D
        \\@SP
        \\M=M+1
        \\
    ;
    try codeGen.writePushPop(parser.Command.PUSH, parser.Memory.LOCAL, 20);
    const pushLocal = codeGen.fileContents.items;

    try testing.expectEqualStrings(expectPushLocal, pushLocal);
}
test "codegen push argument" {
    var allocator = std.testing.allocator;
    var codeGen = CodeGen.init(allocator);
    defer codeGen.deinit();
    codeGen.filename = "test.vm";
    const expectPushArgument =
        \\//parser.Command.PUSH parser.Memory.ARGUMENT 30
        \\@30
        \\D=A
        \\@ARG
        \\D=D+M
        \\@addr
        \\M=D
        \\A=M
        \\D=M
        \\@SP
        \\A=M
        \\M=D
        \\@SP
        \\M=M+1
        \\
    ;
    try codeGen.writePushPop(parser.Command.PUSH, parser.Memory.ARGUMENT, 30);
    const pushArgument = codeGen.fileContents.items;
    try testing.expectEqualStrings(expectPushArgument, pushArgument);
}
test "codegen push static" {
    var allocator = std.testing.allocator;
    var codeGen = CodeGen.init(allocator);
    defer codeGen.deinit();
    codeGen.filename = "test.vm";
    const expectPushStatic =
        \\//parser.Command.PUSH parser.Memory.STATIC 40
        \\@test.vm.40
        \\D=M
        \\@SP
        \\A=M
        \\M=D
        \\@SP
        \\M=M+1
        \\
    ;

    try codeGen.writePushPop(parser.Command.PUSH, parser.Memory.STATIC, 40);
    const pushStatic = codeGen.fileContents.items;

    try testing.expectEqualStrings(expectPushStatic, pushStatic);
}
test "codegen push this" {
    var allocator = std.testing.allocator;
    var codeGen = CodeGen.init(allocator);
    defer codeGen.deinit();
    codeGen.filename = "test.vm";
    const expectPushThis =
        \\//parser.Command.PUSH parser.Memory.THIS 40
        \\@40
        \\D=A
        \\@THIS
        \\D=D+M
        \\@addr
        \\M=D
        \\A=M
        \\D=M
        \\@SP
        \\A=M
        \\M=D
        \\@SP
        \\M=M+1
        \\
    ;
    try codeGen.writePushPop(parser.Command.PUSH, parser.Memory.THIS, 40);
    const pushThis = codeGen.fileContents.items;

    try testing.expectEqualStrings(expectPushThis, pushThis);
}
test "codegen push that" {
    var allocator = std.testing.allocator;
    var codeGen = CodeGen.init(allocator);
    defer codeGen.deinit();
    codeGen.filename = "test.vm";
    const expectPushThat =
        \\//parser.Command.PUSH parser.Memory.THAT 40
        \\@40
        \\D=A
        \\@THAT
        \\D=D+M
        \\@addr
        \\M=D
        \\A=M
        \\D=M
        \\@SP
        \\A=M
        \\M=D
        \\@SP
        \\M=M+1
        \\
    ;
    try codeGen.writePushPop(parser.Command.PUSH, parser.Memory.THAT, 40);
    const pushThat = codeGen.fileContents.items;

    try testing.expectEqualStrings(expectPushThat, pushThat);
}
test "codegen push temp" {
    var allocator = std.testing.allocator;
    var codeGen = CodeGen.init(allocator);
    defer codeGen.deinit();
    codeGen.filename = "test.vm";
    const expectPushTemp =
        \\//parser.Command.PUSH parser.Memory.TEMP 4
        \\@4
        \\D=A
        \\@5
        \\D=D+A
        \\@addr
        \\M=D
        \\A=M
        \\D=M
        \\@SP
        \\A=M
        \\M=D
        \\@SP
        \\M=M+1
        \\
    ;
    try codeGen.writePushPop(parser.Command.PUSH, parser.Memory.TEMP, 4);
    const pushTemp = codeGen.fileContents.items;

    try testing.expectEqualStrings(expectPushTemp, pushTemp);
}

test "codegen pop local" {
    var allocator = std.testing.allocator;
    var codeGen = CodeGen.init(allocator);
    defer codeGen.deinit();
    codeGen.filename = "test.vm";

    const expectedPopLocal =
        \\//parser.Command.POP parser.Memory.LOCAL 20
        \\@20
        \\D=A
        \\@LCL
        \\D=D+M
        \\@addr
        \\M=D
        \\@SP
        \\AM=M-1
        \\D=M
        \\@addr
        \\A=M
        \\M=D
        \\
    ;
    try codeGen.writePushPop(parser.Command.POP, parser.Memory.LOCAL, 20);
    const popLocal = codeGen.fileContents.items;
    try testing.expectEqualStrings(expectedPopLocal, popLocal);
}

test "codegen pop argument" {
    var allocator = std.testing.allocator;
    var codeGen = CodeGen.init(allocator);
    defer codeGen.deinit();
    codeGen.filename = "test.vm";

    const expectedPopArgument =
        \\//parser.Command.POP parser.Memory.ARGUMENT 30
        \\@30
        \\D=A
        \\@ARG
        \\D=D+M
        \\@addr
        \\M=D
        \\@SP
        \\AM=M-1
        \\D=M
        \\@addr
        \\A=M
        \\M=D
        \\
    ;
    try codeGen.writePushPop(parser.Command.POP, parser.Memory.ARGUMENT, 30);
    const popArgument = codeGen.fileContents.items;
    try testing.expectEqualStrings(expectedPopArgument, popArgument);
}

test "codegen pop static" {
    var allocator = std.testing.allocator;
    var codeGen = CodeGen.init(allocator);
    defer codeGen.deinit();
    codeGen.filename = "test.vm";

    const expectedPopStatic =
        \\//parser.Command.POP parser.Memory.STATIC 40
        \\@SP
        \\AM=M-1
        \\D=M
        \\@test.vm.40
        \\M=D
        \\
    ;

    try codeGen.writePushPop(parser.Command.POP, parser.Memory.STATIC, 40);
    const popStatic = codeGen.fileContents.items;
    try testing.expectEqualStrings(expectedPopStatic, popStatic);
}

test "codegen pop this" {
    var allocator = std.testing.allocator;
    var codeGen = CodeGen.init(allocator);
    defer codeGen.deinit();
    codeGen.filename = "test.vm";
    const expectedPopThis =
        \\//parser.Command.POP parser.Memory.THIS 40
        \\@40
        \\D=A
        \\@THIS
        \\D=D+M
        \\@addr
        \\M=D
        \\@SP
        \\AM=M-1
        \\D=M
        \\@addr
        \\A=M
        \\M=D
        \\
    ;
    try codeGen.writePushPop(parser.Command.POP, parser.Memory.THIS, 40);
    const popThis = codeGen.fileContents.items;
    try testing.expectEqualStrings(expectedPopThis, popThis);
}

test "codegen pop that" {
    var allocator = std.testing.allocator;
    var codeGen = CodeGen.init(allocator);
    defer codeGen.deinit();
    codeGen.filename = "test.vm";

    const expectedPopThat =
        \\//parser.Command.POP parser.Memory.THAT 40
        \\@40
        \\D=A
        \\@THAT
        \\D=D+M
        \\@addr
        \\M=D
        \\@SP
        \\AM=M-1
        \\D=M
        \\@addr
        \\A=M
        \\M=D
        \\
    ;
    try codeGen.writePushPop(parser.Command.POP, parser.Memory.THAT, 40);
    const popThat = codeGen.fileContents.items;
    try testing.expectEqualStrings(expectedPopThat, popThat);
}

test "codegen pop temp" {
    var allocator = std.testing.allocator;
    var codeGen = CodeGen.init(allocator);
    defer codeGen.deinit();
    codeGen.filename = "test.vm";

    const expectedPopTemp =
        \\//parser.Command.POP parser.Memory.TEMP 4
        \\@4
        \\D=A
        \\@5
        \\D=D+A
        \\@addr
        \\M=D
        \\@SP
        \\AM=M-1
        \\D=M
        \\@addr
        \\A=M
        \\M=D
        \\
    ;
    try codeGen.writePushPop(parser.Command.POP, parser.Memory.TEMP, 4);
    const popTemp = codeGen.fileContents.items;
    try testing.expectEqualStrings(expectedPopTemp, popTemp);
}

test "call" {
    var allocator = std.testing.allocator;
    var codeGen = CodeGen.init(allocator);
    defer codeGen.deinit();
    codeGen.filename = "test.vm";

    const expectedCall =
        \\//call testFn 2
        \\@return$testFn.0
        \\D=A
        \\@SP
        \\A=M
        \\M=D
        \\@SP
        \\M=M+1
        \\@LCL
        \\D=M
        \\@SP
        \\A=M
        \\M=D
        \\@SP
        \\M=M+1
        \\@ARG
        \\D=M
        \\@SP
        \\A=M
        \\M=D
        \\@SP
        \\M=M+1
        \\@THIS
        \\D=M
        \\@SP
        \\A=M
        \\M=D
        \\@SP
        \\M=M+1
        \\@THAT
        \\D=M
        \\@SP
        \\A=M
        \\M=D
        \\@SP
        \\M=M+1
        \\@SP
        \\D=M
        \\@5
        \\D=D-A
        \\@2
        \\D=D-A
        \\@ARG
        \\M=D
        \\@SP
        \\D=M
        \\@LCL
        \\M=D
        \\//goto testFn
        \\@testFn
        \\0;JMP
        \\//label return$testFn.0
        \\(return$testFn.0)
        \\
    ;
    try codeGen.writeCall("testFn", 2);
    const call = codeGen.fileContents.items;
    try testing.expectEqualStrings(expectedCall, call);
}

test "function" {
    var allocator = std.testing.allocator;
    var codeGen = CodeGen.init(allocator);
    defer codeGen.deinit();
    codeGen.filename = "test.vm";

    const expectedFunction =
        \\//function testFn 2
        \\(testFn)
        \\@LCL
        \\D=M
        \\@0
        \\A=D+A
        \\M=0
        \\@SP
        \\M=M+1
        \\@LCL
        \\D=M
        \\@1
        \\A=D+A
        \\M=0
        \\@SP
        \\M=M+1
        \\
    ;
    try codeGen.writeFunction("testFn", 2);
    const function = codeGen.fileContents.items;
    try testing.expectEqualStrings(expectedFunction, function);
}

test "return" {
    var allocator = std.testing.allocator;
    var codeGen = CodeGen.init(allocator);
    defer codeGen.deinit();
    codeGen.filename = "test.vm";

    const expectedReturn =
        \\//return
        \\@LCL
        \\D=M
        \\@R13
        \\M=D
        \\@5
        \\A=D-A
        \\D=M
        \\@R14
        \\M=D
        \\@SP
        \\AM=M-1
        \\D=M
        \\@ARG
        \\A=M
        \\M=D
        \\@ARG
        \\D=M+1
        \\@SP
        \\M=D
        \\@R13
        \\AM=M-1
        \\D=M
        \\
        \\@THAT
        \\M=D
        \\@R13
        \\AM=M-1
        \\D=M
        \\
        \\@THIS
        \\M=D
        \\@R13
        \\AM=M-1
        \\D=M
        \\
        \\@ARG
        \\M=D
        \\@R13
        \\AM=M-1
        \\D=M
        \\
        \\@LCL
        \\M=D
        \\@R14
        \\A=M
        \\0;JMP
        \\
    ;
    try codeGen.writeReturn();
    const actualReturn = codeGen.fileContents.items;
    try testing.expectEqualStrings(expectedReturn, actualReturn);
}

test "codegen init" {
    var allocator = std.testing.allocator;
    var codeGen = CodeGen.init(allocator);
    defer codeGen.deinit();
    codeGen.filename = "test.vm";

    const expectedInit =
        \\@256
        \\D=A
        \\@SP
        \\M=D
        \\//call Sys.init 0
        \\@return$Sys.init.0
        \\D=A
        \\@SP
        \\A=M
        \\M=D
        \\@SP
        \\M=M+1
        \\@LCL
        \\D=M
        \\@SP
        \\A=M
        \\M=D
        \\@SP
        \\M=M+1
        \\@ARG
        \\D=M
        \\@SP
        \\A=M
        \\M=D
        \\@SP
        \\M=M+1
        \\@THIS
        \\D=M
        \\@SP
        \\A=M
        \\M=D
        \\@SP
        \\M=M+1
        \\@THAT
        \\D=M
        \\@SP
        \\A=M
        \\M=D
        \\@SP
        \\M=M+1
        \\@SP
        \\D=M
        \\@5
        \\D=D-A
        \\@0
        \\D=D-A
        \\@ARG
        \\M=D
        \\@SP
        \\D=M
        \\@LCL
        \\M=D
        \\//goto Sys.init
        \\@Sys.init
        \\0;JMP
        \\//label return$Sys.init.0
        \\(return$Sys.init.0)
        \\@return$Sys.init.0
        \\0;JMP
        \\
    ;
    try codeGen.writeInit();
    const actualInit = codeGen.fileContents.items;
    try testing.expectEqualStrings(expectedInit, actualInit);
}
