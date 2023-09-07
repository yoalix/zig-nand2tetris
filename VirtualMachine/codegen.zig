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
    filename: []const u8,
    fileContents: std.ArrayList(u8),
    labelCount: u32 = 0,

    pub fn init(filename: []const u8, alloc: std.mem.Allocator) Self {
        var it = std.mem.tokenize(u8, filename, "/.\n");

        const splitfilename = while (it.next()) |token| {
            if (std.mem.eql(u8, it.peek().?, "vm")) {
                break token;
            }
        } else filename;
        return Self{
            .alloc = alloc,
            .filename = splitfilename,
            .fileContents = std.ArrayList(u8).init(alloc),
        };
    }

    pub fn generate(self: *Self, parsed: *parser.Parser) !void {
        for (parsed.operations) |op| {
            switch (op.command) {
                parser.Command.PUSH, parser.Command.POP => {
                    try self.writePushPop(op.command, op.arg1.?, op.arg2.?);
                },
                else => {
                    try self.writeArithmetic(op.command);
                },
            }
        }
    }

    pub fn deinit(self: *Self) void {
        self.fileContents.deinit();
    }

    pub fn write(self: *Self, filename: []const u8) !void {
        var file = try std.fs.cwd().createFile(
            filename,
            .{ .read = true },
        );
        defer file.close();
        try file.writeAll(self.fileContents.items);
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
    pub fn writeUpdateStack(self: *Self) !void {
        const updateStack =
            \\@SP
            \\A=M
            \\M=D
            \\
        ;
        try self.fileContents.appendSlice(updateStack);
    }

    pub fn writeUpdateAddr(self: *Self) !void {
        const updateAddr =
            \\@SP
            \\A=M
            \\D=M
            \\@addr
            \\A=M
            \\M=D
            \\
        ;
        try self.fileContents.appendSlice(updateAddr);
    }

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

    //*SP=THIS/THAT,SP++
    pub fn writePushPointer(self: *Self, index: u32) !void {
        const thisOrThat = if (index == 0) HackCode.THIS else HackCode.THAT;
        var strThisOrThat = HackCode.getHackInstruction(thisOrThat);
        try self.fileContents.appendSlice(strThisOrThat);
        try self.fileContents.appendSlice("\nD=M\n");
        try self.writeUpdateStack();
        try self.writeIncrementSP();
    }

    pub fn writePushStatic(self: *Self, index: u32) !void {
        try self.fileContents.writer().print("@{s}.{}", .{ self.filename, index });
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
        try self.fileContents.appendSlice("D=A\n");
        try self.fileContents.appendSlice(Segment.getSegmentPointer(segment));
        try self.fileContents.appendSlice("\nD=D+M\n");
        try self.fileContents.appendSlice("@addr\n");
        try self.fileContents.appendSlice("M=D\n");
        try self.fileContents.appendSlice("A=M\n");
        try self.fileContents.appendSlice("D=M\n");
        try self.writeDecrementSP();
        try self.writeUpdateAddr();
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
            \\
        ;
        try self.fileContents.appendSlice(popTemp);
        try self.writeDecrementSP();

        const popTemp2 =
            \\@SP
            \\A=M
            \\D=M
            \\@addr
            \\A=M
            \\M=D
        ;
        try self.fileContents.appendSlice(popTemp2);
    }

    fn writePopStatic(self: *Self, index: u32) !void {
        try self.writeDecrementSP();
        const popStatic =
            \\A=M
            \\D=M
            \\@{s}.{}
            \\M=D
            \\@SP
            \\A=M
            \\M=0
            \\
        ;
        try self.fileContents.writer().print(popStatic, .{ self.filename, index });
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

test "push constant\n" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    var allocator = arena.allocator();
    var codeGen = CodeGen.init("test", allocator);
    defer codeGen.deinit();
    try codeGen.writePushPop(parser.Command.PUSH, parser.Memory.CONSTANT, 10);
    try codeGen.writePushPop(parser.Command.PUSH, parser.Memory.LOCAL, 20);
    try codeGen.writePushPop(parser.Command.PUSH, parser.Memory.ARGUMENT, 30);
    try codeGen.writePushPop(parser.Command.PUSH, parser.Memory.STATIC, 40);
    try codeGen.writePushPop(parser.Command.POP, parser.Memory.THIS, 40);
    std.debug.print("{s}", .{codeGen.fileContents.items});
}
