const std = @import("std");
const io = std.io;
const math = std.math;
const mem = std.mem;
const testing = std.testing;
const unicode = std.unicode;
const assert = std.debug.assert;
const print = std.debug.print;

/// Validate checks that provided slice is a valid JSON.
pub fn validate(s: []const u8) bool {
    var d: *Decoder = &Decoder.initWithOptions(s, Options.strict());
    while (d.nextToken()) |_| {} else |err| {
        return err == error.EOF;
    }
}

pub const Token = union(enum) {
    ArrayBegin, // [
    ArrayEnd, // ]
    ObjectBegin, // {
    ObjectEnd, // }
    NameSep, // :
    ValueSep, // ,
    True, // true
    False, // false
    Null, // null
    String: struct {
        const Self = @This();

        start: usize,
        count: usize,
        escaped: bool,

        fn get(s: Self, a: *mem.Allocator, js: []const u8) ![]const u8 {
            const str_source = js[s.start..];
            var str = try a.alloc(u8, s.count);
            errdefer a.free(str);
            if (!s.escaped) {
                mem.copy(u8, str, str_source[0..s.count]);
                return str;
            }

            var index: usize = 0;
            var src_index: usize = 0;
            var state = StringState.Inside;
            var char1: ?u16 = null;
            var char2: ?u16 = null;
            var finished = false;
            for (str_source) |c, i| {
                switch (state) {
                    .Inside => switch (c) {
                        0x00...0x1F => return DecodeError.UnescapedControlChar,
                        '\\' => state = .StartEscape,
                        '"' => {
                            if (char1 != null)
                                index += @as(usize, try utf8Encode(@as(u21, char1.?), str[index..]));
                            finished = true;
                            break;
                        },
                        else => {
                            str[index] = c;
                            index += 1;
                        },
                    },
                    .StartEscape => {
                        switch (c) {
                            '\\', '/', 'b', 'f', 'n', 'r', 't', '"' => {
                                str[index] = switch (c) {
                                    '/' => '/',
                                    'b' => 8,
                                    'f' => 12,
                                    'n' => '\n',
                                    'r' => '\r',
                                    't' => '\t',
                                    else => unreachable,
                                };
                                index += 1;
                                state = .Inside;
                            },
                            'u' => state = .Unicode0,
                            else => return DecodeError.InvalidEscapeSequence,
                        }
                    },
                    .Unicode0 => {
                        char2 = @as(u16, try fromHexDigit(c));
                        state = .Unicode1;
                    },
                    .Unicode1 => {
                        char2 = (char2.? << 4) + @as(u16, try fromHexDigit(c));
                        state = .Unicode2;
                    },
                    .Unicode2 => {
                        char2 = (char2.? << 4) + @as(u16, try fromHexDigit(c));
                        state = .Unicode3;
                    },
                    .Unicode3 => {
                        char2 = (char2.? << 4) + @as(u16, try fromHexDigit(c));
                        index += try encodeSurrogates(str[index..], char1, char2.?, &char1);
                        state = .Inside;
                    },
                }
            }
            if (!finished)
                return DecodeError.UnexpectedEOF;
            return str;
        }

        // Standard library does not allow to encode incomplete surrogates, thus this function.
        fn utf8Encode(c: u21, out: []u8) !u3 {
            const length = try unicode.utf8CodepointSequenceLength(c);
            switch (length) {
                1 => out[0] = @intCast(u8, c),
                2 => {
                    out[0] = @intCast(u8, 0b11000000 | (c >> 6));
                    out[1] = @intCast(u8, 0b10000000 | (c & 0b111111));
                },
                3 => {
                    out[0] = @intCast(u8, 0b11100000 | (c >> 12));
                    out[1] = @intCast(u8, 0b10000000 | ((c >> 6) & 0b111111));
                    out[2] = @intCast(u8, 0b10000000 | (c & 0b111111));
                },
                4 => {
                    out[0] = @intCast(u8, 0b11110000 | (c >> 18));
                    out[1] = @intCast(u8, 0b10000000 | ((c >> 12) & 0b111111));
                    out[2] = @intCast(u8, 0b10000000 | ((c >> 6) & 0b111111));
                    out[3] = @intCast(u8, 0b10000000 | (c & 0b111111));
                },
                else => unreachable,
            }
            return length;
        }

        fn encodeSurrogates(s: []u8, c1: ?u16, c2: u16, postpone: *?u16) !usize {
            if (c1 == null) {
                if (isBetween(c2, high_surr_start, high_surr_end)) {
                    // Start of a surrogate pair.
                    postpone.* = c2;
                    return 0;
                }
                // Normal symbol or unexpected low surrogate, just encode
                // as validation was done earlier.
                postpone.* = null;
                const code_point = @as(u21, c2);
                return @as(usize, try utf8Encode(code_point, s));
            }

            if (isBetween(c2, low_surr_start, low_surr_end)) {
                // End of a surrogate pair.
                const code_point = codePointFromSurrogates(c1.?, c2);
                postpone.* = null;
                return @as(usize, try utf8Encode(code_point, s));
            }

            // Encode first symbol whatever it is.
            var index = @as(usize, try utf8Encode(@as(u21, c1.?), s));
            if (isBetween(c2, high_surr_start, high_surr_end)) {
                postpone.* = c2;
                return index;
            }

            // Encode second as it isn't a start of a surrogate pair.
            var code_point = @as(u21, c2);
            index += @as(usize, try utf8Encode(code_point, s));
            postpone.* = null;
            return index;
        }
    },
    Number: struct {
        const Self = @This();
        is_float: bool,
        start: usize,
        count: usize,

        fn getInt(s: Self, comptime T: type, js: []const u8) !?T {
            if (s.is_float) {
                return null;
            }
            return try std.fmt.parseInt(T, js[s.start .. s.start + s.count], 10);
        }

        fn getFloat(s: Self, comptime T: type, js: []const u8) !T {
            return try std.fmt.parseFloat(T, js[s.start .. s.start + s.count]);
        }
    },
};

pub const DecodeError = error{
    UnexpectedToken,
    UnexpectedTokenAfterEnd,
    UnescapedControlChar,
    NumberIsTooBig,
    InvalidEscapeSequence,
    InvalidUTF16,
    TooNestedValue,
    UnexpectedEOF,
    EOF,
};

const StringState = enum {
    Inside,
    StartEscape,
    Unicode0,
    Unicode1,
    Unicode2,
    Unicode3,
};

const min_number: i54 = math.minInt(i54) + 1;
const max_number: i54 = math.maxInt(i54);
const min_exp: i11 = -1022;
const max_exp: i11 = 1023;

// Options represents Decoder options.
pub const Options = struct {
    const Self = @This();

    // allow_big_numbers specifies if numbers outside of range [-(2**53)+1, (2**53)-1] should be allowed.
    // TODO precisely specify rules for floats.
    allow_big_numbers: bool = true,

    // allow_incomplete_surrogates specifies if incomplete UTF-16 surrogate pair should be allowed.
    allow_incomplete_surrogates: bool = false,

    // validate_only specifies if only validation s to be performed.
    // If true, allocator can be omitted.
    validate_only: bool = false,

    // default returns default options (similar to strict, but allow big numbers).
    fn default() Self {
        return Self{};
    }

    // strict returns the strictest set of options which
    // is RFC8259-compliant.
    fn strict() Self {
        return Self{
            .allow_big_numbers = false,
            .allow_incomplete_surrogates = false,
        };
    }

    // tolerant returns the least strict set of options.
    fn tolerant() Self {
        return Self{
            .allow_big_numbers = true,
            .allow_incomplete_surrogates = true,
        };
    }
};

pub const Decoder = struct {
    /// slice contains JSON value to be parsed.
    slice: []const u8 = undefined,
    /// pos is current index in the provided slice.
    pos: usize = undefined,
    /// stack represents state of nested literals.
    /// bit=0 => inside array
    /// bit=1 => inside map
    stack: usize = 0,
    /// state contains inner state of DFA.
    state: State = undefined,
    /// stack_size represents stack size.
    stack_size: u64 = 0,
    options: Options = Options.default(),

    const Self = @This();
    const State = enum {
        ArrayValueSep = 0,
        ObjectValueSep = 1,

        TopLevel,

        ArrayValue,
        ArrayValueOrEnd,

        ObjectKey,
        NameSep,
        ObjectValue,
        ObjectKeyOrEnd,

        Finished,
    };

    pub fn init(slice: []const u8) Decoder {
        return initWithOptions(slice, Options{});
    }

    pub fn initWithOptions(slice: []const u8, opts: Options) Decoder {
        return Decoder{
            .slice = slice,
            .pos = 0,
            .state = .TopLevel,
            .options = opts,
        };
    }

    /// nextToken returns next token from the stream.
    pub fn nextToken(d: *Self) !Token {
        assert(d.pos <= d.slice.len);

        if (d.pos == d.slice.len)
            return if (d.state != .Finished)
                DecodeError.UnexpectedEOF
            else
                DecodeError.EOF;

        const b = try d.trimWhiteSpace();
        return switch (d.state) {
            .Finished => DecodeError.UnexpectedTokenAfterEnd,
            .TopLevel => try d.consumeValue(b, .Finished),
            .ArrayValue => try d.consumeValue(b, .ArrayValueSep),
            .ObjectValue => try d.consumeValue(b, .ObjectValueSep),
            .ArrayValueOrEnd => switch (b) {
                ']' => try d.popStack(Token.ArrayEnd),
                else => try d.consumeValue(b, .ArrayValueSep),
            },
            .ArrayValueSep => switch (b) {
                ',' => d.setState(.ArrayValue, Token.ValueSep),
                ']' => try d.popStack(Token.ArrayEnd),
                else => DecodeError.UnexpectedToken,
            },
            .ObjectKey => switch (b) {
                '"' => d.consumeString(.NameSep),
                else => DecodeError.UnexpectedToken,
            },
            .ObjectKeyOrEnd => switch (b) {
                '}' => try d.popStack(Token.ObjectEnd),
                '"' => try d.consumeString(.NameSep),
                else => DecodeError.UnexpectedToken,
            },
            .ObjectValueSep => switch (b) {
                ',' => d.setState(.ObjectKey, Token.ValueSep),
                '}' => try d.popStack(Token.ObjectEnd),
                else => DecodeError.UnexpectedToken,
            },
            .NameSep => switch (b) {
                ':' => d.setState(.ObjectValue, Token.NameSep),
                else => DecodeError.UnexpectedToken,
            },
        };
    }

    inline fn trimWhiteSpace(d: *Self) !u8 {
        for (d.slice[d.pos..]) |c, i| {
            switch (c) {
                0x20, 0x09, 0x0A, 0x0D => continue,
                else => {
                    d.pos += i + 1;
                    return c;
                },
            }
        }
        d.pos = d.slice.len;
        return if (d.state != .Finished)
            DecodeError.UnexpectedEOF
        else
            DecodeError.EOF;
    }

    inline fn setState(d: *Self, s: State, tok: Token) Token {
        d.state = s;
        return tok;
    }

    inline fn popStack(d: *Self, tok: Token) !Token {
        switch (d.stack_size) {
            0 => return DecodeError.UnexpectedToken,
            1 => {
                d.stack_size = 0;
                d.state = State.Finished;
            },
            else => {
                d.stack >>= 1;
                d.stack_size -= 1;
                d.state = @intToEnum(State, @intCast(u2, d.stack & 1));
            },
        }
        return tok;
    }

    fn consumeValue(d: *Self, b: u8, s: State) !Token {
        return switch (b) {
            '[' => blk: {
                // Because 0 bit is used for arrays error is returned only
                // if object is too nested. This doesn't affect parsing correctness
                // because right shifts fill MSB with 0 again.
                if (@shlWithOverflow(usize, d.stack, 1, &d.stack))
                    return DecodeError.TooNestedValue;
                d.stack_size += 1;
                d.state = .ArrayValueOrEnd;
                break :blk Token.ArrayBegin;
            },
            '{' => blk: {
                if (@shlWithOverflow(usize, d.stack, 1, &d.stack))
                    return DecodeError.TooNestedValue;
                d.stack |= 1;
                d.stack_size += 1;
                d.state = .ObjectKeyOrEnd;
                break :blk Token.ObjectBegin;
            },
            '"' => try d.consumeString(s),
            '-', '0'...'9' => try d.consumeNumber(s),
            'n' => try d.ensureToken("ull", s, Token.Null),
            't' => try d.ensureToken("rue", s, Token.True),
            'f' => try d.ensureToken("alse", s, Token.False),
            else => DecodeError.UnexpectedToken,
        };
    }

    inline fn ensureToken(d: *Self, comptime s: []const u8, next_state: State, ret: Token) !Token {
        if ((d.pos + s.len) > d.slice.len)
            return DecodeError.UnexpectedEOF;

        inline for (s) |c, i| {
            if (c != d.slice[d.pos + i])
                return DecodeError.UnexpectedToken;
        }
        d.pos += s.len;
        d.state = next_state;
        return ret;
    }

    fn consumeString(d: *Self, nextState: State) !Token {
        const start = d.pos;
        const s = d.slice;
        var state = StringState.Inside;
        var char1: ?u16 = null;
        var char2: ?u16 = null;
        var finished = false;
        var escaped = false;
        var size: usize = 0;
        for (s[start..]) |c, i| {
            switch (state) {
                .Inside => switch (c) {
                    0x00...0x1F => return DecodeError.UnescapedControlChar,
                    '\\' => state = .StartEscape,
                    '"' => {
                        if (char1 != null) {
                            if (!d.options.allow_incomplete_surrogates)
                                return DecodeError.InvalidUTF16;
                            const code_point = @as(u21, char1.?);
                            size += @as(usize, try unicode.utf8CodepointSequenceLength(code_point));
                        }
                        d.pos += i + 1;
                        finished = true;
                        break;
                    },
                    else => size += 1,
                },
                .StartEscape => {
                    escaped = true;
                    switch (c) {
                        '\\', '/', 'b', 'f', 'n', 'r', 't', '"' => {
                            size += 1;
                            state = .Inside;
                        },
                        'u' => state = .Unicode0,
                        else => return DecodeError.InvalidEscapeSequence,
                    }
                },
                .Unicode0 => {
                    char2 = @as(u16, try fromHexDigit(c));
                    state = .Unicode1;
                },
                .Unicode1 => {
                    char2 = (char2.? << 4) + @as(u16, try fromHexDigit(c));
                    state = .Unicode2;
                },
                .Unicode2 => {
                    char2 = (char2.? << 4) + @as(u16, try fromHexDigit(c));
                    state = .Unicode3;
                },
                .Unicode3 => {
                    char2 = (char2.? << 4) + @as(u16, try fromHexDigit(c));
                    if (char1 != null) {
                        if (isBetween(char2.?, low_surr_start, low_surr_end)) {
                            const codePoint = codePointFromSurrogates(char1.?, char2.?);
                            size += @as(usize, try unicode.utf8CodepointSequenceLength(codePoint));
                        } else if (!d.options.allow_incomplete_surrogates) {
                            return DecodeError.InvalidUTF16;
                        }
                        char1 = null;
                    } else if (isBetween(char2.?, high_surr_start, high_surr_end)) {
                        char1 = char2.?;
                    } else if (isBetween(char2.?, low_surr_start, low_surr_end)) {
                        if (!d.options.allow_incomplete_surrogates)
                            return DecodeError.InvalidUTF16;
                    } else {
                        size += 1;
                    }
                    char2 = null;
                    state = .Inside;
                },
            }
        }
        if (!finished) {
            return DecodeError.UnexpectedEOF;
        }
        d.state = nextState;
        return Token{
            .String = .{
                .count = size,
                .start = start,
                .escaped = escaped,
            },
        };
    }

    fn consumeNumber(d: *Self, nextState: State) !Token {
        assert(d.pos > 0);
        const NumState = enum {
            Start,
            IntOr0,
            Int,
            AfterInt,
            StartFrac,
            FracInt,
            StartExp,
            StartExpInt,
            ExpInt,
        };

        const start = d.pos - 1;
        const s = d.slice[start..];
        var num: usize = 0;
        var exp: isize = 0;
        var is_exp_negative: bool = false;
        var state = NumState.Start;
        loop: for (s) |c, i| {
            switch (state) {
                .Start => switch (c) {
                    '-' => state = .IntOr0,
                    '0' => state = .AfterInt,
                    '1'...'9' => {
                        num = c - '0';
                        state = .Int;
                    },
                    else => return DecodeError.UnexpectedToken,
                },
                .IntOr0 => switch (c) {
                    '0' => state = .AfterInt,
                    '1'...'9' => {
                        num = c - '0';
                        state = .Int;
                    },
                    else => return DecodeError.UnexpectedToken,
                },
                .Int => switch (c) {
                    '0'...'9' => {
                        const is_big = @mulWithOverflow(usize, num, 10, &num) or @addWithOverflow(usize, num, c - '0', &num);
                        if (!d.options.allow_big_numbers and is_big)
                            return DecodeError.NumberIsTooBig;

                        state = .Int;
                    },
                    '.' => state = .StartFrac,
                    'e', 'E' => state = .StartExp,
                    else => {
                        d.pos += i - 1;
                        break :loop;
                    },
                },
                .AfterInt => switch (c) {
                    '.' => state = .StartFrac,
                    'e', 'E' => state = .StartExp,
                    else => {
                        d.pos += i - 1;
                        break :loop;
                    },
                },
                .StartFrac => switch (c) {
                    '0'...'9' => state = .FracInt,
                    else => return DecodeError.UnexpectedToken,
                },
                .FracInt => switch (c) {
                    '0'...'9' => state = .FracInt,
                    'e', 'E' => state = .StartExp,
                    else => {
                        d.pos += i - 1;
                        break :loop;
                    },
                },
                .StartExp => switch (c) {
                    '+' => state = .StartExpInt,
                    '-' => {
                        is_exp_negative = true;
                        state = .StartExpInt;
                    },
                    '0'...'9' => {
                        exp = c - '0';
                        state = .ExpInt;
                    },
                    else => return DecodeError.UnexpectedToken,
                },
                .StartExpInt => switch (c) {
                    '0'...'9' => {
                        if (!d.options.allow_big_numbers and (@mulWithOverflow(isize, exp, 10, &exp) or @addWithOverflow(isize, exp, c - '0', &exp))) {
                            return DecodeError.NumberIsTooBig;
                        }
                        state = .ExpInt;
                    },
                    else => return DecodeError.UnexpectedToken,
                },
                .ExpInt => switch (c) {
                    '0'...'9' => {
                        if (!d.options.allow_big_numbers and (@mulWithOverflow(isize, exp, 10, &exp) or @addWithOverflow(isize, exp, c - '0', &exp))) {
                            return DecodeError.NumberIsTooBig;
                        }
                        state = .ExpInt;
                    },
                    else => {
                        d.pos += i - 1;
                        break :loop;
                    },
                },
            }
        } else {
            switch (state) {
                .ExpInt, .FracInt, .Int, .AfterInt => d.pos = start + s.len,
                else => return DecodeError.UnexpectedEOF,
            }
        }
        if (!d.options.allow_big_numbers) {
            if (!isBetween(num, 0, max_number)) {
                return DecodeError.NumberIsTooBig;
            }
            if (is_exp_negative) {
                exp = -exp;
            }
            if (!isBetween(exp, min_exp, max_exp)) {
                return DecodeError.NumberIsTooBig;
            }
        }
        d.state = nextState;
        return Token{
            .Number = .{
                .start = start,
                .count = d.pos - start,
                .is_float = false,
            },
        };
    }
};

const high_surr_start = 0xD800;
const high_surr_end = 0xDBFF;
const low_surr_start = 0xDC00;
const low_surr_end = 0xDFFF;

// codePointFromSurrogates returns unicode codepoint from UTF-16 surrogate pair.
inline fn codePointFromSurrogates(high: u16, low: u16) u21 {
    return 0x1_0000 +
        @as(u21, (high - high_surr_start) << 10) +
        @as(u21, low - low_surr_start);
}

inline fn isBetween(a: anytype, min: @TypeOf(a), max: @TypeOf(a)) bool {
    comptime const type_info = @typeInfo(@TypeOf(a));
    comptime assert(type_info == .Int or type_info == .Float);

    return min <= a and a <= max;
}

inline fn fromHexDigit(b: u8) DecodeError!u8 {
    return switch (b) {
        'a'...'h' => 10 + b - 'a',
        'A'...'H' => 10 + b - 'A',
        '0'...'9' => b - '0',
        else => DecodeError.InvalidEscapeSequence,
    };
}

inline fn fromHex(num: [4]u8) !u16 {
    var d: u16 = 0;
    d += @as(u16, try fromHexDigit(num[0]));
    d <<= 4;
    d += @as(u16, try fromHexDigit(num[1]));
    d <<= 4;
    d += @as(u16, try fromHexDigit(num[2]));
    d <<= 4;
    d += @as(u16, try fromHexDigit(num[3]));
    return d;
}

fn okString(d: *Decoder, str: []const u8, json: []const u8) void {
    const actual = d.nextToken() catch unreachable;
    const actualStr = actual.String.get(std.testing.allocator, json) catch unreachable;
    defer std.testing.allocator.free(actualStr);
    testing.expectEqualStrings(str, actualStr);
}

fn okNumber(d: *Decoder, num: isize, js: []const u8) void {
    const actual = d.nextToken() catch unreachable;
    const actualNum = actual.Number.getInt(isize, js) catch unreachable;
    testing.expect(num == actualNum);
}

fn okNext(d: *Decoder, exp: Token) void {
    const actual = d.nextToken() catch unreachable;
    testing.expectEqual(exp, actual);
}

fn errorNext(d: *Decoder, err: anyerror) void {
    testing.expectError(err, d.nextToken());
}

test "empty" {
    var js = "";
    var d: *Decoder = &Decoder.init(js);
    errorNext(d, DecodeError.UnexpectedEOF);
}

test "number, negative exp" {
    var js = "1e-2";
    var d: *Decoder = &Decoder.init(js);
    const actual = d.nextToken() catch unreachable;
    const actualNum = actual.Number.getFloat(f64, js) catch unreachable;
    testing.expect(1e-2 == actualNum);
}

test "array, garbage after" {
    var js =
        \\[] "x"
    ;
    var d: *Decoder = &Decoder.init(js);
    okNext(d, Token.ArrayBegin);
    okNext(d, Token.ArrayEnd);
    errorNext(d, DecodeError.UnexpectedTokenAfterEnd);
}

test "handle whitespace" {
    var js = "  [  null  ]  ";
    var d: *Decoder = &Decoder.init(js);
    okNext(d, Token.ArrayBegin);
    okNext(d, Token.Null);
    okNext(d, Token.ArrayEnd);
    errorNext(d, DecodeError.EOF);
}

test "string space" {
    var js = "\" \"";
    var d: *Decoder = &Decoder.init(js);
    okString(d, " ", js);
}

test "escaped string" {
    var js = "[\"a\\nb\"]";
    var d: *Decoder = &Decoder.init(js);
    okNext(d, Token.ArrayBegin);
    okString(d, "a\nb", js);
    okNext(d, Token.ArrayEnd);
}

fn testDecode(s: []const u8, opts: Options, expected: anyerror) void {
    var d: *Decoder = &Decoder.initWithOptions(s, opts);
    while (d.nextToken()) {} else |e| {
        testing.expectEqual(expected, e);
    }
}

test "surrogate pairs utf16" {
    testDecode("\"\\uD83D\\uDC69\"", Options.strict(), DecodeError.EOF);
    testDecode("\"\\uD83D\"", Options.strict(), DecodeError.InvalidUTF16);
    testDecode("\"\\uD83D\"", Options.tolerant(), DecodeError.EOF);
    testDecode("\"\\uDC69\"", Options.strict(), DecodeError.InvalidUTF16);
    testDecode("\"\\uDC69\"", Options.tolerant(), DecodeError.EOF);
}

test "high surrogate at the end of the string" {
    var js = "\"\\uD83D\"";
    var d: *Decoder = &Decoder.initWithOptions(js, Options.tolerant());
    okString(d, "\u{d83d}", js);
}

test "number: big interer" {
    testDecode("9007199254740991", Options.strict(), DecodeError.EOF);
    testDecode("9007199254740992", Options.strict(), DecodeError.NumberIsTooBig);
    testDecode("9007199254740992", Options.tolerant(), DecodeError.EOF);
    testDecode("-9007199254740991", Options.strict(), DecodeError.EOF);
    testDecode("-9007199254740992", Options.strict(), DecodeError.NumberIsTooBig);
    testDecode("-9007199254740992", Options.tolerant(), DecodeError.EOF);
}

test "number: big exponent" {
    testDecode("1e1023", Options.strict(), DecodeError.EOF);
    testDecode("1e1024", Options.strict(), DecodeError.NumberIsTooBig);
    testDecode("1e1024", Options.tolerant(), DecodeError.EOF);
    testDecode("1e-1022", Options.strict(), DecodeError.EOF);
    testDecode("1e-1023", Options.strict(), DecodeError.NumberIsTooBig);
    testDecode("1e-1023", Options.tolerant(), DecodeError.EOF);
}

test "raw tokens" {
    var js = "[true,false,null]";
    var d: *Decoder = &Decoder.init(js);
    okNext(d, Token.ArrayBegin);
    okNext(d, Token.True);
    okNext(d, Token.ValueSep);
    okNext(d, Token.False);
    okNext(d, Token.ValueSep);
    okNext(d, Token.Null);
    okNext(d, Token.ArrayEnd);
}

test "basic decoder functionality" {
    const js = "[\"abc\",123,\"a\",{\"key\":\"value\",\"x\":[1,2]}]";
    var d: *Decoder = &Decoder.init(js);
    okNext(d, Token.ArrayBegin);
    okString(d, "abc", js);
    okNext(d, Token.ValueSep);
    okNumber(d, 123, js);
    okNext(d, Token.ValueSep);
    okString(d, "a", js);
    okNext(d, Token.ValueSep);
    okNext(d, Token.ObjectBegin);
    okString(d, "key", js);
    okNext(d, Token.NameSep);
    okString(d, "value", js);
    okNext(d, Token.ValueSep);
    okString(d, "x", js);
    okNext(d, Token.NameSep);
    okNext(d, Token.ArrayBegin);
    okNumber(d, 1, js);
    okNext(d, Token.ValueSep);
    okNumber(d, 2, js);
    okNext(d, Token.ArrayEnd);
    okNext(d, Token.ObjectEnd);
    okNext(d, Token.ArrayEnd);
    errorNext(d, DecodeError.EOF);
}

test "unexpected array end" {
    var d: *Decoder = &Decoder.init("[}");
    okNext(d, Token.ArrayBegin);
    errorNext(d, DecodeError.UnexpectedToken);
}

test "unexpected object end" {
    var d: *Decoder = &Decoder.init("{]");
    okNext(d, Token.ObjectBegin);
    errorNext(d, DecodeError.UnexpectedToken);
}

fn testParseFile(filename: []const u8, s: []const u8, opts: Options) void {
    var d: *Decoder = &Decoder.initWithOptions(s, opts);
    while (d.nextToken()) |_| {} else |err| {
        switch (filename[0]) {
            'y', 'p' => if (err != error.EOF) {
                print("file: {}, error: {}\n", .{ filename, err });
                unreachable;
            },
            'n', 'f' => if (err == error.EOF) {
                print("file: {}, error: {}\n", .{ filename, err });
                unreachable;
            },
            else => {
                // For now just print to check expected errors.
                print("file: {}, error: {}\n", .{ filename, err });
            },
        }
    }
}

// Tests from http://www.json.org/JSON_checker/ .
test "JSON_Checker" {
    const path = "./tests/JSON_checker/";
    const fd = try std.os.open(path, 0, std.os.O_RDONLY);
    const dir = &std.fs.Dir{ .fd = fd };
    defer dir.close();

    const it = &dir.iterate();
    const a = testing.allocator;
    while (try it.next()) |entry| {
        if (mem.eql(u8, entry.name, "fail1.json")) {
            // Excluded, because strings are valid payloads as of RFC 8259.
            continue;
        }
        if (mem.eql(u8, entry.name, "fail18.json")) {
            // Excluded, because depth is not specified in RFC 8259.
            continue;
        }
        var f = try dir.readFileAlloc(a, entry.name, 300 * 1024 * 1024);
        defer a.free(f);
        testParseFile(entry.name, f, Options.strict());
    }
}

// Tests from https://github.com/nst/JSONTestSuite .
test "JSONTestSuite" {
    const path = "./JSONTestSuite/test_parsing/";
    const fd = try std.os.open(path, 0, std.os.O_RDONLY);
    const dir = &std.fs.Dir{ .fd = fd };
    defer dir.close();

    const it = &dir.iterate();
    const a = testing.allocator;
    while (try it.next()) |entry| {
        var f = try dir.readFileAlloc(a, entry.name, 300 * 1024 * 1024);
        defer a.free(f);
        //testParseFile(entry.name, f, Options.strict());
        testParseFile(entry.name, f, Options.tolerant());
    }
}
