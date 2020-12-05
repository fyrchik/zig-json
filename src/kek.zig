const std = @import("std");

const SomeEnum = enum {
    State1,
    State2,
};

fn advance(s: SomeEnum, c: u8) SomeEnum {
    return switch (s) {
        .State1 => switch (c) {
            //'0'...'9' => .State1,
            '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' => .State1,
            ','...',' => blk: {
                std.debug.print("HERE\n", .{});
                break :blk .State2;
            },
            else => return .State2,
        },
        .State2 => .State1,
    };
}

test "next state" {
    std.testing.expect(advance(.State1, ',') == SomeEnum.State2);
}
