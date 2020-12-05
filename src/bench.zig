const std = @import("std");
const jsonStd = std.json;
const jsonLib = @import("./main.zig");
const time = std.time;

const BenchCase = struct {
    name: []const u8,
    func: fn ([]const u8) bool,
};

var stdout = std.io.getStdOut().writer();

pub fn main() anyerror!void {
    const s =
        \\"123124151235151235\u0F12\u1345\n\n\t12knoh"
    ;
    const iter_num = 3000000;

    const testCases = [_]BenchCase{
        .{ .name = "lib", .func = jsonLib.validate },
        .{ .name = "std", .func = jsonStd.validate },
    };

    for (testCases) |tc| {
        var timer = try time.Timer.start();
        const start = timer.lap();
        var i: usize = 0;
        while (i < iter_num) {
            if (!tc.func(s)) {
                @panic("invalid result");
            }
            i += 1;
        }
        const end = timer.read();
        const elapsed_s = @intToFloat(f64, end - start) / time.ns_per_s;
        try stdout.print("{}: {}s\n", .{ tc.name, elapsed_s });
    }
}
