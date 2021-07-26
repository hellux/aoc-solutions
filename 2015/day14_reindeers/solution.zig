const std = @import("std");

const Reindeer = struct {
    speed: u32,
    time_move: u32,
    time_rest: u32,
};

pub fn parse_input(reindeers: [*]Reindeer) ![]Reindeer {
    const stdin = std.io.getStdIn().reader();

    var buf_line: [100]u8 = undefined;
    var i: usize = 0;

    while (try stdin.readUntilDelimiterOrEof(buf_line[0..], '\n')) |line| {
        var iter = std.mem.split(buf_line[0..], " ");
        _ = iter.next(); // <name>
        _ = iter.next(); // can
        _ = iter.next(); // run
        const speed = try std.fmt.parseUnsigned(u32, iter.next().?, 10);
        _ = iter.next(); // km/s
        _ = iter.next(); // for
        const time_move = try std.fmt.parseUnsigned(u32, iter.next().?, 10);
        _ = iter.next(); // seconds
        _ = iter.next(); // but
        _ = iter.next(); // must
        _ = iter.next(); // then
        _ = iter.next(); // rest
        _ = iter.next(); // for
        const time_rest = try std.fmt.parseUnsigned(u32, iter.next().?, 10);

        reindeers[i] = Reindeer{ .speed = speed, .time_move = time_move, .time_rest = time_rest };
        i += 1;
    }

    return reindeers[0..i];
}

pub fn distance(r: Reindeer, time: u32) u32 {
    const period = r.time_move + r.time_rest;
    const full_cycles = time / period;
    const dist_full_cycles = full_cycles * r.speed * r.time_move;
    const time_last = time % period;
    const dist_last = r.speed * std.math.min(r.time_move, time_last);

    return dist_full_cycles + dist_last;
}

pub fn part1(reindeers: []Reindeer) u32 {
    var max_distance: u32 = 0;

    for (reindeers) |r| {
        const d = distance(r, 2503);
        max_distance = std.math.max(d, max_distance);
    }

    return max_distance;
}

pub fn main() !void {
    var buf_reindeers: [20]Reindeer = undefined;
    const reindeers = try parse_input(&buf_reindeers);

    const stdout = std.io.getStdOut().writer();
    try stdout.print("{d}\n", .{part1(reindeers)});
}
