const std = @import("std");

pub fn Bitset(comptime N: usize) type {
    return struct {
        bitset: [N]bool = undefined,

        set_bits: [N]usize = undefined,
        set_len: usize = 0,

        const size = N;

        const Self = @This();

        pub fn initEmpty() Self {
            const bitset = [_]bool{false} ** N;
            return .{ .bitset = bitset };
        }

        pub fn initFull() Self {
            const bitset = [_]bool{true} ** N;
            var set_bits: [N]usize = undefined;
            for (0..N) |i| {
                set_bits[i] = i;
            }
            return .{ .bitset = bitset, .set_bits = set_bits, .set_len = N };
        }

        pub fn clone(self: Self) Self {
            var bitset = [_]bool{false} ** N;
            var set_bits: [N]usize = undefined;
            for (0..self.set_len) |i| {
                const b = self.set_bits[i];
                set_bits[i] = b;
                bitset[b] = true;
            }

            return .{
                .bitset = bitset,
                .set_bits = set_bits,
                .set_len = self.set_len,
            };
        }

        pub fn set(self: *Self, idx: usize) void {
            if (self.bitset[idx]) {
                return;
            }
            self.bitset[idx] = true;

            self.set_bits[self.set_len] = idx;
            self.set_len += 1;

            std.mem.sort(
                usize,
                self.set_bits[0..self.set_len],
                {},
                comptime std.sort.asc(usize),
            );
        }

        pub fn unset(self: *Self, idx: usize) void {
            if (!self.bitset[idx]) {
                return;
            }
            self.bitset[idx] = false;
            self.set_len -= 1;

            for (self.set_bits, 0..) |b, i| {
                if (b == idx) {
                    self.set_bits[i] = self.set_bits[self.set_len];
                }
            }
            std.mem.sort(
                usize,
                self.set_bits[0..self.set_len],
                {},
                comptime std.sort.asc(usize),
            );
        }

        pub fn isSet(self: Self, i: usize) bool {
            return i < N and self.bitset[i];
        }

        pub fn isOtherSubset(self: Self, other: Bitset(N)) bool {
            if (self.set_len < other.set_len) return false;

            for (other.set_bits[0..other.set_len]) |b| {
                if (!self.isSet(b)) return false;
            }
            return true;
        }
    };
}

test "bitset" {
    {
        var bitset = Bitset(10).initEmpty();

        bitset.set(5);
        bitset.set(8);
        bitset.set(0);
        bitset.set(3);
        bitset.set(5);
        bitset.set(9);
        bitset.set(1);

        try std.testing.expect(std.mem.eql(usize, bitset.set_bits[0..bitset.set_len], &[_]usize{ 0, 1, 3, 5, 8, 9 }));
        try std.testing.expectEqual(6, bitset.set_len);

        bitset.unset(0);
        bitset.unset(0);
        try std.testing.expect(std.mem.eql(usize, bitset.set_bits[0..bitset.set_len], &[_]usize{ 1, 3, 5, 8, 9 }));
        bitset.unset(5);
        try std.testing.expect(std.mem.eql(usize, bitset.set_bits[0..bitset.set_len], &[_]usize{ 1, 3, 8, 9 }));

        try std.testing.expectEqual(4, bitset.set_len);
        bitset.unset(9);
        try std.testing.expect(std.mem.eql(usize, bitset.set_bits[0..bitset.set_len], &[_]usize{ 1, 3, 8 }));
    }
    {
        const bitset = Bitset(20).initFull();
        try std.testing.expectEqual(20, bitset.set_len);
        try std.testing.expect(std.mem.eql(
            usize,
            &bitset.set_bits,
            &[_]usize{ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19 },
        ));
    }
}
