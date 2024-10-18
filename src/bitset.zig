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
