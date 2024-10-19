const std = @import("std");
const Bitset = @import("bitset.zig").Bitset;

pub const EntityID = usize;
const ComponentID = usize;
const ArchetypeID = usize;
const ComponentData = std.AutoArrayHashMapUnmanaged(ComponentID, []const u8);

const TypeInfo = struct {
    name: []const u8,
    size: usize,
};

// type erased component column
const Column = struct {
    bytes: std.ArrayListUnmanaged(u8) = .{},
    type_info: TypeInfo,
    count: usize = 0,

    pub fn deinit(self: *Column, allocator: std.mem.Allocator) void {
        self.bytes.deinit(allocator);
    }

    pub fn add(
        self: *Column,
        allocator: std.mem.Allocator,
        value: anytype,
    ) !usize {
        const bytes = std.mem.asBytes(&value);
        return self.addBytes(allocator, bytes);
    }

    pub fn addBytes(
        self: *Column,
        allocator: std.mem.Allocator,
        bytes: []const u8,
    ) !usize {
        try self.bytes.appendSlice(allocator, bytes);
        self.count += 1;
        return self.count - 1;
    }

    pub fn update(
        self: *Column,
        value: anytype,
        row: usize,
    ) !void {
        if (row >= self.count) {
            return error.RowNotPresent;
        }

        const bytes = std.mem.asBytes(&value);
        const element_size = self.type_info.size;
        const offset = row * element_size;
        std.debug.print("my name is {s}\n", .{self.type_info.name});
        @memcpy(self.bytes.items[offset .. offset + element_size], bytes.ptr);
    }

    pub fn remove(self: *Column, index: usize) void {
        if (index >= self.count) {
            return;
        }

        self.count -= 1;
        const element_size = self.type_info.size;
        const offset = element_size * index;
        const last_element = element_size * self.count;
        if (offset != last_element) {
            @memcpy(
                self.bytes.items[offset .. offset + element_size],
                self.bytes.items[last_element .. last_element + element_size],
            );
        }
        self.bytes.items.len -= element_size;
    }

    pub fn get(self: *Column, index: usize) ?[]u8 {
        if (index >= self.count) {
            return null;
        }

        const element_size = self.type_info.size;
        const offset = element_size * index;
        return self.bytes.items[offset .. offset + element_size];
    }
};

pub fn ECS(comptime components: []const type) type {
    const N = components.len + 1;

    const component_map = blk: {
        var component_id_map_: [N]TypeInfo = undefined;
        component_id_map_[0] = TypeInfo{ .size = 0, .name = "void_type" };

        for (components, 0..) |component, i| {
            const name = @typeName(component);
            const size = @sizeOf(component);
            component_id_map_[i + 1] = TypeInfo{ .name = name, .size = size };
        }
        break :blk component_id_map_;
    };

    return struct {
        entity_counter: usize = 0,
        /// A mapping of entity IDs (array indices) to its archetype
        entity_index: std.AutoHashMapUnmanaged(EntityID, Record) = .{},
        /// A mapping of archetype hash to their storage
        archetype_index: std.HashMapUnmanaged(
            Bitset(N),
            Archetype,
            BitsetContext,
            std.hash_map.default_max_load_percentage,
        ) = .{},

        allocator: std.mem.Allocator,

        const Self = @This();

        const Archetype = struct {
            type: Bitset(N),
            columns: std.AutoArrayHashMapUnmanaged(ComponentID, Column),
            entity_ids: std.ArrayListUnmanaged(EntityID) = .{},

            pub fn init(allocator: std.mem.Allocator, bitset: Bitset(N)) !Archetype {
                var columns: std.AutoArrayHashMapUnmanaged(ComponentID, Column) = .{};
                for (bitset.set_bits[0..bitset.set_len]) |i| {
                    try columns.put(
                        allocator,
                        i,
                        Column{ .type_info = component_map[i] },
                    );
                }
                return .{
                    .type = bitset,
                    .columns = columns,
                };
            }

            pub fn deinit(self: *Archetype, allocator: std.mem.Allocator) void {
                for (self.columns.values()) |*c| {
                    c.deinit(allocator);
                }
                self.entity_ids.deinit(allocator);
                self.columns.deinit(allocator);
            }

            pub fn add(
                self: *Archetype,
                allocator: std.mem.Allocator,
                entity: EntityID,
                data: ComponentData,
            ) !usize {
                if (self.type.set_len > 0) {
                    for (data.keys(), data.values()) |component_idx, d| {
                        var column = self.columns.getPtr(component_idx).?;
                        _ = try column.addBytes(allocator, d);
                    }
                }
                try self.entity_ids.append(allocator, entity);
                return self.entity_ids.items.len - 1;
            }

            pub fn move(
                self: *Archetype,
                allocator: std.mem.Allocator,
                new_archetype: *Archetype,
                component: anytype,
                index: usize,
            ) !Record {
                const bitset = self.type.set_bits[0..self.type.set_len];
                if (bitset.len > 0) {
                    for (self.columns.values(), bitset) |*column, b| {
                        const bytes = column.get(index) orelse continue;
                        if (new_archetype.columns.getPtr(b)) |new_column| {
                            _ = try new_column.addBytes(allocator, bytes);
                        }
                        column.remove(index);
                    }
                }
                if (@TypeOf(component) != @TypeOf(null)) {
                    const component_index = getComponentIndex(@TypeOf(component)).?;
                    const c = new_archetype.columns.getPtr(component_index).?;
                    const bytes = std.mem.asBytes(&component);
                    _ = try c.addBytes(allocator, bytes);
                }
                const entity = self.entity_ids.swapRemove(index);
                try new_archetype.entity_ids.append(allocator, entity);
                return Record{
                    .archetype = new_archetype,
                    .row = new_archetype.entity_ids.items.len - 1,
                };
            }
        };

        const BitsetContext = struct {
            pub fn hash(_: BitsetContext, bitset: Bitset(N)) u64 {
                // only the empty bitset set has a hash o 0
                if (bitset.set_len == 0) {
                    return 0;
                }

                if (bitset.set_bits[0] >= 64) {
                    return 1 << 63;
                }

                var acc: u64 = 0;
                for (bitset.set_bits[0..bitset.set_len]) |i| {
                    if (i >= 64) {
                        continue;
                    }

                    const p: u6 = @intCast(i);
                    acc += @as(u64, 1) << p;
                }

                return acc;
            }

            pub fn eql(_: BitsetContext, bitset1: Bitset(N), bitset2: Bitset(N)) bool {
                if (bitset1.set_len != bitset2.set_len) {
                    return false;
                }

                for (
                    bitset1.set_bits[0..bitset1.set_len],
                    bitset2.set_bits[0..bitset2.set_len],
                ) |b1, b2| {
                    if (b1 != b2) {
                        return false;
                    }
                }
                return true;
            }
        };

        const Record = struct {
            archetype: *Archetype,
            row: usize,
        };

        pub fn getComponentIndex(comptime Component: type) ?usize {
            const name = @typeName(Component);
            for (component_map, 0..) |type_info, i| {
                if (std.mem.eql(u8, type_info.name, name)) {
                    return i;
                }
            }
            return null;
        }

        pub fn getHash(_: Self, b: Bitset(N)) u64 {
            const bc: BitsetContext = .{};
            return bc.hash(b);
        }

        pub fn getComponentMap(_: Self) [N]TypeInfo {
            return component_map;
        }

        pub fn init(allocator: std.mem.Allocator) Self {
            return .{
                .allocator = allocator,
            };
        }

        pub fn deinit(self: *Self) void {
            var ai_iter = self.archetype_index.valueIterator();
            while (ai_iter.next()) |v| {
                v.deinit(self.allocator);
            }

            self.entity_index.deinit(self.allocator);
            self.archetype_index.deinit(self.allocator);
        }

        pub fn newEntity(self: *Self) !EntityID {
            self.entity_counter += 1;

            const bitset = Bitset(N).initEmpty();

            // get or create void archetype
            const void_archetype = self.archetype_index.getPtr(bitset) orelse blk: {
                var columns: std.AutoArrayHashMapUnmanaged(ComponentID, Column) = .{};
                const column = Column{ .type_info = TypeInfo{
                    .size = 0,
                    .name = "void_type",
                } };
                try columns.put(self.allocator, 0, column);

                const archetype = Archetype{ .type = bitset, .columns = columns };
                try self.archetype_index.put(self.allocator, bitset, archetype);
                break :blk self.archetype_index.getPtr(bitset).?;
            };

            // add to records
            const row = try void_archetype.add(self.allocator, self.entity_counter, .{});
            try self.entity_index.put(self.allocator, self.entity_counter, Record{
                .archetype = void_archetype,
                .row = row,
            });

            return self.entity_counter;
        }

        pub fn getComponent(self: Self, entity: EntityID, comptime Component: type) ?*Component {
            const record = self.entity_index.get(entity) orelse return null;
            const archetype = record.archetype;

            const component_index = getComponentIndex(Component) orelse return null;
            const c = archetype.columns.getPtr(component_index) orelse return null;
            const bytes = c.get(record.row) orelse return null;

            return @alignCast(std.mem.bytesAsValue(Component, bytes));
        }

        pub fn getComponents(
            self: Self,
            entity: EntityID,
            callback: anytype,
        ) !void {
            const record = self.entity_index.get(entity) orelse return error.EntityNotFound;
            const archetype = record.archetype;

            var values: std.meta.ArgsTuple(@TypeOf(callback)) = undefined;
            inline for (values, 0..) |v, i| {
                const type_info = @typeInfo(@TypeOf(v));
                const T = switch (type_info) {
                    .Pointer => |info| info.child,
                    else => @TypeOf(v),
                };

                if (T == EntityID) {
                    values[i] = entity;
                    continue;
                }

                const component_index = getComponentIndex(T) orelse
                    return error.ComponentNotPresent;
                const c = archetype.columns.getPtr(component_index) orelse
                    return error.ComponentNotInArchetype;
                const bytes = c.get(record.row) orelse return error.ErrorFetchingRow;

                const value: *T = @alignCast(std.mem.bytesAsValue(T, bytes));
                values[i] = switch (type_info) {
                    .Pointer => value,
                    else => value.*,
                };
            }

            @call(.auto, callback, values);
        }

        pub fn search(
            self: Self,
            callback: anytype,
        ) !void {
            var values: std.meta.ArgsTuple(@TypeOf(callback)) = undefined;
            var archetype_iters = self.archetype_index.valueIterator();

            var bitset = Bitset(N).initEmpty();
            inline for (values) |v| {
                const type_info = @typeInfo(@TypeOf(v));
                const T = switch (type_info) {
                    .Pointer => |info| info.child,
                    else => @TypeOf(v),
                };

                if (T == EntityID) {
                    continue;
                }

                const i = getComponentIndex(T) orelse return error.ComponentNotFound;
                bitset.set(i);
            }

            while (archetype_iters.next()) |archetype| {
                if (!archetype.type.isOtherSubset(bitset)) {
                    continue;
                }

                const rows: usize = archetype.entity_ids.items.len;
                for (0..rows) |row| {
                    var c_i: usize = 0;
                    inline for (values, 0..) |v, i| {
                        const type_info = @typeInfo(@TypeOf(v));
                        const T = switch (type_info) {
                            .Pointer => |info| info.child,
                            else => @TypeOf(v),
                        };

                        if (T == EntityID) {
                            values[i] = archetype.entity_ids.items[row];
                            continue;
                        }

                        const component_index = bitset.set_bits[c_i];
                        const c = archetype.columns.getPtr(component_index) orelse
                            return error.ComponentNotInArchetype;
                        const bytes = c.get(row) orelse return error.ErrorFetchingRow;

                        const value: *T = @alignCast(std.mem.bytesAsValue(T, bytes));
                        values[i] = switch (type_info) {
                            .Pointer => value,
                            else => value.*,
                        };
                        c_i += 1;
                    }
                    @call(.auto, callback, values);
                }
            }
        }

        fn move(
            self: *Self,
            entity: EntityID,
            component: anytype,
            bitset: Bitset(N),
            old_record: Record,
        ) !void {
            const new_archetype = self.archetype_index.getPtr(bitset) orelse blk: {
                const archetype = try Archetype.init(self.allocator, bitset);
                try self.archetype_index.put(self.allocator, bitset, archetype);
                break :blk self.archetype_index.getPtr(bitset).?;
            };

            const last_entity = old_record.archetype.entity_ids.getLast();
            try self.entity_index.put(self.allocator, last_entity, old_record);

            const new_record = try old_record.archetype.move(
                self.allocator,
                new_archetype,
                component,
                old_record.row,
            );
            try self.entity_index.put(self.allocator, entity, new_record);
        }

        pub fn addComponent(self: *Self, entity: EntityID, component: anytype) !void {
            const Component = @TypeOf(component);
            const component_index = getComponentIndex(Component) orelse
                return error.NoSuchComponent;

            const old_record = self.entity_index.get(entity) orelse
                return error.EntityNotPresent;
            var bitset = old_record.archetype.type.clone();
            // update component
            if (bitset.isSet(component_index)) {
                const archetype = old_record.archetype;
                var c = archetype.columns.getPtr(component_index).?;
                try c.update(component, old_record.row);
                return;
            }

            // get or create the new archetype
            bitset.set(component_index);
            try self.move(entity, component, bitset, old_record);
        }

        pub fn removeComponent(
            self: *Self,
            entity: EntityID,
            comptime Component: type,
        ) !void {
            const component_index = getComponentIndex(Component) orelse
                return error.NoSuchComponent;

            const old_record = self.entity_index.get(entity) orelse
                return error.EntityNotPresent;
            const old_bitset = old_record.archetype.type;
            var bitset = old_bitset.clone();
            // doesn't have component
            if (!bitset.isSet(component_index)) {
                return;
            }

            bitset.unset(component_index);
            try self.move(entity, null, bitset, old_record);
        }
    };
}
