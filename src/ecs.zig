const std = @import("std");
const Bitset = @import("bitset.zig").Bitset;

const EntityID = usize;
const ComponentID = usize;
const ArchetypeID = usize;

const TypeInfo = struct {
    name: []const u8,
    size: usize,
};

// type erased component column
const Column = struct {
    bytes: std.ArrayListUnmanaged(u8) = .{},
    entity_ids: std.ArrayListUnmanaged(EntityID) = .{},
    type_info: TypeInfo,
    count: usize = 0,

    pub fn deinit(self: *Column, allocator: std.mem.Allocator) void {
        self.bytes.deinit(allocator);
        self.entity_ids.deinit(allocator);
    }

    pub fn add(
        self: *Column,
        allocator: std.mem.Allocator,
        value: anytype,
        entity_id: EntityID,
    ) !usize {
        const bytes = std.mem.asBytes(&value);
        return self.addBytes(allocator, bytes, entity_id);
    }

    pub fn addBytes(
        self: *Column,
        allocator: std.mem.Allocator,
        bytes: []const u8,
        entity_id: EntityID,
    ) !usize {
        try self.bytes.appendSlice(allocator, bytes);
        try self.entity_ids.append(allocator, entity_id);
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
        @memcpy(self.bytes.items[offset .. offset + element_size], bytes.ptr);
    }

    pub fn remove(self: *Column, index: usize) !EntityID {
        if (index >= self.count) {
            return error.EntityNotFound;
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
        const e_id = self.entity_ids.getLast();
        _ = self.entity_ids.swapRemove(index);
        return e_id;
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

const ArchetypeRecord = struct {
    column: usize,
};

const ArchetypeMap = std.AutoHashMapUnmanaged(ArchetypeID, ArchetypeRecord);

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

        /// Find the archetypes for a component
        component_index: std.AutoHashMapUnmanaged(ComponentID, ArchetypeMap) = .{},

        allocator: std.mem.Allocator,

        const Self = @This();

        const Archetype = struct {
            type: Bitset(N),
            columns: std.AutoArrayHashMapUnmanaged(ComponentID, Column),

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
                for (v.columns.values()) |*column| {
                    column.deinit(self.allocator);
                }
                v.columns.deinit(self.allocator);
            }

            self.entity_index.deinit(self.allocator);
            self.archetype_index.deinit(self.allocator);
            self.component_index.deinit(self.allocator);
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

            var column = void_archetype.*.columns.getPtr(0).?;

            // add to records
            const row = try column.add(self.allocator, {}, self.entity_counter);
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
            blk: while (archetype_iters.next()) |archetype| {
                var rows: usize = undefined;
                inline for (values) |v| {
                    const type_info = @typeInfo(@TypeOf(v));
                    const T = switch (type_info) {
                        .Pointer => |info| info.child,
                        else => @TypeOf(v),
                    };
                    const component_index = getComponentIndex(T) orelse
                        return error.ComponentNotPresent;
                    const c = archetype.columns.getPtr(component_index) orelse
                        continue :blk;
                    rows = c.count;
                }

                for (0..rows) |row| {
                    inline for (values, 0..) |v, i| {
                        const type_info = @typeInfo(@TypeOf(v));
                        const T = switch (type_info) {
                            .Pointer => |info| info.child,
                            else => @TypeOf(v),
                        };
                        const component_index = getComponentIndex(T) orelse
                            return error.ComponentNotPresent;
                        const c = archetype.columns.getPtr(component_index) orelse
                            return error.ComponentNotInArchetype;
                        const bytes = c.get(row) orelse return error.ErrorFetchingRow;

                        const value: *T = @alignCast(std.mem.bytesAsValue(T, bytes));
                        values[i] = switch (type_info) {
                            .Pointer => value,
                            else => value.*,
                        };
                    }
                    @call(.auto, callback, values);
                }
            }
        }

        pub fn addComponent(self: *Self, entity: EntityID, component: anytype) !void {
            const Component = @TypeOf(component);
            const component_index = getComponentIndex(Component) orelse return error.NoSuchComponent;

            const old_record = self.entity_index.get(entity) orelse return error.EntityNotPresent;
            var bitset = old_record.archetype.type.clone();
            // update component
            if (bitset.isSet(component_index)) {
                const archetype = old_record.archetype;
                var c = archetype.columns.getPtr(component_index).?;
                try c.update(component, old_record.row);
                return;
            }

            var old_archetype = old_record.archetype;

            // remove from empty archetype
            if (bitset.set_len == 0) {
                var c = old_archetype.columns.getPtr(0).?;
                const moved_entity_id = try c.remove(old_record.row);
                if (moved_entity_id != entity) {
                    const moved_record = self.entity_index.getPtr(moved_entity_id).?;
                    moved_record.*.row = old_record.row;
                }
            }

            bitset.set(component_index);

            // get or create the new archetype
            var new_archetype = self.archetype_index.getPtr(bitset) orelse blk: {
                const archetype = try Archetype.init(self.allocator, bitset);
                try self.archetype_index.put(self.allocator, bitset, archetype);
                break :blk self.archetype_index.getPtr(bitset).?;
            };

            // remove components from old archetype to new one
            // and update moved recods rows.
            for (bitset.set_bits[0..bitset.set_len]) |i| {
                var c = old_archetype.columns.getPtr(i) orelse {
                    var new_column = new_archetype.columns.getPtr(i).?;
                    const row = try new_column.add(self.allocator, component, entity);
                    const new_record = Record{
                        .archetype = new_archetype,
                        .row = row,
                    };

                    try self.entity_index.put(self.allocator, entity, new_record);
                    continue;
                };
                // get column and move it to new archetype
                const bytes = c.get(old_record.row).?;
                var new_column = new_archetype.columns.getPtr(i).?;
                _ = try new_column.addBytes(self.allocator, bytes, entity);

                // remove column
                const moved_entity_id = try c.remove(old_record.row);
                if (moved_entity_id == entity) {
                    continue;
                }
                const moved_record = self.entity_index.getPtr(moved_entity_id).?;
                moved_record.row = old_record.row;
            }
        }

        pub fn removeComponent(self: *Self, entity: EntityID, comptime Component: type) !void {
            const component_index = getComponentIndex(Component) orelse return error.NoSuchComponent;

            const old_record = self.entity_index.get(entity) orelse return error.EntityNotPresent;
            const old_bitset = old_record.archetype.type;
            var bitset = old_bitset.clone();
            // doesn't have component
            if (!bitset.isSet(component_index)) {
                return;
            }

            var old_archetype = old_record.archetype;

            bitset.unset(component_index);

            var new_archetype = self.archetype_index.getPtr(bitset) orelse blk: {
                const archetype = try Archetype.init(self.allocator, bitset);
                try self.archetype_index.put(self.allocator, bitset, archetype);
                break :blk self.archetype_index.getPtr(bitset).?;
            };

            var maybe_row: ?usize = null;
            // remove components from old archetype to new one
            // and update moved records rows.
            for (old_bitset.set_bits[0..old_bitset.set_len]) |i| {
                var c = old_archetype.columns.getPtr(i).?;
                const bytes = c.get(old_record.row).?;

                defer {
                    // remove value from column
                    const moved_entity_id = c.remove(old_record.row) catch unreachable;
                    if (moved_entity_id != entity) {
                        const moved_record = self.entity_index.getPtr(moved_entity_id).?;
                        moved_record.row = old_record.row;
                    }
                }

                // if the column is not present, then ignore
                var new_column = new_archetype.columns.getPtr(i) orelse {
                    continue;
                };

                // move data to new archetype
                maybe_row = try new_column.addBytes(self.allocator, bytes, entity);
                if (maybe_row) |row| {
                    const new_record = Record{
                        .archetype = new_archetype,
                        .row = row,
                    };
                    try self.entity_index.put(self.allocator, entity, new_record);
                }
            }

            if (maybe_row) |_| {
                return;
            }

            // empty archetype
            var c = new_archetype.columns.getPtr(0).?;
            const new_record = Record{
                .archetype = new_archetype,
                .row = c.count,
            };

            try self.entity_index.put(self.allocator, entity, new_record);
            c.count += 1;
        }
    };
}

test "ecs" {
    const Vec2 = struct {
        x: f32,
        y: f32,
    };

    const Name = struct {
        name: []const u8,
    };

    const f = struct {
        pub fn f(v: *Vec2, n: *Name) void {
            std.debug.print("v is {any} and name {s}\n", .{ v.*, n.*.name });
        }
    }.f;
    const f2 = struct {
        pub fn f2(v: Vec2, n: Name) void {
            std.debug.print("v is {any} and name {s}\n", .{ v, n.name });
        }
    }.f2;

    const allocator = std.testing.allocator;

    var ecs = ECS(&[_]type{ Vec2, Name }).init(allocator);
    defer ecs.deinit();

    for (ecs.getComponentMap()) |c| {
        std.debug.print("name: {s}, size: {d}\n", .{ c.name, c.size });
    }

    const entity = try ecs.newEntity();
    const entity2 = try ecs.newEntity();
    const entity3 = try ecs.newEntity();
    const entity4 = try ecs.newEntity();

    try ecs.addComponent(entity, Vec2{ .x = 2, .y = 4 });
    try ecs.addComponent(entity, Name{ .name = "john" });
    try ecs.addComponent(entity3, Name{ .name = "scott" });
    try ecs.addComponent(entity3, Vec2{ .x = 5, .y = 1 });
    try ecs.addComponent(entity3, Name{ .name = "pablo" });
    try ecs.addComponent(entity2, Name{ .name = "maria" });
    try ecs.addComponent(entity2, Vec2{ .x = 4, .y = 8 });
    try ecs.addComponent(entity4, Vec2{ .x = 9, .y = 3 });
    try ecs.addComponent(entity4, Name{ .name = "xico" });

    if (ecs.getComponent(entity, Vec2)) |v| {
        try std.testing.expectEqual(Vec2{ .x = 2, .y = 4 }, v.*);
    } else {
        try std.testing.expect(false);
    }

    try ecs.getComponents(entity, f);
    try ecs.getComponents(entity, f2);

    try ecs.removeComponent(entity, Name);

    var entities_iter = ecs.entity_index.keyIterator();
    while (entities_iter.next()) |e| {
        const record = ecs.entity_index.get(e.*).?;
        std.debug.print(
            "hash: {}, archetype {}, row {} \n",
            .{ e.*, ecs.getHash(record.archetype.type), record.row },
        );
    }

    if (ecs.getComponent(entity, Vec2)) |v| {
        try std.testing.expectEqual(Vec2{ .x = 2, .y = 4 }, v.*);
    } else {
        try std.testing.expect(false);
    }

    if (ecs.getComponent(entity3, Vec2)) |v| {
        try std.testing.expectEqual(Vec2{ .x = 5, .y = 1 }, v.*);
    } else {
        try std.testing.expect(false);
    }

    var archetypes_iter = ecs.archetype_index.keyIterator();
    while (archetypes_iter.next()) |a| {
        const arc = ecs.archetype_index.get(a.*).?;
        for (arc.columns.values()) |c| {
            std.debug.print(
                "archetype {}, component {s}, row count {} \n",
                .{ ecs.getHash(arc.type), c.type_info.name, c.count },
            );
        }
    }
    std.debug.print("search:\n", .{});
    try ecs.search(f);
    std.debug.print("search end\n", .{});

    try ecs.removeComponent(entity, Vec2);
    try ecs.removeComponent(entity3, Vec2);
    if (ecs.getComponent(entity3, Name)) |v| {
        try std.testing.expectEqual(Name{ .name = "pablo" }, v.*);
    } else {
        try std.testing.expect(false);
    }
}
