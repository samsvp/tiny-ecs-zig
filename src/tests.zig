const std = @import("std");
const Bitset = @import("bitset.zig").Bitset;
const ECS = @import("ecs.zig").ECS;
const EntityID = @import("ecs.zig").EntityID;

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
        pub fn f2(e: EntityID, v: Vec2, n: Name) void {
            std.debug.print("entity {} v is {any} and name {s}\n", .{ e, v, n.name });
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

    std.debug.print("\nsearch 2:\n", .{});
    try ecs.search(f2);
    std.debug.print("search end\n\n", .{});

    try ecs.removeComponent(entity3, Vec2);
    if (ecs.getComponent(entity3, Name)) |v| {
        try std.testing.expectEqual(Name{ .name = "pablo" }, v.*);
    } else {
        try std.testing.expect(false);
    }

    if (ecs.getComponent(entity, Vec2)) |v| {
        try std.testing.expectEqual(Vec2{ .x = 2, .y = 4 }, v.*);
    } else {
        try std.testing.expect(false);
    }

    try ecs.removeComponent(entity, Vec2);
}
