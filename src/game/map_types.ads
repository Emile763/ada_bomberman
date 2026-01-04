with Ada.Containers;
package Map_Types is
    --  Type of the cells used in the map
    type T_Cell_Type is
       (EMPTY,
        WALL,
        BREAKABLE_WALL,
        BOMB0,
        BOMB1,
        BOMB2,
        BOMB3,
        BOMB4,
        BOMB5,
        BOMB6,
        BOMB7,
        BOMB8,
        BOMB9,
        EXPLOSION,
        EXPLOSION_SOUTH,
        EXPLOSION_NORTH,
        EXPLOSION_EAST,
        EXPLOSION_WEST,
        PLAYER_IDLE,
        PLAYER_MOVING_UP,
        PLAYER_MOVING_DOWN,
        PLAYER_MOVING_LEFT,
        PLAYER_MOVING_RIGHT);
    subtype T_Entity_Type is T_Cell_Type range BOMB0 .. PLAYER_MOVING_RIGHT;
    subtype T_Inert_Type is T_Cell_Type range EMPTY .. BREAKABLE_WALL;
    subtype T_Explosion_Type is T_Cell_Type range EXPLOSION .. EXPLOSION_WEST;
    subtype T_Explosion_Direction is
       T_Explosion_Type range EXPLOSION_SOUTH .. EXPLOSION_WEST;

    type T_Coord is record
        X, Y : Integer;
    end record;

    type T_Player_Instructions is (UP, DOWN, RIGHT, LEFT, BOMB);
    function Hash
       (Player_Instructions : T_Player_Instructions)
        return Ada.Containers.Hash_Type;
end Map_Types;
