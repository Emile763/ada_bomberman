with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Entity;    use Entity;
with Map_Types; use Map_Types;
use Ada.Containers;
with Stack;

package Map is

    function Hash (Key : T_Coord) return Hash_Type
    is (Hash_Type'Mod (Hash_Type'Mod (Key.X) * Hash_Type'Mod (16#1f1f1f1f#))
        xor Hash_Type'Mod (Key.Y));
    package P_Coord_Set is new
       Ada.Containers.Hashed_Sets
          (Element_Type        => T_Coord,
           Hash                => Hash,
           Equivalent_Elements => "=");
    package P_Coord_Stack is new Stack (T_Data => T_Coord);
    type T_Map
       (Width  : Positive;
        Height : Positive)
    is
       tagged private;

    procedure Set_Cell
       (This : in out T_Map; X, Y : Positive; Cell : T_Cell_Type);
    function Get_Cell (This : T_Map; X, Y : Positive) return T_Cell_Type;

    procedure Teleport_Player_To_Random_Cell
       (This : in out T_Map; Index : Positive);

    procedure Randomize (This : in out T_Map);

    procedure Update
       (This : in out T_Map; Updated_Cells_Set : in out P_Coord_Set.Set);

    procedure Add_Bomb (This : in out T_Map; Pos_X, Pos_Y : Positive);

    procedure Set_Drawing_Offset
       (This : in out T_Map; X_Offset, Y_Offset : Integer);

    procedure Set_Player_Instruction
       (This        : in out T_Map;
        Index       : Positive;
        Instruction : T_Player_Instructions);
    procedure Add_Player (This : in out T_Map; Index : Positive);
    procedure Remove_Player (This : in out T_Map; Index : Positive);
    function Get_Number_Of_Alive_Players (This : T_Map) return Integer;

private
    function Hash (Key : Positive) return Hash_Type
    is (Ada.Containers.Hash_Type (Key));

    package P_Entity_Vector is new
       Ada.Containers.Vectors
          (Index_Type   => Positive,
           Element_Type => T_Entity_Access);
    package P_Player_Map is new
       Ada.Containers.Hashed_Maps
          (Key_Type        => Positive,
           Element_Type    => T_Player,
           Hash            => Hash,
           Equivalent_Keys => "=");

    type T_Visited_Grid is
       array (Positive range <>, Positive range <>) of Boolean;
    type T_Grid is array (Positive range <>, Positive range <>) of T_Cell_Type;
    type T_Map
       (Width  : Positive;
        Height : Positive)
    is tagged record

        Inert_Grid : T_Grid (1 .. Width, 1 .. Height) :=
           [others => [others => EMPTY]];
        Grid       : T_Grid (1 .. Width, 1 .. Height) :=
           [others => [others => EMPTY]];
        Players    : P_Player_Map.Map;
        Entities   : P_Entity_Vector.Vector;

        Drawing_X_Offset : Integer := 0;
        Drawing_Y_Offset : Integer := 0;

    end record;

end Map;
