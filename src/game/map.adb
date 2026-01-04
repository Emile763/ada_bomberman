with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Numerics.Discrete_Random;

--  Need some refacto
package body Map is

    procedure Set_Cell
       (This : in out T_Map; X, Y : Positive; Cell : T_Cell_Type) is
    begin
        This.Grid (X, Y) := Cell;
    end Set_Cell;
    function Get_Cell (This : T_Map; X, Y : Positive) return T_Cell_Type is
    begin
        return This.Grid (X, Y);
    end Get_Cell;

    function Check_Coord_In_Grid
       (This : in out T_Map; Coord : T_Coord) return Boolean;
    function Check_Coord_In_Grid
       (This : in out T_Map; Coord : T_Coord) return Boolean is
    begin
        return
           Coord.X > 0
           and then Coord.X <= This.Width
           and then Coord.Y > 0
           and then Coord.Y <= This.Height;
    end Check_Coord_In_Grid;

    function Get_Empty_Cell_Coords
       (This : in out T_Map) return P_Coord_Set.Set;
    function Get_Empty_Cell_Coords (This : in out T_Map) return P_Coord_Set.Set
    is
        Coords : P_Coord_Set.Set;
    begin
        for X in 1 .. This.Width loop
            for Y in 1 .. This.Height loop
                if This.Grid (X, Y) = EMPTY then
                    Coords.Include ((X, Y));
                end if;
            end loop;
        end loop;
        return Coords;
    end Get_Empty_Cell_Coords;

    procedure Teleport_Player_To_Random_Cell
       (This : in out T_Map; Index : Positive)
    is
        package Random_Positive is new Ada.Numerics.Discrete_Random (Positive);
        Positive_Generator : Random_Positive.Generator;
        Empty_Cells        : constant P_Coord_Set.Set :=
           This.Get_Empty_Cell_Coords;
        Random_Index       : Positive;
        Coord              : T_Coord;
    begin
        Random_Positive.Reset (Positive_Generator);
        Random_Index :=
           Random_Positive.Random
              (Positive_Generator, 1, Positive (Empty_Cells.Length));

        declare
            Curs : P_Coord_Set.Cursor := Empty_Cells.First;
        begin
            for I in 1 .. Random_Index loop
                Curs := P_Coord_Set.Next (Curs);
            end loop;
            Coord := P_Coord_Set.Element (Curs);
        end;
        This.Players (Index).Pos_X := Coord.X;
        This.Players (Index).Pos_Y := Coord.Y;
        This.Players (Index).Last_Pos_X := Coord.X;
        This.Players (Index).Last_Pos_Y := Coord.Y;
    end Teleport_Player_To_Random_Cell;

    --  Create a new random map and resurrect all players in a random cell
    procedure Randomize (This : in out T_Map) is
        Visited     : P_Coord_Set.Set;
        Coord_Stack : P_Coord_Stack.T_Stack;

        package Random_Positive is new Ada.Numerics.Discrete_Random (Positive);
        Positive_Generator : Random_Positive.Generator;
    begin
        --  Delete all entities
        for Entity of This.Entities loop
            Free (Entity);
        end loop;
        This.Entities.Clear;

        --  Resurect all players
        for player_cursor in This.Players.Iterate loop
            This.Players (player_cursor).Alive := True;
            This.Teleport_Player_To_Random_Cell
               (P_Player_Map.Key (player_cursor));
        end loop;

        --  Generate a random Maze
        Random_Positive.Reset (Positive_Generator);
        Visited.Insert ((1, 1));
        Coord_Stack.Insert ((1, 1));
        This.Inert_Grid := [others => [others => WALL]];
        This.Inert_Grid (1, 1) := EMPTY;
        while not Coord_Stack.Is_Empty loop
            declare
                Current_Coord     : constant T_Coord := Coord_Stack.Pop;
                Next_Coord        : T_Coord;
                Valid_Next_Coords : array (1 .. 4) of T_Coord;
                Valid_Number      : Integer := 0;
                procedure Insert_Valid_Next_Coords;
                procedure Insert_Valid_Next_Coords is
                begin
                    if This.Check_Coord_In_Grid (Next_Coord)
                       and then not Visited.Contains (Next_Coord)
                    then
                        Valid_Number := Valid_Number + 1;
                        Valid_Next_Coords (Valid_Number) := Next_Coord;
                    end if;
                end Insert_Valid_Next_Coords;
            begin

                Next_Coord := (Current_Coord.X + 2, Current_Coord.Y);
                Insert_Valid_Next_Coords;
                Next_Coord := (Current_Coord.X - 2, Current_Coord.Y);
                Insert_Valid_Next_Coords;
                Next_Coord := (Current_Coord.X, Current_Coord.Y + 2);
                Insert_Valid_Next_Coords;
                Next_Coord := (Current_Coord.X, Current_Coord.Y - 2);
                Insert_Valid_Next_Coords;

                if Valid_Number > 0 then
                    Coord_Stack.Insert (Current_Coord);
                    Next_Coord :=
                       Valid_Next_Coords
                          (Random_Positive.Random
                              (Positive_Generator, 1, Valid_Number));
                    This.Inert_Grid
                       ((Current_Coord.X + Next_Coord.X) / 2,
                        (Current_Coord.Y + Next_Coord.Y) / 2) :=
                       EMPTY;
                    This.Inert_Grid (Next_Coord.X, Next_Coord.Y) := EMPTY;
                    Coord_Stack.Insert (Next_Coord);
                    Visited.Insert (Next_Coord);
                end if;

            end;
        end loop;

        --  Weakening of some Walls
        for X in 1 .. This.Width loop
            for Y in 1 .. This.Height loop
                if This.Inert_Grid (X, Y) = WALL then
                    declare
                        Random_Bool : constant Positive :=
                           Random_Positive.Random (Positive_Generator, 1, 10);
                    begin
                        if Random_Bool = 1 then
                            This.Inert_Grid (X, Y) := BREAKABLE_WALL;
                        end if;
                    end;
                end if;
            end loop;
        end loop;

    exception
        when E : Constraint_Error | Tasking_Error | Program_Error =>
            Put_Line (Exception_Message (E));
        when others =>
            Put_Line ("Erreur");
    end Randomize;

    function Check_Collision
       (Entity1 : T_Entity_Access; Entity2 : T_Entity_Access) return Boolean;
    function Check_Collision
       (Entity1 : T_Entity_Access; Entity2 : T_Entity_Access) return Boolean is
    begin
        return
           --  Handle passing through collisions
           --  E1 v  E2 ^
           --  E2 ^  E1 v
           (Entity1.Pos_X = Entity2.Last_Pos_X
            and then Entity1.Pos_Y = Entity2.Last_Pos_Y
            and then Entity1.Last_Pos_X = Entity2.Pos_X
            and then Entity1.Last_Pos_Y = Entity2.Pos_Y)
           or
           --  Handle direct collisions
           --  E1 v
           --
           --  E2 ^
           (Entity1.Pos_X = Entity2.Pos_X
            and then Entity1.Pos_Y = Entity2.Pos_Y);
    end Check_Collision;

    function Handle_Collisions
       (Entity : T_Entity_Access; Entity_List : in out P_Entity_Vector.Vector)
        return Boolean;
    function Handle_Collisions
       (Entity : T_Entity_Access; Entity_List : in out P_Entity_Vector.Vector)
        return Boolean
    is
        Index   : Positive := 1;
        Collide : Boolean := False;
    begin
        loop
            exit when Count_Type (Index) > Entity_List.Length;
            if Check_Collision
                  (Entity1 => Entity, Entity2 => Entity_List (Index))
            then
                Free (Entity_List (Index));
                Entity_List.Delete (Index);
                Collide := True;
            else
                Index := Index + 1;
            end if;
        end loop;
        return Collide;
    end Handle_Collisions;

    procedure Correct_Players_Position (This : in out T_Map);
    procedure Correct_Players_Position (This : in out T_Map) is
        use P_Player_Map;
    begin
        for Player of This.Players loop
            if not This.Check_Coord_In_Grid ((Player.Pos_X, Player.Pos_Y)) then
                Player.Pos_X := Player.Last_Pos_X;
                Player.Pos_Y := Player.Last_Pos_Y;
            else
                case This.Inert_Grid (Player.Pos_X, Player.Pos_Y) is
                    when WALL | BREAKABLE_WALL =>
                        Player.Pos_X := Player.Last_Pos_X;
                        Player.Pos_Y := Player.Last_Pos_Y;

                    when others                =>
                        null;
                end case;
            end if;

            for Cursor1 in This.Players.Iterate loop
                declare
                    Collision : Boolean;
                begin
                    loop
                        Collision := False;
                        for Cursor2 in This.Players.Iterate loop
                            if Cursor1 /= Cursor2
                               and then Check_Collision
                                           (This.Players.Reference (Cursor1)
                                               .Element,
                                            This.Players.Reference (Cursor2)
                                               .Element)
                            then
                                if This.Players (Cursor1).Pos_X
                                   /= This.Players (Cursor1).Last_Pos_X
                                   or This.Players (Cursor1).Pos_Y
                                      /= This.Players (Cursor1).Last_Pos_Y
                                then
                                    This.Players (Cursor1).Pos_X :=
                                       This.Players (Cursor1).Last_Pos_X;
                                    This.Players (Cursor1).Pos_Y :=
                                       This.Players (Cursor1).Last_Pos_Y;
                                else
                                    This.Players (Cursor2).Pos_X :=
                                       This.Players (Cursor2).Last_Pos_X;
                                    This.Players (Cursor2).Pos_Y :=
                                       This.Players (Cursor2).Last_Pos_Y;
                                end if;
                                Collision := True;
                            end if;
                        end loop;
                        --  If a collision occur, then
                        --  check again until no collision found
                        exit when not Collision;
                    end loop;
                end;
            end loop;
        end loop;
    end Correct_Players_Position;
    procedure Update_Players (This : in out T_Map);
    procedure Update_Players (This : in out T_Map) is
    begin
        for Player of This.Players loop
            if Player.Alive then
                if Player.Putting_Bomb then
                    This.Add_Bomb (Player.Pos_X, Player.Pos_Y);
                end if;
                Player.Putting_Bomb := False;
                Player.Update;
            end if;
        end loop;
        This.Correct_Players_Position;
    end Update_Players;

    procedure Update
       (This : in out T_Map; Updated_Cells_Set : in out P_Coord_Set.Set) is
    begin
        --  Update Entities
        for Elem of This.Entities loop
            Elem.Update;
        end loop;

        declare
            Index : Positive := 1;
        begin
            loop
                exit when Count_Type (Index) > This.Entities.Length;

                if not This.Entities (Index).Alive then
                    case This.Entities (Index).Entity_Type is
                        when BOMB0 .. BOMB9 =>
                            declare
                                X : constant Integer :=
                                   This.Entities (Index).Pos_X;
                                Y : constant Integer :=
                                   This.Entities (Index).Pos_Y;
                            begin
                                Free (This.Entities (Index));
                                This.Entities (Index) :=
                                   new T_Omnidirectional_Explosion'
                                      (Pos_X       => X,
                                       Pos_Y       => Y,
                                       Entity_Type => EXPLOSION,
                                       Last_Pos_X  => X,
                                       Last_Pos_Y  => Y,
                                       Alive       => True);
                                Index := Index + 1;
                            end;

                        when EXPLOSION      =>
                            declare
                                X : constant Integer :=
                                   This.Entities (Index).Pos_X;
                                Y : constant Integer :=
                                   This.Entities (Index).Pos_Y;
                            begin
                                Free (This.Entities (Index));
                                This.Entities.Delete (Index);
                                This.Entities.Append
                                   (new T_Unidirectional_Explosion'
                                       (Pos_X       => X,
                                        Pos_Y       => Y - 1,
                                        Entity_Type => EXPLOSION_NORTH,
                                        Last_Pos_X  => X,
                                        Last_Pos_Y  => Y,
                                        Alive       => True));
                                This.Entities.Append
                                   (new T_Unidirectional_Explosion'
                                       (Pos_X       => X,
                                        Pos_Y       => Y + 1,
                                        Entity_Type => EXPLOSION_SOUTH,
                                        Last_Pos_X  => X,
                                        Last_Pos_Y  => Y,
                                        Alive       => True));
                                This.Entities.Append
                                   (new T_Unidirectional_Explosion'
                                       (Pos_X       => X + 1,
                                        Pos_Y       => Y,
                                        Entity_Type => EXPLOSION_EAST,
                                        Last_Pos_X  => X,
                                        Last_Pos_Y  => Y,
                                        Alive       => True));
                                This.Entities.Append
                                   (new T_Unidirectional_Explosion'
                                       (Pos_X       => X - 1,
                                        Pos_Y       => Y,
                                        Entity_Type => EXPLOSION_WEST,
                                        Last_Pos_X  => X,
                                        Last_Pos_Y  => Y,
                                        Alive       => True));
                            end;

                        when others         =>
                            Free (This.Entities (Index));
                            This.Entities.Delete (Index);
                    end case;
                else
                    Index := Index + 1;
                end if;
            end loop;
        end;
        --  Check Explosion colisions with inert objects and map's boundaries
        declare
            Index : Positive := 1;
        begin
            while Count_Type (Index) <= This.Entities.Length loop
                if This.Entities (Index).Entity_Type in T_Explosion_Type then
                    if This.Entities (Index).Pos_X in 1 .. This.Width
                       and then This.Entities (Index).Pos_Y in 1 .. This.Height
                    then
                        case This.Inert_Grid
                                (Positive (This.Entities (Index).Pos_X),
                                 Positive (This.Entities (Index).Pos_Y))
                        is
                            when WALL           =>
                                Free (This.Entities (Index));
                                This.Entities.Delete (Index);

                            when BREAKABLE_WALL =>
                                This.Inert_Grid
                                   (Positive (This.Entities (Index).Pos_X),
                                    Positive (This.Entities (Index).Pos_Y)) :=
                                   EMPTY;
                                Free (This.Entities (Index));
                                This.Entities.Delete (Index);

                            when others         =>
                                Index := Index + 1;

                        end case;
                    else
                        Free (This.Entities (Index));
                        This.Entities.Delete (Index);
                    end if;
                else
                    Index := Index + 1;
                end if;
            end loop;
        end;

        This.Update_Players;

        --  Check explosions colisions with other explosions and players
        declare
            Entities_Temp : P_Entity_Vector.Vector := This.Entities;
        begin
            This.Entities.Clear;
            loop
                exit when Entities_Temp.Is_Empty;
                declare
                    Entity  : T_Entity_Access := Entities_Temp (1);
                    Collide : Boolean;
                begin
                    Entities_Temp.Delete (1);
                    Collide := Handle_Collisions (Entity, Entities_Temp);
                    if not Collide then
                        This.Entities.Append (Entity);
                    else
                        Free (Entity);
                    end if;
                end;
            end loop;
        end;

        for Player_Cursor in This.Players.Iterate loop
            if This.Players (Player_Cursor).Alive then
                for Entity of This.Entities loop
                    if Entity.Entity_Type in T_Explosion_Type
                       and then Check_Collision
                                   (Entity,
                                    This.Players.Reference (Player_Cursor)
                                       .Element)
                    then
                        This.Players (Player_Cursor).Alive := False;
                        exit;
                    end if;
                end loop;
            end if;
        end loop;

        Updated_Cells_Set.Clear;
        --  updates cells
        for X in 1 .. This.Width loop
            for Y in 1 .. This.Height loop
                if This.Grid (X, Y) /= This.Inert_Grid (X, Y) then
                    This.Grid (X, Y) := This.Inert_Grid (X, Y);
                    Updated_Cells_Set.Include ((X, Y));
                end if;
            end loop;
        end loop;
        for Elem of This.Entities loop
            This.Grid (Elem.Pos_X, Elem.Pos_Y) := Elem.Entity_Type;
            Updated_Cells_Set.Include ((Elem.Pos_X, Elem.Pos_Y));
            Updated_Cells_Set.Include ((Elem.Last_Pos_X, Elem.Last_Pos_Y));
        end loop;
        for Player of This.Players loop
            if Player.Alive then
                This.Grid (Player.Pos_X, Player.Pos_Y) := Player.Entity_Type;
                Updated_Cells_Set.Include ((Player.Pos_X, Player.Pos_Y));
                Updated_Cells_Set.Include
                   ((Player.Last_Pos_X, Player.Last_Pos_Y));
            end if;
        end loop;

    exception
        when E : Constraint_Error | Tasking_Error | Program_Error =>
            Put_Line (Exception_Message (E));
        when others =>
            Put_Line ("Erreur");
    end Update;

    procedure Add_Bomb (This : in out T_Map; Pos_X, Pos_Y : Positive) is
    begin
        This.Entities.Append (new T_Bomb);
        This.Entities.Last_Element.Entity_Type := BOMB9;
        This.Entities.Last_Element.Pos_X := Pos_X;
        This.Entities.Last_Element.Pos_Y := Pos_Y;
        This.Entities.Last_Element.Last_Pos_X := Pos_X;
        This.Entities.Last_Element.Last_Pos_Y := Pos_Y;
    end Add_Bomb;

    procedure Set_Drawing_Offset
       (This : in out T_Map; X_Offset, Y_Offset : Integer) is
    begin
        This.Drawing_X_Offset := X_Offset;
        This.Drawing_Y_Offset := Y_Offset;
    end Set_Drawing_Offset;

    procedure Set_Player_Instruction
       (This        : in out T_Map;
        Index       : Positive;
        Instruction : T_Player_Instructions) is
    begin
        if This.Players.Contains (Index) then
            case Instruction is
                when UP    =>
                    This.Players (Index).Entity_Type :=
                       Map_Types.PLAYER_MOVING_UP;

                when DOWN  =>
                    This.Players (Index).Entity_Type :=
                       Map_Types.PLAYER_MOVING_DOWN;

                when RIGHT =>
                    This.Players (Index).Entity_Type :=
                       Map_Types.PLAYER_MOVING_RIGHT;

                when LEFT  =>
                    This.Players (Index).Entity_Type :=
                       Map_Types.PLAYER_MOVING_LEFT;

                when BOMB  =>
                    This.Players (Index).Putting_Bomb := True;
            end case;
        end if;
    end Set_Player_Instruction;
    procedure Add_Player (This : in out T_Map; Index : Positive) is
    begin
        This.Players.Insert
           (Key      => Index,
            New_Item =>
               (Pos_X        => 1,
                Pos_Y        => 1,
                Entity_Type  => PLAYER_IDLE,
                Last_Pos_X   => 1,
                Last_Pos_Y   => 1,
                Alive        => True,
                Putting_Bomb => False));
        This.Teleport_Player_To_Random_Cell (Index);
    end Add_Player;
    procedure Remove_Player (This : in out T_Map; Index : Positive) is
    begin
        This.Players.Delete (Index);
    end Remove_Player;

    function Get_Number_Of_Alive_Players (This : T_Map) return Integer is
        Count : Integer := 0;
    begin
        for Player of This.Players loop
            if Player.Alive then
                Count := Count + 1;
            end if;
        end loop;
        return Count;
    end Get_Number_Of_Alive_Players;

end Map;
