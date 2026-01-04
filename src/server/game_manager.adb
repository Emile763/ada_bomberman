with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Containers;
with Ada.Exceptions; use Ada.Exceptions;

package body Game_Manager is

    function Hash (Key : Positive) return Ada.Containers.Hash_Type is
    begin
        return Ada.Containers.Hash_Type (Key);
    end Hash;

    procedure Start_Game (This : not null access T_Game) is
    begin
        --  Set This as an observer of the new
        --  clients and the clients that quit
        This.Client_Manager.Set_Observer (This);
        --  Init Communications
        This.Client_Manager.Init_Client_Manager;
        --  Generate a random map
        This.theMap.Randomize;
    end Start_Game;

    procedure End_Game (This : in out T_Game) is
    begin
        This.Client_Manager.Close_Client_Manager;
    end End_Game;

    procedure Update_Map
       (This : in out T_Game; Updated_Cells_Set : out P_Coord_Set.Set);
    procedure Update_Map
       (This : in out T_Game; Updated_Cells_Set : out P_Coord_Set.Set) is
    begin
        --  Set player instructions
        for Index of This.Client_Indexes loop
            declare
                messages : constant P_Client_BAL.P_Set.Set :=
                   This.Client_Manager.Get_Client_Data (Index);
            begin
                if messages.Contains (UP) then
                    This.theMap.Set_Player_Instruction (Index, UP);
                elsif messages.Contains (DOWN) then
                    This.theMap.Set_Player_Instruction (Index, DOWN);
                elsif messages.Contains (RIGHT) then
                    This.theMap.Set_Player_Instruction (Index, RIGHT);
                elsif messages.Contains (LEFT) then
                    This.theMap.Set_Player_Instruction (Index, LEFT);
                end if;

                if messages.Contains (BOMB) then
                    This.theMap.Set_Player_Instruction (Index, BOMB);

                end if;
            end;
        end loop;
        --  Update all the entities and interaction of the map
        This.theMap.Update (Updated_Cells_Set);
    end Update_Map;

    procedure New_Game (This : in out T_Game);
    procedure New_Game (This : in out T_Game) is
    begin
        This.theMap.Randomize;

        --  Clear all messages from last frame
        for Index of This.Client_Indexes loop
            declare
                messages : constant P_Client_BAL.P_Set.Set :=
                   This.Client_Manager.Get_Client_Data (Index);
                pragma Unreferenced (messages);
            begin
                null;
            end;
        end loop;
    end New_Game;

    procedure Update (This : in out T_Game) is
    begin
        --  Only update when there are players
        if not This.Client_Indexes.Is_Empty then
            if This.theMap.Get_Number_Of_Alive_Players > 0 then
                declare
                    Updated_Cells_Set : P_Coord_Set.Set;
                    Message           : T_Server_Message_Data (CELL);
                begin
                    This.Update_Map (Updated_Cells_Set);

                    --  Dispatch the updated cells to all players
                    for Coord of Updated_Cells_Set loop
                        Message.Cell_Type :=
                           This.theMap.Get_Cell (Coord.X, Coord.Y);
                        Message.Pos_X := Coord.X;
                        Message.Pos_Y := Coord.Y;
                        This.Client_Manager.Dispatch (Message);
                    end loop;
                end;
                This.Tick := This.Tick + 1;

            --  Create a new game when all players are dead

            else
                This.New_Game;

                --  Dispatch the new map to all players
                declare
                    Message : T_Server_Message_Data (MAP_CONTENT);
                begin
                    for X in 1 .. This.theMap.Width loop
                        for Y in 1 .. This.theMap.Height loop
                            Message.New_Cell := This.theMap.Get_Cell (X, Y);
                            This.Client_Manager.Dispatch (Message);
                        end loop;
                    end loop;
                end;
            end if;
        end if;

    exception
        when E : Constraint_Error | Tasking_Error | Program_Error =>
            Put_Line (Exception_Message (E));
        when others =>
            Put_Line ("Erreur");
    end Update;

    procedure New_Client (Observer : in out T_Game; Index : Positive) is
    begin
        Put_Line ("New Client Index: " & Index'Image);
        --  Add the player to the map
        Observer.theMap.Add_Player (Index);

        --  Send the size of the map to the new client
        Observer.Client_Manager.Send
           (Index,
            (MAP_HEADER, Observer.theMap.Width, Observer.theMap.Height));

        --  Send the content of the map to the new player
        for X in 1 .. Observer.theMap.Width loop
            for Y in 1 .. Observer.theMap.Height loop
                Observer.Client_Manager.Send
                   (Index, (MAP_CONTENT, Observer.theMap.Get_Cell (X, Y)));
            end loop;
        end loop;

        --  Save the new player client index
        Observer.Client_Indexes.Insert (Index);

        --  Authorize the dispatch to the new player
        Observer.Client_Manager.Set_Client_Dispatch_Authorization
           (Index, True);

    end New_Client;

    procedure Client_Quit (Observer : in out T_Game; Index : Positive) is
    begin
        Put_Line ("Client Has Leaved Index: " & Index'Image);
        Observer.Client_Indexes.Exclude (Index);
        Observer.theMap.Remove_Player (Index);
    end Client_Quit;
end Game_Manager;
