with Communication_Manager;
with Game_Types; use Game_Types;
with Ada.Containers.Hashed_Sets;
with Map; use Map;
with Map_Types; use Map_Types;
package Game_Manager is

    package Game_Communication_Manager is new
       Communication_Manager
          (T_Client_Message_Data => T_Player_Instructions,
           Hash                  => Hash,
           T_Server_Message_Data => T_Server_Message_Data);
    use Game_Communication_Manager;

    function Hash (Key : Positive) return Ada.Containers.Hash_Type;

    package Positive_Set is new
       Ada.Containers.Hashed_Sets
          (Element_Type        => Positive,
           Hash                => Hash,
           Equivalent_Elements => "=");

    type T_Game (Max_Player_Number : Positive) is limited
       new I_Client_Manager_Observer with private;

    procedure Start_Game (This : not null access T_Game);
    procedure End_Game (This : in out T_Game);

    procedure Update (This : in out T_Game);

    overriding
    procedure New_Client (Observer : in out T_Game; Index : Positive);

    overriding
    procedure Client_Quit (Observer : in out T_Game; Index : Positive);

private

    type T_Game (Max_Player_Number : Positive) is limited
       new I_Client_Manager_Observer
    with record
        Client_Manager : T_Client_Manager (Max_Player_Number);
        Client_Indexes : Positive_Set.Set;
        theMap : T_Map (51, 21);

        Tick : Long_Long_Long_Integer := 0;
    end record;
end Game_Manager;
