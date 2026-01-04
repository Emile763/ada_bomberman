with Ada.Containers; use Ada.Containers;
with Ada.Containers.Hashed_Sets;
with Ada.Unchecked_Deallocation;

--  Generic non blocking mail box
--  The data is stored in sets
--  The sets are clear after
--  the data has been requested

generic
    type Data_Type is private;

    with function Hash (Element : Data_Type) return Hash_Type;
    with function "=" (Left, Right : Data_Type) return Boolean is <>;
package BAL is

    package P_Set is new
       Ada.Containers.Hashed_Sets
          (Element_Type        => Data_Type,
           Hash                => Hash,
           Equivalent_Elements => "=");

    protected type T_BAL is
        procedure Send (Data : Data_Type);
        procedure Receive_All (Elements : out P_Set.Set);
    private
        Data_Set : P_Set.Set;

    end T_BAL;
    type T_BAL_Access is access all T_BAL;

    procedure Delete is new
       Ada.Unchecked_Deallocation (Object => T_BAL, Name => T_BAL_Access);

    type T_BAL_Access_Array is array (Positive range <>) of T_BAL_Access;

    --  needs to be protected to prevent a task from removing
    --  an element when another task is using it
    protected type T_BAL_List (Capacity : Positive) is
        procedure Add_Element (Index : Positive);
        procedure Remove_Element (Index : Positive);

        procedure Clear;

        procedure Add_Data (Index : Positive; Data : Data_Type);
        procedure Get_Data (Index : Positive; Data : out P_Set.Set);
    private
        BALs : T_BAL_Access_Array (1 .. Capacity) := [others => null];
        Size : Integer := 0;
    end T_BAL_List;
end BAL;
