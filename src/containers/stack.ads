with Ada.Containers; use Ada.Containers;
with Ada.Unchecked_Deallocation;

--  Simple generic and dynamic stack
generic
    type T_Data is private;
package Stack is

    type T_Stack is tagged private;

    function Is_Empty (This : in out T_Stack) return Boolean;
    function Size (This : in out T_Stack) return Count_Type;

    function Pop (This : in out T_Stack) return T_Data
    with Pre => This.Size > 0;

    procedure Insert (This : in out T_Stack; Data : T_Data);

private
    type T_Stack_Node;
    type T_Stack_Node_Access is access T_Stack_Node;

    type T_Stack_Node is record
        Next_Element : T_Stack_Node_Access := null;
        Data         : T_Data;
    end record;

    procedure Free is new
       Ada.Unchecked_Deallocation
          (Object => T_Stack_Node,
           Name   => T_Stack_Node_Access);

    type T_Stack is tagged record
        Last_Node : T_Stack_Node_Access := null;
        Size      : Count_Type := 0;
    end record;
end Stack;
