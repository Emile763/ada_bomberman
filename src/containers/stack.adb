package body Stack is

    function Is_Empty (This : in out T_Stack) return Boolean is
    begin
        return This.Size = 0;
    end Is_Empty;

    function Size (This : in out T_Stack) return Count_Type is
    begin
        return This.Size;
    end Size;

    function Pop (This : in out T_Stack) return T_Data is
        Data      : constant T_Data := This.Last_Node.Data;
        Last_Node : T_Stack_Node_Access := This.Last_Node;
    begin
        This.Last_Node := Last_Node.Next_Element;
        This.Size := This.Size - 1;
        Free (Last_Node);
        return Data;
    end Pop;

    procedure Insert (This : in out T_Stack; Data : T_Data) is
    begin
        This.Last_Node := new T_Stack_Node'(This.Last_Node, Data);
        This.Size := This.Size + 1;
    end Insert;

end Stack;
