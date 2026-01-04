package body Map_Types is
    function Hash
       (Player_Instructions : T_Player_Instructions)
        return Ada.Containers.Hash_Type is
    begin
        return
           Ada.Containers.Hash_Type
              (T_Player_Instructions'Pos (Player_Instructions));
    end Hash;
end Map_Types;