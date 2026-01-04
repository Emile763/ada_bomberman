with Communication_Types;
with Map_Types; use Map_Types;
package Game_Types is

    --  Type of the message sent by the server (TCP)
    type T_Server_Message_Data_Type is
       (MAP_HEADER, MAP_CONTENT, CELL);

    --  Structure of the messages sent by the server (TCP)
    type T_Server_Message_Data
       (Data_Type : T_Server_Message_Data_Type := CELL)
    is record
        case Data_Type is
            when CELL =>
                Pos_X, Pos_Y : Positive;
                Cell_Type    : T_Cell_Type;

            when MAP_HEADER =>
                Width, Height : Positive;

            when MAP_CONTENT =>
                New_Cell : T_Cell_Type;
        end case;
    end record;

    --  Init the communication type
    --  Message sent by the clients (UDP) : T_Player_Instructions
    --  Message sent by the server (TCP) : T_Server_Message_Data
    package Game_Communication_Type is new
       Communication_Types
          (T_Client_Message_Data => T_Player_Instructions,
           T_Server_Message_Data => T_Server_Message_Data);
end Game_Types;
