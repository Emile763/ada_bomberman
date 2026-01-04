with GNAT.Sockets; use GNAT.Sockets;

generic
    type T_Client_Message_Data is private;
    type T_Server_Message_Data is private;
package Communication_Types is
    type T_Client_Message_Type is (CONNECTION_CONFIRMATION, DATA);

    type T_Client_Message
       (Message_Type : T_Client_Message_Type := CONNECTION_CONFIRMATION)
    is record
        case Message_Type is
            when CONNECTION_CONFIRMATION =>
                Id : Positive;

            when DATA =>
                Message : T_Client_Message_Data;
        end case;
    end record;

    type T_Server_Message_Type is (CONNECTION_IDENTIER, DATA);
    type T_Server_Message
       (Message_Type : T_Server_Message_Type := CONNECTION_IDENTIER)
    is record
        case Message_Type is
            when CONNECTION_IDENTIER =>
                Id   : Positive;
                Port : Port_Type;

            when DATA =>
                Message : T_Server_Message_Data;
        end case;
    end record;
end Communication_Types;
