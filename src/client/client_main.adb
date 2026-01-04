with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Text_IO;     use Ada.Text_IO;
with Map;             use Map;
with Map.Map_Drawing; use Map.Map_Drawing;
with Drawing_Utility; use Drawing_Utility;
with GNAT.Sockets;    use GNAT.Sockets;
with Game_Types;      use Game_Types;
with Map_Types;       use Map_Types;
use Game_Types.Game_Communication_Type;

--  Need a big refactor

procedure Client_Main is

    Key  : Character;
    More : Boolean;

    Server_Address_TCP : Sock_Addr_Type := No_Sock_Addr;
    Server_Address_UDP : Sock_Addr_Type := No_Sock_Addr;
    Client_Socket_TCP  : Socket_Type;
    Client_Socket_UDP  : Socket_Type;
    Channel_TCP        : Stream_Access;
    Channel_UDP        : Stream_Access;

    Client_Instruction_Message : T_Client_Message (DATA);

    Map_Width  : Positive;
    Map_Height : Positive;

begin
    --  Init the terminal
    Initialize_Resized_Callback;
    Clear_Screen;

    --  Get the server TCP address
    Put_Line ("Enter Server IPv4 :");
    declare
        User_Input : constant String := Get_Line;
    begin
        Server_Address_TCP.Addr := Inet_Addr (User_Input);
        Server_Address_UDP.Addr := Server_Address_TCP.Addr;
    end;
    Put_Line ("Enter Server Port :");
    declare
        User_Input : constant String := Get_Line;
    begin
        Server_Address_TCP.Port := Port_Type'Value (User_Input);
    end;

    --  Connection to the TCP server
    Put_Line ("Initialize communication...");
    Create_Socket (Socket => Client_Socket_TCP);
    Put_Line ("TCP Socket Created !");
    Connect_Socket (Socket => Client_Socket_TCP, Server => Server_Address_TCP);
    Put_Line ("Connected!");
    Channel_TCP := Stream (Client_Socket_TCP);

    --  Move to a clean alternative console where scrolling is disabled
    Alternative_Console;
    --  Hide the console cursor for smooth drawing
    Hide_Cursor;

    --  Create the UDP "connection"
    declare
        Server_Message : constant T_Server_Message :=
           T_Server_Message'Input (Channel_TCP);
        Client_Message : T_Client_Message (CONNECTION_CONFIRMATION);
    begin
        --  Get the identifier needed to register the client UDP address
        --  And the Server UDP Port
        Put_Line ("Received Identifier : " & Server_Message.Id'Img);
        Client_Message.Id := Server_Message.Id;
        Server_Address_UDP.Port := Server_Message.Port;

        --  Init Client UDP socket
        Create_Socket
           (Socket => Client_Socket_UDP,
            Family => Family_Inet,
            Mode   => Socket_Datagram);
        Connect_Socket
           (Socket => Client_Socket_UDP, Server => Server_Address_UDP);
        Channel_UDP := Stream (Client_Socket_UDP);

        --  Send the identifier to the server UDP address
        --  So that it can register this UDP socket address
        T_Client_Message'Output (Channel_UDP, Client_Message);
    end;

    --  Wait for the server to send the map dimensions
    declare
        Server_Message : T_Server_Message :=
           T_Server_Message'Input (Channel_TCP);
    begin
        Put_Line ("Received Map Dimension");
        Map_Height := Server_Message.Message.Height;
        Map_Width := Server_Message.Message.Width;
        declare
            theMap : T_Map (Map_Width, Map_Height);
        begin
            theMap.Set_Drawing_Offset (0, 1);

            declare
                --  Task listening for the Server (TCP) messages
                --  Update the map and the drawing as a result
                task Incoming_Messages_Task;
                task body Incoming_Messages_Task is
                begin
                    loop
                        Server_Message := T_Server_Message'Input (Channel_TCP);
                        case Server_Message.Message.Data_Type is
                            --  When Cell, Only one cell is updated

                            when CELL        =>
                                theMap.Set_Cell
                                   (Server_Message.Message.Pos_X,
                                    Server_Message.Message.Pos_Y,
                                    Server_Message.Message.Cell_Type);
                                --  Update only the drawing of the new cell
                                Draw_Cell
                                   (theMap,
                                    Server_Message.Message.Pos_X,
                                    Server_Message.Message.Pos_Y);

                            --  When MAP_CONTENT, the entire map is sent

                            when MAP_CONTENT =>
                                theMap.Set_Cell
                                   (1, 1, Server_Message.Message.New_Cell);
                                for Y in 2 .. Map_Height loop
                                    Server_Message :=
                                       T_Server_Message'Input (Channel_TCP);
                                    theMap.Set_Cell
                                       (1, Y, Server_Message.Message.New_Cell);
                                end loop;
                                for X in 2 .. Map_Width loop
                                    for Y in 1 .. Map_Height loop
                                        Server_Message :=
                                           T_Server_Message'Input
                                              (Channel_TCP);
                                        theMap.Set_Cell
                                           (X,
                                            Y,
                                            Server_Message.Message.New_Cell);
                                    end loop;
                                end loop;
                                --  Redraw the entire map
                                Draw (theMap);

                            when others      =>
                                null;
                        end case;

                        --  if the terminal as been resized,
                        --  the entire map is redraw to prevent some artifact
                        --  doesn't work for windows for now
                        if Check_Resized > 0 then
                            Clear_Screen;
                            Draw (theMap);
                        end if;

                    end loop;
                end Incoming_Messages_Task;
            begin
                --  Main loop, handle users inputs
                loop
                    --  Get the pressed key
                    Get_Immediate (Item => Key);
                    --  Clear the buffer to avoid dealing with Escape Sequences
                    if Key = Ada.Characters.Latin_1.ESC then
                        loop
                            Get_Immediate (Item => Key, Available => More);
                            exit when not More;
                        end loop;
                        Key := Ada.Characters.Latin_1.NUL;
                    end if;

                    --  Check if the pressed key is in ZQSD or Space
                    Key := Ada.Characters.Handling.To_Lower (Key);
                    declare
                        Player_Moved : Boolean := False;
                    begin
                        case Key is
                            when 'z'    =>
                                Player_Moved := True;
                                Client_Instruction_Message.Message := UP;

                            when 'q'    =>
                                Player_Moved := True;
                                Client_Instruction_Message.Message := LEFT;

                            when 's'    =>
                                Player_Moved := True;
                                Client_Instruction_Message.Message := DOWN;

                            when 'd'    =>
                                Player_Moved := True;
                                Client_Instruction_Message.Message := RIGHT;

                            when ' '    =>
                                Player_Moved := True;
                                Client_Instruction_Message.Message := BOMB;

                            when others =>
                                null;
                        end case;

                        --  Send the new instructions to the server via UDP
                        if Player_Moved then
                            T_Client_Message'Output
                               (Channel_UDP, Client_Instruction_Message);
                        end if;
                    end;

                    --  quit when backspace is pressed
                    exit when Key = Ada.Characters.Latin_1.DEL;
                end loop;
            end;
        end;

    end;
end Client_Main;
