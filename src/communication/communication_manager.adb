with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

package body Communication_Manager is

    procedure Set_Observer
       (This     : in out T_Client_Manager;
        Observer : I_Client_Manager_Observer_Access) is
    begin
        This.Observer := Observer;
    end Set_Observer;

    procedure Init_Client_Manager (This : in out T_Client_Manager) is
    begin
        --  Needed to generate random identifier
        Rand_Positive.Reset (This.Id_Generator);

        --  Used to detect timeout and to exit tasks
        Create_Selector (This.Connection_Selector);
        Create_Selector (This.UDP_Listening_Selector);

        --  Init TCP Socket
        Create_Socket (Socket => This.Server_TCP_Socket);
        This.Server_TCP_Address.Addr := Inet_Addr ("0.0.0.0");
        This.Server_TCP_Address.Port := 0;
        Bind_Socket
           (Socket  => This.Server_TCP_Socket,
            Address => This.Server_TCP_Address);
        This.Server_TCP_Address :=
           Get_Socket_Name (Socket => This.Server_TCP_Socket);

        --  Init UDP Socket
        Create_Socket
           (Socket => This.Server_UDP_Socket, Mode => Socket_Datagram);
        This.Server_UDP_Address.Addr := Inet_Addr ("0.0.0.0");
        This.Server_UDP_Address.Port := 0;
        Bind_Socket
           (Socket  => This.Server_UDP_Socket,
            Address => This.Server_UDP_Address);
        This.Server_UDP_Address :=
           Get_Socket_Name (Socket => This.Server_UDP_Socket);

        --  Start the task waiting for Client connection
        This.Connection_Task.Start;

        --  Start the task listening to client incomming messages
        This.UDP_Listening_Task.Start;
    end Init_Client_Manager;

    procedure Close_Client_Manager (This : in out T_Client_Manager) is
    begin
        This.Client_List.Close;
        Put_Line ("Client List Closed");

        Abort_Selector (This.Connection_Selector);
        Abort_Selector (This.UDP_Listening_Selector);
        Put_Line ("Selectors aborted");

        --  Wait until the tasks have ended to ensure
        --  safety when closing the socket and selector
        This.Connection_Task.Has_Ended;
        Put_Line ("wait for end of udp");
        This.UDP_Listening_Task.Has_Ended;
        Put_Line ("end of udp");

        Close_Socket (This.Server_TCP_Socket);
        Close_Selector (This.Connection_Selector);

        Close_Socket (This.Server_UDP_Socket);
        Close_Selector (This.UDP_Listening_Selector);
        Put_Line ("Sockets closed");

    end Close_Client_Manager;

    function Get_Client_Data
       (This : in out T_Client_Manager; Index : Positive)
        return P_Client_BAL.P_Set.Set
    is
        Set : P_Client_BAL.P_Set.Set;
    begin
        --  Get all the data from the client sent since last call
        This.Incomming_Data.Get_Data (Index, Set);
        return Set;
    end Get_Client_Data;

    procedure Set_Client_Dispatch_Authorization
       (This : in out T_Client_Manager; Index : Positive; State : Boolean) is
    begin
        --  Authorize dispatch to the client
        This.Client_List.Set_Client_Dispatch_Authorization (Index, State);
    end Set_Client_Dispatch_Authorization;

    procedure Send
       (This    : in out T_Client_Manager;
        Index   : Positive;
        Message : T_Server_Message_Data)
    is
        Client_Fully_Configured : constant Boolean :=
           This.Client_List.Is_Client_Fully_Configured (Index);
        Client_TCP_Socket       : Socket_Type;
        Server_Message          : constant T_Server_Message := (DATA, Message);
        Channel                 : Stream_Access;

    begin
        if Client_Fully_Configured then
            Client_TCP_Socket :=
               This.Client_List.Get_Client_TCP_Socket (Index);
            Channel := Stream (Client_TCP_Socket);
            declare
            begin
                T_Server_Message'Output (Channel, Server_Message);
            exception
                when E : Socket_Error =>
                    declare
                        Client_Address : constant Sock_Addr_Type :=
                           Get_Socket_Name (Socket => Client_TCP_Socket);
                    begin
                        This.Client_List.Remove_Client (Index);
                        Put_Line
                           ("Error while sending message to "
                            & Image (Client_Address.Addr)
                            & ":"
                            & Client_Address.Port'Image
                            & " => "
                            & Exception_Message (E));
                        This.Incomming_Data.Remove_Element (Index);
                        if This.Observer /= null then
                            This.Observer.Client_Quit (Index);
                        end if;
                    end;
            end;
            Free (Channel);
        end if;

    end Send;

    procedure Dispatch
       (This : in out T_Client_Manager; Message : T_Server_Message_Data) is
    begin
        for Index in 1 .. This.Max_Client_Nbr loop
            if This.Client_List.Get_Client_Dispatch_Authorization (Index) then
                This.Send (Index, Message);
            end if;
        end loop;
    end Dispatch;

    task body T_Connection_Task is
        Accepting_Status   : Selector_Status;
        New_Client_Socket  : Socket_Type;
        New_Client_Address : Sock_Addr_Type;

        New_Client_Index : Positive;

        Client_Channel : Stream_Access;
    begin
        accept Start;

        Listen_Socket (Client_Manager.Server_TCP_Socket);
        Put_Line
           (Image (Client_Manager.Server_TCP_Address.Addr)
            & ":"
            & Client_Manager.Server_TCP_Address.Port'Image);
        loop
            Client_Manager.Client_List.Add_Client (New_Client_Index);
            loop
                Accept_Socket
                   (Server   => Client_Manager.Server_TCP_Socket,
                    Socket   => New_Client_Socket,
                    Address  => New_Client_Address,
                    Timeout  => 1.0,
                    Selector => Client_Manager.Connection_Selector'Access,
                    Status   => Accepting_Status);
                if Accepting_Status = Completed then
                    Client_Channel :=
                       Stream
                          (Socket  => New_Client_Socket,
                           Send_To => New_Client_Address);
                    declare
                        Connection_Message :
                           T_Server_Message (CONNECTION_IDENTIER);

                        New_Client_Id : constant Positive :=
                           Rand_Positive.Random (Client_Manager.Id_Generator);
                    begin
                        Client_Manager.Client_List.Set_Client_TCP_Socket
                           (Index  => New_Client_Index,
                            Socket => New_Client_Socket);

                        Client_Manager.Client_List.Set_Client_Id
                           (Index => New_Client_Index, Id => New_Client_Id);

                        --  Send the informations need to connect with UDP
                        --  and start a time out
                        Client_Manager
                           .Client_List
                           .Start_Client_Identification_Timeout
                              (New_Client_Index);

                        Connection_Message.Id := New_Client_Id;
                        Connection_Message.Port :=
                           Client_Manager.Server_UDP_Address.Port;

                        T_Server_Message'Output
                           (Client_Channel, Connection_Message);
                    end;
                    Free (Client_Channel);
                end if;
                exit when
                   Accepting_Status = Completed or Accepting_Status = Aborted;
            end loop;
            exit when Accepting_Status = Aborted;
        end loop;

        accept Has_Ended;
    end T_Connection_Task;

    procedure Process_Message
       (Client_Manager : not null access T_Client_Manager;
        Channel        : Stream_Access);
    procedure Process_Message
       (Client_Manager : not null access T_Client_Manager;
        Channel        : Stream_Access)
    is
        Received_Data      : constant T_Client_Message :=
           T_Client_Message'Input (Channel);
        Client_UDP_Address : constant Sock_Addr_Type := Get_Address (Channel);
        Client_Found       : Boolean;
        Client_Index       : Positive;

    begin
        case Received_Data.Message_Type is
            --  If CONNECTION_CONFIRMATION,
            --  finish the configuration of the new client

            when CONNECTION_CONFIRMATION =>
                --  Find the client corresponding to the identifier
                Client_Manager.Client_List.Find_Client_By_Id
                   (Id    => Received_Data.Id,
                    Index => Client_Index,
                    Found => Client_Found);
                if Client_Found
                   and then not Client_Manager
                               .Client_List
                               .Is_Client_Fully_Configured (Client_Index)
                then
                    Client_Manager.Client_List.Confirm_Client_Identification
                       (Client_Index, Client_UDP_Address);
                    --  New Bal for this client
                    Client_Manager.Incomming_Data.Add_Element (Client_Index);
                    --  Signal the Observer that a new client arrived
                    if Client_Manager.Observer /= null then
                        Client_Manager.Observer.New_Client (Client_Index);
                    end if;
                end if;
                Put_Line ("New Connection : " & Received_Data.Id'Img);

            --  When Data, send the message to the corresponding bal
            when DATA                    =>
                Client_Manager.Client_List.Find_Client_By_Udp_Address
                   (Address => Get_Address (Channel),
                    Index   => Client_Index,
                    Found   => Client_Found);
                if Client_Found then
                    Client_Manager.Incomming_Data.Add_Data
                       (Client_Index, Received_Data.Message);
                    Put_Line
                       ("New Message from "
                        & Image (Client_UDP_Address.Addr)
                        & ":"
                        & Client_UDP_Address.Port'Img
                        & " --> "
                        & Received_Data.Message'Image);
                end if;
        end case;
    end Process_Message;

    task body T_UDP_Listening_Task is
        New_Message_Status : Selector_Status;

        R_Socket_Set : Socket_Set_Type;
        W_Socket_Set : Socket_Set_Type;

        Channel : Stream_Access;

    begin
        accept Start;
        Channel :=
           Stream
              (Client_Manager.Server_UDP_Socket,
               Client_Manager.Server_TCP_Address);
        loop
            Empty (R_Socket_Set);
            Set (R_Socket_Set, Client_Manager.Server_UDP_Socket);
            Check_Selector
               (Selector     => Client_Manager.UDP_Listening_Selector,
                R_Socket_Set => R_Socket_Set,
                W_Socket_Set => W_Socket_Set,
                Status       => New_Message_Status,
                Timeout      => 1.0);

            if New_Message_Status = Completed then
                Process_Message (Client_Manager, Channel);
            end if;

            exit when New_Message_Status = Aborted;
        end loop;
        Free (Channel);
        accept Has_Ended;

    end T_UDP_Listening_Task;
end Communication_Manager;
