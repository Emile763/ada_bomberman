with GNAT.Sockets;   use GNAT.Sockets;
with Client_List;    use Client_List;
with Ada.Numerics.Discrete_Random;
with BAL;
with Ada.Containers; use Ada.Containers;
with Communication_Types;

generic
    type T_Client_Message_Data is private;
    with function Hash (Element : T_Client_Message_Data) return Hash_Type;
    type T_Server_Message_Data is private;
package Communication_Manager is

    package P_Communication_Type is new
       Communication_Types
          (T_Client_Message_Data => T_Client_Message_Data,
           T_Server_Message_Data => T_Server_Message_Data);
    use P_Communication_Type;
    package P_Client_BAL is new
       BAL (Data_Type => T_Client_Message_Data, Hash => Hash);

    type I_Client_Manager_Observer is limited interface;
    type I_Client_Manager_Observer_Access is
       access all I_Client_Manager_Observer'Class;

    procedure New_Client
       (Observer : in out I_Client_Manager_Observer; Index : Positive)
    is abstract;
    procedure Client_Quit
       (Observer : in out I_Client_Manager_Observer; Index : Positive)
    is abstract;

    type T_Client_Manager (Max_Client_Nbr : Positive) is
       tagged limited private;

    procedure Set_Observer
       (This     : in out T_Client_Manager;
        Observer : I_Client_Manager_Observer_Access);
    procedure Init_Client_Manager (This : in out T_Client_Manager);
    procedure Close_Client_Manager (This : in out T_Client_Manager);
    function Get_Client_Data
       (This : in out T_Client_Manager; Index : Positive)
        return P_Client_BAL.P_Set.Set;

    procedure Set_Client_Dispatch_Authorization
       (This : in out T_Client_Manager; Index : Positive; State : Boolean);

    procedure Send
       (This    : in out T_Client_Manager;
        Index   : Positive;
        Message : T_Server_Message_Data);
    procedure Dispatch
       (This : in out T_Client_Manager; Message : T_Server_Message_Data);

private
    package Rand_Positive is new Ada.Numerics.Discrete_Random (Positive);

    task type T_Connection_Task
       (Client_Manager : not null access T_Client_Manager)
    is
        entry Start;
        entry Has_Ended;
    end T_Connection_Task;

    task type T_UDP_Listening_Task
       (Client_Manager : not null access T_Client_Manager)
    is
        entry Start;
        entry Has_Ended;
    end T_UDP_Listening_Task;

    type T_Client_Manager (Max_Client_Nbr : Positive) is tagged limited record
        Observer : I_Client_Manager_Observer_Access := null;

        Client_List        : T_Client_List (Max_Client_Nbr);
        Server_TCP_Socket  : Socket_Type;
        Server_TCP_Address : Sock_Addr_Type;

        Server_UDP_Socket  : Socket_Type;
        Server_UDP_Address : Sock_Addr_Type;

        Connection_Task     : T_Connection_Task (T_Client_Manager'Access);
        Connection_Selector : aliased Selector_Type;

        UDP_Listening_Task     :
           T_UDP_Listening_Task (T_Client_Manager'Access);
        UDP_Listening_Selector : aliased Selector_Type;

        Id_Generator : Rand_Positive.Generator;

        Incomming_Data : P_Client_BAL.T_BAL_List (Max_Client_Nbr);
    end record;

end Communication_Manager;
