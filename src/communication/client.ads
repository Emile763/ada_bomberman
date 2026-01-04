with GNAT.Sockets; use GNAT.Sockets;

package Client is
    type T_Client is tagged limited private;
    type T_Client_Access is access T_Client;

    procedure Free (Client_Access : in out T_Client_Access);

    procedure Set_TCP_Socket (This : in out T_Client; Socket : Socket_Type);
    procedure Set_UDP_Address
       (This : in out T_Client; Address : Sock_Addr_Type);

    procedure Set_Id (This : in out T_Client; Id : Positive);

    function Get_TCP_Socket (This : T_Client) return Socket_Type;
    function Get_UDP_Address (This : T_Client) return Sock_Addr_Type;
    function Is_Fully_Configured (This : T_Client) return Boolean;

    function Get_Id (This : T_Client) return Positive;

    procedure Start_Identification_Timeout (This : T_Client);
    procedure Confirm_Identification
       (This : T_Client; UDP_Address : Sock_Addr_Type);
    procedure Abort_Identification (This : T_Client);

    function Get_Dispatch_Authorization (This : T_Client) return Boolean;
    procedure Set_Dispatch_Authorization
       (This : in out T_Client; State : Boolean);

private
    task type T_Identification_Timeout_Task
       (This : not null access T_Client)
    is
        entry Start_Identification_Timeout;
        entry Confirm_Identification (UDP_Address : Sock_Addr_Type);
        entry Abort_Identification;
    end T_Identification_Timeout_Task;

    type T_Client is tagged limited record
        Client_TCP_Socket  : Socket_Type;
        Client_UDP_Address : Sock_Addr_Type;
        Fully_Configured   : Boolean := False;
        Authorize_Dispatch : Boolean := False;

        Id : Positive;

        Identification_Timeout_Task :
           T_Identification_Timeout_Task (T_Client'Access);
    end record;
end Client;
