with Client;       use Client;
with GNAT.Sockets; use GNAT.Sockets;

package Client_List is

    type T_Client_Access_Array is array (Positive range <>) of T_Client_Access;

    protected type T_Client_List (Max_Client_Nbr : Positive) is
        entry Add_Client (New_Client_Index : out Positive);

        procedure Remove_Client (Index : Positive)
        with Pre => Index <= Max_Client_Nbr;

        procedure Set_Client_TCP_Socket
           (Index : Positive; Socket : Socket_Type);

        procedure Set_Client_Id (Index : Positive; Id : Positive);
        function Get_Client_Dispatch_Authorization
           (Index : Positive) return Boolean;
        procedure Set_Client_Dispatch_Authorization
           (Index : Positive; State : Boolean);
        function Get_Client_TCP_Socket (Index : Positive) return Socket_Type;
        function Is_Client_Fully_Configured (Index : Positive) return Boolean;

        procedure Start_Client_Identification_Timeout (Index : Positive);
        procedure Confirm_Client_Identification
           (Index : Positive; UDP_Address : Sock_Addr_Type);

        procedure Find_Client_By_Id
           (Id : Positive; Index : out Positive; Found : out Boolean);

        procedure Find_Client_By_Udp_Address
           (Address : Sock_Addr_Type;
            Index   : out Positive;
            Found   : out Boolean);

        function Is_Full return Boolean;

        procedure Close;

    private
        Clients       : T_Client_Access_Array (1 .. Max_Client_Nbr) :=
           [others => null];
        Client_Number : Integer := 0;
        Closed        : Boolean := False;
    end T_Client_List;

end Client_List;
