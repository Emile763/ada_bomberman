package body Client_List is
    protected body T_Client_List is

        entry Add_Client (New_Client_Index : out Positive)
           when Client_Number < Max_Client_Nbr
        is
        begin
            if Closed then
                New_Client_Index := 1;
            else
                --  Find an Index with no client
                --  then create a new client
                for Index in Clients'Range loop
                    if Clients (Index) = null then
                        Clients (Index) := new T_Client;
                        New_Client_Index := Index;
                        Client_Number := Client_Number + 1;
                        exit;
                    end if;
                end loop;
            end if;
        end Add_Client;

        procedure Remove_Client (Index : Positive) is
        begin
            if Client_Number > 0 and then Clients (Index) /= null then
                Client_Number := Client_Number - 1;
            end if;
            Free (Clients (Index));
        end Remove_Client;

        procedure Set_Client_TCP_Socket
           (Index : Positive; Socket : Socket_Type) is
        begin
            if Clients (Index) /= null then
                Clients (Index).Set_TCP_Socket (Socket);
            end if;
        end Set_Client_TCP_Socket;

        procedure Set_Client_Id (Index : Positive; Id : Positive) is
        begin
            if Clients (Index) /= null then
                Clients (Index).Set_Id (Id);
            end if;
        end Set_Client_Id;
        function Get_Client_Dispatch_Authorization
           (Index : Positive) return Boolean is
        begin
            if Clients (Index) /= null then
                return Clients (Index).Get_Dispatch_Authorization;
            end if;
            return False;
        end Get_Client_Dispatch_Authorization;
        procedure Set_Client_Dispatch_Authorization
           (Index : Positive; State : Boolean) is
        begin
            Clients (Index).Set_Dispatch_Authorization (State);
        end Set_Client_Dispatch_Authorization;

        function Get_Client_TCP_Socket (Index : Positive) return Socket_Type is
        begin
            if Clients (Index) /= null then
                return Clients (Index).Get_TCP_Socket;
            end if;
            return No_Socket;
        end Get_Client_TCP_Socket;
        function Is_Client_Fully_Configured (Index : Positive) return Boolean
        is
        begin
            if Clients (Index) /= null then
                return Clients (Index).Is_Fully_Configured;
            end if;
            return False;
        end Is_Client_Fully_Configured;
        procedure Start_Client_Identification_Timeout (Index : Positive) is
        begin
            if Clients (Index) /= null then
                Clients (Index).Start_Identification_Timeout;
            end if;
        end Start_Client_Identification_Timeout;

        procedure Confirm_Client_Identification
           (Index : Positive; UDP_Address : Sock_Addr_Type) is
        begin
            if Clients (Index) /= null
               and then not Clients (Index).Is_Fully_Configured
            then
                Clients (Index).Confirm_Identification (UDP_Address);
            end if;
        end Confirm_Client_Identification;

        procedure Find_Client_By_Id
           (Id : Positive; Index : out Positive; Found : out Boolean) is
        begin
            for Ind in Clients'Range loop
                Found :=
                   Clients (Ind) /= null and then Clients (Ind).Get_Id = Id;
                Index := Ind;
                exit when Found;
            end loop;
        end Find_Client_By_Id;

        procedure Find_Client_By_Udp_Address
           (Address : Sock_Addr_Type;
            Index   : out Positive;
            Found   : out Boolean) is
        begin
            for Ind in Clients'Range loop
                Found :=
                   Clients (Ind) /= null
                   and then Clients (Ind).Get_UDP_Address = Address;
                Index := Ind;
                exit when Found;
            end loop;
        end Find_Client_By_Udp_Address;
        function Is_Full return Boolean is
        begin
            return Client_Number /= Max_Client_Nbr;
        end Is_Full;

        procedure Close is
        begin
            for Index in Clients'Range loop
                Remove_Client (Index);
            end loop;
            Client_Number := 0;
            Closed := True;
        end Close;
    end T_Client_List;

end Client_List;
