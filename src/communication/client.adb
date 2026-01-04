with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;

package body Client is

    --  Task used to Check if the UDP address is sent in time
    task body T_Identification_Timeout_Task is
    begin
        select
            accept Start_Identification_Timeout;
            Put_Line ("Waiting for identification");
            select
                --  Finalize configuration when
                --  the identification has been confirmed
                accept Confirm_Identification (UDP_Address : Sock_Addr_Type) do
                    This.Client_UDP_Address := UDP_Address;
                end Confirm_Identification;
                This.Fully_Configured := True;
            or
                delay 10.0;
                Put_Line ("Not identified");
            end select;
        or
            accept Abort_Identification;
        end select;
    end T_Identification_Timeout_Task;

    procedure Delete is new
       Ada.Unchecked_Deallocation
          (Object => T_Client,
           Name   => T_Client_Access);
    procedure Free (Client_Access : in out T_Client_Access) is
    begin
        if Client_Access /= null then
            --  Needed so that the task doesn't block runtime
            if not Client_Access.Identification_Timeout_Task'Terminated then
                Client_Access.Identification_Timeout_Task.Abort_Identification;
            end if;
            Close_Socket (Client_Access.Client_TCP_Socket);

            Delete (Client_Access);
        end if;
    end Free;

    procedure Set_TCP_Socket (This : in out T_Client; Socket : Socket_Type) is
    begin
        This.Client_TCP_Socket := Socket;
    end Set_TCP_Socket;
    procedure Set_UDP_Address
       (This : in out T_Client; Address : Sock_Addr_Type) is
    begin
        This.Client_UDP_Address := Address;
    end Set_UDP_Address;

    procedure Set_Id (This : in out T_Client; Id : Positive) is
    begin
        This.Id := Id;
    end Set_Id;

    function Get_TCP_Socket (This : T_Client) return Socket_Type is
    begin
        return This.Client_TCP_Socket;
    end Get_TCP_Socket;
    function Get_UDP_Address (This : T_Client) return Sock_Addr_Type is
    begin
        return This.Client_UDP_Address;
    end Get_UDP_Address;
    function Is_Fully_Configured (This : T_Client) return Boolean is
    begin
        return This.Fully_Configured;
    end Is_Fully_Configured;

    function Get_Id (This : T_Client) return Positive is
    begin
        return This.Id;
    end Get_Id;

    procedure Start_Identification_Timeout (This : T_Client) is
    begin
        This.Identification_Timeout_Task.Start_Identification_Timeout;
    end Start_Identification_Timeout;

    procedure Confirm_Identification
       (This : T_Client; UDP_Address : Sock_Addr_Type) is
    begin
        This.Identification_Timeout_Task.Confirm_Identification (UDP_Address);
    end Confirm_Identification;

    procedure Abort_Identification (This : T_Client) is
    begin
        if not This.Identification_Timeout_Task'Terminated then
            This.Identification_Timeout_Task.Abort_Identification;
        end if;
    end Abort_Identification;
    function Get_Dispatch_Authorization (This : T_Client) return Boolean is
    begin
        return This.Authorize_Dispatch;
    end Get_Dispatch_Authorization;
    procedure Set_Dispatch_Authorization
       (This : in out T_Client; State : Boolean) is
    begin
        This.Authorize_Dispatch := State;
    end Set_Dispatch_Authorization;

end Client;
