
package body BAL is

    protected body T_BAL is
        procedure Send (Data : Data_Type) is
        begin
            Data_Set.Include (Data);
        end Send;

        procedure Receive_All (Elements : out P_Set.Set) is
        begin
            Elements := Data_Set;
            Data_Set.Clear;
        end Receive_All;
    end T_BAL;

    protected body T_BAL_List is
        procedure Add_Element (Index : Positive) is
        begin
            if Size < Capacity and then BALs (Index) = null then
                BALs (Index) := new T_BAL;
                Size := Size + 1;
            end if;
        end Add_Element;

        procedure Remove_Element (Index : Positive) is
        begin
            if Index <= Capacity and then BALs (Index) /= null then
                Delete (BALs (Index));
                Size := Size - 1;
            end if;
        end Remove_Element;

        procedure Clear is
        begin
            for Index in BALs'Range loop
                Delete (BALs (Index));
            end loop;
            Size := 0;
        end Clear;
        procedure Add_Data (Index : Positive; Data : Data_Type) is
        begin
            if Index <= Capacity and then BALs (Index) /= null then
                BALs (Index).Send (Data);
            end if;
        end Add_Data;
        procedure Get_Data (Index : Positive; Data : out P_Set.Set) is
        begin
            if Index <= Capacity and then BALs (Index) /= null then
                BALs (Index).Receive_All (Data);
            end if;
        end Get_Data;
    end T_BAL_List;

end BAL;
