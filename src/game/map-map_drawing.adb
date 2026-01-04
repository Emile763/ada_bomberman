with Ada.Text_IO;     use Ada.Text_IO;
with Drawing_Utility; use Drawing_Utility;

package body Map.Map_Drawing is
    procedure Put (Cell : T_Cell_Type);
    procedure Put (Cell : T_Cell_Type) is
    begin
        Set_Background_Color (R => 180, G => 180, B => 180);
        case Cell is
            when PLAYER_IDLE .. PLAYER_MOVING_UP =>
                Set_Foreground_Color (R => 50, G => 50, B => 50);
                Put ("🯆");

            when BOMB0 .. BOMB9                  =>
                Set_Foreground_Color (R => 227, G => 85, B => 14);
                Put
                   (Natural_To_String
                       (Integer
                           (T_Cell_Type'Pos (Cell)
                            - T_Cell_Type'Pos (BOMB0))));

            when EXPLOSION .. EXPLOSION_WEST     =>
                Set_Foreground_Color (R => 255, G => 255, B => 255);
                Put ("#");

            when WALL                            =>
                Set_Background_Color (R => 50, G => 50, B => 50);
                Put (" ");

            when BREAKABLE_WALL                  =>
                Set_Background_Color (R => 100, G => 100, B => 100);
                Put (" ");

            when others                          =>
                Put (" ");
        end case;
        Reset_Colors;

    end Put;

    procedure Draw (This : T_Map) is
    begin
        Set_Cursor_Pos (1 + This.Drawing_X_Offset, 1 + This.Drawing_Y_Offset);
        Put (" ");
        for I in 1 .. This.Width loop
            Put ("🭻");
        end loop;
        Put (" ");
        for J in 1 .. This.Height loop
            New_Line;
            Put ("🭵");
            for I in 1 .. This.Width loop
                Set_Background_Color
                   (R => T_Color_Byte (255 / This.Height * J),
                    G => T_Color_Byte (255 / This.Width * I),
                    B => 0);
                Put (This.Get_Cell (I, J));
            end loop;

            Put ("🭰");
        end loop;
        New_Line;
        Put (" ");
        for I in 1 .. This.Width loop
            Put ("🭶");
        end loop;
        Put (" ");
    end Draw;

    procedure Draw_Cell (This : T_Map; Pos_X, Pos_Y : Positive) is
    begin

        Set_Cursor_Pos
           (1 + This.Drawing_X_Offset + Pos_X,
            1 + This.Drawing_Y_Offset + Pos_Y);
        Set_Background_Color
           (R => T_Color_Byte (255 / This.Height * Pos_Y),
            G => T_Color_Byte (255 / This.Width * Pos_X),
            B => 0);
        Put (This.Get_Cell (Pos_X, Pos_Y));
        Reset_Colors;

    end Draw_Cell;

end Map.Map_Drawing;
