--  with Interfaces.C; use Interfaces.C;

package Drawing_Utility is

    type T_Color_Byte is new Integer range 0 .. 255;
    function Positive_To_String (Number : Positive) return String;
    function Natural_To_String (Number : Integer) return String;

    procedure Clear_Screen;
    procedure Clear_Line;
    procedure Set_Cursor_Pos (Pos_X, Pos_Y : Positive);

    procedure Set_Foreground_Color (R, G, B : T_Color_Byte);
    procedure Set_Background_Color (R, G, B : T_Color_Byte);

    procedure Reset_Colors;

    procedure Hide_Cursor;

    procedure Alternative_Console;

    procedure Initialize_Resized_Callback
    with Import => True, Convention => C, External_Name => "initResizeHandler";

    function Check_Resized return Integer
    with Import => True, Convention => C, External_Name => "checkResized";

end Drawing_Utility;
