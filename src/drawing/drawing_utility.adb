with Ada.Characters.Latin_1;
with Ada.Text_IO; use Ada.Text_IO;

package body Drawing_Utility is

    function Positive_To_String (Number : Positive) return String is
    begin
        return Positive'Image (Number) (2 .. Number'Image'Length);
    end Positive_To_String;
    function Natural_To_String (Number : Integer) return String is
    begin
        return Integer'Image (Number) (2 .. Number'Image'Length);
    end Natural_To_String;

    function Color_Byte_To_String (Number : T_Color_Byte) return String;
    function Color_Byte_To_String (Number : T_Color_Byte) return String is
    begin
        return T_Color_Byte'Image (Number) (2 .. Number'Image'Length);
    end Color_Byte_To_String;

    procedure Clear_Screen is
        --  Escape sequence to clear the visible portion of the console
        Clear_Screen_Sequence : constant String :=
           Ada.Characters.Latin_1.ESC & "[2J";
    begin
        Put (Clear_Screen_Sequence);
        Set_Cursor_Pos (1, 2);
    end Clear_Screen;

    procedure Clear_Line is
        --  Escape sequence to clear a line
        Set_Cursor_Pos_Sequence : constant String :=
           Ada.Characters.Latin_1.ESC & "[2K";
    begin
        Put (Set_Cursor_Pos_Sequence);
    end Clear_Line;

    procedure Set_Cursor_Pos (Pos_X, Pos_Y : Positive) is
        --  Escape sequence to set the cursor pos
        Set_Cursor_Pos_Sequence : constant String :=
           Ada.Characters.Latin_1.ESC
           & "["
           & Positive_To_String (Pos_Y)
           & ";"
           & Positive_To_String (Pos_X)
           & "H";
    begin
        Put (Set_Cursor_Pos_Sequence);
    end Set_Cursor_Pos;

    procedure Set_Foreground_Color (R, G, B : T_Color_Byte) is
        --  Escape sequence to set the foreground color
        --  ESC[38;2;⟨r⟩;⟨g⟩;⟨b⟩m Select RGB foreground color
        Set_Foreground_Color_Sequence : constant String :=
           Ada.Characters.Latin_1.ESC
           & "[38;2;"
           & Color_Byte_To_String (R)
           & ";"
           & Color_Byte_To_String (G)
           & ";"
           & Color_Byte_To_String (B)
           & "m";
    begin
        Put (Set_Foreground_Color_Sequence);
    end Set_Foreground_Color;

    procedure Set_Background_Color (R, G, B : T_Color_Byte) is
        --  Escape sequence to set the background color
        --  ESC[48;2;⟨r⟩;⟨g⟩;⟨b⟩m Select RGB background color
        Set_Background_Color_Sequence : constant String :=
           Ada.Characters.Latin_1.ESC
           & "[48;2;"
           & Color_Byte_To_String (R)
           & ";"
           & Color_Byte_To_String (G)
           & ";"
           & Color_Byte_To_String (B)
           & "m";
    begin
        Put (Set_Background_Color_Sequence);
    end Set_Background_Color;

    procedure Reset_Colors is
        --  Escape sequence to reset background and foreground colors
        --  ESC[39;49m
        Reset_Colors_Sequence : constant String :=
           Ada.Characters.Latin_1.ESC & "[39;49m";
    begin
        Put (Reset_Colors_Sequence);
    end Reset_Colors;

    procedure Hide_Cursor is
        --  Escape sequence to hide the cursor
        --  ESC[?25l
        Hide_Cursor_Sequence : constant String :=
           Ada.Characters.Latin_1.ESC & "[?25l";
    begin
        Put (Hide_Cursor_Sequence);
    end Hide_Cursor;

    procedure Alternative_Console is
        --  Escape sequence to hide the cursor
        --  ESC[?25l
        Alternative_Console_Sequence : constant String :=
           Ada.Characters.Latin_1.ESC & "[?1049h";
    begin
        Put (Alternative_Console_Sequence);
    end Alternative_Console;
end Drawing_Utility;
