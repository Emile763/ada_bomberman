with Ada.Tags;
with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;

package body Entity is

    procedure Update (This : in out T_Bomb) is
    begin
        if T_Entity_Type'Pos (This.Entity_Type) > T_Entity_Type'Pos (BOMB0)
        then
            --  Decrease the bombe timer
            This.Entity_Type :=
               T_Entity_Type'Val (T_Entity_Type'Pos (This.Entity_Type) - 1);
        else
            This.Alive := False;
        end if;
    end Update;

    procedure Update (This : in out T_Omnidirectional_Explosion) is
    begin
        --  An omnidirection explosion only last one frame
        This.Alive := False;
    end Update;

    procedure Update (This : in out T_Unidirectional_Explosion) is
    begin
        --  Update position depending on direction
        This.Last_Pos_X := This.Pos_X;
        This.Last_Pos_Y := This.Pos_Y;
        case This.Entity_Type is
            when EXPLOSION_EAST  =>
                This.Pos_X := This.Pos_X + 1;

            when EXPLOSION_WEST  =>
                This.Pos_X := This.Pos_X - 1;

            when EXPLOSION_NORTH =>
                This.Pos_Y := This.Pos_Y - 1;

            when EXPLOSION_SOUTH =>
                This.Pos_Y := This.Pos_Y + 1;

            when others          =>
                This.Alive := False;
        end case;
    end Update;
    procedure Update (This : in out T_Player) is
    begin
        --  update position depending on direction
        This.Last_Pos_X := This.Pos_X;
        This.Last_Pos_Y := This.Pos_Y;
        case This.Entity_Type is
            when PLAYER_MOVING_RIGHT =>
                This.Pos_X := This.Pos_X + 1;

            when PLAYER_MOVING_LEFT  =>
                This.Pos_X := This.Pos_X - 1;

            when PLAYER_MOVING_UP    =>
                This.Pos_Y := This.Pos_Y - 1;

            when PLAYER_MOVING_DOWN  =>
                This.Pos_Y := This.Pos_Y + 1;

            when PLAYER_IDLE         =>
                null;

            when others              =>
                This.Alive := False;
        end case;

        --  Reset the player movement to IDLE
        This.Entity_Type := PLAYER_IDLE;
    end Update;

    procedure Free is new
       Ada.Unchecked_Deallocation (Object => T_Bomb, Name => T_Bomb_Access);
    procedure Free is new
       Ada.Unchecked_Deallocation
          (Object => T_Omnidirectional_Explosion,
           Name   => T_Omnidirectional_Explosion_Access);
    procedure Free is new
       Ada.Unchecked_Deallocation
          (Object => T_Unidirectional_Explosion,
           Name   => T_Unidirectional_Explosion_Access);
    procedure Free is new
       Ada.Unchecked_Deallocation
          (Object => T_Player,
           Name   => T_Player_Access);

    procedure Free (Entity_Access : in out T_Entity_Access) is
        use Ada.Tags;
    begin
        if Entity_Access /= null then
            if Entity_Access.all'Tag = T_Bomb'Tag then
                Free (T_Bomb_Access (Entity_Access));
            elsif Entity_Access.all'Tag = T_Omnidirectional_Explosion'Tag then
                Free (T_Omnidirectional_Explosion_Access (Entity_Access));
            elsif Entity_Access.all'Tag = T_Unidirectional_Explosion'Tag then
                Free (T_Unidirectional_Explosion_Access (Entity_Access));
            elsif Entity_Access.all'Tag = T_Player'Tag then
                Free (T_Player_Access (Entity_Access));
            else
                Put_Line ("Unknown Tag for dealocation of entity");
            end if;

        end if;
    end Free;
end Entity;
