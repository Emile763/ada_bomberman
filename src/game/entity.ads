with Map_Types; use Map_Types;

package Entity is

    type T_Entity is abstract tagged record
        Pos_X, Pos_Y           : Integer;
        Entity_Type            : T_Entity_Type;
        Last_Pos_X, Last_Pos_Y : Integer := 0;
        Alive                  : Boolean := True;
    end record;

    type T_Entity_Access is access all T_Entity'Class;
    procedure Free (Entity_Access : in out T_Entity_Access);

    procedure Update (This : in out T_Entity) is abstract;

    type T_Bomb is new T_Entity with null record;
    type T_Bomb_Access is access all T_Bomb;
    overriding
    procedure Update (This : in out T_Bomb);

    type T_Omnidirectional_Explosion is new T_Entity with null record;
    type T_Omnidirectional_Explosion_Access is
       access all T_Omnidirectional_Explosion;
    overriding
    procedure Update (This : in out T_Omnidirectional_Explosion);

    type T_Unidirectional_Explosion is new T_Entity with null record;
    type T_Unidirectional_Explosion_Access is
       access all T_Unidirectional_Explosion;
    overriding
    procedure Update (This : in out T_Unidirectional_Explosion);

    type T_Player is new T_Entity with record
        Putting_Bomb : Boolean := False;
    end record;
    type T_Player_Access is access all T_Player;
    overriding
    procedure Update (This : in out T_Player);

end Entity;
