with Ada.Characters.Latin_1;
with Ada.Text_IO;  use Ada.Text_IO;
with Game_Manager; use Game_Manager;
with Ada.Calendar;

procedure Server_Main is

    task Game_Loop_Task is
        entry Stop;
    end Game_Loop_Task;
    task body Game_Loop_Task is
        Game : aliased T_Game (4);
    begin
        Game.Start_Game;

        declare
            use Ada.Calendar;
            Period    : constant Duration := 1.0 / 10.0;
            Next_Time : Time := Clock + Period;
        begin
            Game_Loop :
            loop
                select
                    delay until Next_Time;
                or
                    accept Stop;
                    exit Game_Loop;
                end select;
                Game.Update;
                Next_Time := Next_Time + Period;
            end loop Game_Loop;
        end;

        Put_Line ("Wait for client Manager to close...");
        Game.End_Game;

    end Game_Loop_Task;

    Buffer : Character;

begin

    Put_Line ("Press DEL to stop...");

    loop
        Get_Immediate (Buffer);
        exit when Buffer = Ada.Characters.Latin_1.DEL;
    end loop;

    Game_Loop_Task.Stop;
end Server_Main;
