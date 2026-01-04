# Global Description
This project aims to cover a wide range of concepts in the Ada language. To achieve this, a Bomberman game was chosen as a suitable example, being interesting without being too complex. A Bomberman game takes place on a map with solid and breakable walls, where each player can use bombs to navigate the map and eliminate other players.

A server hosts the game, and clients communicate with the server by sending player inputs via UDP. The server sends map updates to the clients via TCP. The game state is updated exclusively by the server. When all players are dead, a new game with a new map is launched.


https://github.com/user-attachments/assets/a5cb5334-00c4-40d1-990c-3077253e5c13


# Warning
Some files, such as "communication_manager.adb" and especially "client.adb," need some refactoring to clarify the code.

# Some Concepts Used
For the drawing :  
* ANSI Code
  - Clear the console
  - Move the cursor
  - Change the foreground/background color
  - Switch to the alternative console
* System signals
  - SIGWINCH handlet to know if the console has been resized (only for Unix)
General Ada concepts:
* Interfacing with C
* Access, access all
* tagged / limited tagged / subtype/ abstract / interface / protected
* record with parameters (for polymorphism)
* Ada std containers -> Hashed_Map; Hashed_Set; Vector
* task / task types
* entry (for tasks and protected objects) / select / accept / terminate / delay ...
* generic packages
* arrays
* GNAT sockets
* Allocation (new) and Deallocation (Ada.Unchecked_Deallocation)
* Exceptions

# How To Compile
Gprbuild is needed to build this project.  
At the root of the repository:
```console
gprbuild
```
It will build the server and the client.
By default, the executables are generated in exe/default.  
If you are on Windows, the "libwinpthread-1.dll" is needed.

# How To Use
When launching the server, it will display the port.  
When launching the client, it will ask for the server’s IPv4 address and port.  
Once this information is provided, the client can move its character using <kbd>Z</kbd><kbd>Q</kbd><kbd>S</kbd><kbd>D</kbd>.  
A bomb can be placed by pressing <kbd>Space</kbd>.
