# Global Description
This project aims to cover a wide range of concepts of the Ada language. To achieve this, a Bomberman game seems interesting enough without being too much complex.
A Bomberman game takes place on a map with solid and breakable walls. Each player can use bombs to move through the map and eliminate other players.  
A server hosts the game, and clients communicate with the server by sending player inputs via UDP.  
The server communicates with the clients by sending map updates via TCP.  
The game state is updated only by the server.  
When all players are dead, a new game with a new map is launched.  


https://github.com/user-attachments/assets/a5cb5334-00c4-40d1-990c-3077253e5c13


# Warning
Some files, such as "communication_manager.adb" and especially "client.adb," need some refactoring to clarify the code.

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
