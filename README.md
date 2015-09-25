# What is this?

This is a Multiplayer mod for the single player game Mirror's Edge, the Windows PC version. It runs as a client Multiplayer.exe and a server Server.exe. The server can run on a linux machine like a Raspberry pi as well.


# How to install

1. Find where Mirror's Edge is installed and write that in gamedir.txt

2. Copy the Maps folder over the corresponding folder in TdGame\CookedPC. This will overwrite original files. You can also use the batch files, then it makes backups, and you can uninstall them later.

3. Put the memultiplayersettings.ini in your Users\xxx\AppData\Roaming folder

4. Run Server.exe, then Multiplayer.exe, then the game

If everything worked out right, the Multiplayer.exe client and the Server.exe should exchange packets on the localhost (127.0.0.1).
To go a step further, you can try the multiplayer in your local network.
For this you need the game installed on an additional machine like a laptop.
Enter ipconfig in a command line on the machine where the server will be running.
Put the ip in the memultiplayersettings.ini instead of the 127.0.0.1, on both machines.
Run Server.exe on the PC, and Multiplayer.exe on the PC and the laptop.
You should see another player now on both machines if you go e.g. in the prologue.


# How to play over the internet

The clients now need to know the internet adress of the computer the Server.exe is running on.
Either you figure this ip out everytime you play e.g. by using a website like www.whatismyip.com or you use a dynamic dns like www.noip.com.
In addition now you need to configure your router to forward packets to the computer the Server.exe is running on. The port is 1234 but you can change this in the memultiplayersettings.ini and the serversettings.ini. The protocol type is UDP.
For choosing a ddns name make sure, it is very unique, since if you choose something easy like "memultiplayer.ddns.net" chances are this is already taken, and then people can play over your router (since there is no login/password system).


# How it works

The Multiplayer.exe reads with the help of the hook.dll data from the game's memory. From the location, orientation and animation data it makes a packet that we call movement packet, every frame. Additionally it reads detailed animation data for every joint and makes a packet that we like to call bones packet. Since this is more data, that also doesnt change sometimes from frame to frame, we send only the differences and pack it. Every now and then keyframes for this data are sent. This is the number in the memultiplayersettings.ini that is 600 by default, send a bones packet keyframe every 10 seconds (at 60 fps). Now if such delta packets are lost, this leads to wrong animation data. So depending on the quality of your connection you might want to lower this number to send keyframes more often. But keep in mind that this will increase your upload and that of the server.

The Multiplayer.exe sends these packets every frame to the Server.exe and the Server.exe duplicates them and sends them to every other Multiplayer.exe. So the upload scales as n*(n-1) for the Server with n being the number of players. So if you have a low upload and want to player with >5 people, there is something you can do: you can switch off the bones packets. Just press L and your fellow players should notice more crude animations. Also the Server should show less data throughput.


# Keys

L toggle bones packets
J toggle collision for Faith
B cycle through meshes
V ghost view
G teleport to ghost view


# How to compile

You need freebasic freebasic.net, the 32bit version, even on 64bit machines, since MirrorEdge.exe is a 32bit application, and for all the pointers and offsets to be calculated correctly we need 32bit. Also, Windows doesn't allow a 64bit dll to be hooked into a 32bit program.

For the Multiplayer to compile with the gzip stuff, you can take a static libz from here:
http://sourceforge.net/projects/fbc/files/Older%20versions/0.90.0/
Or you just use a dynamically linked version.

For the hook.dll use the commandline given in the header remarks section.

For the Server.exe there is the option to compile it under linux. Yes, freebasic also works under linux.

If you use a freebasic IDE like FbEdit you can choose to compile as GUI or as Console. In the latter case a debug console window will be present with valuable information, for the Multiplayer. For Server and hookdll there is a switch in the code to turn on debug output.








