/'
		globale sachen für hookdll und multiplayer und server
'/

Const pi = 3.14159265358979323846

Const nclientsmax = 10


Const nbonesges = 108					' bones
Const szbonesbuf = nbonesges*32

Const bblock1 = 7
Const bblock2 = 63
Const szbblock1 = bblock1*32
Const szbblock2 = bblock2*32
Const offsetbblock2 = &h5A0


Type quaternion
	As Single x, y, z, w
End Type

Type achsewinkel
	As Single x, y, z, w
End Type


Type gameversiontable						' game version
	As Integer ExecutableSize
	As ZString Ptr VersionName
	As Integer LevelLoadFunc
	As Integer InterpActorFunc
	As Integer BonesFunc
	As Integer FrameFunc
End Type

Const ngamevers = 8
Dim Shared As gameversiontable GVers(ngamevers) = {_
Type(60167392, @"Reloaded V1.01",   &hDC7650, &h75F2C0, &h8F6BEB, &h15B2),_
Type(31946072, @"Steam V1.01",      &hDC6A70, &h75F1F0, &h8F6B1B, &h15B2),_
Type(60298504, @"Reloaded V1.00",   &hDC7050, &h75F090, &h8F69BB, &h15B2),_
Type(36466688, @"DVD V1.00",        &hDC7050, &h75F090, &h8F69BB, &h15B2),_
Type(36484440, @"Origin/DVD V1.01", &hDC7650, &h75F2C0, &h8F6BEB, &h15B2),_
Type(37291352, @"Origin with DLC",  &hDCA200, &h75F310, &h8F6D3B, &h15B2),_
Type(31596544, @"SteamProphet",     &hDC6A70, &h75F1F0, &h8F6B1B, &h15B2),_
Type(-1, @"Unknown Version!", 0, 0, 0, 0)}

Enum versiontype
version_reloaded101
version_steam101
version_reloaded100
version_dvd100
version_origindvd101
version_origindlc
version_steamprophet
End Enum

Dim Shared As versiontype vers


Type pipepaket_movement		' für dll memory writing
	As Single wposx
	As Single wposy
	As Single wposz
	As Integer wrotb
	As Integer varwert
	As Integer gwert
End Type


Enum pakettyptype			' client/server pakete
pakettyp_init
pakettyp_clientalive
pakettyp_serveralive
pakettyp_movement
pakettyp_bones
pakettyp_packetclaim
End Enum

Type paket_init Field=1
	As Integer typ
End Type

Type paket_clientalive Field=1
	As Integer typ
	As Double id
	As ZString*16 nickname
End Type

Type paket_serveralive Field=1
	As Integer typ
	As Integer nclients
	As Double id(nclientsmax)
	As ZString*16 nickname(nclientsmax)
End Type

Type paket_movement Field=1
	As Integer typ
	As Double id
	As Single x				' koos
	As Single y
	As Single z
	As Integer b			' winkel
	As Integer v			' movement state
	As Integer levelnumber
End Type

Type paket_bones Field=1
	As Integer typ
	As Double id
	As Double idother
	As Double temps
	As Integer sequencenumber
	As Integer keyframe
	As Integer compressedsize
	As Byte cdata(szbonesbuf)	' das ist nur das maximum, die eigentliche länge kommt aus recv result
End Type

Type paket_packetclaim Field=1
	As Integer typ
	As Double id				' eigene id vom anfordernden (?)
	As Double idother			' id vom anderen, von dem was fehlt (?)
	As Double temps
	As Integer seqnr
End Type
