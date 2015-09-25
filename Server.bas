/'
	mirrors edge multiplayer server
'/


'#Define SERVER_LINUX
#Define SERVER_WINDOWS

#Define SCREENANZEIGE

#Ifdef SERVER_LINUX

#Define __FB_LINUX__
#Undef __FB_WIN32__

#Include "crt/string.bi"
#Include "crt/sys/types.bi"
#Include "crt/sys/socket.bi"
#Include "crt/netinet/in.bi"
#Include "crt/stdio.bi"
#Include "crt/errno.bi"
#Include "crt/time.bi"
#Include "crt/arpa/inet.bi"
#Include "crt/unistd.bi"
#Include "multiplayerglobals.bi"
#Include "datetime.bi"
#Include "string.bi"

#Else

#Include "win/winsock2.bi"
#Include "win/ws2tcpip.bi"
#Include "win/shellapi.bi"
#Include "fbgfx.bi"
#Include "windows.bi"
#Include "win/tlhelp32.bi"
#Include "crt/string.bi"
#Include "string.bi"
#Include "crt/stdio.bi"
#Include "win/psapi.bi"
#Include "multiplayerglobals.bi"
#Include "vbcompat.bi"
#Include "crt/errno.bi"

#EndIf


Const inidateiname = "serversettings.ini"

Dim Shared As Integer selbersenden		' falls man mal auch an sich selber sendern will
Dim Shared As Integer serverport


Const debug = 0		' macht debugausgaben in debugdatei
Const debugdateiname = "debugdatei.txt"


#Ifdef SERVER_LINUX
Const www = "www/"
#Else
Const www = "www\"
#EndIf
Const npeopleonlinedat  = www+"npeopleonline.txt" : Const npeopleonlinedatnr  = 1
Const serveroutput = www+"serveroutput.txt" : Const serveroutputnr = 2
Const clientposdat = www+"clientpos.txt" : Const clientposdatnr = 3
Const clientnicknamedat = www+"clientnickname.txt" : Const clientnicknamedatnr = 4

Const vordergrund = 0			' farben
Const hintergrund = 15

Const buffsize = 4096			' genereller paket receive buffer

Dim Shared As SOCKET sock		' socket des servers

Type clientlistentry
	As IN_ADDR ip
	As Integer port
	As Double id
	As String nickname
	As Double lasttime
	As Single mapposx, mapposy
	As Integer mapangle
	As Integer levelnumber
End Type

Dim Shared As clientlistentry clientlist(nclientsmax)
Dim Shared As Integer nclients

Dim Shared As Any Ptr printmutex, dateimutex
Dim Shared As Integer sendrate, recvrate

Dim Shared As FILE Ptr file

#Ifdef SERVER_LINUX
Type timeval Field=1
	As Long sec, usec
End Type
#EndIf

Declare Sub main()
main()
End


Sub pushtophone(message As String)
	Dim As String s
	
	Return
	
	#Ifdef SERVER_LINUX
	'Shell("python /home/pi/Instapush/pushtophone.py '" + message + "'")
	
	s = "curl --silent -X POST "
	s+= "-H "+Chr(34)+"x-instapush-appid: xxx"+Chr(34)+" "
	s+= "-H "+Chr(34)+"x-instapush-appsecret: xxx"+Chr(34)+" "
	s+= "-H "+Chr(34)+"Content-Type: application/json"+Chr(34)+" "
	s+= "-d '{"+Chr(34)+"event"+Chr(34)+":"+Chr(34)+"arbitrary_message"+Chr(34)+","+Chr(34)+"trackers"+Chr(34)+":{"+Chr(34)+"message"+Chr(34)+":"+Chr(34)+message+Chr(34)+"}}' "
	s+= "https://api.instapush.im/post >/dev/null &"
	Shell(s)
	
	#Else
	Const w="\"+Chr(34)
	'Shell("python c:/user/xxxx/Instapush/pushtophone.py '" + message + "'")
	
	's = "C:\Windows\System32\curl --silent -X POST "
	s = "curl --silent -k -X POST "
	s+= "-H "+Chr(34)+"x-instapush-appid: xxx"+Chr(34)+" "
	s+= "-H "+Chr(34)+"x-instapush-appsecret: xxx"+Chr(34)+" "
	s+= "-H "+Chr(34)+"Content-Type: application/json"+Chr(34)+" "
	s+= "-d "+Chr(34)+"{"+w+"event"+w+":"+w+"arbitrary_message"+w+","+w+"trackers"+w+":{"+w+"message"+w+":"+w+message+w+"}}"+Chr(34)+" "
	s+= "https://api.instapush.im/post >nul"
	Shell(s)
	
	#EndIf
	
End Sub


Sub mysleep (d As Integer)
	#Ifdef SERVER_LINUX
	usleep (d*1000)
	#Else
	SleepEx (d, 0)
	#EndIf
End Sub


Sub myprint (s As String)
	If debug Then
		s = Format(Now,"dd.mm.yyyy hh:mm:ss ") + Format(Timer,"0.00000  ") + s
		fprintf (file, s+Chr(13)+Chr(10)) : fflush (file)
	EndIf
End Sub


' debug ausgabe auf bildschirm und in serveroutput datei für die webseite
'
Sub debugprint (s As String)

	myprint ("debugprint routine start")

	s = Format(Now, "dd.mm.yyyy, hh:mm:ss  ")+s
	MutexLock (printmutex)
	MutexLock (dateimutex)
	Print s
	Open serveroutput For Append Lock write As #serveroutputnr
	If Err=0 Then
		Print #serveroutputnr, s + "<br>"
		Close #serveroutputnr
	Else
		Print "habnen error in serveroutputdatei"
	EndIf
	MutexUnlock (dateimutex)
	MutexUnlock (printmutex)
End Sub


' liefert die clientnumber zur id
'
Function idtoclientnr (id As Double) As Integer
	Dim As Integer t

	myprint ("idtoclientnr start")

	For t=0 To nclientsmax-1
		If clientlist(t).id=id Then Return t
	Next

	Return -1
End Function


' schaut, ob zwei clients identisch sind
'
Function sameclient (a As clientlistentry, b As clientlistentry) As Integer
	myprint ("sameclient routine start")
	Return memcmp (@a.ip, @b.ip, SizeOf (IN_ADDR))=0 And a.port=b.port
End Function


' sucht, ob client schon in liste vorhanden
'
Function isinclientlist (c As clientlistentry) As Integer
	Dim As Integer t, gef

	myprint ("isinclientlist routine start")

	gef = 0
	For t = 0 To nclientsmax-1
		If sameclient (clientlist(t), c) Then gef = 1 : Exit For
	Next
	Return gef
End Function


' trägt client in liste ein auf niedrigstem freiem platz
'
Function addclient (c As clientlistentry) As Integer
	Dim As Integer t

	myprint ("addclient routine start")

	For t = 0 To nclientsmax-1
		If clientlist(t).port=0 Then
			clientlist(t) = c
			nclients += 1
			Return t
		EndIf
	Next
	Return -1
End Function


' initialisiert einen listenplatz
'
Sub initclientlistentry (t As Integer)
	memset (@clientlist(t).ip, 0, SizeOf(IN_ADDR))
	clientlist(t).port = 0
	clientlist(t).id = 0
	clientlist(t).nickname = ""
	clientlist(t).lasttime = 0
	clientlist(t).mapposx = 2
	clientlist(t).mapposy = 2
	clientlist(t).levelnumber = 12
End Sub


' löscht client auf listenplatz
'
Sub removeclient (t As Integer)

	myprint ("removeclient routine start")

	If t>=0 And t<=nclientsmax-1 Then
		initclientlistentry (t)
		nclients -= 1
	Else
		debugprint ("error: remove client für "+Str(t))
		mysleep (1000)
	EndIf
End Sub


' fragt ab ob listenplatz ein client ist oder leer
'
Function isclient (t As Integer) As Integer

	myprint ("isclient routine start")

	If t>=0 And t<=nclientsmax-1 Then
		Return clientlist(t).port<>0
	Else
		debugprint ("error: isclient für "+Str(t))
		mysleep (1000)
	EndIf
End Function


' aktualisiert die serverdatei für die webseite
'
Sub aktdat ()
	myprint ("aktdat routine start")

	MutexLock (dateimutex)
	Open npeopleonlinedat For Output Lock write As #npeopleonlinedatnr
	If Err=0 Then
		Print #npeopleonlinedatnr, Using "###";nclients
		Close #npeopleonlinedatnr
		MutexUnlock (dateimutex)
	Else
		MutexUnlock (dateimutex)
		debugprint ("hab nen error bei serverdatei")
	EndIf

End Sub


' konvertiert ip und port aus clientlistentry in einen string und gibt ihn zurück
'
Function clientiptostring (c As clientlistentry) As String
	myprint ("clientiptostring routine start")

#Ifdef SERVER_LINUX

	Dim As ZString*128 z
	inet_ntop (AF_INET, @c.ip, @z, 128)
	Return z+":"+Str(c.port)

#Else

	Dim As ZString Ptr z
	z = inet_ntoa (c.ip)
	Return *z+":"+Str(c.port)

#EndIf
End Function


' liefert nickname zu id
'
Function nickfromid (id As Double) As String
	Dim As Integer t

	For t = 0 To nclientsmax-1
		If clientlist(t).id=id Then
			Return clientlist(t).nickname
		EndIf
	Next
	debugprint ("id not found!")
	Return "häää"
End Function


' empfängt pakete und sendet evtl antwort oder leitet sie gleich weiter
'
Sub receivethread (param As Any Ptr)
	Dim As Integer result, gef, t, d
	Dim As SOCKADDR_IN remoteaddr
	Dim As SOCKADDR_IN clientaddr
	Dim As Integer remoteaddrlen
	Dim As Byte Ptr buf
	Dim As clientlistentry c
	Dim As Integer clientnr
	Dim As Integer anzahlpakete(nclientsmax), seqnr, keyframe, x, y
	Dim As Integer pakettyp
	Dim As paket_clientalive Ptr pakca
	Dim As paket_movement Ptr pakm
	Dim As paket_bones Ptr pakb
	Dim As paket_packetclaim Ptr ppcp

	Color vordergrund, hintergrund

	buf = Callocate (buffsize)

	myprint ("receivethread start")

	Do
		remoteaddrlen = SizeOf(SOCKADDR_IN)
		result = recvfrom (sock, buf, buffsize, 0, Cast(SOCKADDR Ptr, @remoteaddr), @remoteaddrlen)

		If result=SOCKET_ERROR Then

#Ifdef SERVER_LINUX
			t = errno
			d = EAGAIN
#Else
			t = WSAGetLastError ()
			d = WSAETIMEDOUT
#EndIf
			
			If t=d Then
				'debugprint ("recv socket timeout")
				myprint ("receivethread socket timeout")

			Else
				debugprint ("recv socket error "+Str(t))
				myprint ("receivethread socket error")
				mysleep (1000)
			EndIf

		ElseIf result<SizeOf(Integer) Or result>=buffsize Then

			myprint ("receivethread komische paket size")

			debugprint ("paket der laenge "+Str(result)+" erhalten")
			'Sleep 1000

		Else

			recvrate += result

			c.ip = remoteaddr.sin_addr
			c.port = remoteaddr.sin_port

			myprint ("receivethread vor pakettyp lesen")

			pakettyp = *Cast(Integer Ptr, buf)

			myprint ("receivethread nach pakettyp lesen")

			If pakettyp=pakettyp_init Then

				myprint ("receivethread pakettyp_init")

				'Print "init von client ";clientiptostring (c)

			ElseIf pakettyp=pakettyp_clientalive Then

				myprint ("receivethread pakettyp_clientalive")

				pakca = Cast (paket_clientalive Ptr, buf)

				For t = 0 To nclientsmax-1
					If sameclient (c, clientlist(t)) Then
						clientlist(t).lasttime = Timer
					EndIf
				Next
				If isinclientlist (c)=0 And nclients<nclientsmax Then
					c.id = pakca->id
					c.nickname = pakca->nickname
					c.lasttime = Timer
					c.mapposx = 2
					c.mapposy = 2
					c.levelnumber = 12
					t = addclient (c)
					If t>-1 Then
						aktdat ()
						debugprint ("added "+c.nickname+", "+clientiptostring(c)+", pos "+Str(t)+", nclients "+Str(nclients))
						pushtophone("added "+c.nickname+", "+clientiptostring(c)+", pos "+Str(t)+", nclients "+Str(nclients))
					EndIf
				EndIf

			ElseIf pakettyp=pakettyp_movement Then

				myprint ("receivethread pakettyp_movement")

				For t = 0 To nclientsmax-1
					If isclient(t) And (sameclient (c, clientlist(t))=0 Or selbersenden=1) Then
						clientaddr.sin_family = AF_INET
						clientaddr.sin_addr = clientlist(t).ip
						clientaddr.sin_port = clientlist(t).port
						result = sendto (sock, buf, result, 0, Cast(SOCKADDR Ptr, @clientaddr), SizeOf(SOCKADDR_IN))
						If result=SOCKET_ERROR Then
							debugprint ("movement relay send error ")
						Else
							sendrate += result
						EndIf
					EndIf
				Next

				clientnr = -1
				For t = 0 To nclientsmax-1
					If sameclient (c, clientlist(t)) Then
						clientnr = t
						Exit For
					EndIf
				Next
				If clientnr>-1 Then
					pakm = Cast (paket_movement Ptr, buf)
					clientlist(clientnr).mapposx = pakm->x
					clientlist(clientnr).mapposy = pakm->y
					clientlist(clientnr).mapangle = pakm->b
					clientlist(clientnr).levelnumber = pakm->levelnumber
				EndIf

			ElseIf pakettyp=pakettyp_bones Then

				myprint ("receivethread pakettyp_bones")

				'clientnr = -1
				'For t = 0 To nclientsmax-1
				'	If sameclient (c, clientlist(t)) Then
				'		clientnr = t
				'		Exit For
				'	EndIf
				'Next
				'If clientnr<>-1 Then
				'	pakb = Cast (paket_bones Ptr, buf)
				'	anzahlpakete(clientnr)+=1
				'	seqnr = pakb->sequencenumber
				'	keyframe = pakb->keyframe
				'	If keyframe=1 Then anzahlpakete(clientnr) = seqnr

				'	MutexLock (printmutex)
				'	x = Pos : y = CsrLin
				'	Locate 2+clientnr,1 : Print Using "######    ";seqnr-anzahlpakete(clientnr)
				'	Locate y,x
				'	MutexUnLock (printmutex)
				'EndIf

				pakb = Cast (paket_bones Ptr, buf)
				For t = 0 To nclientsmax-1
					If isclient(t) Then
						If pakb->idother=0 And (sameclient (c, clientlist(t))=0 Or selbersenden=1) Or pakb->idother>0 And clientlist(t).id=pakb->idother Then

							If pakb->idother>0 Then
								debugprint ("nachgefordertes paket #"+Str(pakb->sequencenumber)+" von "+nickfromid(pakb->id)+" an "+nickfromid(pakb->idother))
							EndIf

							clientaddr.sin_family = AF_INET
							clientaddr.sin_addr = clientlist(t).ip
							clientaddr.sin_port = clientlist(t).port
							result = sendto (sock, buf, result, 0, Cast(SOCKADDR Ptr, @clientaddr), SizeOf(SOCKADDR_IN))
							If result=SOCKET_ERROR Then
								debugprint ("bones relay send error")
							Else
								sendrate += result
							EndIf

						EndIf
					EndIf
				Next

			ElseIf pakettyp=pakettyp_packetclaim Then

				myprint ("receivethread pakettyp_packetclaim")

				ppcp = Cast(paket_packetclaim Ptr, buf)
				For t = 0 To nclientsmax-1
					If isclient(t) And clientlist(t).id=ppcp->idother Then
						clientaddr.sin_family = AF_INET
						clientaddr.sin_addr = clientlist(t).ip
						clientaddr.sin_port = clientlist(t).port
						result = sendto (sock, buf, result, 0, Cast(SOCKADDR Ptr, @clientaddr), SizeOf(SOCKADDR_IN))

						debugprint ("packetclaim von "+nickfromid(ppcp->id)+" an "+nickfromid(ppcp->idother)+" #"+Str(ppcp->seqnr))

						If result=SOCKET_ERROR Then
							debugprint ("packetclaim relay send error")
						Else
							sendrate += result
						EndIf
					EndIf
				Next

			Else

				myprint ("receivethread unknown pakettyp")

				debugprint ("unknown pakettyp "+Str(pakettyp)+" erhalten")
				'Sleep 1000

			EndIf

		EndIf
	Loop While nclients>=0

	debugprint ("receivethread ended")

	myprint ("receivethread ended")

End Sub


' removed clients aus der liste, von denen lange kein alive erhalten wurde, und sendet remove ghost an alle
'
Sub checkthread (param As Any Ptr)
	Dim As Integer t, d, result
	Dim As clientlistentry c
	Dim As sockaddr_in clientaddr

	myprint ("checkthread start")

	Color vordergrund, hintergrund
	Do
		mysleep (2500)
		For t = 0 To nclientsmax-1
			myprint ("checkthread for "+Str(t))
			If isclient(t) And clientlist(t).lasttime<Timer-5 Then
				c = clientlist(t)
				removeclient (t)
				aktdat ()
				debugprint ("removed "+c.nickname+", "+clientiptostring (c)+", nclients "+Str(nclients))
				pushtophone("removed "+c.nickname+", "+clientiptostring (c)+", nclients "+Str(nclients))
			EndIf
		Next
	Loop While nclients>=0

	debugprint ("checkthread ended")
	myprint ("checkthread ended")
End Sub


' initialisiert die gesamte client liste
'
Sub initclientlist ()
	Dim As Integer t

	For t=0 To nclientsmax-1
		initclientlistentry (t)
	Next
	nclients = 0
End Sub


' schreibt die positionsdaten in die datei für die webseite
'
Sub mapupdatethread (param As Any Ptr)
	Dim As Integer t, x, y, counter, nicknamelen
	Dim As String nickname

	myprint ("mapupdatethread start")

	Color hintergrund,vordergrund

	initclientlist ()

	Do
		mysleep (200)
		counter+=1

		myprint ("mapupdate vor dateimutexlock")
		MutexLock (dateimutex)
		myprint ("mapupdate vor open")
		Open clientposdat For Output Lock write As #clientposdatnr
		myprint ("mapupdate nach open")
		If Err=0 Then
			For t=0 To nclientsmax-1
				Print #clientposdatnr, Using "####### ";clientlist(t).mapposx;
				Print #clientposdatnr, Using "####### ";clientlist(t).mapposy;
				Print #clientposdatnr, Using "####### ";clientlist(t).mapangle;
				Print #clientposdatnr, Using "## ";clientlist(t).levelnumber;
			Next
			Close #clientposdatnr
			MutexUnlock (dateimutex)
		Else
			MutexUnlock (dateimutex)
			debugprint ("error: couldnt open clientpos file")
		EndIf

		If counter Mod 5 = 0 Then
			MutexLock (dateimutex)
			Open clientnicknamedat For Output Lock write As #clientnicknamedatnr
			If Err=0 Then
				For t=0 To nclientsmax-1
					nickname = clientlist(t).nickname
					nicknamelen = Len(nickname)
					While nicknamelen<16
						nickname += " "
						nicknamelen += 1
					Wend
					Print #clientnicknamedatnr, nickname
				Next
				Close #clientnicknamedatnr
			Else
				debugprint ("error: couldnt open clientpos file")
			EndIf
			MutexUnlock (dateimutex)

			''''''''''''''''''''''''

			myprint ("mapupdate vor printmutex")

			MutexLock (printmutex)
			x = Pos : y = CsrLin
			Locate 1,1
			Print Format(Now, "dd.mm.yyyy, hh:mm:ss")+"   ";
			Print Using "send: ######## byte/s ##.### mbit/s   ";sendrate;sendrate*8/1000000;
			Print Using "recv: ######## byte/s ##.### mbit/s   ";recvrate;recvrate*8/1000000;
			Locate y,x
			MutexUnlock (printmutex)

			myprint ("mapupdate nach printmutex")

			sendrate = 0
			recvrate = 0
		EndIf
	Loop While nclients>=0

	debugprint ("mapupdatethread ended")
	myprint ("mapupdatethread ended")
End Sub


' sendet server alive an alle clients
'
Sub alivethread (param As Any Ptr)
	Dim As clientlistentry c
	Dim As sockaddr_in clientaddr
	Dim As Integer result, d, t
	Dim As paket_serveralive paksa

	Do
		For d = 0 To nclientsmax-1
			If isclient(d) Then
				clientaddr.sin_family = AF_INET
				clientaddr.sin_addr = clientlist(d).ip
				clientaddr.sin_port = clientlist(d).port

				paksa.typ = pakettyp_serveralive
				paksa.nclients = nclients
				For t=0 To nclientsmax-1
					paksa.id(t) = clientlist(t).id
					paksa.nickname(t) = clientlist(t).nickname
				Next

				result = sendto (sock, Cast (Byte Ptr, @paksa), SizeOf(paksa), 0, Cast(SOCKADDR Ptr, @clientaddr), SizeOf(SOCKADDR_IN))
				If result=SOCKET_ERROR Then
					debugprint ("server alive send error "+Str(result))
				Else
					sendrate += result
				EndIf
			EndIf
		Next

		myprint ("alivethread")

		mysleep (1000)
	Loop While nclients>=0

	debugprint ("alivethread ended")
	myprint ("alivethread ended")
End Sub


#Ifdef SERVER_WINDOWS
Sub initnetwork ()
	Dim As WSADATA wsa
	Dim As Integer result

	Print "WSAStartup... ";
	result = WSAStartup (MAKEWORD(2,2), @wsa)
	If result=0 And wsa.wVersion=&h202 Then
		Print "ok"
	Else
		Print result, Hex(wsa.wVersion)
	EndIf
End Sub
#EndIf


Sub loadinifile ()
	Open inidateiname For Input As #1
	Input #1,serverport
	Input #1,selbersenden
	Close #1
End Sub


' hauptprogramm
'
Sub main()
	Dim As Integer result
	Dim As SOCKADDR_IN addr
	Dim As Any Ptr thr1, thr2, thr3, thr4
	Dim As String i

	If debug Then
		file = fopen (debugdateiname, "a")
		If file=0 Then Print "error: no debugdatei!"
	EndIf

	printmutex = MutexCreate () : If printmutex=0 Then Print "error: no printmutex!"
	dateimutex = MutexCreate () : If dateimutex=0 Then Print "error: no dateimutex!"

	loadinifile ()

	aktdat ()

	pushtophone("Server gestartet")

	Const WID = 1024
	Const HEI = 592		' vielfaches von 16 für hohen font
	#Ifdef SCREENANZEIGE
	ScreenRes WID,HEI
	Width WID/8,HEI/16
	WindowTitle "Mirror's Edge Multiplayer Server"
	#EndIf
	Color vordergrund, hintergrund
	Cls

	Print "mirrors edge multiplayer server started..."

	Print "serverport: ";serverport
	Print "selbersenden: ";selbersenden

#Ifdef SERVER_WINDOWS
	initnetwork ()
#EndIf

	Print "opensocket... ";
	sock = OpenSocket (AF_INET, SOCK_DGRAM, 0)
	If sock<0 Then Print "fehler" : Else Print "ok"

	addr.sin_family = AF_INET
	addr.sin_port = htons(serverport)
	addr.sin_addr.s_addr = INADDR_ANY

	Print "bind... ";
	result = bind (sock, Cast(SOCKADDR Ptr, @addr), SizeOf(SOCKADDR_IN))
	If result=SOCKET_ERROR Then Print "fehler" : Else Print "ok"

	Print "setsockopt... ";
#Ifdef SERVER_WINDOWS
	Dim As Integer tv
	tv = 1000
	result = setsockopt (sock, SOL_SOCKET, SO_RCVTIMEO, Cast (Any Ptr, @tv), SizeOf(tv))
#Else
	Dim As timeval tv
	tv.sec = 1
	tv.usec = 0
	result = setsockopt (sock, SOL_SOCKET, SO_RCVTIMEO, @tv, SizeOf(tv))
#EndIf
	If result=SOCKET_ERROR Then Print "fehler" : Else Print "ok"

	' threads starten

	thr1 = ThreadCreate (@receivethread, 0)
	thr2 = ThreadCreate (@checkthread, 0)
	thr3 = ThreadCreate (@mapupdatethread, 0)
	thr4 = ThreadCreate (@alivethread, 0)

	Do
		mysleep (1123)
		myprint ("main loop")
		i = InKey
	Loop Until i="q"

	' ende
	Print "closing..."
	nclients = -1
	aktdat ()
	pushtophone("Server beendet")
	ThreadWait (thr4)
	ThreadWait (thr3)
	ThreadWait (thr2)
	ThreadWait (thr1)
	closesocket (sock)
	'Print "ende, bitte taste druecken..."
	'Sleep
	MutexDestroy (printmutex)
	MutexDestroy (dateimutex)
	If debug And file<>0 Then fclose (file)
	End
End Sub
