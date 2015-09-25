/'
	endlich ein gescheiter mirrors edge multiplayer
	
	done
	
	todo
	
		keyframe senden jedesmal wenn bones wieder angehen
'/

#Include "win/winsock2.bi"
#Include "win/ws2tcpip.bi"
#Include "win/shellapi.bi"
#Include "fbgfx.bi"
#Include "windows.bi"
#Include "win/tlhelp32.bi"
#Include "crt/string.bi"
#Include "string.bi"
#Include "win/psapi.bi"
#Include "myzlib.bi"
#Include "multiplayerglobals.bi"

Const AUFNAHME = 0		' benutzt gleiche tasten ipojlk, deshalb nur eins von beiden
Const AUGEN = 0

Const inifile = "memultiplayersettings.ini"			' Settings

Const WX = 400		' fenster breite höhe
Const WY = 150

Const hookdllname = "hookdll.dll"
Const exefilename = "MirrorsEdge.exe"
Const windowtitlename = "Mirror's Edge Multiplayer"

Const SINGMAX = 16000		' single nach short umwandlung maximaler wert

Dim Shared As Integer keyframeintervall		' alle x frames ein keyframe
'Const bonesframes = 1					' bones alle x frames senden

Const claimdelay = 3
Const nstoredpackets = 60
Const nlostseqnr = 10

Dim Shared As Integer packetstorepointer
Dim Shared As paket_bones Ptr storedpackets(nstoredpackets)

Dim Shared As HFONT hfont, hfont2		' winapi stuff
Dim Shared As MSG msg
Dim Shared As HWND hWnd
Dim Shared As HDC hdc, hdc2

Dim Shared As SOCKET sock
Dim Shared As SOCKADDR_IN remoteaddr

Dim Shared As HANDLE hProcess		' mirrorsedge.exe process
Dim Shared As Any Ptr baseaddress

Dim Shared As Any Ptr libmodule
Dim Shared As Any Ptr hookdllbaseaddress, hookdllleveloffset
Dim Shared As Any Ptr pipemembones, pipememmovement, pipeframe, pipefaithbones, pipefaithbonesfrom

Type clientlistentry
	As Double id
	As String nickname
End Type

Dim Shared As clientlistentry clientlist(nclientsmax)

Dim Shared As Integer nplayers
Dim Shared As String nickname
Dim Shared As String serveradresse
Dim Shared As Integer serverport

Dim Shared As Integer running, serverrunning

Dim Shared As Integer nghosts
Dim Shared As Integer ghostsoffset
Dim Shared As Double clientid, serverlastalivetime

Dim Shared As Integer programquit

Dim Shared As Integer sendbones

Const gfname = "d:\tmp\test"
Const szgfframe = SizeOf(paket_movement)+szbonesbuf
Dim Shared As Integer ghostrecordtoggle, ghostreplaytoggle, autoresettoggle
Dim Shared As Integer grecfilenr, gplayfilenr, gfframes(nclientsmax), grfmax
Dim Shared As Integer replayspeed, resetframes


Declare Sub main ()
main ()
End


' inifile mit nickname, etc laden
'
Sub loadinifile ()
	Dim As Integer r
	Dim As String s

	s = Environ("appdata")+"\"+inifile
	Print "inifilepath: ";s

	r = Open (s For Input As #1)

	If r=0 Then
		Input #1,nickname
		Input #1,serveradresse
		Input #1,serverport
		Input #1,keyframeintervall
		Close #1
		Print "nickname: ";nickname
		Print "serveradresse: ";serveradresse
		Print "serverport: ";serverport
		Print "keyframeintervall: ";keyframeintervall
	Else
		Print "couldnt open inifile"
	EndIf
End Sub


' compress raw data
'
Function mycompress (ub As UByte Ptr, us As Integer, cb As UByte Ptr) As Integer
	Dim As Integer l

	l = us
	If compress (cb, @l, ub, us)<>Z_OK Then Print "compress returned an error"

	Return l
End Function


' decompress raw data
'
Sub decompress (cb As UByte Ptr, cs As Integer, ub As UByte Ptr, us As Integer)
	Dim As Integer l

	l = us
	If uncompress (ub, @l, cb, cs)<>Z_OK Then Print "uncompress returned an error"

	If l<>us Then Print "decompressed len not matching predicted"
End Sub


Function base1 () As Any Ptr
	Dim As Any Ptr tm

	If vers=version_steam101 Or vers=version_steamprophet Then
		ReadProcessMemory (hProcess, Cast (Byte Ptr, baseaddress+&h1B73F1C), @tm, SizeOf(Integer), 0)
		ReadProcessMemory (hProcess,                          tm+&hcc,       @tm, SizeOf(Integer), 0)
	ElseIf vers = version_origindlc Then
		ReadProcessMemory (hProcess, Cast (Byte Ptr, baseaddress+&h1B83414), @tm, SizeOf(Integer), 0)
		ReadProcessMemory (hProcess,                          tm+&he4,       @tm, SizeOf(Integer), 0)
	Else
		ReadProcessMemory (hProcess, Cast (Byte Ptr, baseaddress+&h1B7C39C), @tm, SizeOf(Integer), 0)
		ReadProcessMemory (hProcess,                          tm+&hcc,       @tm, SizeOf(Integer), 0)
	EndIf

	Return tm
End Function


Function path1 () As Any Ptr
	Dim As Any Ptr tm

	ReadProcessMemory (hProcess, base1()+&h4a4, @tm, SizeOf(Integer), 0)
	ReadProcessMemory (hProcess,      tm+&h214, @tm, SizeOf(Integer), 0)

	Return tm
End Function


Function bbase () As Any Ptr
	Dim As Any Ptr tm

	ReadProcessMemory (hProcess, path1()+&h5dC, @tm, SizeOf(Integer), 0)
	ReadProcessMemory (hProcess,      tm+&h24C, @tm, SizeOf(Integer), 0)

	Return tm
End Function


Function faithcam () As Any Ptr
	Dim As Any Ptr tm

	If vers=version_steam101 Or vers=version_steamprophet Then
		ReadProcessMemory (hProcess, Cast (Any Ptr, baseaddress+&h1C55FCC), @tm, SizeOf(Integer), 0)
	ElseIf vers = version_origindlc Then
		Print "UNKNOWN POINTER PATH FOR FAITHCAM OMG :D"
		Sleep 500
	Else
		ReadProcessMemory (hProcess, Cast (Any Ptr, baseaddress+&h1C6F104), @tm, SizeOf(Integer), 0)
	EndIf

	ReadProcessMemory (hProcess, tm+&h144, @tm, SizeOf(Integer), 0)
	ReadProcessMemory (hProcess, tm+&h40, @tm, SizeOf(Integer), 0)

	Return tm+&h504
End Function


Function gameinfo () As Any Ptr
	Dim As Any Ptr tm

	ReadProcessMemory (hProcess, base1()+&h9C, @tm, SizeOf(Integer), 0)

	Return tm
End Function


Function quaternionenprodukt (a As quaternion, b As quaternion) As quaternion
	Dim As quaternion q

	q.w = a.w*b.w - a.x*b.x - a.y*b.y - a.z*b.z
	q.x = a.w*b.x + a.x*b.w + a.y*b.z - a.z*b.y
	q.y = a.w*b.y - a.x*b.z + a.y*b.w + a.z*b.x
	q.z = a.w*b.z + a.x*b.y - a.y*b.x + a.z*b.w

	Return q
End Function


Function achsewinkelzuquaternion (a As achsewinkel) As quaternion
	Dim As quaternion q

	q.w = Cos(a.w/2)
	q.x = a.x*Sin(a.w/2)
	q.y = a.y*Sin(a.w/2)
	q.z = a.z*Sin(a.w/2)

	Return q
End Function


Function quaternionzuachsewinkel (q As quaternion) As achsewinkel
	Dim As achsewinkel a

	a.w = ACos(q.w)*2

	If a.w<>0 Then
		a.x = q.x/Sin(a.w/2)
		a.y = q.y/Sin(a.w/2)
		a.z = q.z/Sin(a.w/2)
	Else
		a.x = 0
		a.y = 0
		a.z = 0
	EndIf

	Return a
End Function


Function quaternionnormieren (a As quaternion) As quaternion
	Dim As Single d

	d = Sqr(a.w^2 + a.x^2 + a.y^2 + a.z^2)
	a.w/=d : a.x/=d : a.y/=d : a.z/=d

	Return a
End Function


Sub quaterniondrucken (q As quaternion)
	Print Using "qw: ##.####   ";q.w;
	Print Using "qx: ##.####   ";q.x;
	Print Using "qy: ##.####   ";q.y;
	Print Using "qz: ##.####   ";q.z
End Sub


Sub achsewinkeldrucken (q As achsewinkel)
	Print Using "wi: ####.##   ";q.w/pi*180;
	Print Using "ex: ##.####   ";q.x;
	Print Using "ey: ##.####   ";q.y;
	Print Using "ez: ##.####   ";q.z
End Sub


Sub dosystem()
	While PeekMessage (@msg, hWnd, 0, 0, PM_REMOVE)
		TranslateMessage (@msg)
		DispatchMessage (@msg)
	Wend
End Sub


' bestimmt mirrors edge exe version nach dateigröße
'
Function getmeversion () As Integer
	Dim As ZString*1024 buf
	Dim As HANDLE hfile
	Dim As ULongInt fsize
	Dim As Integer gef, t, r, ret

	t = 0
	Do
		ret = GetModuleFileNameEx (hProcess, 0, @buf, 1024)
		If ret=0 Then Sleep 1000
		t+=1
	Loop While ret=0 And t<10
	hfile = CreateFile (@buf, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, 0, 0)
	ret = GetFileSizeEx (hfile, Cast (PLARGE_INTEGER, @fsize))

	gef = 0
	For t = 0 To ngamevers-2
		If fsize=gvers(t).executablesize Then
			gef = 1
			r = t
			Exit For
		EndIf
	Next
	If gef=0 Then
		r = ngamevers-1
	EndIf

	CloseHandle (hfile)

	Return r
End Function


' liefert den nickname zu der id
'
Function getnickfromid (id As Double) As String
	Dim As Integer t

	If id=clientid Then Return nickname

	For t = 0 To nclientsmax-1
		If clientlist(t).id=id Then
			Return clientlist(t).nickname
		EndIf
	Next
	Print "id not found in getnickfromid() ";id
	Return "haeaeaeae"
End Function


' macht aus zeilen wieder spalten
'
Sub drehbufrueck (a As Byte Ptr, b As Byte Ptr, c As Integer, d As Integer)
	Dim As Integer t, r
	Dim As Byte Ptr dp, p1

	p1 = a
	For t = 0 To d-1
		dp =  b + t
		For r = 0 To c-1
			*dp = *p1
			dp+=d
			p1+=1
		Next
	Next
End Sub


' macht aus short wieder singles und fügt die letzte 1 spalte dazu
' zusätzlich werden die quaternionen auf 1 normiert
'
Sub shorttosingle (a As Byte Ptr, b As Byte Ptr, c As Integer)
	Dim As Integer v, s
	Dim As Double x, y, z, w, d

	For v = 0 To c-1
		x = CDbl(*Cast(Short Ptr,a+v*14+0*2)) / SINGMAX
		y = CDbl(*Cast(Short Ptr,a+v*14+1*2)) / SINGMAX
		z = CDbl(*Cast(Short Ptr,a+v*14+2*2)) / SINGMAX
		w = CDbl(*Cast(Short Ptr,a+v*14+3*2)) / SINGMAX
		d = Sqr (x^2 + y^2 + z^2 + w^2)
		*Cast(Single Ptr,b+v*32+0*4) = x/d
		*Cast(Single Ptr,b+v*32+1*4) = y/d
		*Cast(Single Ptr,b+v*32+2*4) = z/d
		*Cast(Single Ptr,b+v*32+3*4) = w/d
		For s = 4 To 7-1
			*Cast(Single Ptr,b+v*32+s*4) = CSng(CDbl(*Cast(Short Ptr,a+v*14+s*2)) / SINGMAX * 200)
		Next
		*Cast(Single Ptr,b+v*32+7*4) = 1
	Next
End Sub

/'
' macht aus integer wieder singles und fügt die letzte 1 spalte dazu
' zusätzlich werden die quaternionen auf 1 normiert
'
Sub integertosingle (a As Byte Ptr, b As Byte Ptr, c As Integer)
	Dim As Integer v, s
	Dim As Double x, y, z, w, d

	For v = 0 To c-1
		x = CDbl(*Cast(Integer Ptr,a+v*28+0*4)) / SINGMAX
		y = CDbl(*Cast(Integer Ptr,a+v*28+1*4)) / SINGMAX
		z = CDbl(*Cast(Integer Ptr,a+v*28+2*4)) / SINGMAX
		w = CDbl(*Cast(Integer Ptr,a+v*28+3*4)) / SINGMAX
		d = Sqr (x^2 + y^2 + z^2 + w^2)
		*Cast(Single Ptr,b+v*32+0*4) = x/d
		*Cast(Single Ptr,b+v*32+1*4) = y/d
		*Cast(Single Ptr,b+v*32+2*4) = z/d
		*Cast(Single Ptr,b+v*32+3*4) = w/d
		For s = 4 To 7-1
			*Cast(Single Ptr,b+v*32+s*4) = CSng(CDbl(*Cast(Integer Ptr,a+v*28+s*4)) / SINGMAX * 200)
		Next
		*Cast(Single Ptr,b+v*32+7*4) = 1
	Next
End Sub


' statt memcpy wenn man für differenzen integer statt short benutzen will
'
Sub shorttointegercpy (a As Byte Ptr, b As Byte Ptr, c As Integer)
	Dim As Integer v, d
	For v = 0 To c-1
		For d = 0 To 7-1
			*Cast(Integer Ptr, a+v*28+d*4) = *Cast(Short Ptr, b+v*14+d*2)
		Next
	Next
End Sub
'/

' dieser thread schreibt in ghost memory
' recievethread, recieveproc, damit nulaft das findet...
'
Sub receivethread (param As Any Ptr)
	Dim As Byte buf(256)
	Dim As Integer ret, gef, result
	Dim As Single x, y, z
	Dim As Integer t, b, v, d
	Dim As Single nn(0 To 2)
	Dim As Integer nm, dwWritten, remoteaddrlen
	Dim As Double id
	Dim As ZString*256 s
	Dim As Byte Ptr bonesbuf, readbuf, decompbuf, diffqbuf
	Dim As Short Ptr p2, dp
	'Dim As Integer Ptr p1
	Dim As Short Ptr p1
	Dim As Integer p0
	Dim As Byte Ptr shortbuf(nclientsmax)
	Dim As Integer szcomp, seqnr
	Dim As Integer lastseqnr(nclientsmax)', templastseqnr
	Dim As Integer paketcnt(nclientsmax), g(nclientsmax)
	Dim As Integer lastkeyframeseqnr(nclientsmax), lostseqnr(nclientsmax, nlostseqnr)
	Dim As Integer aktghost, keyframe
	Dim As paket_movement Ptr pakm
	Dim As paket_bones Ptr pakb
	Dim As paket_packetclaim pakpc
	Dim As paket_packetclaim Ptr ppcp
	Dim As Integer pakettyp
	Dim As pipepaket_movement Ptr pm
	Dim As Any Ptr ta
	Dim As paket_bones Ptr pbp
	Dim As Double idother
	Dim As paket_serveralive Ptr paksa
	Dim As Double gtimer(nclientsmax)


	pm = Callocate (SizeOf(pipepaket_movement))

	readbuf = Callocate (szbonesbuf)
	bonesbuf = Callocate (szbonesbuf)
	decompbuf = Callocate (szbonesbuf)
	diffqbuf = Callocate (szbonesbuf)

	For t=0 To nclientsmax-1
		shortbuf(t) = Callocate (szbonesbuf)
	Next

	For d=0 To nstoredpackets-1
		storedpackets(d) = Callocate (szbonesbuf)
	Next


	Do
		remoteaddrlen = SizeOf(SOCKADDR_IN)
		result = recvfrom (sock, readbuf, szbonesbuf, 0, Cast(SOCKADDR Ptr, @remoteaddr), @remoteaddrlen)

		For t = 0 To nclientsmax-1
			If Timer-gtimer(t)>0.1 Then g(t) = 0
		Next

		If result=SOCKET_ERROR Then

			'Print "recv socket error ";WSAGetLastError ();result
			'Sleep 1000

		ElseIf result<SizeOf(Integer) Or result>=szbonesbuf Then

			Print "paket der länge ";result;" erhalten"
			Sleep 1000

		Else

			pakettyp = *Cast(Integer Ptr, readbuf)

			If pakettyp=pakettyp_serveralive Then

				paksa = Cast (paket_serveralive Ptr, readbuf)
				serverlastalivetime = Timer
				nplayers = paksa->nclients

				For t=0 To nclientsmax-1
					clientlist(t).id = paksa->id(t)
					clientlist(t).nickname = paksa->nickname(t)
				Next

				If autoresettoggle Then
					For t = 0 To nclientsmax-1
						If clientlist(t).id=0 Then
							d = (t+ghostsoffset) Mod nclientsmax
							pm->wposx = 0
							pm->wposy = 0
							pm->wposz = 0
							pm->wrotb = 0
							pm->varwert = 0
							pm->gwert = 0
							WriteProcessMemory (hProcess, pipememmovement+d*SizeOf(pipepaket_movement), pm, SizeOf(pipepaket_movement), 0)
						EndIf
					Next
				EndIf

			ElseIf pakettyp=pakettyp_movement Then

				If running=1 Then
					pakm = Cast (paket_movement Ptr, readbuf)

					id = pakm->id
					x = pakm->x : y = pakm->y : z = pakm->z
					b = pakm->b
					v = pakm->v
				
					gef = 0
					For t = 0 To nclientsmax-1
						If id=clientlist(t).id Then
							d = (t + ghostsoffset) Mod nclientsmax
							pm->wposx = x
							pm->wposy = y
							pm->wposz = z
							pm->wrotb = b
							pm->varwert = v
							pm->gwert = g(t)
							WriteProcessMemory (hProcess, pipememmovement+d*SizeOf(pipepaket_movement), pm, SizeOf(pipepaket_movement), 0)
							gef = 1 : Exit For
						EndIf
					Next
				
				EndIf

			ElseIf pakettyp=pakettyp_bones Then

				pakb = Cast (paket_bones Ptr, readbuf)

				If pakb->idother>0 Then
					If pakb->idother<>clientid Then
						Print "hääääääää? komisches paket erhalten! id ";pakb->idother
					Else
						If pakb->sequencenumber>0 Then
							Print "nachgef erhalten #"+Str(pakb->sequencenumber)+" von "+getnickfromid(pakb->id)+" an "+getnickfromid(pakb->idother)+" nach "+Str(Int((Timer-pakb->temps)*1000))+" ms"
						EndIf
					EndIf
				EndIf

				id = pakb->id
				seqnr = pakb->sequencenumber
				keyframe = pakb->keyframe
				szcomp = pakb->compressedsize
				gef = 0
				For t = 0 To nclientsmax-1
					If id=clientlist(t).id Then
						aktghost = t
						gef = 1 : Exit For
					EndIf
				Next
				If gef=1 Then

					gtimer(aktghost) = Timer
		
					decompress (@pakb->cdata(0), szcomp, diffqbuf, nbonesges*14)

					drehbufrueck (diffqbuf, decompbuf, nbonesges, 14)

					'If seqnr-templastseqnr<>1 Then Print seqnr-templastseqnr;" ";
					'templastseqnr = seqnr

					paketcnt(aktghost)+=1

					'Locate 5+aktghost,1 : Print Using "######   ";seqnr-paketcnt(aktghost)
					'If seqnr-paketcnt(aktghost)<>0 Then Print seqnr-paketcnt(aktghost);" ";

					If keyframe=1 Then
					
						g(aktghost) = 1
						memcpy (shortbuf(aktghost), decompbuf, nbonesges*14)
						'shorttointegercpy (shortbuf(aktghost), decompbuf, nbonesges)
						lastseqnr(aktghost) = seqnr
						lastkeyframeseqnr(aktghost) = seqnr
						paketcnt(aktghost) = seqnr
						For t=0 To nlostseqnr-1
							lostseqnr(aktghost, t) = -1
						Next

					ElseIf Abs(seqnr-paketcnt(aktghost))<=5 And seqnr>lastkeyframeseqnr(aktghost) Then

						g(aktghost) = 1
						'p1 = Cast (Integer Ptr, shortbuf(aktghost))
						p1 = Cast (Short Ptr, shortbuf(aktghost))
						p2 = Cast (Short Ptr, decompbuf)
						For t = 1 To nbonesges*7
							p0 = *p1
							p0 += *p2
							If p0> SINGMAX Then p0-=2*SINGMAX
							If p0<-SINGMAX Then p0+=2*SINGMAX
							*p1 = p0
							p1+=1
							p2+=1
						Next

						For t=0 To nlostseqnr-1							' löschen wenn gekriegt
							If lostseqnr(aktghost, t)=seqnr Then
								lostseqnr(aktghost, t) = -1
								Exit For
							EndIf
						Next

						If seqnr>lastseqnr(aktghost)+1 Then				' lücken in lost eintragen
							For t=lastseqnr(aktghost)+1 To seqnr-1

								For d=0 To nlostseqnr-1
									If lostseqnr(aktghost, d)=-1 Then
										lostseqnr(aktghost, d) = t
										Exit For
									EndIf
								Next

							Next
						EndIf

						For t=0 To nlostseqnr-1			' für verlorene nachforderung senden
							If lostseqnr(aktghost, t)>0 And lostseqnr(aktghost, t)+claimdelay<seqnr Then
								pakpc.typ = pakettyp_packetclaim
								pakpc.id  = clientid
								pakpc.idother = id
								pakpc.temps = Timer
								pakpc.seqnr = lostseqnr(aktghost, t)
								sendto (sock, Cast (Byte Ptr, @pakpc), SizeOf(pakpc), 0, Cast(SOCKADDR Ptr, @remoteaddr), SizeOf(SOCKADDR_IN))
								Print "nachforderung fr #"+Str(lostseqnr(aktghost, t))+" gesendet an "+getnickfromid(pakpc.idother)
								lostseqnr(aktghost, t) = -1
							EndIf
						Next

						If seqnr>lastseqnr(aktghost) Then
							lastseqnr(aktghost) = seqnr
						EndIf
					Else
						g(aktghost) = 0
					EndIf

					shorttosingle (shortbuf(aktghost), bonesbuf, nbonesges)

					d = (aktghost+ghostsoffset) Mod nclientsmax
					ReadProcessMemory (hProcess, pipemembones+d*4, @ta, 4, 0)
					WriteProcessMemory (hProcess, ta, bonesbuf, szbonesbuf, 0)

				EndIf

			ElseIf pakettyp=pakettyp_packetclaim Then

				ppcp = Cast(paket_packetclaim Ptr, readbuf)
				idother = ppcp->id
				seqnr = ppcp->seqnr

				gef=0
				For t=0 To nstoredpackets-1
					pbp = storedpackets(t)
					If pbp->sequencenumber=seqnr Then
						pbp->idother = idother
						pbp->temps = ppcp->temps
						result = sendto (sock, Cast (Byte Ptr, pbp), SizeOf(paket_bones)-szbonesbuf+pbp->compressedsize, 0, Cast(SOCKADDR Ptr, @remoteaddr), SizeOf(SOCKADDR_IN))
						If result=SOCKET_ERROR Then Print "send error"
						If seqnr>0 Then Print "packetclaim erfolgreich, #"+Str(seqnr)+" idother: "+getnickfromid(idother)
						gef=1
						Exit For
					EndIf
				Next
				If gef=0 Then
					Print "packetclaim gescheitert, #"+Str(seqnr)+" idother: "+getnickfromid(idother)
				EndIf

			Else

				Print "unknown pakettyp ";pakettyp
				Sleep 1000

			EndIf
		EndIf
	Loop Until programquit
End Sub


' macht aus spalten zeilen, damit es sich besser packen lässt
'
Sub drehbufhin (a As Byte Ptr, b As Byte Ptr, c As Integer, d As Integer)
	Dim As Byte Ptr dp, p1
	Dim As Integer t, r

	dp = b
	For t = 0 To d-1
		p1 = a+t
		For r = 0 To c-1
			*dp = *p1
			dp+=1
			p1+=d
		Next
	Next
End Sub


' macht aus einem bonesbuf mit singles einen aus shorts
' letzte spalte wird weggelassen, da immer 1
'
Sub singletoshort (a As Byte Ptr, b As Byte Ptr, c As Integer)
	Dim As Integer z, s
	Dim As Single f

	For z=0 To c-1
		For s=0 To 4-1
			*Cast(Short Ptr,b+z*14+s*2) = CShort(CDbl(*Cast(Single Ptr,a+z*32+s*4)) * SINGMAX)
		Next
		For s=4 To 7-1
			f = *Cast(Single Ptr,a+z*32+s*4)
			If f>200 Then f=200 : If f<-200 Then f=-200
			*Cast(Short Ptr,b+z*14+s*2) = CShort(CDbl(f) / 200 * SINGMAX)
		Next
	Next
End Sub


' schreibt einen frame ghostdaten in das file bestehend aus einem movementpaktet und einem bonesbuf
'
Sub writeghostfile (mo As paket_movement Ptr, bo As Byte Ptr)
	Dim As Integer ff
	Static As Integer wgfpos(nclientsmax)
	Dim As String wgfname
	
	wgfname = gfname+Str(grecfilenr)+".dat"

	If mo=0 And bo=0 Then
		wgfpos(grecfilenr) = 0
		gfframes(grecfilenr) = 0
		Kill wgfname
	Else
		ff = FreeFile
		Open wgfname For Binary As #ff
		Seek #ff, 1+wgfpos(grecfilenr)
		Put #ff,,*mo,1
		Put #ff,,*bo,szbonesbuf
		wgfpos(grecfilenr) += szgfframe
		gfframes(grecfilenr) += 1
		Close #ff
	EndIf
End Sub


' liest einen frame ghostdaten aus dem file bestehend aus einem movementpaktet und einem bonesbuf
'
Sub readghostfile (nr As Integer, fr As Integer, mo As paket_movement Ptr, bo As Byte Ptr)
	Dim As Integer ff, r, bytesread, rgfpos
	Dim As String rgfname

	rgfpos = fr*szgfframe
	rgfname = gfname+Str(nr)+".dat"
	ff = FreeFile
	r = Open (rgfname For Binary Access Read As #ff)
	If r Then Print "ghost file open error!"
	Do
		Seek #ff, 1+rgfpos
		If Eof(ff) Then rgfpos = 0 Else Exit Do
	Loop
	Get #ff,,*mo,1, bytesread
	If bytesread<>SizeOf(paket_movement) Then Print "bytesread<>paket_movment!"
	Get #ff,,*bo,szbonesbuf,bytesread
	If bytesread<>szbonesbuf Then Print "bytesread<>szbonesbuf!"
	'rgfpos += SizeOf(paket_movement) + szbonesbuf
	'Print rgfpos;" ";
	Close #ff
End Sub


' thread that reads faith data from me memory and sends it
'
Sub sendthread (param As Any Ptr)
	Dim As Integer result, ret, boffset, t, d
	Dim As Byte buf(256), v, v1, v2, v3, v4
	Dim As Any Ptr m, mt, tma
	Dim As Single tm, otm, dt, x, y, z, zoffset
	Dim As Short b
	Dim As Single nn(0 To 2), xoffset, yoffset
	Dim As Integer nm, f
	Dim As Integer remoteaddrlen
	Dim As Byte Ptr bonesbuf, lastbuf, diffbuf, diffqbuf, compbuf, ap, lp, shortbuf
	Dim As Short Ptr p1, p2, dp
	Dim As paket_movement pakm
	Dim As paket_bones pakb
	Dim As Integer szcomp, seqnr, pad, keyframe, levelnumber
	Dim As Double tim1, tim2

	Dim As Any Ptr adrfaithbones, adr
	Dim As Single bonessingle(0 To 107, 0 To 7)
	Dim As quaternion p, q, qq
	Dim As achsewinkel k
	Dim As Single m00, m01, m02, m10, m11, m12, m20, m21, m22
	Dim As Double ez, ey, ex, w, n0, n1, n2, rollw, hochrunter, linksrechts
	Dim As Double tr, s
	Dim As UShort nu(3)
	Dim As Short d0, d1
	
	Dim As Integer ofrm, frm
	Dim As Single augenlr, augenou, augenoff


	bonesbuf = Callocate (szbonesbuf)
	lastbuf = Callocate (szbonesbuf)
	diffbuf = Callocate (szbonesbuf)
	diffqbuf = Callocate (szbonesbuf)
	compbuf = Callocate (szbonesbuf)
	shortbuf = Callocate (szbonesbuf)

	ap = shortbuf
	lp = lastbuf

	Do
		Do
			Sleep 1
			otm = tm
			ReadProcessMemory (hProcess, path1()+&hA8, @tm, SizeOf(Single), 0) 'Faith time
			If programquit Then Exit Do
		Loop While tm-otm<=0		' 0.010 = minimum damit server nicht gespamt wird, geht nicht wg reaction time!
		If programquit Then Exit Do

			
		adrfaithbones = bbase ()

		If pipefaithbonesfrom<>0 Then	
			WriteProcessMemory (hprocess, pipefaithbonesfrom, @adrfaithbones, SizeOf(Any Ptr), 0)
		EndIf
		If pipefaithbones<>0 Then
			ReadProcessMemory (hProcess, pipefaithbones, bonesbuf, szbonesbuf, 0)	   'Alles
		EndIf


		ReadProcessMemory (hProcess, path1()+&h4FE, @v1, SizeOf(Byte), 0) 'EMovement
		ReadProcessMemory (hProcess, path1()+&h503, @v2, SizeOf(Byte), 0) 'WalkingState
		ReadProcessMemory (hProcess, path1()+&h505, @v3, SizeOf(Byte), 0) 'EMoveActionHint
		ReadProcessMemory (hProcess, path1()+&h4F4, @v4, SizeOf(Byte), 0) 'EAgainstWallState

		ReadProcessMemory (hProcess, path1()+&h5D4, @zoffset, SizeOf(Single), 0) 'Zoffset

		'boffset=0 : zoffset=0

		'Standing, Walking, Running
		If v1= 1 And v2=0 And v4=0 Then v =  1		'Standing
		If v1= 1 And v2=1 And v4=0 Then v =  1
		If v1= 1 And v2=2          Then v =  2		'Walking
		If v1= 1 And v2=3          Then v =  2
		If v1= 1 And v2=4          Then v =  3		'Running
		If v1= 1 And v2=5          Then v =  3

		'Crouched, Crouch Walking, Sliding
		If v1=15 And v2=0          Then v =  4		'Crouch Stand
		If v1=15 And v2=1 And v3=3 Then v =  5		'Crouch Walking Fwd
		If v1=15 And v2=2 And v3=3 Then v =  5
		If v1=15 And v2=1 And v3=4 Then v = 33		'Crouch Walking Fwd
		If v1=15 And v2=2 And v3=4 Then v = 33
		If v1=15 And v2=1 And v3=1 Then v = 34		'Crouch Walking Fwd
		If v1=15 And v2=2 And v3=1 Then v = 34
		If v1=15 And v2=1 And v3=2 Then v = 35		'Crouch Walking Fwd
		If v1=15 And v2=2 And v3=2 Then v = 35
		If v1=16                   Then v = 12		'Slide
		If v1=38                   Then v = 41		'RumpSlide
		If v1=63                   Then v = 32		'MeleeCrouch
		If v1=48                   Then v = 23		'MeleeSlide

		'Jump Spot and Fwd
		If v1=11 And v2=0          Then v =  8		'Vertical Jump
		If v1=11 And v2=1          Then v =  8
		If v1=32 And v2=0          Then v = 31		'Vertical Kick
		If v1=32 And v2=1          Then v = 31
		If v1=11 And v2=2          Then v =  9		'Forward Jump
		If v1=11 And v2=3          Then v =  9
		If v1=11 And v2=4          Then v =  9
		If v1=11 And v2=5          Then v =  9
		If v1=61                   Then v = 15		'Coiling
		If v1=32 And v2=2          Then v = 24		'Jump Kick
		If v1=32 And v2=3          Then v = 24
		If v1=32 And v2=4          Then v = 24
		If v1=32 And v2=5          Then v = 24

		'Sidestep
		If v1=33 And v3=1          Then v = 13		'Sidestep Left
		If v1=33 And v3=2          Then v = 14		'Sidestep Right

		'Ledge Walk
		If v1=30 And v2=0          Then v = 45		'LedgeWalkIdle
		If v1=30 And v2=2          Then v = 46		'LedgeWalkR/L

		'Backflip shit
		If v1=24                   Then v = 42		'180Turn
		If v1=25                   Then v = 43		'180TurnInAir
		If v1=26                   Then v = 44		'LayOnGround

		'Falling and Uncontrolled Falling
		If v1= 2                   Then v = 10
		If v1=72                   Then v = 11

		'Wallrun, Kickglitch
		If v1= 4                   Then v =  6
		If v1= 5                   Then v =  7
		If v1=62 And v=6           Then v = 16
		If v1=62 And v=7           Then v = 17

		'Rolling
		If v1=91                   Then v = 18

		'Zipline
		If v1=27                   Then v = 19
		If v1=28                   Then v = 20

		'Wallclimb
		If v1= 6                   Then v = 21

		'Hanging
		If v1= 3                   Then v = 22		'Hanging
		If v1=10                   Then v = 30		'GrabPullUp

		'Pipe
		If v1=21 And v3=0          Then v = 36		'PipeClimbIdle
		If v1=21 And v3=3          Then v = 37		'PipeClimbUp
		If v1=21 And v3=4          Then v = 38		'PipeClimbDown
		If v1=31                   Then v = 39		'GrabTransfer
		If v1=22                   Then v = 40		'IntoClimb

		'Other stuff
		If v1=19                   Then v = 25		'Barge
		If v1=17                   Then v = 26		'Melee
		If v1=39                   Then v = 27		'Interact
		If v1= 7                   Then v = 28		'Springboard

		'Against Wall
		If v1= 1 And v2=0 And v4>0 Then v = 29		'Both hands against wall
		If v1= 1 And v2=1 And v4>0 Then v = 29

		ReadProcessMemory (hProcess, path1()+&hE8, @nn(0), 3*SizeOf(Single), 0)		' Faith XYZ
		'x = nn(0) - 200	' für selbersenden
		x = nn(0)
		y = nn(1)
		z = nn(2)+zoffset

		ReadProcessMemory (hProcess, path1()+&hF8, @nm, SizeOf(Integer), 0)			' Faith b
		b = nm

		result = ReadProcessMemory (hProcess, Cast (Byte Ptr, hookdllleveloffset), @nm, SizeOf(Integer), 0)
		levelnumber = nm


		pakm.typ = pakettyp_movement
		pakm.id = clientid
		pakm.x = x : pakm.y = y : pakm.z = z
		pakm.b = b
		pakm.v = v
		pakm.levelnumber = levelnumber

		result = sendto (sock, Cast (Byte Ptr, @pakm), SizeOf(pakm), 0, Cast(SOCKADDR Ptr, @remoteaddr), SizeOf(SOCKADDR_IN))

		
		If AUFNAHME And ghostrecordtoggle Then writeghostfile (@pakm, bonesbuf)


		'If f Mod bonesframes=0 Then
		If sendbones Then

			memcpy (@bonessingle(0, 0), bonesbuf, szbonesbuf)


			' schiefe kamera korrigieren für ghostview

			k.w = b/65536*2*pi
			k.x = 0
			k.y = -1
			k.z = 0
			p = achsewinkelzuquaternion(k)

			For t=0 To 30
				If t<=6 Or t=14 Then
					q.x = bonessingle(t, 0)
					q.y = bonessingle(t, 1)
					q.z = bonessingle(t, 2)
					q.w = bonessingle(t, 3)

					p = quaternionenprodukt(p, q)
				EndIf
			Next

			p.x=-p.x : p.y=-p.y : p.z=-p.z

			adr = path1()+&h9AC
			ReadProcessMemory (hProcess, adr+3*SizeOf(Single), @nu(0), SizeOf(UShort), 0)
			ReadProcessMemory (hProcess, adr+4*SizeOf(Single), @nu(1), SizeOf(UShort), 0)
			ReadProcessMemory (hProcess, adr+5*SizeOf(Single), @nu(2), SizeOf(UShort), 0)

			rollw = nu(2)/65536*360
			hochrunter = nu(0)/65536*360
			linksrechts = nu(1)/65536*360

			k.w = linksrechts/180*pi
			k.x = 0
			k.y = -1
			k.z = 0
			qq = achsewinkelzuquaternion(k)
			p = quaternionenprodukt (p, qq)

			k.w = hochrunter/180*pi
			k.x = 1
			k.y = 0
			k.z = 0
			qq = achsewinkelzuquaternion(k)
			p = quaternionenprodukt (p, qq)

			k.w = rollw/180*pi
			k.x = 0
			k.y = 0
			k.z = 1
			qq = achsewinkelzuquaternion(k)
			p = quaternionenprodukt (p, qq)

			bonessingle(18, 0) = p.x
			bonessingle(18, 1) = p.y
			bonessingle(18, 2) = p.z
			bonessingle(18, 3) = p.w

			bonessingle(18, 4) = 0	' positiv ist links
			bonessingle(18, 5) = 0	' positiv ist runter
			bonessingle(18, 6) = 0	' positiv ist vor

			memcpy (bonesbuf+18*32, @bonessingle(18,0), 32)


			' augen
			If AUGEN Then

				If GetAsyncKeyState (VK_P)<0 Then augenlr += 2
				If GetAsyncKeyState (VK_O)<0 Then augenlr -= 2
	
				If GetAsyncKeyState (VK_U)<0 Then augenou += 1
				If GetAsyncKeyState (VK_J)<0 Then augenou -= 1
	
				If GetAsyncKeyState (VK_K)<0 Then augenoff += 0.005
				If GetAsyncKeyState (VK_L)<0 Then augenoff -= 0.005
	
				k.w = -augenlr/180*pi
				k.x = 0
				k.y = -1
				k.z = 0
				qq = achsewinkelzuquaternion(k)
	
				k.w = augenou/180*pi
				k.x = 1
				k.y = 0
				k.z = 0
				p = achsewinkelzuquaternion(k)
	
				p = quaternionenprodukt (p, qq)
	
				bonessingle(15, 0) = p.x
				bonessingle(15, 1) = p.y
				bonessingle(15, 2) = p.z
				bonessingle(15, 3) = p.w
				bonessingle(15, 6) += augenoff	' z
	
				memcpy (bonesbuf+15*32, @bonessingle(15,0), 7*4)
	
				k.w = (augenlr+180)/180*pi
				k.x = 0
				k.y = 1
				k.z = 0
				qq = achsewinkelzuquaternion(k)
	
				k.w = augenou/180*pi
				k.x = 1
				k.y = 0
				k.z = 0
				p = achsewinkelzuquaternion(k)
				p = quaternionenprodukt (p, qq)
	
				bonessingle(19, 0) = p.x
				bonessingle(19, 1) = p.y
				bonessingle(19, 2) = p.z
				bonessingle(19, 3) = p.w
				bonessingle(19, 6) += augenoff	' z
	
				memcpy (bonesbuf+19*32, @bonessingle(19,0), 7*4)
			EndIf
		
			
			' bonesbuf daten packen			

			singletoshort (bonesbuf, ap, nbonesges)

			If keyframe=1 Then
				memcpy (diffbuf, ap, nbonesges*14)
				'Print "timer: ";(tim2-tim1)*1000;" ms"
			Else
				tim1 = Timer

				dp = Cast (Short Ptr, diffbuf)
				p1 = Cast (Short Ptr, ap)
				p2 = Cast (Short Ptr, lp)
				For t = 1 To nbonesges*14/2
					d0 = *p1 - *p2
					If d0<-SINGMAX Then d0+=SINGMAX*2
					If d0> SINGMAX Then d0-=SINGMAX*2
					*dp = d0
					dp+=1
					p1+=1
					p2+=1
				Next
			EndIf

			drehbufhin (diffbuf, diffqbuf, nbonesges, 14)

			szcomp = mycompress (diffqbuf, nbonesges*14, @pakb.cdata(0))
			pakb.compressedsize = szcomp
			pakb.sequencenumber = seqnr
			pakb.keyframe = keyframe
			pakb.id = clientid
			pakb.idother = 0		' an alle
			pakb.typ = pakettyp_bones

			Swap ap, lp
			seqnr+=1

			If seqnr Mod keyframeintervall=0 Then keyframe = 1 Else keyframe = 0

			tim2 = Timer

			memcpy (storedpackets(packetstorepointer), @pakb, SizeOf(pakb)-szbonesbuf+szcomp)
			packetstorepointer+=1
			If packetstorepointer>nstoredpackets-1 Then packetstorepointer=0

			result = sendto (sock, Cast (Byte Ptr, @pakb), SizeOf(pakb)-szbonesbuf+szcomp, 0, Cast(SOCKADDR Ptr, @remoteaddr), SizeOf(SOCKADDR_IN))
			'Print result;" ";
		EndIf

		f+=1
	Loop Until programquit
End Sub


' netzwerk kommunikation initialisieren
'
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


' server anfrage
'
Sub getserverip ()
	Dim As SOCKADDR_IN addr
	Dim As ADDRINFO hints
	Dim As ADDRINFO Ptr res
	Dim As IN_ADDR inaddr
	Dim As Integer result
	Dim As SOCKADDR_IN serveraddr
	Dim As Integer serv, strl, serveraddrlen
	Dim As ZString*100 z
	Dim As paket_init paki
	Dim As Integer tv

	Print "contacting server..."

	'Print "getaddrinfo... ";
	hints.ai_flags = 0
	hints.ai_family = AF_INET
	hints.ai_socktype = SOCK_DGRAM
	hints.ai_protocol = IPPROTO_UDP
	res = 0
	result = getaddrinfo (serveradresse, Str(serverport), @hints, @res)
	'If result=0 Then
	'	Print "getaddrinfo ok"
	'Else
	'	Print "error ";result
	'EndIf

	closesocket(sock)
	'Print "opensocket... ";
	sock = OpenSocket (res->ai_family, res->ai_socktype, res->ai_protocol)
	'If sock=INVALID_SOCKET Then Print "fehler" : Else Print "ok"

	tv = 100
	If setsockopt (sock, SOL_SOCKET, SO_RCVTIMEO, Cast(Byte Ptr, @tv), SizeOf(tv))<0 Then
		Print "setsockopt error"
	EndIf

	'Print "wsa address to string... ";
	strl = 100
	result = WSAAddressToString (res->ai_addr, res->ai_addrlen, 0, @z, @strl)
	'If result=SOCKET_ERROR Then
	'	Print "error ";WSAGetLastError ()
	'Else
	'	Print "ok"
	'EndIf
	'Print z

	'Print "wsa string to address... ";
	serveraddrlen = SizeOf (serveraddr)
	result = WSAStringToAddress (@z, AF_INET, 0, Cast (SOCKADDR Ptr, @serveraddr), @serveraddrlen)
	'If result=SOCKET_ERROR Then
	'	Print "error ";WSAGetLastError ()
	'Else
	'	Print "ok"
	'EndIf

	'Print "sendto init... ";
	paki.typ = pakettyp_init
	result = sendto (sock, Cast (Byte Ptr, @paki), SizeOf(paki), 0, Cast(SOCKADDR Ptr, @serveraddr), SizeOf(SOCKADDR_IN))
	'If result=SOCKET_ERROR Then
	'	Print "error ";WSAGetLastError ()
	'Else
	'	Print "ok"
	'EndIf

	remoteaddr = serveraddr
End Sub


' sendet alive paket an server
'
Sub alivethread (param As Any Ptr)
	Dim As Integer result
	Dim As paket_clientalive pakca

	Do
		pakca.typ = pakettyp_clientalive
		pakca.id = clientid
		pakca.nickname = nickname
		result = sendto (sock, Cast(Byte Ptr, @pakca), SizeOf(pakca), 0, Cast(SOCKADDR Ptr, @remoteaddr), SizeOf(SOCKADDR_IN))
		If result=SOCKET_ERROR Then Print "error alivethread "+Str(WSAGetLastError ())
		Sleep 1000
	Loop Until programquit
End Sub


' stellt sachen im fenster dar
'
Sub displayproc (userdata As Any Ptr)
	Dim As Integer l
	Dim As String i
	Dim As SIZE size
	Const zh = 30

	Do
		dosystem()
		Sleep 10

		SelectObject (hdc2, GetStockObject(WHITE_PEN))
		Rectangle (hdc2, 0, 0, WX, WY)
		SelectObject (hdc2, GetStockObject(BLACK_PEN))
		SelectObject (hdc2, hfont2)

		If running=1 Then
			i = "ME is running, Version: " + *gvers(vers).versionname
			GetTextExtentPoint32 (hdc2, i, Len(i), @size)
			TextOut (hdc2, (WX - size.cx)/2, zh*1, i, Len(i))
		ElseIf running=0 Then
			i = "ME is not running."
			GetTextExtentPoint32 (hdc2, i, Len(i), @size)
			TextOut (hdc2, (WX - size.cx)/2, zh*1, i, Len(i))
		EndIf

		If serverrunning=1 Then
			If nplayers=1 Then i = "" Else i = "s"
			i = "Server is running, " + Str(nplayers) + " player" + i + " online"
			GetTextExtentPoint32 (hdc2, i, Len(i), @size)
			TextOut (hdc2, (WX - size.cx)/2, zh*2, i, Len(i))
		Else
			i = "Waiting for server..."
			GetTextExtentPoint32 (hdc2, i, Len(i), @size)
			TextOut (hdc2, (WX - size.cx)/2, zh*2, i, Len(i))
		EndIf

		BitBlt (hdc, 0, 0, WX, WY, hdc2, 0, 0, SRCCOPY)
	Loop Until programquit
End Sub


' install hook braucht das
'
Sub EnableDebugPriv ()
	Dim As HANDLE hToken
	Dim As LUID luid
	Dim As TOKEN_PRIVILEGES tkp
	Dim As Integer res

	res = OpenProcessToken (GetCurrentProcess(), TOKEN_ADJUST_PRIVILEGES+TOKEN_QUERY, @hToken)
	res = LookupPrivilegeValue (NULL, SE_DEBUG_NAME, @luid)
	tkp.PrivilegeCount = 1
	tkp.Privileges(0).Luid = luid
	tkp.Privileges(0).Attributes = SE_PRIVILEGE_ENABLED
	res = AdjustTokenPrivileges (hToken, FALSE, @tkp, SizeOf(tkp), NULL, NULL)
	CloseHandle (hToken)
End Sub


' installiert die hook dll in der mirrorsedge.exe
' und holt die speicheradressen in der dll
'
Sub installhook ()
	Dim As PROCESSENTRY32 entry
	Dim As String dirpath, fullpath
	Dim As HANDLE snapshot, hprocess, res, hThread
	Dim As Any Ptr libaddr, llparam
	Dim As Integer result, t, tm
	Dim As ZString*256 z

	entry.dwSize = SizeOf (PROCESSENTRY32)
	snapshot = CreateToolhelp32Snapshot (TH32CS_SNAPPROCESS, NULL)

	If Process32First (snapshot, @entry) Then
		Do
			If entry.szExeFile=exefilename Then
				dirPath = CurDir
				fullPath = dirpath+"\"+hookdllname
				'Print "dll path: ";fullpath

				EnableDebugPriv ()
				hProcess = OpenProcess (PROCESS_ALL_ACCESS, FALSE, entry.th32ProcessID)

				libAddr = GetProcAddress (GetModuleHandle ("kernel32.dll"), "LoadLibraryA")
				llParam = VirtualAllocEx (hProcess, NULL, Len(fullPath)+1, MEM_RESERVE+MEM_COMMIT, PAGE_READWRITE)
				'Print "loadlibrary adr, virtualalloc adr ";Hex(libaddr);" ";Hex(llparam)
				result = WriteProcessMemory (hProcess, llParam, StrPtr(fullPath), Len(fullPath)+1, NULL)
				'Print "writeprocessmemory result ";result
				ReadProcessMemory (hProcess, llparam, @z, Len(fullpath)+1, 0)
				'Print "nochmal dll path aus memory ";z
				hThread = CreateRemoteThread (hProcess, NULL, NULL, libAddr, llParam, NULL, NULL)
				'Print "createremotethread handle, error ";Hex(hThread);", ";GetLastError ()
				WaitForSingleObject (hThread, INFINITE)
				GetExitCodeThread (hThread, @libmodule)
				Print "getexitcodethread result ";Hex(libmodule);", error ";GetLastError ()
				CloseHandle (hThread)
				VirtualFreeEx (hProcess, llParam, Len(fullpath)+1, MEM_RELEASE)

				hookdllbaseaddress = libmodule
				For t = 0 To 60000/4-1
					result = ReadProcessMemory (hProcess, Cast (Byte Ptr, hookdllbaseaddress+t*4), @tm, SizeOf(Integer), 0)
					'If result=0 Then Print "readmemory failed"
					If tm=&h13370420 Then
						Print "found at offset ";Hex(t*4)
						hookdllleveloffset = hookdllbaseaddress + t*4 + 1*4
						ReadProcessMemory (hProcess, hookdllbaseaddress + t*4 + 2*4, @pipemembones, SizeOf(Any Ptr), 0)
						ReadProcessMemory (hProcess, hookdllbaseaddress + t*4 + 3*4, @pipememmovement, SizeOf(Any Ptr), 0)
						pipeframe = hookdllbaseaddress + t*4 + 4*4
						ReadProcessMemory (hProcess, hookdllbaseaddress + t*4 + 5*4, @pipefaithbones, SizeOf(Any Ptr), 0)
						pipefaithbonesfrom = hookdllbaseaddress + t*4 + 6*4
						Exit For
					EndIf
				Next
				Print "hookdllleveloffset ";Hex(hookdllleveloffset)
				Print "pipemembones ";Hex(pipemembones)
				Print "pipememmovement ";Hex(pipememmovement)
				Print "pipeframe ";Hex(pipeframe)

				CloseHandle (hProcess)
			EndIf
		Loop While Process32Next (snapshot, @entry)
	EndIf

	CloseHandle (snapshot)
End Sub


' deinstalliert die hook dll aus mirrorsedge.exe
'
Sub uninstallhook ()
	Dim As PROCESSENTRY32 entry
	Dim As String dirpath, fullpath
	Dim As HANDLE snapshot, hprocess, res, hThread
	Dim As Any Ptr libaddr, llparam
	Dim As Integer result
	Dim As ZString*256 z

	entry.dwSize = SizeOf (PROCESSENTRY32)
	snapshot = CreateToolhelp32Snapshot (TH32CS_SNAPPROCESS, NULL)

	If Process32First (snapshot, @entry) Then
		Do
			If entry.szExeFile=exefilename Then
				EnableDebugPriv ()
				hProcess = OpenProcess (PROCESS_ALL_ACCESS, FALSE, entry.th32ProcessID)

				libAddr = GetProcAddress (GetModuleHandle ("kernel32.dll"), "FreeLibrary")
				hThread = CreateRemoteThread (hProcess, NULL, NULL, libAddr, libmodule, NULL, NULL)
				Print "createremotethread return ";Hex(hThread);", error ";GetLastError ()
				WaitForSingleObject (hThread, INFINITE)
				GetExitCodeThread (hThread, @libmodule)
				Print "freelibrary return ";Hex(libmodule);", error ";GetLastError ()
				CloseHandle (hThread)

				CloseHandle (hProcess)
			EndIf
		Loop While Process32Next (snapshot, @entry)
	EndIf

	CloseHandle (snapshot)
End Sub


' thread der immer schaut ob mirrorsedge.exe noch läuft
'
Sub runningproc (userdata As Any Ptr)
	Dim As Integer gef, t, hookdllgefunden
	Dim As HANDLE snapshot, hToken
	Dim As PROCESSENTRY32 entry
	Dim As MODULEENTRY32 modentry
	Dim As LUID luid
	Dim As TOKEN_PRIVILEGES tkp
	Dim As String i

	OpenProcessToken (GetCurrentProcess (), TOKEN_ADJUST_PRIVILEGES Or TOKEN_QUERY, @hToken)
	LookupPrivilegeValue (NULL, SE_DEBUG_NAME, @luid)
	tkp.PrivilegeCount = 1
	tkp.Privileges(0).Luid = luid
	tkp.Privileges(0).Attributes = SE_PRIVILEGE_ENABLED
	AdjustTokenPrivileges (hToken, FALSE, @tkp, SizeOf(tkp), NULL, NULL)
	CloseHandle(hToken)

	Do
		gef=0
		entry.dwSize = SizeOf(PROCESSENTRY32)
		snapshot = CreateToolhelp32Snapshot (TH32CS_SNAPPROCESS, NULL)
		If Process32First (snapshot, @entry) = TRUE Then
			Do
				If entry.szExeFile=exefilename Then gef=1 : Exit Do
			Loop While Process32Next (snapshot, @entry) = TRUE
		EndIf
		CloseHandle (snapshot)
		If gef=1 Then
			If running<>1 Then
				hProcess = OpenProcess (PROCESS_ALL_ACCESS, FALSE, entry.th32ProcessID)

				For t = 1 To 100
					gef = 0
					modentry.dwSize = SizeOf(MODULEENTRY32)
					snapshot = CreateToolhelp32Snapshot (TH32CS_SNAPMODULE, entry.th32ProcessID)

					If Module32First (snapshot, @modentry) = TRUE Then
						Do
							If modentry.szModule=exefilename Then gef=1 : Exit Do
						Loop While Module32Next (snapshot, @modentry) = TRUE
					EndIf

					CloseHandle (snapshot)
					If gef=1 Then Exit For
					Sleep 10
				Next
				If gef=1 Then
					Print "memodul gefunden nach ";t;" versuchen"
					baseaddress = modentry.modBaseAddr
					vers = getmeversion ()
					installhook ()
					running = 1
					Print "habe running auf 1 gesetzt"
					hookdllgefunden = 0
				Else
					Print "couldnt find memodule!"
				EndIf

			EndIf
		Else
			running=0
			CloseHandle (hProcess)
		EndIf
		Sleep 100
	Loop Until programquit
End Sub


' windows message procedure
'
Function WndProc (hWnd As HWND, uMsg As UINT, wParam As WPARAM, lParam As LPARAM) As Integer

	Select Case (uMsg)
		Case WM_PAINT
			BitBlt (hdc, 0, 0, WX, WY, hdc2, 0, 0, SRCCOPY)

		Case WM_KEYDOWN
			'If LoByte(wParam)=27 Then PostMessage (hWnd, WM_CLOSE, 0, 0)

		Case WM_DESTROY
			PostQuitMessage (0)
			programquit = 1
	End Select

	Return DefWindowProc (hWnd, uMsg, wParam, lParam)
End Function


' öffnet window
'
Sub openwindow ()
	Dim As HBITMAP hbm
	Dim As WNDCLASS wcls
	Dim As String appname
	Dim As Integer sx, sy, sycaption

	appname = "mirrors edge multiplayer"
	With wcls
		.style         = CS_HREDRAW Or CS_VREDRAW Or CS_DBLCLKS
		.lpfnWndProc   = @WndProc
		.cbClsExtra    = 0
		.cbWndExtra    = 0
		.hInstance     = GetModuleHandle (NULL)
		.hIcon         = LoadIcon (NULL, IDI_APPLICATION)
		.hCursor       = LoadCursor (NULL, IDC_ARROW)
		.hbrBackground = CPtr (HGDIOBJ, COLOR_BACKGROUND)
		.lpszMenuName  = NULL
		.lpszClassName = StrPtr (appName)
	End With
	RegisterClass (@wcls)

	hfont = CreateFont (40, 20, 0, 0, FW_NORMAL, 0, 0, 0, ANSI_CHARSET, OUT_DEFAULT_PRECIS, _
	CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY, DEFAULT_PITCH+FF_ROMAN, "Arial")

	hfont2 = CreateFont (20, 8, 0, 0, FW_NORMAL, 0, 0, 0, ANSI_CHARSET, OUT_DEFAULT_PRECIS, _
	CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY, DEFAULT_PITCH+FF_ROMAN, "Arial")

	sx = GetSystemMetrics (SM_CXFULLSCREEN)
	sy = GetSystemMetrics (SM_CYFULLSCREEN)
	sycaption = GetSystemMetrics (SM_CYSIZE)

	hWnd = CreateWindowEx (0, appname, windowtitlename, _
	WS_OVERLAPPED+WS_SYSMENU+WS_VISIBLE+WS_BORDER, _
	sx-WX, sy-WY+sycaption, WX, WY, 0, 0, 0, 0 )

	SendMessage (hwnd, WM_SETICON, ICON_SMALL, _
	Cast( Integer, LoadImage ( GetModuleHandle (NULL), _
	"MY_ICON", IMAGE_ICON, 0, 0, LR_COPYFROMRESOURCE  ) ) )

	hdc = GetDC (hwnd)

	hdc2 = CreateCompatibleDC (hdc)
	hbm = CreateCompatibleBitmap (hdc, WX, WY)
	SelectObject (hdc2, hbm)
End Sub


' ein sleep, das aber trotzdem das window responsive lässt
'
Sub sleepanddosystem (t As Integer)
	Dim As Integer d
	For d = 1 To t
		dosystem ()
		Sleep 1
	Next
End Sub


' thread der immer testet ob der server noch läuft
' wenn nicht, wird automatisch versucht neu zu connecten
'
Sub serverrunningproc (userdata As Any Ptr)
	Dim As Integer remoteaddrlen, result
	Dim As paket_serveralive paksa

	Do
		Sleep 2500

		If serverlastalivetime<Timer-5 Then
			serverrunning = 0
			getserverip()
		Else
			serverrunning = 1
		EndIf
	Loop Until programquit
End Sub


' spielt das ghostfile ab
'
Sub ghostreplaythread (userdata As Any Ptr)
	Dim As paket_movement Ptr pakm
	Dim As pipepaket_movement Ptr pm
	Dim As Byte Ptr bonesbuf, shortbuf, integerbuf
	Dim As Integer t, d, gesch, f
	'Dim As Single tm, otm
	Dim As Any Ptr ta
	
	bonesbuf = Allocate (szbonesbuf)
	shortbuf = Allocate (szbonesbuf)
	integerbuf = Allocate (szbonesbuf)
	pm = Allocate (SizeOf(pipepaket_movement))
	pakm = Allocate (SizeOf(paket_movement))

	Do
		'Do
		'	Sleep 1
		'	otm = tm
		'	ReadProcessMemory (hProcess, path1()+&hA8, @tm, SizeOf(Single), 0) 'Faith time
		'	If programquit Then Exit Do
		'Loop While tm-otm<=0		' 0.010 = minimum damit server nicht gespamt wird, geht nicht wg reaction time!
		'If programquit Then Exit Do

		If replayspeed=0 Then
			Sleep 10
		Else
			Sleep 1000.0/60*(10.0/Abs(replayspeed))^2
		EndIf

		'If ghostreplaytoggle=0 Then f = 0	' für stop statt pause
	
		If resetframes Then
			f = 0
			resetframes = 0
		EndIf

		If ghostreplaytoggle And running Then
		
			For t = 1 To gplayfilenr
				readghostfile (t, f, pakm, bonesbuf)
		
				d = (t + ghostsoffset) Mod nclientsmax
		
				'If gesch=0 Then
				pm->wposx = pakm->x
				pm->wposy = pakm->y
				pm->wposz = pakm->z
				pm->wrotb = pakm->b
				pm->varwert = pakm->v
				pm->gwert = 1 'g(t)
				'gesch = 1
				'EndIf
				WriteProcessMemory (hProcess, pipememmovement+d*SizeOf(pipepaket_movement), pm, SizeOf(pipepaket_movement), 0)
		
				'singletoshort (bonesbuf, shortbuf, nbonesges)
				'shorttosingle (shortbuf, bonesbuf, nbonesges)
		
				ReadProcessMemory (hProcess, pipemembones+d*4, @ta, 4, 0)
				WriteProcessMemory (hProcess, ta, bonesbuf, szbonesbuf, 0)
			Next
			
			f += Sgn(replayspeed)
			If f>grfmax-1 Then f = 0
			If f<0 Then f = grfmax-1
		EndIf
	Loop
End Sub


Sub getghostrecordingfiledata ()
	Dim As Integer t, ff, r
	
	For t = 1 To nclientsmax
		ff = FreeFile
		r = Open (gfname+Str(t)+".dat" For Binary Access Read As #ff)
		If r<>0 Then
			Exit For
		Else
			gplayfilenr = t
			gfframes(gplayfilenr) = Lof(ff)/szgfframe
			Print "found ghostfile nr ";gplayfilenr;" with ";gfframes(gplayfilenr);" frames"
			Close #ff
		EndIf
	Next
End Sub


' hauptroutine
'
Sub main ()
	Dim As HANDLE thr1, thr2, thr3, thr4, thr5, thr6
	Dim As Integer t, d, vkpcnt, vklcnt
	Dim As Integer ghostrecordtogglecnt, ghostreplaytogglecnt, autoresettogglecnt, bkeycnt
	Dim As pipepaket_movement pm
	Dim As Integer rkeycnt, rkeytoggle, ikeycnt, kkeycnt, stopkeycnt, sendboneskeycnt
	Dim As Single gamespeed

	clientid = Timer

	autoresettoggle = 1
	replayspeed = 10
	sendbones = 1
	
	loadinifile()

	openwindow ()

	Print "starting display thread... ";
	thr1 = ThreadCreate (@displayproc, 0)
	If thr1=0 Then Print "displaythread failed" : Else Print "displaythread ok"

	initnetwork ()
	getserverip ()

	Print "starting alive thread... ";
	thr2 = ThreadCreate (@alivethread, 0)
	If thr2=0 Then Print "alivethread failed" : Else Print "alivethread ok"

	Print "starting runningproc... ";
	thr3 = ThreadCreate (@runningproc, 0)
	If thr3=0 Then Print "runningproc failed" : Else Print "runningproc ok"

	Print "starting serverrunning thread... ";
	thr4 = ThreadCreate (@serverrunningproc, 0)
	If thr4=0 Then Print "serverrunning failed" : Else Print "serverrunning ok"

	'Print "waiting for server"
	'Do
	'	sleepanddosystem (10)
	'Loop Until serverrunning=1
	'Print "server running"

	Print "starting sendthread... ";
	thr5 = ThreadCreate (@sendthread, 0)
	If thr5=0 Then Print "sendthread failed" : Else Print "sendthread ok"

	Print "starting receivethread... ";
	thr6 = ThreadCreate (@receivethread, 0)
	If thr6=0 Then Print "receivethread failed" : Else Print "receivethread ok"

	If AUFNAHME Then ThreadCreate (@ghostreplaythread, 0)

	'sleepanddosystem (3000)
	'Cls
	
	Do
		sleepanddosystem (3)


		If AUFNAHME Then

			If GetAsyncKeyState (VK_L)<0 Then autoresettogglecnt+=1 Else autoresettogglecnt = 0
			If autoresettogglecnt=1 Then
				autoresettoggle = 1-autoresettoggle
				Print "autoresettoggle: ";autoresettoggle
			EndIf
	
			If GetAsyncKeyState (VK_O)<0 Then ghostrecordtogglecnt+=1 Else ghostrecordtogglecnt = 0
			If ghostrecordtogglecnt=1 Then
				If ghostrecordtoggle=0 Then
					grecfilenr+=1
					Print "recording nr ";grecfilenr
				Else
					Print "recorded ";gfframes(grecfilenr);" frames, recording off"
				EndIf
				ghostrecordtoggle = 1-ghostrecordtoggle
			EndIf
	
			If GetAsyncKeyState (VK_P)<0 Then ghostreplaytogglecnt+=1 Else ghostreplaytogglecnt = 0
			If ghostreplaytogglecnt=1 Then
				If ghostreplaytoggle=0 Then
					getghostrecordingfiledata ()
					If ghostrecordtoggle Then gplayfilenr-=1
					grfmax = gfframes(gplayfilenr)
					For t = 1 To gplayfilenr
						If gfframes(t)<grfmax Then grfmax = gfframes(t)
					Next
					Print "replay on for ";gplayfilenr;" ghosts, fmax=";grfmax
				Else
					Print "replay off"
				EndIf
				ghostreplaytoggle = 1-ghostreplaytoggle
			EndIf
	
			If GetAsyncKeyState(VK_T)<0 Then rkeycnt+=1 Else rkeycnt = 0
			If rkeycnt=1 Then
				rkeytoggle = 1-rkeytoggle
				If rkeytoggle Then gamespeed = 0.1 Else gamespeed = 1
				WriteProcessMemory (hProcess, gameinfo()+&hC2C, @gamespeed, SizeOf(Single), 0)
			EndIf
	
			If GetAsyncKeyState(VK_I)<0 Then ikeycnt+=1 Else ikeycnt = 0
			If ikeycnt=1 Then
				replayspeed += 1
				If replayspeed>10 Then replayspeed = 10
			EndIf
	
			If GetAsyncKeyState(VK_K)<0 Then kkeycnt+=1 Else kkeycnt = 0
			If kkeycnt=1 Then
				replayspeed -= 1
				If replayspeed<-10 Then replayspeed = -10
			EndIf
			
			If GetAsyncKeyState(VK_OEM_1)<0 Then stopkeycnt+=1 Else stopkeycnt = 0		' Ü, + wäre OEM_PLUS
			If stopkeycnt=1 Then
				Print "stop gedrückt"
				resetframes = 1
			EndIf

		EndIf


		If AUFNAHME=0 And AUGEN=0 Then

			If GetAsyncKeyState (VK_L)<0 Then sendboneskeycnt+=1 Else sendboneskeycnt = 0
			If sendboneskeycnt=1 Then
				sendbones = 1-sendbones
				Print "sendbones: ";sendbones
			EndIf

		EndIf


		If GetAsyncKeyState (VK_B)<0 Then bkeycnt+=1 Else bkeycnt = 0
		If bkeycnt=1 Then
			ghostsoffset+=1
			If ghostsoffset=nclientsmax Then ghostsoffset = 0
			For t = 0 To nclientsmax-1
				pm.wposx = 0
				pm.wposy = 0
				pm.wposz = 0
				pm.wrotb = 0
				pm.varwert = 0
				pm.gwert = 0
				WriteProcessMemory (hProcess, pipememmovement+t*SizeOf(pipepaket_movement), @pm, SizeOf(pipepaket_movement), 0)
			Next
		EndIf
		
	Loop Until programquit

	Print "closing..."

	ThreadWait (thr1)
	Print "displaythread ended"

	ThreadWait (thr2)
	Print "alivethread ended"

	ThreadWait (thr3)
	Print "runningproc ended"

	ThreadWait (thr4)
	Print "serverrunningproc ended"

	ThreadWait (thr5)
	Print "sendthread ended"

	ThreadWait (thr6)
	Print "receivethread ended"

	closesocket (sock)

	uninstallhook ()

	Print "ende. bitte taste drücken..."
	'Sleep
	End
End Sub

