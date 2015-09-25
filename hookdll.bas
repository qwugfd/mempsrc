/'
	fbc -R -dll -export -Wl " -e _MAIN@12"
'/

#Include "windows.bi"
#Include "crt/stdio.bi"
#Include "multiplayerglobals.bi"

Const debug = 0		' hier debug ein und ausschalten

Const JMP_SIZE = 5

Dim Shared As Any Ptr globalbuf(nclientsmax)
Dim Shared As Integer globalszbuf

Dim Shared As Integer thrstarted
Dim Shared As HANDLE thr, currentprocess

Dim Shared As Integer gadr, gadrmerk(nclientsmax)

Dim Shared As Single posx, posy, posz, drawscale
Dim Shared As Integer rotb

Dim Shared As pipepaket_movement ppm(nclientsmax)

Dim Shared As Integer gef, v, nbones, result, gd, gt
Dim Shared As Any Ptr badr, madr, cadr
Dim Shared As Integer dadr

Dim Shared As Integer wt

' achtung reihenfolge nicht ändern!
Dim Shared As Integer findval = &h13370420, mplevelnumber = 12
Dim Shared As Any Ptr pipemembones = @globalbuf(0), pipememmovement = @ppm(0)
Dim Shared As Integer pipeframe = 0
Dim Shared As Any Ptr pipefaithbones = 0, pipefaithbonesfrom = 0

Dim Shared As Integer levelnumber = 12

Dim Shared As Integer gadrprinten

Enum ghostnamen
Faith
Kate
Celeste
Jacknife
Miller
Kreeg
CopKarl
CopHelm
RunnerCop
AssaultCeleste
End Enum


Declare Function StaticLevelLoadGate Alias "StaticLevelLoadGate" (levelInfo As Any Ptr, unk As Integer, unk2 As Any Ptr) As Integer
Asm
	_StaticLevelLoadGate@12:
	nop
	nop
	nop
	nop
	nop
	nop
	nop

	nop
	nop
	nop
	nop
	nop
End Asm


Declare Sub UpdateActorsGate Alias "UpdateActorsGate" ()
Asm
	_UpdateActorsGate@0:
	nop
	nop
	nop
	nop
	nop
	nop

	nop
	nop
	nop
	nop
	nop
End Asm


Declare Sub BonesFuncGate Alias "BonesFuncGate" ()
Asm
	_BonesFuncGate@0:
	nop
	nop
	nop
	nop
	nop
	nop

	nop
	nop
	nop
	nop
	nop
End Asm


Declare Sub FrameFuncGate Alias "FrameFuncGate" ()
Asm
	_FrameFuncGate@0:
	nop
	nop
	nop
	nop
	nop

	nop
	nop
	nop
	nop
	nop
End Asm


' routine die jeden frame aufgerufen wird
'
Sub FrameFuncHook ()

	Asm
		pop edi
		pop esi
		pop ebx
		
		push eax
		push ebx
		push ecx
		push edx
	End Asm

	If pipefaithbones<>0 And pipefaithbonesfrom<>0 Then
		ReadProcessMemory (currentprocess, pipefaithbonesfrom, pipefaithbones, szbonesbuf, 0)
	EndIf

	pipeframe += 1

	Asm
		pop edx
		pop ecx
		pop ebx
		pop eax

		jmp _FrameFuncGate@0
	End Asm

End Sub


' interp actor update routine
'
Sub UpdateActorsHook ()

	Asm
		pop edi
		pop esi
		pop ebx

		push eax
		push ebx
		push ecx
		push edx

		mov [gadr],ecx

		mov eax,[ecx+&hE8]
		mov [posx],eax

		mov eax,[ecx+&hEC]
		mov [posy],eax

		mov eax,[ecx+&hF0]
		mov [posz],eax

		mov eax,[ecx+&hF8]
		mov [rotb],eax

		mov eax,[ecx+&h154]
		mov [drawscale],eax

	End Asm

	For gt = 0 To nclientsmax-1
		If posx=-237887-gt*100 And posy=107302 And posz=182292 Then
			drawscale = ppm(gt).varwert
			If drawscale<0 Then drawscale = 0
			If drawscale>46 Then drawscale = 46
			Exit For
		EndIf
	Next

	For gt = 0 To nclientsmax-1
		If posx=-237887-gt*100 And posy=107302 And posz=182492 Then
			If gadrmerk(gt)=0 Then gadrmerk(gt) = gadr
			Exit For
		EndIf
	Next

	For gt = 0 To nclientsmax-1
		If gadrmerk(gt)=gadr And gadr<>0 Then
			posx = ppm(gt).wposx
			posy = ppm(gt).wposy
			posz = ppm(gt).wposz
			rotb = ppm(gt).wrotb
			Exit For
		EndIf
	Next

	Asm

		mov eax,[posx]
		mov [ecx+&hE8],eax

		mov eax,[posy]
		mov [ecx+&hEC],eax

		mov eax,[posz]
		mov [ecx+&hF0],eax

		mov eax,[rotb]
		mov [ecx+&hF8],eax

		mov eax,[drawscale]
		mov [ecx+&h154],eax

		pop edx
		pop ecx
		pop ebx
		pop eax

		jmp _UpdateActorsGate@0
	End Asm
End Sub


' kopiert irgendwie bones memory
'
Sub BonesFuncHook ()
	Asm
		pop edi
		pop esi
		pop ebx

		push eax
		push ebx
		push ecx
		push edx

		mov eax,[esp+16]
		mov [cadr],eax

		mov eax,[esp+16+4]
		mov [badr],eax

		mov eax,[esp+16+8]
		mov [nbones],eax
	End Asm


	nbones-=&h7E0

	If nbones=&h5A0 Or nbones=&h4E0 Or nbones=&h320 Or nbones=&h240 Then
		For gd = 0 To nclientsmax-1
			If gadrmerk(gd)>0 And ppm(gd).gwert=1 Then
				dadr = gadrmerk(gd)
				result = ReadProcessMemory (currentprocess, Cast(Any Ptr, dadr+&h1C0), @dadr, SizeOf(Integer), 0)
				If result<>0 Then
					result = ReadProcessMemory (currentprocess, Cast(Any Ptr, dadr+&h24C), @dadr, SizeOf(Integer), 0)
					If result<>0 Then

						If dadr=cadr Or dadr=badr Then

							Select Case gd
								Case Faith
									'ghost0adr = dadr
									memcpy (Cast (Any Ptr, dadr), globalbuf(gd), szbonesbuf)

								Case Kate
									'ghost1adr = dadr
									memcpy (Cast (Any Ptr, dadr), globalbuf(gd), 7*32)
									memcpy (Cast (Any Ptr, dadr+39*32), globalbuf(gd)+45*32, 63*32)

									memcpy (Cast (Any Ptr, dadr+18*32), globalbuf(gd)+18*32, 1*32) 'CameraJoint

									memcpy (Cast (Any Ptr, dadr+14*32), globalbuf(gd)+14*32, 1*32) 'EyeJoint
									memcpy (Cast (Any Ptr, dadr+15*32), globalbuf(gd)+15*32, 1*32) 'LeftEye
									memcpy (Cast (Any Ptr, dadr+19*32), globalbuf(gd)+19*32, 1*32) 'RightEye

									memcpy (Cast (Any Ptr, dadr+16*32), globalbuf(gd)+16*32, 1*32) 'LeftUpEyelid
									'memcpy (Cast (Any Ptr, dadr+17*32), globalbuf(gd)+17*32, 1*32) 'LeftLowEyelid
									'memcpy (Cast (Any Ptr, dadr+20*32), globalbuf(gd)+20*32, 1*32) 'RightLowEyelid
									memcpy (Cast (Any Ptr, dadr+21*32), globalbuf(gd)+21*32, 1*32) 'RightUpEyelid

									memcpy (Cast (Any Ptr, dadr+22*32), globalbuf(gd)+22*32, 1*32) 'LeftOuterEyeBrow
									memcpy (Cast (Any Ptr, dadr+23*32), globalbuf(gd)+23*32, 1*32) 'LeftInnerEyebrow
									memcpy (Cast (Any Ptr, dadr+33*32), globalbuf(gd)+39*32, 1*32) 'RightInnerEyeBrow
									memcpy (Cast (Any Ptr, dadr+36*32), globalbuf(gd)+42*32, 1*32) 'RightOuterEyebrow

								Case Celeste
									memcpy (Cast (Any Ptr, dadr), globalbuf(gd), 7*32)
									memcpy (Cast (Any Ptr, dadr+nbones), globalbuf(gd)+45*32, 63*32)
									memcpy (Cast (Any Ptr, dadr+18*32), globalbuf(gd)+18*32, 1*32) 'CameraJoint

								Case Jacknife
									memcpy (Cast (Any Ptr, dadr), globalbuf(gd), 7*32)
									memcpy (Cast (Any Ptr, dadr+nbones), globalbuf(gd)+45*32, 63*32)
									memcpy (Cast (Any Ptr, dadr+18*32), globalbuf(gd)+18*32, 1*32) 'CameraJoint

								Case Miller
									memcpy (Cast (Any Ptr, dadr), globalbuf(gd), 7*32)
									memcpy (Cast (Any Ptr, dadr+nbones), globalbuf(gd)+45*32, 63*32)
									memcpy (Cast (Any Ptr, dadr+18*32), globalbuf(gd)+18*32, 1*32) 'CameraJoint

								Case Kreeg
									memcpy (Cast (Any Ptr, dadr), globalbuf(gd), 7*32)
									memcpy (Cast (Any Ptr, dadr+nbones), globalbuf(gd)+45*32, 63*32)
									memcpy (Cast (Any Ptr, dadr+18*32), globalbuf(gd)+18*32, 1*32) 'CameraJoint

								Case CopKarl
									memcpy (Cast (Any Ptr, dadr), globalbuf(gd), 7*32)
									memcpy (Cast (Any Ptr, dadr+nbones), globalbuf(gd)+45*32, 63*32)
									memcpy (Cast (Any Ptr, dadr+15*32), globalbuf(gd)+18*32, 1*32) 'CameraJoint

								Case CopHelm
									memcpy (Cast (Any Ptr, dadr), globalbuf(gd), 7*32)
									memcpy (Cast (Any Ptr, dadr+nbones), globalbuf(gd)+45*32, 63*32)
									memcpy (Cast (Any Ptr, dadr+18*32), globalbuf(gd)+18*32, 1*32) 'CameraJoint

								Case RunnerCop
									memcpy (Cast (Any Ptr, dadr), globalbuf(gd), 7*32)
									memcpy (Cast (Any Ptr, dadr+nbones), globalbuf(gd)+45*32, 63*32)
									memcpy (Cast (Any Ptr, dadr+15*32), globalbuf(gd)+18*32, 1*32) 'CameraJoint

								Case AssaultCeleste
									memcpy (Cast (Any Ptr, dadr), globalbuf(gd), 7*32)
									memcpy (Cast (Any Ptr, dadr+nbones), globalbuf(gd)+45*32, 63*32)
									memcpy (Cast (Any Ptr, dadr+17*32), globalbuf(gd)+18*32, 1*32) 'CameraJoint

							End Select


							'memcpy (Cast (Any Ptr, dadr),        globalbuf(gd),           szbblock1)
							'memcpy (Cast (Any Ptr, dadr+nbones), globalbuf(gd)+szbblock1, szbblock2)
							Exit For
						EndIf
					EndIf
				EndIf
			EndIf
		Next
	EndIf

	Asm
		pop edx
		pop ecx
		pop ebx
		pop eax
		jmp _BonesFuncGate@0
	End Asm

End Sub


' static level load
'
Function StaticLevelLoadHook (levelInfo As Any Ptr, unk As Integer, unk2 As Any Ptr) As Integer
	Dim As Integer ret
	Dim As Any Ptr this_
	Dim As WString Ptr nam
	Dim As String s

	Asm
		mov [this_], ecx
	End Asm

	For wt=0 To nclientsmax-1
		gadrmerk(wt)=0
	Next

	nam = *Cast (WString Ptr Ptr, levelinfo+&h1c)
	s = *nam

	Select Case LCase(s)
		Case "edge_p"       : levelnumber = 0
		Case "escape_p"     : levelnumber = 1
		Case "stormdrain_p" : levelnumber = 2
		Case "cranes_p"     : levelnumber = 3
		Case "subway_p"     : levelnumber = 4
		Case "mall_p"       : levelnumber = 5
		Case "factory_p"    : levelnumber = 6
		Case "boat_p"       : levelnumber = 7
		Case "convoy_p"     : levelnumber = 8
		Case "scraper_p"    : levelnumber = 9
		Case "tutorial_p"   : levelnumber = 10
		Case "tdmainmenu"   : levelnumber = 11
		Case Else           : levelnumber = 12
	End Select
	mplevelnumber = levelnumber

	printf ("level load: %s %d"+Chr(10), s, mplevelnumber)

	Asm
		mov ecx, [this_]
	End Asm

	ret = StaticLevelLoadGate (levelInfo, unk, unk2)

	Return ret
End Function


Sub copyjmp (src As Byte Ptr, dest As Byte Ptr, nops As Integer)
	Dim As Integer oldProtect, i

	If VirtualProtect (src, JMP_SIZE+nops, PAGE_EXECUTE_READWRITE, @oldProtect) Then
		*src = &hE9
		*Cast (Any Ptr Ptr, src+1) = Cast(Any Ptr, dest-(src+JMP_SIZE))
		For i = 0 To nops-1
			*(src + JMP_SIZE + i) = &h90
		Next
		VirtualProtect (src, JMP_SIZE+nops, oldProtect, @oldProtect)
	Else
		printf ("JMP: couldnt overwrite"+Chr(10))
	EndIf
End Sub


Sub TrampolineHook (src As Byte Ptr, dest As Byte Ptr, gate As Byte Ptr, overwritten As Integer)
	Dim As Integer basea
	Dim As Integer nops, oldprotect

	basea = Cast (Integer, GetModuleHandle (0))
	src += basea

	If VirtualProtect (gate, overwritten, PAGE_EXECUTE_READWRITE, @oldProtect) Then
		memcpy (gate, src, overwritten)
		VirtualProtect (gate, overwritten, oldProtect, @oldProtect)
	Else
		printf ("TrampolineHook: couldnt overwrite"+Chr(10))
	EndIf

	copyjmp (gate+overwritten, src+overwritten, 0)

	If overwritten>JMP_SIZE Then
		nops = overwritten - JMP_SIZE
	Else
		nops = 0
	EndIf
	copyjmp (src, dest, nops)
End Sub


Sub TrampolineUnHook (dest As Byte Ptr, src As Byte Ptr, overwritten As Integer)
	Dim As Integer basea
	Dim As Integer nops, oldprotect

	basea = Cast (Integer, GetModuleHandle (0))
	dest += basea

	If VirtualProtect (dest, overwritten, PAGE_EXECUTE_READWRITE, @oldProtect) Then
		memcpy (dest, src, overwritten)
		VirtualProtect (dest, overwritten, oldProtect, @oldProtect)
	Else
		printf ("TrampolineUnHook: couldnt overwrite"+Chr(10))
	EndIf
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
		ret = GetModuleFileName (0, @buf, 1024)
		If ret=0 Then Sleep 1000
		t+=1
	Loop While ret=0 And t<10

	printf ("module name "+buf+Chr(10))

	hfile = CreateFile (@buf, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, 0, 0)
	ret = GetFileSizeEx (hfile, Cast (PLARGE_INTEGER, @fsize))

	gef = 0
	For t = 0 To ngamevers-2
		If fsize=GVers(t).ExecutableSize Then
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


Sub initall ()
	Dim As Integer ll, t

	vers = getmeversion ()

	printf ("game version "+Str(vers)+"  "+*GVers(vers).VersionName+Chr(10))

	For t = 0 To nclientsmax-1
		globalbuf(t) = Allocate (szbonesbuf)
	Next

	pipefaithbones = Allocate (szbonesbuf)

	TrampolineHook (Cast (Byte Ptr, GVers(vers).LevelLoadFunc), Cast (Byte Ptr, @StaticLevelLoadHook), Cast (Byte Ptr, @StaticLevelLoadGate), JMP_SIZE+2)
	TrampolineHook (Cast (Byte Ptr, GVers(vers).InterpActorFunc), Cast (Byte Ptr, @UpdateActorsHook)+6, Cast (Byte Ptr, @UpdateActorsGate), JMP_SIZE+1)
	TrampolineHook (Cast (Byte Ptr, GVers(vers).BonesFunc), Cast (Byte Ptr, @BonesFuncHook)+6, Cast (Byte Ptr, @BonesFuncGate), JMP_SIZE+1)
	TrampolineHook (Cast (Byte Ptr, GVers(vers).FrameFunc), Cast (Byte Ptr, @FrameFuncHook)+6, Cast (Byte Ptr, @FrameFuncGate), JMP_SIZE)
End Sub


' main thread
'
Function mainthread (para As Any Ptr) As Integer
	'Dim As DWORD dwBytesRead
	'Dim As Byte Ptr mbuf
	'Const szmbuf = 4096
	'Dim As pipepaket_movement Ptr pm

	If debug Then
		AllocConsole ()
		freopen ("CONOUT$", "w", stdout)
		freopen ("CONIN$", "r", stdin)
	EndIf

	printf ("process attach, console open"+Chr(10))

	initall ()

	'mbuf = Allocate (szmbuf)

	currentprocess = GetCurrentProcess ()

	Do
		printf ("%08x %08x %08x"+Chr(10),gadrmerk(0),gadrmerk(1),gadrmerk(2))
		If gadrprinten>0 Then
			printf ("habe 0 bones kopiert"+Chr(10))
			gadrprinten = 0
		EndIf
		SleepEx (100, 0)
	Loop

	printf ("thread end"+Chr(10))

	Return 1
End Function


Sub closeall ()
	TerminateThread (thr, 0)

	FreeConsole ()

	TrampolineUnHook (Cast (Byte Ptr, GVers(vers).LevelLoadFunc), Cast (Byte Ptr, @StaticLevelLoadGate), JMP_SIZE+2)
	TrampolineUnHook (Cast (Byte Ptr, GVers(vers).InterpActorFunc), Cast (Byte Ptr, @UpdateActorsGate), JMP_SIZE+1)
	TrampolineUnHook (Cast (Byte Ptr, GVers(vers).BonesFunc), Cast (Byte Ptr, @BonesFuncGate), JMP_SIZE+1)
	TrampolineUnHook (Cast (Byte Ptr, GVers(vers).FrameFunc), Cast (Byte Ptr, @FrameFuncGate), JMP_SIZE)

	findval += 1
	findval -= 1
	pipemembones += 1
	pipemembones -= 1
	pipememmovement += 1
	pipememmovement -= 1
End Sub


Function MyDllMain Alias "MAIN" (hinst As HINSTANCE, dwReason As DWORD, lpv As LPVOID) As BOOL Export
	Select Case dwReason
		Case DLL_PROCESS_ATTACH

			If thrstarted=0 Then
				thr = CreateThread (0, 0, Cast (LPTHREAD_START_ROUTINE, @mainthread), 0, 0, 0)
				thrstarted = 1
			EndIf

		Case DLL_PROCESS_DETACH

			closeall ()

		Case DLL_THREAD_ATTACH

		Case DLL_THREAD_DETACH
	End Select

	Return TRUE
End Function
