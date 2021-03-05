; ==========================================================
; Diagnostic LCD Strings	
; ==========================================================
passstr:		
		.text " PASS"
		.db   0
sysfailstr:
lcdfailstr:
piofailstr:	
clkfailstr:
testfailstr:
lptfailstr:
dongledatafail:
loopbackfail:
;		.text "0123456789ABCDEF"
		.text " FAIL"
		.db   0
testdone:	
		.text " DONE!"
		.db   0
ramstr:		
		.text "PIO "
		.db   0
piostr:		
		.text "PIO "
		.db   0
kbdstr:	
		.text "KBD "
		.db   0
lcdstr:	
		.text "LCD "
		.db   0
lcdfound:	
		.text " FOUND "
		.db   0		
lcddone:
		.text " LCD OK?"
		.db   0
clkteststr:
		.text "CLK IC " 
		.db 0
donglestrdata:
		.text "DNGL-AD7-0 " 
		.db 0
donglestrins1:
		.text "DNGL-C20-27" 
		.db 0
donglestrins2:
		.text "DNGL-C28-29" 
		.db 0
dongleins2data:
		.text "RST X CS28-29 X"
		.db 0
donglestrstat:
		.text "WR XX RD XX     "
		.db 0
loopbackstrdata:
		.text "KEY LOOPED " 
		.db 0
loopbackstrstat:
		.text "WR XX RD XX     "
		.db 0
kbdteststr:
		.text "KEYBOARD " 
		.db 0
keypressedstr:
		.text "KEY: " 
		.db 0
kbdtestfailstr:
		.text "TEST FAILED     " 
		.db 0
kbdtestdonestr:
		.text "TEST COMPLETED  " 
		.db 0
rst75teststr:
		.text "RST 7.5 "
		.db 0
sysbusteststr:
		.text "SYS BUS         "
		.db 0
systeststr:
		.text "WR XX RD XX S xx"
		.db 0
rst65teststr:
		.text "CTRL BITS       "
		.db 0
dsrteststr:
		.text "RT/CT X DS/DT X "
		.db 0
loopbackteststr:
		.text "LOOPBACK TEST   "
		.db 0
loopbackstatstr:
		.text "C xx TX xx RX xx"
		.db 0		
loopbackfailstr:
		.text "LOOPBACK FAIL   "
		.db 0
loopbackpassstr:
		.text "LOOPBACK PASS   "
		.db 0
txfailstr: 
		.text "TX Failed       "
		.db 0
rxfailstr: 
		.text "RX Failed       "
		.db 0
lptteststr:
		.text "LPT I/F "
		.db 0
lptloopteststr:
		.text "WR XX RD XX"
		.db 0
nbusyteststr:
		.text "Busy- ??        "
		.db 0
busyteststr:
		.text "Busy  ??        "
		.db 0
rst55teststr:
		.text "BCR I/F "
		.db 0
rst55loopteststr:
		.text "LP XX CT XX P   "
		.db 0
casteststr:
		.text "CAS REMOTE      "  ; 11
		.db 0
castestfailstr:
		.text "CAS REMOTE FAIL "  ; 11
		.db 0
castestpassstr:
		.text "CAS REMOTE PASS "  ; 11
		.db 0
castestaudiostr:
		.text "CAS AUDIO TEST  "  ; 3
		.db 0
castestaudiosyncstr:
		.text "SYNC PASS:      "  ; 3
		.db 0
castestaudiosyncfailstr:
		.text "SYNC FAIL:      "  ; 3
		.db 0
castestaudiodonestr:
		.text "CAS AUDIO DONE  "  ; 3
		.db 0
castestoffstr:
		.text "CAS REMOTE OFF  "  ; 3
		.db 0
castestonstr:
		.text "CAS REMOTE ON   "  ; 3
		.db 0
castestplaystr:
		.text "PLAY[BLK]ctl-brk"
		.db 0	
castestrecstr:
		.text "REC [GRY]ctl-brk"
		.db 0
castestplaystartedstr
		.text "LISTENING....   "
		.db 0
castestrecstartedstr
		.text "SENDING AUDIO   "
		.db 0
stromteststr:
		.text "SWITCH 2 OPTROM "
		.db 0
optromfoundstr:
		.text "OPTROM SWITCHED "
		.db 0
nooptromstr:
		.text "OPTROM FAILED   "
		.db 0
stromfailurestr:
		.text "NO RAM"
		.db 0
ram0used:
		.text "0"
		.db 0
ram1used:
		.text "1"
		.db 0
ram2used:
		.text "2"
		.db 0
ram3used:
		.text "3"
		.db 0	
chksum:
;		.text "0123456789ABCDEF"
		.text " xxxx PASS yyyy"
		.db 0
optromunknow:
		.text "UNK?  "
		.db 0
idrex:
		.text "REX   "
		.db 0
settingsvalue:
		.text "SETTINGS:"
		.db 0
goodbye:
		.text "Powering off!   "
		.db 0	
testcompletestr:
		.text "WAIT FOR PWR OFF"
		.db 0
versioninfostr:
		.text "(c)  2/2021 "
		VERSION
		.db 0
;		.text "0123456789ABCDEF"
dumplogstr:
		.text "DUMP LOG START  "
		.db 0
dumplogstrend:
		.text "DUMP LOG DONE   "
		.db 0
dumpromstr:
		.text "DUMP ROM BEGIN  "
		.db 0
dumpromstrend:
		.text "DUMP ROM END    "
		.db 0