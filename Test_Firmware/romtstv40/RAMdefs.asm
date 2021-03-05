; ======================================================
; The code below will appear in RAM it is here to generate
; the HEX code for the ROM to load into RAM when running. 
; Changes made here need to be reflected in the ROM data.
; ======================================================		
; ======================================================
; RAM3 OPTROM Test Code 
; ======================================================		
		.org ram3+1100H
optram3:
		MVI A,09H;  
		OUT 0E8H; 
		LXI H,0040H;  
		LXI D,optram3open ; FAA4H
optram3loop:		
		MOV A,M;   
		STAX D;  
		INX H; 
		INX D; 
		MOV A,L; 
		SUI 48H
		JNZ optram3loop   ; F60FH; 
	
;Calculate Checksum over complete OPTROM 0-07FFFH
		LXI H, 0  ; Start of ROM
		LXI B, 0  ; Counter
		LXI D, 0  ; storage
optram3checksum:
		MOV C,M   ; Get byte
		XCHG      ; Swap HL and DE
		DAD B     ; Add to form 16bit sum
		XCHG      ; Swap HL and DE
		INX  H    ; Point to next byte
		MOV A,H   ; Test count for 0: 4
		SBI 080H  ;
		ORA L     ; Test lower byte: 4
        JNZ optram3checksum  ; If ~0 continue 
		
        XCHG
        SHLD checksum3      ; Store the checksum 
		MVI A,08H
		OUT 0E8H;  ; Return to main ROM	
		
;Calculate Checksum over complete ROM 0-07FFFH
		LXI H, 0  ; Start of ROM
		LXI B, 0  ; Counter
		LXI D, 0  ; storage
optram3tchecksum:
		MOV C,M   ; Get byte
		XCHG      ; Swap HL and DE
		DAD B     ; Add to form 16bit sum
		XCHG      ; Swap HL and DE
		INX  H    ; Point to next byte
		MOV A,H   ; Test count for 0: 4
		SBI 080H  ;
		ORA L     ; Test lower byte: 4
        JNZ optram3tchecksum  ; If ~0 continue 			
		XCHG
		SHLD checksum3t	
		RET
optram3open:
		.db 000H,000H,000H,000H,000H,000H,000H,000H
checksum3:
		.dw 0000H  
checksum3t:
		.dw 0000H 

getrex3:
		MVI A,09H;  
		OUT 0E8H; 
		LXI H,0004H;  
		LXI D,rexstore3 ;
getrex3loop:		
		MOV A,M;   
		STAX D;  
		INX H; 
		INX D; 
		MOV A,L; 
		SUI 08H
		JNZ getrex3loop   ;
		MVI A,09H;  
		OUT 0E8H; 		
		RET				
rexstore3:
		.db 00H,00H,00H,00H
logbegin3:
		.text "Test Log Data\r\nRAM 0 PASS\r\nRAM 1 PASS\r\nRAM 2 PASS\r\nRAM 3 PASS\r\n"
logptrstart3: .equ $
		
; ======================================================
; RAM2 OPTROM Test Code 
; ======================================================
		.org ram2+1100H
optram2:
		MVI A,09H;  
		OUT 0E8H; 
		LXI H,0040H;  
		LXI D,optram2open ; FAA4H
optram2loop:		
		MOV A,M;   
		STAX D;  
		INX H; 
		INX D; 
		MOV A,L; 
		SUI 048H
		JNZ optram2loop   ; F60FH;  
		
;Calculate Checksum over complete OPTROM 0-07FFFH
		LXI H, 0  ; Start of ROM
		LXI B, 0  ; Counter
		LXI D, 0  ; storage
optram2checksum:
		MOV C,M   ; Get byte
		XCHG      ; Swap HL and DE
		DAD B     ; Add to form 16bit sum
		XCHG      ; Swap HL and DE
		INX  H    ; Point to next byte
		MOV A,H   ; Test count for 0: 4
		SBI 080H  ;
		ORA L     ; Test lower byte: 4
;		MOV A,L
;		CPI 0
        JNZ optram2checksum  ; If ~0 continue 
		
        XCHG
        SHLD checksum1       ; Store the checksum 
		MVI A,08H
		OUT 0E8H;  ; Return to main ROM	
		
;Calculate Checksum over complete ROM 0-07FFFH
		LXI H, 0  ; Start of ROM
		LXI B, 0  ; Counter
		LXI D, 0  ; storage
optram2tchecksum:
		MOV C,M   ; Get byte
		XCHG      ; Swap HL and DE
		DAD B     ; Add to form 16bit sum
		XCHG      ; Swap HL and DE
		INX  H    ; Point to next byte
		MOV A,H   ; Test count for 0: 4
		SBI 080H  ;
		ORA L     ; Test lower byte: 4
;		MOV A,L
;		CPI 0
        JNZ optram2tchecksum  ; If ~0 continue 			
		XCHG
		SHLD checksum2t
	
		RET
optram2open:
		.db 000H,000H,000H,000H,000H,000H,000H,000H
checksum2:
		.dw 0000H  
checksum2t:
		.dw 0000H 

getrex2:
		MVI A,09H;  
		OUT 0E8H; 
		LXI H,0004H;  
		LXI D,rexstore2 ;
getrex2loop:		
		MOV A,M;   
		STAX D;  
		INX H; 
		INX D; 
		MOV A,L; 
		SUI 08H
		JNZ getrex2loop   ;
		MVI A,09H;  
		OUT 0E8H; 		
		RET				
rexstore2:
		.db 00H,00H,00H,00H
logbegin2:
		.text "Test Log Data\r\nRAM 0 PASS\r\nRAM 1 PASS\r\nRAM 2 PASS\r\nRAM 3 PASS\r\n"
logptrstart2: .equ $
		
; ======================================================
; RAM1 OPTROM Test Code 
; ======================================================
		.org ram1+1100H
optram1:
		MVI A,09H;  
		OUT 0E8H; 
		LXI H,0040H;  
		LXI D,optram1open ; FAA4H
optram1loop:		
		MOV A,M;   
		STAX D;  
		INX H; 
		INX D; 
		MOV A,L; 
		SUI 048H
		JNZ optram1loop   ; F60FH;
		
;Calculate Checksum over complete OPTROM 0-07FFFH
		LXI H, 0  ; Start of ROM
		LXI B, 0  ; Counter
		LXI D, 0  ; storage
optram1checksum:
		MOV C,M   ; Get byte
		XCHG      ; Swap HL and DE
		DAD B     ; Add to form 16bit sum
		XCHG      ; Swap HL and DE
		INX  H    ; Point to next byte
		MOV A,H   ; Test count for 0: 4
		SBI 080H  ;
		ORA L     ; Test lower byte: 4
;		MOV A,L
;		CPI 0
        JNZ optram0checksum  ; If ~0 continue 
		
        XCHG
        SHLD checksum1       ; Store the checksum 
		MVI A,08H
		OUT 0E8H;  ; Return to main ROM	
		
;Calculate Checksum over complete ROM 0-07FFFH
		LXI H, 0  ; Start of ROM
		LXI B, 0  ; Counter
		LXI D, 0  ; storage
optram1tchecksum:
		MOV C,M   ; Get byte
		XCHG      ; Swap HL and DE
		DAD B     ; Add to form 16bit sum
		XCHG      ; Swap HL and DE
		INX  H    ; Point to next byte
		MOV A,H   ; Test count for 0: 4
		SBI 080H  ;
		ORA L     ; Test lower byte: 4
;		MOV A,L
;		CPI 0
        JNZ optram1tchecksum  ; If ~0 continue 			
		XCHG
		SHLD checksum1t
	
		RET
optram1open:
		.db 000H,000H,000H,000H,000H,000H,000H,000H
checksum1:
		.dw 0000H  
checksum1t:
		.dw 0000H 

getrex1:
		MVI A,09H;  
		OUT 0E8H; 
		LXI H,0004H;  
		LXI D,rexstore1 ;
getrex1loop:		
		MOV A,M;   
		STAX D;  
		INX H; 
		INX D; 
		MOV A,L; 
		SUI 08H
		JNZ getrex1loop   ;
		MVI A,09H;  
		OUT 0E8H; 		
		RET				
rexstore1:
		.db 00H,00H,00H,00H
logbegin1:
		.text "Test Log Data\r\nRAM 0 PASS\r\nRAM 1 PASS\r\nRAM 2 PASS\r\nRAM 3 PASS\r\n"
logptrstart1: .equ $
						
; ======================================================
; RAM0 OPTROM Test Code 
; ======================================================
		.org ram0+1100H
optram0:
		MVI A,09H;  
		OUT 0E8H; 
		LXI H,0040H;  
		LXI D,optram0open ; FAA4H
optram0loop:		
		MOV A,M;   
		STAX D;  
		INX H; 
		INX D; 
		MOV A,L; 
		SUI 048H
		JNZ optram0loop   ; F60FH; 
		
;Calculate Checksum over complete OPTROM 0-07FFFH
		LXI H, 0  ; Start of ROM
		LXI B, 0  ; Counter
		LXI D, 0  ; storage
optram0checksum:
		MOV C,M   ; Get byte
		XCHG      ; Swap HL and DE
		DAD B     ; Add to form 16bit sum
		XCHG      ; Swap HL and DE
		INX  H    ; Point to next byte
		MOV A,H   ; Test count for 0: 4
		SBI 080H  ;
		ORA L     ; Test lower byte: 4
;		MOV A,L
;		CPI 0
        JNZ optram0checksum  ; If ~0 continue 
		
        XCHG
        SHLD checksum0       ; Store the checksum 
		MVI A,08H
		OUT 0E8H;  ; Return to main ROM	
		
;Calculate Checksum over complete ROM 0-07FFFH
		LXI H, 0  ; Start of ROM
		LXI B, 0  ; Counter
		LXI D, 0  ; storage
optram0tchecksum:
		MOV C,M   ; Get byte
		XCHG      ; Swap HL and DE
		DAD B     ; Add to form 16bit sum
		XCHG      ; Swap HL and DE
		INX  H    ; Point to next byte
		MOV A,H   ; Test count for 0: 4
		SBI 080H  ;
		ORA L     ; Test lower byte: 4
;		MOV A,L
;		CPI 0
        JNZ optram0tchecksum  ; If ~0 continue 			
		XCHG
		SHLD checksum0t
		RET
optram0open:
		.db 000H,000H,000H,000H,000H,000H,000H,000H
checksum0:
		.dw 0000H  
checksum0t:
		.dw 0000H 
				
getrex0:
		MVI A,09H;  
		OUT 0E8H; 
		LXI H,0004H;  
		LXI D,rexstore0 ;
getrex0loop:		
		MOV A,M;   
		STAX D;  
		INX H; 
		INX D; 
		MOV A,L; 
		SUI 08H
		JNZ getrex0loop   ;
		MVI A,09H;  
		OUT 0E8H; 		
		RET				
rexstore0:
		.db 00H,00H,00H,00H
logbegin0:
		.text "Test Log Data\r\nRAM 0 PASS\r\nRAM 1 PASS\r\nRAM 2 PASS\r\nRAM 3 PASS\r\n"
logptrstart0: .equ $
size    .equ ((logptrstart0) - optram0)

; ======================================================
; Useful to have to dump OPTROMS
; Following code is only useful to dump OPTROMs from 
; RAM0.  It assumes the RAM is operational and the 
; motherboard is working.  
; ======================================================
; DUMP OPTROM Test Code 
; ======================================================
; ======================================================
		.org ram0+1200H
dumprom:
		MVI A,09H;  ; Switch to OPTROM
		OUT 0E8H; 	
		
		;CALL initserport ;Init the serial port
		NOP
		NOP
		NOP
;Calculate Checksum over complete OPTROM 0-07FFFH
		LXI H, 0  ; Start of ROM
dumpromloop:
		MOV A,M   ; Get byte
		CALL putchserport
		INX  H    ; Point to next byte
		MOV A,H   ; Test count for 0: 4
		SBI 080H  ;
		ORA L     ; Test lower byte: 4
        JNZ dumpromloop  ; If ~0 continue 

		MVI A,08H
		OUT 0E8H;  ; Return to main ROM	
		RET
		
; ==========================================================
; Init serial port
; ==========================================================
initserport:
; Configure the serial port, switch to RS232
		MVI A,024H    ; PIO B configuration (RTS low, DTR low, SPKR=1, Serial=RS232, Keyscan col 9 enable)
		OUT PIOB      ; Set PIO chip port B configuration	

; Set baud rate 19200 using PIO Timer	
		MVI A,008H
		OUT PIOT1     ; BCH Timer 0
		MVI A,040H
		OUT PIOT2	  ; BDH Timer 1
		MVI A,0C3H
		OUT PIOCR	  ; B8H Control Reg
		
; Configure UART Chip
; B11100: 8bits + No Parity + 1 stop
		MVI A,01CH
		OUT UARTmode  ; D8H
		RET
		
; ==========================================================
; Put Char in A to serial port
; ==========================================================
putchserport:
		PUSH PSW       ; Save A
putchserportwait:
		IN UARTmode    ; D8H	
		ANI 010H       ; TX Buffer empty - High buffer empty
		JZ putchserportwait ; Wait for TX buffer to empty		
		POP PSW        ; Restore A
		OUT UARTsend   ; C8H
		RET	
			
dumpromend:
sizerom .equ ((dumpromend + 1) - dumprom) + 3

		.end