;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; $Id: ROMTST.asm $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;  ROM Test firmware for use with the ROM board V2.x.
;  Uses the connection test board to complete the test harness.
;  The use of an LCD dongle test board and keyboard loop back
;  board can assist with debugging however they are not required.
;
;  Uses parts of the M100_dis_2013 data. The disassembly of the
;  M100 ROM was invaluable in producing the test firmware. The 16-bit
;  HEX number next to some routines can be used to reference into 
;  the disassembly to help understanding of the reduced routines.
;
;  Created by IS on 12 Feb 2020.
;  Updated: 
;			10 Dec 2020 - V3.1. Added LCD Dongle and keyboard
;			              Loopback board tests.
;			22 Dec 2020 - V3.2. Improved fault detection on A14-A8 
;						  RAM addresses.  
;			03 Jan 2021 - V3.2rl Just looping RAM test, halts on error Hey Birt!
;
;  Copyright © 2020 Stardust. 
;  All rights reserved.
;  Commercial use prohibited
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TASM 
; Processor: 8080/8085
;
#DEFINE VERSION .text "V3.2rl" ; Change this value to update version

;===========================================================
; Defined locations and constants
; ==========================================================
;Test Board Diagnostic LCD Settings
lcddata  .equ   0001H ;Display DATA shares ROM Address space
lcdins   .equ   0000H ;Display Control Register
lcdwake  .equ   30H   ;Wake up LCD
lcdfunc  .equ   38H   ;Set LCD 2 lines,0 5x7 Dots
;lcdon    .equ   0FH   ;Set LCD on+Cursor+Blink
lcdon    .equ   0CH   ;Set LCD on no cursor
lcdmode  .equ   06H   ;LCD Mode
lcdclr   .equ   01H   ;Clear LCD

;RAM Location settings
ramstart .equ	08000H 
ramend	 .equ   0FFFFH
ram0   	 .equ   0E000H ; E7FFH, EFFFH, F7FFH ;E800H, F000H, F800H
ram1     .equ   0C000H ; C7FFH, CFFFH, D7FFH ;C800H, D000H, D800H
ram2	 .equ   0A000H ; A7FFH, AFFFH, B7FFH ;A800H, B000H, B800H
ram3     .equ 	08000H ; 87FFH, 8FFFH, 97FFH ;8800H, 9000H, 9800H

;I/O Port addresses
sysreg	 .equ   080H   ; Y0 System bus Sysreg
setreg	 .equ   081H   ; Y0 System bus ctlreg
lptreg	 .equ   082H   ; Y0 System bus lptreg

teleIO   .equ   090H   ; Y1 Answering Telephone
modemIO	 .equ   0A0H   ; Y2 Modem control

;PIO registers
PIOCR	 .equ   0B0H   ; Y3 PIO Control Register B8H
PIOA	 .equ   0B1H   ; Y3 PIO Port A Register  B9H
PIOB	 .equ   0B2H   ; Y3 PIO Port B Register  BAH
PIOC	 .equ   0B3H   ; Y3 PIO Port C Register  BBH
PIOT1	 .equ   0B4H   ; Y3 PIO LSB Timer        BCH
PIOT2	 .equ   0B5H   ; Y3 PIO MSB Timer	    BDH

UARTctrl .equ	0C0H   ; Y4 => 0xC0 - 0xCF  = UART Control
UARTread .equ	0C8H   ; Serial RX 
UARTsend .equ   0C8H   ; Serial TX
UARTmode .equ	0D8H   ; Y5 => 0xD0 - 0xDF  = Mode setting on UART
CtrlReg  .equ	0E8H   ; Y6 => 0xE0 - 0xEF  = Write to enable STROM and REMOTE, printer strobe and clock
readkbd  .equ	0E8H   ; Y6 => 0xE0 - 0xEF  = Read from Keyboard
scrins   .equ	0FEH   ; Y7 => 0xF0 - 0xFF  = Enable for LCD instructions, this is active HIGH
scrdata  .equ	0FFH   ; Y7 => 0xF0 - 0xFF  = Enable for LCD data, this is active HIGH

		.org 0
;===========================================================
; Reset Vector
; ==========================================================
		JMP start      ; Start of test code

; ==========================================================
; TRAP entry point
; ==========================================================
		.org 24H
		JMP trap      ; RAM vector for TRAP interrupt
		NOP

; ==========================================================
; RST 5.5 -- Bar Code Reader
; ==========================================================
		; .org 2CH
		; DI
		; JMP bcrint    ; RST 5.5 RAM Vector	
	
; ==========================================================
; RST 6.5 -- RS232 character pending
; ==========================================================
		; .org 34H
		; DI
		; JMP serint    ; RST 6.5 routine (RS232 receive interrupt)

; ==========================================================
; RST 7.5 -- Timer background task
; ==========================================================
		; .org 3CH
		; DI
		; JMP timint      ; RST 7.5 interrupt routine
	
; ==========================================================
; Used to detect when the ROM switches. 
; ==========================================================
		.org 040H
optromtext:
		.text "No Optrom"
		.db 0

; ==========================================================
; Copyright notices 
; ==========================================================
        .org 1000H - 200
;		.text "0123456789012345678901"
;		.text "0123456789ABCDEF"
;copyright notice
copynotice:
;		.text "0123456789ABCDEF"
		.text "  ROM Test "
		VERSION
		.db 0
		.text "(c) IS  Dec 2020"
		.db 0
		.text "M100/102 ROM Test "
		VERSION
		.db 0
		.text "Copyright IS Dec 2020"
		.db 0
		
; **********************************************************
; Start of test code
; **********************************************************
        .org 1000h
start:
		DI
		
		MVI A, 01FH   ; disable all interrupts
		SIM

		LXI SP,ram0   ; Set the SP = E000 just in case
		
		;Startup delay loop, taken from M100 ROM
		LXI H,2710H   ; Load 16-bit delay counter
wait1:
		DCX H         ; Decrement delay counter: 6
		MOV A,H       ; Test count for 0: 4
		ORA L         ; Test lower byte: 4
		JNZ wait1     ; wait for zero count: 7 = 21cycles = 8.54us * 10000 = 86ms

; ==========================================================
; Set up PIO just in case, use M100 ROM settings to configure
; ==========================================================
		MVI A,43H      ; Load configuration for PIO (A=OUT, B=OUT, C=IN, Stop Timer counter)
		OUT PIOCR      ; Set PIO chip configuration
		MVI A,0ECH     ; PIO B configuration (RTS low, DTR low, SPKR=1, Serial=Modem, Keyscan col 9 enable)
		OUT PIOB       ; Set PIO chip port B configuration
		MVI A,0FFH     ; PIO A configuration (Used for Key scan, LCD data, etc.)
		OUT PIOA       ; Initialize PIO chip port A

		MVI B, 3      ; Wake UP LCD 3 times	

; ==========================================================
; Configure the diagnostic screen
; 	Assumes no RAM is available until tested
; ==========================================================
		; INIT diagnostic screen
		LXI H,lcdins  ; Load diagnostic LCD Instruction location

lcdwakeup:
		;diagnostic LCD WAKEUP
		MVI M,lcdwake ; Wake up diagnostic LCD
		
		; Wait 1.1ms
		MVI C,0FFH     ; Counter 255 	
delay1:
		DCR C          ; Decrement C: 4
		JNZ delay1     ; Loop until C = 0: 7 = 11 = 4.45us

		DCR B
		JNZ lcdwakeup

		;diagnostic LCD FUNCTION SET
		MVI M,lcdfunc   ; Function Set

		; Wait 40us
		MVI C,10       ; Counter 10 	
delay2:
		DCR C          ; Decrement C: 4
		JNZ delay2     ; Loop until C = 0: 7 = 11 = 4.45us	
		
		;diagnostic LCD ON
		MVI M,lcdon    ; LCD ON			
	
		; Wait 40us		
		MVI C,10       ; Counter 10 	
delay3:
		DCR C          ; Decrement C: 4
		JNZ delay3     ; Loop until C = 0: 7 = 11 = 4.45us

		;diagnostic LCD MODE
		MVI M,lcdmode  ; Set LCD mode
		
		; Wait 40us		
		MVI C,10      ; Counter 10 	
delay4:
		DCR C          ; Decrement C: 4
		JNZ delay4     ; Loop until C = 0: 7 = 11 = 4.45us


		;diagnostic LCD CLEAR
		MVI M,lcdclr   ; LCD Clear
	
		; Wait 1.64ms		
		; Wait 800us
		MVI B,2
delay5s
		MVI C,190      ; Counter 190 	
delay5:
		DCR C          ; Decrement C: 4
		JNZ delay5     ; Loop until C = 0: 7 = 11 = 4.45us		
		
		DCR B
		JNZ delay5s

; ==========================================================
; Write 'CPU ' text to Diagnostic LCD
; ==========================================================
		MVI A,08DH      ; Set Cursor to top RH corner
		STA lcdins      ; diagnostic  LCD Instruction
		; Wait 40us		
		MVI C,10        ; Counter 10 	
delay5a:
		DCR C          ; Decrement C: 4
		JNZ delay5a    ; Loop until C = 0: 7 = 11 = 4.45us
		
		LXI H,lcddata  ; Load LCD Data location
		MVI M,'C'      ; Load C to screen
		
		; Wait 40us		
		MVI C,10      ; Counter 10 	
delay6:
		DCR C          ; Decrement C: 4
		JNZ delay6    ; Loop until C = 0: 7 = 11 = 4.45us	
		
		MVI M,'P'      ; Load P to screen
		
		; Wait 40us		
		MVI C,10      ; Counter 10 	
delay7:
		DCR C          ; Decrement C: 4
		JNZ delay7    ; Loop until C = 0: 7 = 11 = 4.45us	

		MVI M,'U'      ; Load P to screen
		
		; Wait 40us		
		MVI C,10      ; Counter 10 	
delay8:
		DCR C          ; Decrement C: 4
		JNZ delay8    ; Loop until C = 0: 7 = 11 = 4.45us			

		MVI M,' '      ; Load space to screen
		
		; Wait 40us		
		MVI C,10      ; Counter 10 	
delay9:
		DCR C          ; Decrement C: 4
		JNZ delay9    ; Loop until C = 0: 7 = 11 = 4.45us	
		

; ==========================================================
; Write 'RAM ?' text to Diagnostic LCD
; ==========================================================
		MVI A,0CCH      ; Set Cursor to bottom RH corner
		STA lcdins      ; diagnostic  LCD Instruction
		; Wait 40us		
		MVI C,10        ; Counter 10 	
delay9a:
		DCR C          ; Decrement C: 4
		JNZ delay9a    ; Loop until C = 0: 7 = 11 = 4.45us
		
		LXI H,lcddata  ; Load diagnostic LCD Data location
		MVI M,'R'      ; Load R to diagnostic screen
		
		; Wait 40us		
		MVI C,10      ; Counter 10 	
delay10:
		DCR C          ; Decrement C: 4
		JNZ delay10    ; Loop until C = 0: 7 = 11 = 4.45us	
		
		MVI M,'A'      ; Load A to diagnostic screen
		
		; Wait 40us		
		MVI C,10      ; Counter 10 	
delay11:
		DCR C          ; Decrement C: 4
		JNZ delay11    ; Loop until C = 0: 7 = 11 = 4.45us	

		MVI M,'M'      ; Load M to diagnostic screen
		
		; Wait 40us		
		MVI C,10      ; Counter 10 	
delay12:
		DCR C          ; Decrement C: 4
		JNZ delay12    ; Loop until C = 0: 7 = 11 = 4.45us			

		MVI M,'?'      ; Load space to diagnostic screen
		
		; Wait 40us		
		MVI C,10      ; Counter 10 	
delay13:
		DCR C          ; Decrement C: 4
		JNZ delay13   ; Loop until C = 0: 7 = 11 = 4.45us

; ##########################################################
; ##########################################################		
; **********************************************************
; RAM TESTS
; 	Check each RAM bank
; 	Indicate which banks are present
; 	Test banks that exist by writing each byte.
; 	Indicate any failures
; 	Once tested use the stack as part of the testing 
;	if required
; **********************************************************
; Used in settings for the RAM tests
goodmemch	.equ 'm' ; LC to make it more obvious
badmemch  	.equ 'F' ; UC to make it stand out
badpagech  	.equ 'H' ; UC to make it stand out
backupch	.equ 'B'
nobackupch	.equ '.'
goodblock	.equ 'g'
backupbyte	.equ 0AAH   ; Needs to be set to the last ram test byte

; ==============================================================
; Set up the cursor positions
; ==============================================================
bgrtst:
		MVI A,080H      ; Set Cursor to top LH corner
		STA lcdins      ; diagnostic  LCD Instruction
		; Wait 40us				
		MVI C,10        ; Counter 10 	
delay14:
		DCR C          ; Decrement C: 4
		JNZ delay14    ; Loop until C = 0: 7 = 11 = 4.45us
		
; =============================================================
; TEST Set 1:
;   Is made up of 3 tests:
;   - Attempt to check the battery backup works.  Only works
;     on second attempt.
; 	- Test each memory location. Make sure it is OK.
;   - Test the first 256 bytes of the RAM to check A7-A0 address
;     lines for stuck faults.
; ==============================================================
; Example diagnostic screen layout
;		.text "0123456789ABCDEF"
;		.text ".mgg.mgg.mgg.Fgg" ; 080H
;		.text "mmmmmmmmmmmm1mmm" ; 0C0H

;33333333333333333333333333333
;*****************************
;*** Test RAM3	
;*****************************
;*** Test Battery Backup 3 ***
;*****************************
; Check for data retention from last run should contain 0AAH
		LXI H,ram3+300	 ; Standard RAM location 08064H	
		
		MOV A,M
		CPI backupbyte
		MVI A,nobackupch
		JNZ ramblktop3
		MVI A,backupch
				
; Set HL to start of first RAM block location
ramblktop3:
		STA lcddata	

;*****************************
;*** Complete memory test 3 **	
;*****************************
		LXI H,ram3		 ; Standard RAM location 08000H		
								
ramblktst3:
		LXI D,ramtstdata ; Load the RAM byte test data table
tstloc3:
		LDAX D   		 ; D = Current RAM byte test data
		CPI eotramtests  ; 0FEH Check end of table
		JZ nextloc3       
		MOV M,A			 ; Write RAM location to table entry
		MOV C,M          ; Read RAM location
		CMP C		     ; Compare locations
		JNZ tstfail3     ; Not equal RAM location failed
		INX D            ; Get next RAM test byte
		JMP tstloc3      
nextloc3:                ; Move to next RAM location to test
		INX H            ; Addr++ count will rollover from 0FFFFH
		MOV A,H          ; Check HL == 0
		CPI 0A0H         ; Compare next block
		JZ tstfin3       ; Finished testing
		JNZ ramblktst3
		
tstfin3:
		MVI A,goodmemch   ; Test OK
		LXI SP,ram3+1000H ; Memory Block + 1000H
		PUSH PSW          ; Put the goodmemch on the stack
		JMP exitblk3
tstfail3:
		MVI A,badmemch    ; Test Fail

exitblk3:
		STA lcddata    	 ; Load RAM # to screen
									
;*****************************
;*** First page RAM test 3 ***
;*****************************
		LXI H,ram3	; Standard RAM location 08000H
		XRA A

; Set up the first page
fpramtst3init:
		INR A
		MOV M,A		; Read the value
		INR L		; Next one in the list
		JNZ fpramtst3init   ; Keep going until done

		; Wait see if memory retains contents
		MVI C,200	
delayfp3:
		DCR C          ; Decrement C: 4
		JNZ delayfp3   ; Loop until C = 0: 7 = 11 = 4.45us

		LXI H,ram3     ; Standard RAM location 08000H
		XRA A

fpramtst3:		
		INR A
		CMP M		; Compare with memory
		JNZ fpramtst3fail 

		INR L		; Next one in the list
		JNZ fpramtst3   ; Keep going until done
		MVI A,goodblock
		JMP fpramtst3exit ; Test complete

fpramtst3fail:
		MVI A,badpagech  
	
fpramtst3exit:
		STA lcddata    	 ; Load RAM # to screen
		
;*****************************
;****** 256byte test 3 *******	
;*****************************
ramloopstart3:
		LXI H,ram3		 ; Standard RAM location 08000H		

;Write test data to boundaries to check for SA Address lines
		MVI M,8         ; Set 1st Location to 8
				
		INX H           ; Address 1
		MVI M,7
				
		LXI H,ram3+002H ; Address 2
		MVI M,6
		
		LXI H,ram3+004H ; Address 4
		MVI M,5
		
		LXI H,ram3+008H ; Address 8
		MVI M,4
		
		LXI H,ram3+010H ; Address 16
		MVI M,3
		
		LXI H,ram3+020H ; Address 32
		MVI M,2
		
		LXI H,ram3+040H	; Address 64
		MVI M,1
		
		LXI H,ram3+080H ; Address 128
		MVI M,0

;Check the contents of the tested RAM		
		LXI H,ram3	; Standard RAM location 08000H		
		MVI A,8
		
		CMP M       ; Compare first locations				
		JNZ ramloop3testfail ; Test failed

		INX H       ; Address 1		
		DCR A
		CMP M       ; Compare Address 1			
		JNZ ramloop3testfail ; Test failed

		DCR A				
		LXI H,ram3+002H ; Address 2
		CMP M           ; Compare Address 2		
		JNZ ramloop3testfail ; Test failed

		DCR A				
		LXI H,ram3+004H ; Address 4
		CMP M           ; Compare Address 4		
		JNZ ramloop3testfail ; Test failed
		
		DCR A				
		LXI H,ram3+008H ; Address 8
		CMP M           ; Compare Address 8	
		JNZ ramloop3testfail ; Test failed
		
		DCR A				
		LXI H,ram3+010H ; Address 16
		CMP M           ; Compare Address 16		
		JNZ ramloop3testfail ; Test failed
		
		DCR A				
		LXI H,ram3+020H ; Address 32
		CMP M           ; Compare Address 32		
		JNZ ramloop3testfail ; Test failed
		
		DCR A				
		LXI H,ram3+040H ; Address 64
		CMP M           ; Compare Address 64		
		JNZ ramloop3testfail ; Test failed
		
		DCR A				
		LXI H,ram3+080H ; Address 128
		CMP M           ; Compare Address 128		
		JNZ ramloop3testfail ; Test failed		

		;Test successful
		MVI A,goodblock		
		JMP ramloop3testend		
		
ramloop3testfail:
		MOV C,A
		MVI A,8
		SUB C
		ADI '0'  		

ramloop3testend:
		STA lcddata


		; Wait 40us				
		MVI C,10        ; Counter 10 	
delayr3:
		DCR C          ; Decrement C: 4
		JNZ delayr3    ; Loop until C = 0: 7 = 11 = 4.45us	

;22222222222222222222222222222
;*****************************
;*** Test RAM2	
;*****************************
;*** Test Battery Backup 2 ***
;*****************************
; Check for data retention from last run should contain 0AAH
		LXI H,ram2+300	 ; Standard RAM location 0A064H	
		
		MOV A,M
		CPI backupbyte
		MVI A,nobackupch
		JNZ ramblktop2
		MVI A,backupch
					
; Set HL to start of highest memory location
ramblktop2:
		STA lcddata	

;*****************************
;*** Complete memory test 2 **	
;*****************************
		LXI H,ram2		 ; Standard RAM location 0A000H
ramblktst2:
		LXI D,ramtstdata ; Load the RAM byte test data table
tstloc2:
		LDAX D   		 ; D = Current RAM byte test data
		CPI eotramtests  ; 0FEH Check end of table
		JZ nextloc2       
		MOV M,A		 ; Write RAM location to table entry
		MOV C,M          ; Read RAM location
		CMP C		 ; Compare locations
		JNZ tstfail2   	 ; Not equal RAM location failed
		INX D            ; Get next RAM test byte
		JMP tstloc2       
nextloc2:                ; Move to next RAM location to test
		INX H            ; Addr++ count will rollover from 0FFFFH
		MOV A,H          ; Check HL == 0
		CPI 0C0H         ; Compare next block
		JZ tstfin2       ; Finished testing
		JNZ ramblktst2
		
tstfin2:
		MVI A,goodmemch  ; Test OK
		LXI SP,ram2+1000H; Top Memory Block + 10
		PUSH PSW         ; Put the 'M' on the stack
		JMP exitblk2
tstfail2:
		MVI A,badmemch   ; Test Fail

exitblk2:
		STA lcddata    	 ; Load RAM # to screen	

;*****************************
;*** First page RAM test 2 ***
;*****************************
		LXI H,ram2	; Standard RAM location 0A000H
		XRA A

; Set up the first page
fpramtst2init:
		INR A
		MOV M,A		; Read the value
		INR L		; Next one in the list
		JNZ fpramtst2init   ; Keep going until done

		MVI C,200	
delayfp2:
		DCR C          ; Decrement C: 4
		JNZ delayfp2   ; Loop until C = 0: 7 = 11 = 4.45us

		LXI H,ram2    ; Standard RAM location 0A000H
		XRA A

fpramtst2:		
		INR A
		CMP M		; Compare with memory
		JNZ fpramtst2fail 

		INR L		; Next one in the list
		JNZ fpramtst2   ; Keep going until done
		MVI A,goodblock
		JMP fpramtst2exit ; Test complete

fpramtst2fail:
		MVI A,badpagech  
	
fpramtst2exit:
		STA lcddata    	 ; Load RAM # to screen
						    
;*****************************
;******* 256byte test 2 ******	
;*****************************
		LXI H,ram2	; Standard RAM location 0A000H		

;Write test data to boundaries to check for SA Address lines
		MVI M,8     ; Clear first location
				
		INX H           ; Address 1
		MVI M,7
				
		LXI H,ram2+002H ; Address 2
		MVI M,6
		
		LXI H,ram2+004H ; Address 4
		MVI M,5
		
		LXI H,ram2+008H ; Address 8
		MVI M,4
		
		LXI H,ram2+010H ; Address 16
		MVI M,3
		
		LXI H,ram2+020H ; Address 32
		MVI M,2
		
		LXI H,ram2+040H	; Address 64
		MVI M,1
		
		LXI H,ram2+080H ; Address 128
		MVI M,0

;Check the contents of the tested RAM		
		LXI H,ram2	; Standard RAM location 0A000H		
		MVI A,8
		
		CMP M       ; Compare first locations				
		JNZ ramloop2testfail ; Test failed

		INX H       ; Address 1		
		DCR A
		CMP M       ; Compare Address 1			
		JNZ ramloop2testfail ; Test failed

		DCR A				
		LXI H,ram2+002H ; Address 2
		CMP M           ; Compare Address 2		
		JNZ ramloop2testfail ; Test failed

		DCR A				
		LXI H,ram2+004H ; Address 4
		CMP M           ; Compare Address 4		
		JNZ ramloop2testfail ; Test failed
		
		DCR A				
		LXI H,ram2+008H ; Address 8
		CMP M           ; Compare Address 8	
		JNZ ramloop2testfail ; Test failed
		
		DCR A				
		LXI H,ram2+010H ; Address 16
		CMP M           ; Compare Address 16		
		JNZ ramloop2testfail ; Test failed
		
		DCR A				
		LXI H,ram2+020H ; Address 32
		CMP M           ; Compare Address 32		
		JNZ ramloop2testfail ; Test failed
		
		DCR A				
		LXI H,ram2+040H ; Address 64
		CMP M           ; Compare Address 64		
		JNZ ramloop2testfail ; Test failed
		
		DCR A				
		LXI H,ram2+080H ; Address 128
		CMP M           ; Compare Address 128		
		JNZ ramloop2testfail ; Test failed		

		;Test successful
		MVI A,goodblock		
		JMP ramloop2testend		
		
ramloop2testfail:
		MOV C,A
		MVI A,8
		SUB C
		ADI '0'  		

ramloop2testend:
		STA lcddata	

		; Wait 40us				
		MVI C,10        ; Counter 10 	
delayr2:
		DCR C          ; Decrement C: 4
		JNZ delayr2    ; Loop until C = 0: 7 = 11 = 4.45us

;11111111111111111111111111111
;*****************************
;*** Test RAM1
;*****************************
;*** Test Battery Backup 1 ***
;*****************************
; Check for data retention from last run should contain 0AAH
		LXI H,ram1+300	 ; Standard RAM location 0C064H	
		
		MOV A,M
		CPI backupbyte
		MVI A,nobackupch
		JNZ ramblktop1
		MVI A,backupch
					
; Set HL to start of highest memory location
ramblktop1:
		STA lcddata	
		
;*****************************
;*** Complete memory test 1 **	
;*****************************
		LXI H,ram1		 ; Standard RAM location 0C000H
		
ramblktst1:
		LXI D,ramtstdata ; Load the RAM byte test data table
tstloc1:
		LDAX D   		 ; D = Current RAM byte test data
		CPI eotramtests  ; 0FEH Check end of table
		JZ nextloc1       
		MOV M,A			 ; Write RAM location to table entry
		MOV C,M          ; Read RAM location
		CMP C			 ; Compare locations
		JNZ tstfail1   	 ; Not equal RAM location failed
		INX D            ; Get next RAM test byte
		JMP tstloc1       
nextloc1:                ; Move to next RAM location to test
		INX H            ; Addr++ count will rollover from 0FFFFH
		MOV A,H          ; Check HL == 0
		CPI 0E0H         ; Compare next block
		JZ tstfin1       ; Finished testing
		JNZ ramblktst1
		
tstfin1:
		MVI A,goodmemch  	; Test OK
		LXI SP,ram1+1000H	; Top Memory Block + 10
		PUSH PSW         	; Put the 'X' on the stack
		JMP exitblk1
tstfail1:
		MVI A,badmemch   ; Test Fail

exitblk1:
		STA lcddata    	 ; Load RAM # to screen		
		MVI C,10         ; Counter 10 	
delaybl11:
		DCR C            ; Decrement C: 4
		JNZ delaybl11    ; Loop until C = 0: 7 = 11 = 4.45us	

;*****************************
;*** First page RAM test 1 ***
;*****************************
		LXI H,ram1	; Standard RAM location 0C000H
		XRA A

; Set up the first page
fpramtst1init:
		INR A
		MOV M,A		; Read the value
		INR L		; Next one in the list
		JNZ fpramtst1init   ; Keep going until done

		MVI C,200	
delayfp1:
		DCR C          ; Decrement C: 4
		JNZ delayfp1   ; Loop until C = 0: 7 = 11 = 4.45us

		LXI H,ram1    ; Standard RAM location 0C000H
		XRA A

fpramtst1:		
		INR A
		CMP M		; Compare with memory
		JNZ fpramtst1fail 

		INR L		    ; Next one in the list
		JNZ fpramtst1   ; Keep going until done
		MVI A,goodblock
		JMP fpramtst1exit ; Test complete

fpramtst1fail:
		MVI A,badpagech  
	
fpramtst1exit:
		STA lcddata    	 ; Load RAM # to screen
		
;*****************************
;****** 256byte test 1 *******	
;*****************************
		LXI H,ram1		 ; Standard RAM location 0C000H		

;Write test data to boundaries to check for SA Address lines
		MVI M,8     ; Clear first location
				
		INX H           ; Address 1
		MVI M,7
				
		LXI H,ram1+002H ; Address 2
		MVI M,6
		
		LXI H,ram1+004H ; Address 4
		MVI M,5
		
		LXI H,ram1+008H ; Address 8
		MVI M,4
		
		LXI H,ram1+010H ; Address 16
		MVI M,3
		
		LXI H,ram1+020H ; Address 32
		MVI M,2
		
		LXI H,ram1+040H	; Address 64
		MVI M,1
		
		LXI H,ram1+080H ; Address 128
		MVI M,0

;Check the contents of the tested RAM		
		LXI H,ram1	; Standard RAM location 0C000H		
		MVI A,8
		
		CMP M       ; Compare first locations				
		JNZ ramloop1testfail ; Test failed

		INX H       ; Address 1		
		DCR A
		CMP M       ; Compare Address 1			
		JNZ ramloop1testfail ; Test failed

		DCR A				
		LXI H,ram1+002H ; Address 2
		CMP M           ; Compare Address 2		
		JNZ ramloop1testfail ; Test failed

		DCR A				
		LXI H,ram1+004H ; Address 4
		CMP M           ; Compare Address 4		
		JNZ ramloop1testfail ; Test failed
		
		DCR A				
		LXI H,ram1+008H ; Address 8
		CMP M           ; Compare Address 8	
		JNZ ramloop1testfail ; Test failed
		
		DCR A				
		LXI H,ram1+010H ; Address 16
		CMP M           ; Compare Address 16		
		JNZ ramloop1testfail ; Test failed
		
		DCR A				
		LXI H,ram1+020H ; Address 32
		CMP M           ; Compare Address 32		
		JNZ ramloop1testfail ; Test failed
		
		DCR A				
		LXI H,ram1+040H ; Address 64
		CMP M           ; Compare Address 64		
		JNZ ramloop1testfail ; Test failed
		
		DCR A				
		LXI H,ram1+080H ; Address 128
		CMP M           ; Compare Address 128		
		JNZ ramloop1testfail ; Test failed		

		;Test successful
		MVI A,goodblock		
		JMP ramloop1testend		
		
ramloop1testfail:
		MOV C,A
		MVI A,8
		SUB C
		ADI '0'  		

ramloop1testend:
		STA lcddata	

		; Wait 40us				
		MVI C,10        ; Counter 10 	
delayr1:
		DCR C          ; Decrement C: 4
		JNZ delayr1    ; Loop until C = 0: 7 = 11 = 4.45us

;00000000000000000000000000000
;*****************************
;*** RAM 0
;*****************************
;*** Test Battery Backup 0 ***
;*****************************
; Check for data retention from last run should contain 0AAH		
		LXI H,ram0+300	 ; Standard RAM location 0E064H	
		
		MOV A,M
		CPI backupbyte
		MVI A,nobackupch
		JNZ ramblktop
		MVI A,backupch

; Set HL to start of highest memory location
ramblktop:
		STA lcddata

;*****************************
;*** Complete memory test 0 **
;*****************************
		LXI H,ram0   ; Standard RAM location 0E000H	
ramblktst:
		LXI D,ramtstdata ; Load the RAM byte test data table
tstloc:
		LDAX D   	     ; D = Current RAM byte test data
		CPI eotramtests  ;0FEH Check end of table
		JZ nextloc       
		MOV M,A			 ; Write RAM location to table entry
		MOV C,M          ; Read RAM location
		CMP C			 ; Compare locations
		JNZ tstfail   	 ; Not equal RAM location failed
		INX D            ; Get next RAM test byte
		JMP tstloc       
nextloc:                 ; Move to next RAM location to test
		INX H            ; Addr++ count will rollover from 0FFFFH
		MOV A,H          ; Check HL == 0
		CPI 0H
		JZ tstfin        ; Finished testing if HL == 0
		JNZ ramblktst
		
tstfin:
		MVI A,goodmemch   ; Test OK
		LXI SP,ram0+1000H ; Top Memory Block + 1000H
		PUSH PSW          ; Put the char on the stack
		JMP exitblk
tstfail:
		MVI A,badmemch    ; Test Fail

exitblk:
		STA lcddata    	 ; Load RAM # to screen	
	
;*****************************
;*** First page RAM test 0 ***
;*****************************
		LXI H,ram0	; Standard RAM location 0E000H
		XRA A

; Set up the first page
fpramtst0init:
		INR A
		MOV M,A		; Read the value
		INR L		; Next one in the list
		JNZ fpramtst0init   ; Keep going until done

		MVI C,200	
delayfp0:
		DCR C          ; Decrement C: 4
		JNZ delayfp0   ; Loop until C = 0: 7 = 11 = 4.45us

		LXI H,ram0    ; Standard RAM location 0E000H
		XRA A

fpramtst0:		
		INR A
		CMP M		; Compare with memory
		JNZ fpramtst0fail 

		INR L			; Next one in the list
		JNZ fpramtst0   ; Keep going until done
		MVI A,goodblock
		JMP fpramtst0exit ; Test complete

fpramtst0fail:
		MVI A,badpagech  
	
fpramtst0exit:
		STA lcddata    	 ; Load RAM # to screen

;*****************************
;******* 256byte test 0 ******	
;*****************************
		LXI H,ram0		 ; Standard RAM location 0E000H		

;Write test data to boundaries to check for SA Address lines
		MVI M,8         ; Clear first location
				
		INX H           ; Address 1
		MVI M,7
				
		LXI H,ram0+002H ; Address 2
		MVI M,6
		
		LXI H,ram0+004H ; Address 4
		MVI M,5
		
		LXI H,ram0+008H ; Address 8
		MVI M,4
		
		LXI H,ram0+010H ; Address 16
		MVI M,3
		
		LXI H,ram0+020H ; Address 32
		MVI M,2
		
		LXI H,ram0+040H	; Address 64
		MVI M,1
		
		LXI H,ram0+080H ; Address 128
		MVI M,0

;Check the contents of the tested RAM		
		LXI H,ram0	; Standard RAM location 0E000H		
		MVI A,8
		
		CMP M       ; Compare first locations				
		JNZ ramloop0testfail ; Test failed

		INX H       ; Address 1		
		DCR A
		CMP M       ; Compare Address 1			
		JNZ ramloop0testfail ; Test failed

		DCR A				
		LXI H,ram0+002H ; Address 2
		CMP M           ; Compare Address 2		
		JNZ ramloop0testfail ; Test failed

		DCR A				
		LXI H,ram0+004H ; Address 4
		CMP M           ; Compare Address 4		
		JNZ ramloop0testfail ; Test failed
		
		DCR A				
		LXI H,ram0+008H ; Address 8
		CMP M           ; Compare Address 8	
		JNZ ramloop0testfail ; Test failed
		
		DCR A				
		LXI H,ram0+010H ; Address 16
		CMP M           ; Compare Address 16		
		JNZ ramloop0testfail ; Test failed
		
		DCR A				
		LXI H,ram0+020H ; Address 32
		CMP M           ; Compare Address 32		
		JNZ ramloop0testfail ; Test failed
		
		DCR A				
		LXI H,ram0+040H ; Address 64
		CMP M           ; Compare Address 64		
		JNZ ramloop0testfail ; Test failed
		
		DCR A				
		LXI H,ram0+080H ; Address 128
		CMP M           ; Compare Address 128		
		JNZ ramloop0testfail ; Test failed		

		;Test successful
		MVI A,goodblock		
		JMP ramloop0testend		
		
ramloop0testfail:
		MOV C,A
		MVI A,8
		SUB C
		ADI '0'  		

ramloop0testend:
		STA lcddata	

		MVI C,10       ; Counter 10 	
delayr0:
		DCR C          ; Decrement C: 4
		JNZ delayr0    ; Loop until C = 0: 7 = 11 = 4.45us

; ==============================================================
; TEST 2:
;  Write to each individual 256 block, make sure the block
;  is correctly selected.  May also show the RAM is bad.
;  This will test address lines A14-A8 = 128 blocks.
;  The Address lines A10-A8 are directly attached to the chip
;  and can be identified as to the part they are identified as
;  LKJ when failing.  
;  The Address lines A14-A11 are  indirectly used to access the RAMs 
;  via decoders.  Consequently if these lines fail they are limited
;  to the failed CE.
;  The char 'm' is used to indicate a passing block.
;  The test will stop on the first block to fail in the 2K RAM
;  block.
; ==============================================================	
		MVI A,0C0H      ; Set Cursor to bottom LH corner
		STA lcdins      ; diagnostic  LCD Instruction

		MVI C,10       ; Counter 10 	
delay14a:
		DCR C          ; Decrement C: 4
		JNZ delay14a   ; Loop until C = 0: 7 = 11 = 4.45us
							
		LXI H,ramstart  ; Start of RAM
;		LXI SP,800H     ; 2K Blocks
;		MVI D,16        ; Number of banks to test 16 for 2K blocks
		LXI SP,100H     ; 256byte Blocks this will store 128 numbers in the RAM
		MVI D,128       ; Number of banks to test = 128 * 256 = 32K
						; 
		XRA A           ; Clear A
		
; Set up the RAM banks with unique numbers to check indvidual CS- lines
ramloop:
		INR A           ; 
		MOV M,A
		DAD SP          ; Add on preset block size to get the next block
		DCR D
		JNZ ramloop     ; 

		LXI H,ramstart  ; Start of RAM
;		MVI D,16        ; Number of banks to test
;		MVI D,128       ; Number of banks to test
		XRA A           ; Clear A
	
; Test the ram banks CS- lines are working correctly
; Print M if correct or the numeric location of the failing
; RAM chip Address line.
	
ramloop128:
		INR A           ; Increment A
		MOV E,A         ; Move RAM byte to E
		CMP M
		JNZ ramfail
		ANI 007H        ; See if this is the end of the 2K RAM block
		CPI 007H        ;
		MOV A,E
		JNZ contloop    ; Next location
		MVI A,goodmemch  ; Char for diag LCD to indicate 2K RAM block is OK
		STA lcddata
		MOV A,E
		JMP prtdelay

ramfail:
		ANI 7          ; Get the Address that failed
		
;IF 2,3 or 5 then A8, A9, A10 = JKL
;IF 1 then need to SUB 1 /8 and add 1 to give CE line		
		CPI 1
		JZ idchipenable  ; Check if CE- Failure.
		CPI 2            ; Check for A8
		JNZ nextaddr
		MVI A,'J'   
		JMP sendtodiag
		
nextaddr:
		CPI 3            ; Check for A9
		JNZ nextaddr1
		MVI A,'K'   
		JMP sendtodiag
		
nextaddr1:
		CPI 5            ; Check for A10
		JNZ nextaddr2
		MVI A,'L'   
		JMP sendtodiag

nextaddr2:		
		MVI A,'?'        ; Unkown memory fault could be dual failure
		JMP sendtodiag
				
;ID the failed chip enable:1 - F
idchipenable:
		MOV A,E        ; Retrieve the failing address ID
		SBI 1          ; Subtract 1
		RAR
		RAR
		RAR            ; Divide by 8
		ADI 1          ; Add 1 to give CEx value

;Print out HEX value of Chip Enable
		ANI 00FH 		; Get Bottom Digit
		CPI 0AH 		; A > 10
		JC  hexdigit 	
		ADI 07H 	    ; Add 7 as this is A-F
hexdigit: 
		ADI 30H

sendtodiag:
		STA lcddata    ; Print character
		MOV A,H        ; Skip the 2K RAM block as it failed
		ORI 007H       
		MOV H,A
		MOV A,E        ; Restore test number
		ANI 0F8H       ; Increment to next 2K RAM block
		ADI 008H

prtdelay:
		MVI C,10       ; Counter 10 	
delay15:
		DCR C          ; Decrement C: 4
		JNZ delay15    ; Loop until C = 0: 7 = 11 = 4.45us	

contloop:	
		DAD SP
		CPI 128
		JNZ ramloop128

; ==============================================================				
; Now pick a RAM module to use for the next set of tests. 
; ==============================================================
ramcheck:
		LXI SP,ram0+1000H - 2 ; Module 0				
		POP PSW	
		CPI goodmemch     ; If successful RAM block is OK
		MVI A,'0'
		LXI D,optram0load
		JNZ ramfnd		  ; JZ ramfnd

		LXI SP,ram1+1000H - 2 ; Module 1				
		POP PSW			
		CPI goodmemch     ; If successful RAM block is OK
		MVI A,'1'
		LXI D,optram1load
		JNZ ramfnd		  ; JZ ramfnd
						
		LXI SP,ram2+1000H - 2; Module 2				
		POP PSW			
		CPI goodmemch     ; If successful RAM block is OK
		MVI A,'2'
		LXI D,optram2load
		JNZ ramfnd		  ; JZ ramfnd		

		LXI SP,ram3+1000H -2 ; Module 3				
		POP PSW			
		CPI goodmemch     ; If successful RAM block is OK
		MVI A,'3'		
		LXI D,optram0load
		JNZ ramfnd		  ; JZ ramfnd

; delay so screen can be read
		MVI B,10			  ; adjusts delay time
waithalflongx:
		LXI H,0C350H   ; Load 16-bit delay counter for a 1s Loop
waithalflooplongx:
		DCX H         ; Decrement delay counter: 6
		MOV A,H       ; Test count for 0: 4
		ORA L         ; Test lower byte: 4
		JNZ waithalflooplongx  ; wait for zero count: 7 = 21cycles = 8.54us * 65536 = 0.55s

		DCR B
		JNZ waithalflongx

;diagnostic LCD CLEAR before rerunning test
		MVI M,lcdclr   ; LCD Clear
	
		; Wait 1.64ms		
		; Wait 800us
		MVI B,2
delayls
		MVI C,190      ; Counter 190 	
delayl:
		DCR C          ; Decrement C: 4
		JNZ delayl     ; Loop until C = 0: 7 = 11 = 4.45us		
		
		DCR B
		JNZ delayls

		JMP bgrtst      ; All RAM OK, 

; SP is now set up with valid RAM location	
ramfnd:
		JMP cpuhlt      ; No useful RAM
		;CALL stromlocation ; Temp use this location to store A for later use
		;MOV M,A
		


; ##########################################################
; ##########################################################		
; **********************************************************
; Used at the end of the tests and to test power off.
; prtdone uses RAM so cannot be used until RAM is tested.
; Prints out the DONE! message
; The board will power down to test the power
; down feature is operational.
; **********************************************************
prtdone:
		DI
		CALL buzzer
		CALL clrscreen  ; Reset the screen for the new test
		
		LXI H,testcompletestr ; 11 chars
		CALL putstr		

		MVI A,0C0H      ; Set Cursor to back for new count
		CALL putins

		LXI H,versioninfostr
		CALL putstr

 		MVI B,10
prtwait:
		CALL longdelay  ; Countdown power off	
		DCR B		
		JNZ prtwait		

		CALL buzzer
		
 		MVI A,80H       ; Set Cursor to top line
		CALL putins

		LXI H,goodbye
		CALL putstr
		
		CALL longdelay  ; Let the user see the text
	
		MVI A,0FFH
		OUT PIOB ; Should power down the board after 120s
		JMP haltwait	

; ##########################################################
; ##########################################################				
; **********************************************************
; Halt the CPU testing done or failure
; Routine does not rely on RAM so can be used at any point
; **********************************************************
cpuhlt:
		LXI H,lcddata  ; Load LCD Data location
		MVI M,'H'      ; Load H to screen
		
		; Wait 40us		
		MVI C,10      ; Counter 10 	
delayH0:
		DCR C          ; Decrement C: 4
		JNZ delayH0    ; Loop until C = 0: 7 = 11 = 4.45us	
		
		MVI M,'L'      ; Load L to screen
		
		; Wait 40us		
		MVI C,10      ; Counter 10 	
delayH1:
		DCR C          ; Decrement C: 4
		JNZ delayH1    ; Loop until C = 0: 7 = 11 = 4.45us	

		MVI M,'T'      ; Load T to screen
		
		; Wait 40us		
		MVI C,10      ; Counter 10 	
delayH2:
		DCR C          ; Decrement C: 4
		JNZ delayH2    ; Loop until C = 0: 7 = 11 = 4.45us			

		MVI M,'!'      ; Load ! to screen
		
		; Wait 40us		
		MVI C,10      ; Counter 10 	
delayH3:
		DCR C          ; Decrement C: 4
		JNZ delayH3   ; Loop until C = 0: 7 = 11 = 4.45us
		
haltwait:	
		JMP haltwait	

; **********************************************************
;
; Utility routines to provide generic functions to main test
; Routines
;
; **********************************************************
; ======================================================
; Sound Buzzer
; ======================================================
buzzer:
		MVI B,00H           ; Execute loop 255 times
buzzerwait:
		CALL clicksound     ; Click sound port
		MVI C,50H
		CALL shortdelay     ; Delay routine - decrement C until zero
		DCR B
		JNZ buzzerwait
		RET

; ======================================================
; Click sound port
; ======================================================
clicksound:
		IN  PIOB	; 0BAH Load current value of I/O port BAH
		XRI 020H    ; Toggle the speaker I/O bit
		OUT PIOB    ; 0BAH Write new value to speaker to cause a "click"
		RET
		
; ======================================================
; Trap routine power down the board when power off
; ======================================================
trap:
		IN  0BAH        ; Get Current I/O value of BAH
		ORI 010H        ; Set the PowerDown bit
		OUT 0BAH        ; PowerDown.  We will loose power here
		HLT		
		
; **********************************************************
; Address calculation routines to deal with unknown RAM Locations
; **********************************************************
countdownmem  .equ 14
strommem      .equ countdownmem + 4
rst75testmem  .equ strommem + 4
rxbuffermem   .equ rst75testmem + 4
rst55testmem  .equ rxbuffermem + 4
keyboardmem   .equ rst55testmem + 4
casstoragemem .equ 12 ; Stored at a different location

; ======================================================
; Get Start of RAM block, using current SP, into DE
; ======================================================
ramsblockstart:
		PUSH H
		PUSH PSW
		LXI H,0
		DAD SP
		MOV A,H
		ANI 0E0H	; Remove the 1000H Addition to SP
		MOV H,A
		MVI L,0
		XCHG
		POP PSW
		POP H
		RET

; ======================================================
; Get Start of SP + 1, into DE
; Relies on Stack not getting too large and on a 00H boundary
; ======================================================
stackstart:
		PUSH H
		PUSH PSW
		LXI H,0
		DAD SP
		MOV A,H
		ANI 0E0H
		ORI 010H
		MOV H,A
		MVI L,2
		XCHG
		POP PSW
		POP H
		RET

; ======================================================
; Get Clock Storage into HL, SP + 2
; ======================================================
clocklocation:
		PUSH D
		CALL stackstart
		XCHG
		POP D	
		RET

; ======================================================
; Get Countdown Memory location
; ======================================================
getcountdown:
		PUSH PSW
		PUSH D
		CALL stackstart  
		XCHG	
		POP D
		MOV A,L
		ADI countdownmem ; Add 12 to Stack start
		MOV L,A
		POP PSW
		RET

; ======================================================
; Get STROM memory location
; ======================================================
stromlocation:
		PUSH PSW
		PUSH D
		CALL stackstart
		XCHG	
		POP D
		MOV A,L
		ADI strommem ; Add 14 to Stack start
		MOV L,A
		POP PSW
		RET

; ======================================================
; Get RST7.5 Test Store
; ======================================================
rst75testlocation:
		PUSH PSW
		PUSH D
		CALL stackstart
		XCHG	
		POP D
		MOV A,L
		ADI rst75testmem ; Add 16 to Stack start
		MOV L,A
		POP PSW
		RET

; ======================================================
; Reset RST7.5 count Store
; ======================================================
rst75testlocationreset:
		CALL rst75testlocation
		DI
		MVI M,0
		EI
		RET

; ======================================================
; Retrieve RST7.5 count Store
; ======================================================
rst75testlocationget:
		CALL rst75testlocation
		DI
		MOV A,M
		EI
		RET
	
; ======================================================
; Storage for RX Buffer 2-bytes
; ======================================================
rxbuffer:
		PUSH PSW
		PUSH D
		CALL stackstart
		XCHG	
		POP D
		MOV A,L
		ADI rxbuffermem ; Add 18 to Stack start
		MOV L,A
		POP PSW
		RET		
			
; ======================================================
; Get RST5.5 Test Store
; ======================================================
rst55testlocation:
		PUSH PSW
		PUSH D
		CALL stackstart
		XCHG	
		POP D
		MOV A,L
		ADI rst55testmem ; Add 22 to Stack start
		MOV L,A
		POP PSW
		RET		
	
; ======================================================
; Keyboard storage area
; ======================================================
getkeyboardstorage:
		PUSH PSW
		PUSH D
		CALL stackstart
		XCHG	
		POP D
		MOV A,L
		ADI keyboardmem  ; Add 24 to Stack start
		MOV L,A
		POP PSW
		RET		

; ======================================================
; GET Cassette Sync Read storage area
; ======================================================
getcasstorage:
		PUSH H
		LXI H,0
		DAD SP
		MOV A,H
		ANI 0E0H
		MOV H,A
		MVI L,casstoragemem ; 12
		MOV A,M
		POP H
		RET	

; ======================================================
; PUT Cassette Sync Read storage area
; ======================================================
putcasstorage:
		PUSH H
		PUSH PSW
		LXI H,0
		DAD SP
		MOV A,H
		ANI 0E0H
		MOV H,A
		MVI L,casstoragemem ; 12
		POP PSW
		MOV M,A
		POP H
		RET	
								
; ======================================================
; Move B bytes from M to (DE)
; ======================================================
movemem: ; 2542H
		MOV A,M
		STAX D
		INX H
		INX D
		DCR B
		JNZ movemem	;2542H Move B bytes from M to (DE)
		RET

; **********************************************************
; Debug Helper routines
; **********************************************************
; ======================================================
; HEX Dump memory (HL) to Diagnostic LCD on bottom line
; ======================================================
dumpmem:
		PUSH PSW
		PUSH B
		PUSH H

		MVI A,0C0H
		CALL putins

		MVI B,8
dumploop:
		MOV A,M
		CALL prthex
		INX H
		DCR B
		JNZ dumploop

		POP H
		POP B
		POP PSW
		RET

; ======================================================
; HEX Dump HL
; ======================================================
dumpHL:
		PUSH PSW
		PUSH B
		PUSH H

		MVI A,080H
		CALL putins

		MOV A,H
		CALL prthex
		MOV A,L
		CALL prthex

		POP H
		POP B
		POP PSW
		RET

; ==========================================================
; PUT Char on Diagnostic LCD. A contains character
; ==========================================================
putch: 
		PUSH B           ; Save BC

		STA lcddata    	 ; Send char to LCD		
		MVI C,16         ; Counter 16 	
		CALL shortdelay
		
		POP B	
		RET
		
; ==========================================================
; PUT instruction to Diagnostic LCD.  A contains instruction
; ==========================================================
putins:
		PUSH B           ; Save BC

		STA lcdins    	 ; Send ins to LCD		
		MVI C,16         ; Counter 16 	
		CALL shortdelay
			
		POP B			
		RET	

; ==========================================================
; PUT String on Diagnostic LCD HL points to string
; ==========================================================
putstr:
		PUSH PSW
putsloop:
		MOV A,M   	     ; H = Current Character
		CPI 0H           ; Check end of table
		JZ putstrexit       
		CALL putch       ; Write to screen
		INX H            ; Get next Char
		JMP putsloop
		
putstrexit:
		POP PSW
		RET

; ==========================================================
; Print A as 2 HEX digits
; ==========================================================
hexchar: 
		.text "0123456789ABCDEF"
prthex:
		PUSH PSW
		PUSH H
		PUSH B
		PUSH D
		
		MVI B,0
		
		LXI H,hexchar ; Point to ASCII table
		MOV D,A			; Store away A
		RAR 
		RAR
		RAR
		RAR
		ANI 00FH        ; Get Upper Digit
		MOV C,A		
		DAD B
		MOV A,M
		CALL putch      ; Print upper Digit

		LXI H,hexchar ; Point to ASCII table		
		MOV A,D
		ANI 00FH 		; Get Bottom Digit
		MOV C,A
		DAD B
		MOV A,M
		CALL putch      ; Print Bottom Digit
		
		POP D
		POP B
		POP H
		POP PSW
		RET		
				
; ==========================================================
; Short delay.  C = loop counter
; ==========================================================
shortdelay: ;7657H
		DCR C           ; Decrement C: 4
		JNZ shortdelay  ; Loop until C = 0: 7 = 11 = 4.45us
		RET

; ======================================================
; Disable Background task & barcode interrupts
; ======================================================
disablebackground: ;765CH
		DI             ; Disable interrupts
		MVI A,1DH      ; Load SIM mask to disable RST 5.5 & 7.5
		SIM            ; Set new interrupt mask (disable Background & barcode)
		EI             ; Re-enable interrupts
		RET

; ==========================================================
; Clear Diagnostic LCD Screen
; ==========================================================
clrscreen:
		;LCD CLEAR
		MVI A,lcdclr   ; LCD Clear
		STA lcdins
		
		; Wait 1.64ms		
		; Wait 800us
		MVI B,2
delayclrs:
		MVI C,200      ; Counter 190 	
delayclr:
		DCR C          ; Decrement C: 4
		JNZ delayclr    ; Loop until C = 0: 7 = 11 = 4.45us		
		
		DCR B
		JNZ delayclrs	

		MVI A,lcdmode
		Call putins

		RET
		
; ==========================================================
; Wait for 2s	
; ==========================================================
wait2seconds:
		MVI A,2
wait2secondsloop:
		CALL longdelay
		DCR A
		JNZ wait2secondsloop
		RET

; ==========================================================
; Pause between tests	
; ==========================================================
diagscreenpause:
		PUSH B
		
		MVI B,6
		
diagscreenpauseloop:
		CALL longdelay
		DCR B
		JNZ diagscreenpauseloop
		
		POP B
		RET

; ==========================================================
; Long Delay routine set for 0.5s	
; ==========================================================
longdelay:
		PUSH PSW
		PUSH B
		PUSH H
		MVI B,2
waithalflong:
		LXI H,0C350H   ; Load 16-bit delay counter for a 1s Loop
waithalflooplong:
		DCX H         ; Decrement delay counter: 6
		MOV A,H       ; Test count for 0: 4
		ORA L         ; Test lower byte: 4
		JNZ waithalflooplong  ; wait for zero count: 7 = 21cycles = 8.54us * 65536 = 0.55s

		DCR B
		JNZ waithalflong
		
		POP H
		POP B
		POP PSW
		RET
				
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
		.text "PLAY[BLK] ctl-brk"
		.db 0	
castestrecstr:
		.text "REC [GRY] ctl-brk"
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
		.text "OPTROM FAILED  "
		.db 0
stromfailurestr:
		.text "NO RAM"
		.db 0
ram0used:
		.text "RAM 0"
		.db 0
ram1used:
		.text "RAM 1"
		.db 0
ram2used:
		.text "RAM 2"
		.db 0
ram3used:
		.text "RAM 3"
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
		.text "(c) 12/2020 "
		VERSION
		.db 0
;		.text "0123456789ABCDEF"
; ==========================================================
; Main LCD Screen Strings for MT100/102
; 	The following strings are used for the LCD test
; ==========================================================
tstline0:
		.text "Model 100 Test ROM Version "
		VERSION
		.text " Dec 2020*"
		.db 0
tstline1:	
		.text "!\"#$%&'()*+`-./01234567890:;<=>?@"
		.db   0	
tstline2:	
		.text "ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`"
		.db   0	
tstline3:	
		.text "abcdefghijklmnopqrstuvwxyz{|}~"
		.db   0	
tstline4:
		.db 080H,081H,082H,083H,084H,085H,086H,087H
		.db 088H,089H,08Ah,08BH,08CH,08DH,08EH,08FH		
		.db 090H,098H,092h,093H,094H,095H,096H,097H
		.db 098H,099H,09Ah,09BH,09CH,09DH,09EH,09FH
		.db 0
tstline5:
		.db 0A0H,0A1H,0A2H,0A3H,0A4H,0A5H,0A6H,0A7H
		.db 0A8H,0A9H,0AAH,0ABH,0ACH,0ADH,0AEH,0AFH		
		.db 0B0H,0B1H,0B2H,0B3H,0B4H,0B5H,0B6H,0B7H
		.db 0B8H,0B9H,0BAH,0BBH,0BCH,0BDH,0BEH,0BFH
		.db 0
tstline6:
		.db 0C0H,0C1H,0C2H,0C3H,0C4H,0C5H,0C6H,0C7H
		.db 0C8H,0C9H,0CAH,0CBH,0CCH,0CDH,0CEH,0CFH		
		.db 0D0H,0D1H,0D2H,0D3H,0D4H,0D5H,0D6H,0D7H
		.db 0D8H,0D9H,0DAH,0DBH,0DCH,0DDH,0DEH,0CFH
		.db 0

tstline7:
		.db 0E0H,0E1H,0E2H,0E3H,0E4H,0E5H,0E6H,0E7H
		.db 0E8H,0E9H,0EAH,0EBH,0ECH,0EDH,0EEH,0EFH		
		.db 0F0H,0F1H,0F2H,0F3H,0F4H,0F5H,0F6H,0F7H
		.db 0F8H,0F9H,0FAH,0FBH,0FCH,0FDH,0FEH,0FFH
		.db 0

; ==========================================================
; RAM TEST Data used buy the RAM and all port tests
; ==========================================================
piotstdata:
ramtstdata:
dongletestdata:
loopbacktstdata:
		.db 000H
		.db	001H
		.db	002H
		.db	004H
		.db	008H
		.db	010H
		.db	020H
		.db	040H
		.db	080H
		.db	0FFH
		.db	055H
		.db	0AAH
eotramtests: .equ 0FEH
		.db	0FEH  ; End of table marker

; ======================================================
; 8155 PIO chip bit patterns for LCD drivers
; ======================================================
; 8155 PIO chip bit patterns for Upper LCD drivers
lcduppercs: ;7551H
	.db   001H,000H,000H,001H,000H,006H,001H,000H
	.db   00CH,001H,000H,012H,001H,000H,018H,001H
	.db   000H,01EH,001H,000H,024H,001H,000H,02AH
	.db   001H,000H,030H,002H,000H,004H,002H,000H
	.db   00AH,002H,000H,010H,002H,000H,016H,002H
	.db   000H,01CH,002H,000H,022H,002H,000H,028H
	.db   002H,000H,02EH,004H,000H,002H,004H,000H
	.db   008H,004H,000H,00EH,004H,000H,014H,004H
	.db   000H,01AH,004H,000H,020H,004H,000H,026H
	.db   004H,000H,02CH,008H,000H,000H,008H,000H
	.db   006H,008H,000H,00CH,008H,000H,012H,008H
	.db   000H,018H,008H,000H,01EH,008H,000H,024H
	.db   008H,000H,02AH,008H,000H,030H,010H,000H
	.db   004H,010H,000H,00AH,010H,000H,010H,010H
	.db   000H,016H,010H,000H,01CH,010H,000H,022H
; 8155 PIO chip bit patterns for lower LCD drivers
lcdlowercs: ;75C9H
	.db   020H,000H,000H,020H,000H,006H,020H,000H
	.db   00CH,020H,000H,012H,020H,000H,018H,020H
	.db   000H,01EH,020H,000H,024H,020H,000H,02AH
	.db   020H,000H,030H,040H,000H,004H,040H,000H
	.db   00AH,040H,000H,010H,040H,000H,016H,040H
	.db   000H,01CH,040H,000H,022H,040H,000H,028H
	.db   040H,000H,02EH,080H,000H,002H,080H,000H
	.db   008H,080H,000H,00EH,080H,000H,014H,080H
	.db   000H,01AH,080H,000H,020H,080H,000H,026H
	.db   080H,000H,02CH,000H,001H,000H,000H,001H
	.db   006H,000H,001H,00CH,000H,001H,012H,000H
	.db   001H,018H,000H,001H,01EH,000H,001H,024H
	.db   000H,001H,02AH,000H,001H,030H,000H,002H
	.db   004H,000H,002H,00AH,000H,002H,010H,000H
	.db   002H,016H,000H,002H,01CH,000H,002H,022H

; ======================================================
; 8155 PIO chip bit patterns to enable all LCD drivers
; ======================================================
lcdalldrivers: ;7641H
		.db   0FFH,003H

; ======================================================
; 8155 PIO chip bit patterns for LCD drivers
; ======================================================
lcdchipbitpatterns: ;7643H
		.db 001H,000H,002H,000H,004H,000H,008H,000H
		.db 010H,000H,020H,000H,040H,000H,080H,000H
		.db 000H,001H,000H,002H
	
; ======================================================
; DVI command table - sent to external DVI
; ======================================================
dvicmds: ;770BH
		.db 002H,001H,000H,000H,001H,00H
; ======================================================
; LCD char generator format table (20H-7FH)
; ======================================================
lcdasciitable: ;7711H
		.db   000H,000H,000H,000H,000H,000H,000H,04FH
		.db   000H,000H,000H,007H,000H,007H,000H,014H
		.db   07FH,014H,07FH,014H,024H,02AH,07FH,02AH
		.db   012H,023H,013H,008H,064H,062H,03AH,045H
		.db   04AH,030H,028H,000H,004H,002H,001H,000H
		.db   000H,01CH,022H,041H,000H,000H,041H,022H
		.db   01CH,000H,022H,014H,07FH,014H,022H,008H
		.db   008H,03EH,008H,008H,000H,080H,060H,000H
		.db   000H,008H,008H,008H,008H,008H,000H,060H
		.db   060H,000H,000H,040H,020H,010H,008H,004H
		.db   03EH,051H,049H,045H,03EH,044H,042H,07FH
		.db   040H,040H,062H,051H,051H,049H,046H,022H
		.db   041H,049H,049H,036H,018H,014H,012H,07FH
		.db   010H,047H,045H,045H,029H,011H,03CH,04AH
		.db   049H,049H,030H,003H,001H,079H,005H,003H
		.db   036H,049H,049H,049H,036H,006H,049H,049H
		.db   029H,01EH,000H,000H,024H,000H,000H,000H
		.db   080H,064H,000H,000H,008H,01CH,036H,063H
		.db   041H,014H,014H,014H,014H,014H,041H,063H
		.db   036H,01CH,008H,002H,001H,051H,009H,006H
		.db   032H,049H,079H,041H,03EH,07CH,012H,011H
		.db   012H,07CH,041H,07FH,049H,049H,036H,01CH
		.db   022H,041H,041H,022H,041H,07FH,041H,022H
		.db   01CH,07FH,049H,049H,049H,041H,07FH,009H
		.db   009H,009H,001H,03EH,041H,049H,049H,03AH
		.db   07FH,008H,008H,008H,07FH,000H,041H,07FH
		.db   041H,000H,030H,040H,041H,03FH,001H,07FH
		.db   008H,014H,022H,041H,07FH,040H,040H,040H
		.db   040H,07FH,002H,00CH,002H,07FH,07FH,006H
		.db   008H,030H,07FH,03EH,041H,041H,041H,03EH
		.db   07FH,009H,009H,009H,006H,03EH,041H,051H
		.db   021H,05EH,07FH,009H,019H,029H,046H,026H
		.db   049H,049H,049H,032H,001H,001H,07FH,001H
		.db   001H,03FH,040H,040H,040H,03FH,00FH,030H
		.db   040H,030H,00FH,07FH,020H,018H,020H,07FH
		.db   063H,014H,008H,014H,063H,007H,008H,078H
		.db   008H,007H,061H,051H,049H,045H,043H,000H
		.db   07FH,041H,041H,000H,004H,008H,010H,020H
		.db   040H,000H,041H,041H,07FH,000H,004H,002H
		.db   001H,002H,004H,040H,040H,040H,040H,040H
		.db   000H,001H,002H,004H,000H,020H,054H,054H
		.db   054H,078H,07FH,028H,044H,044H,038H,038H
		.db   044H,044H,044H,028H,038H,044H,044H,028H
		.db   07FH,038H,054H,054H,054H,018H,008H,008H
		.db   07EH,009H,00AH,018H,0A4H,0A4H,098H,07CH
		.db   07FH,004H,004H,004H,078H,000H,044H,07DH
		.db   040H,000H,040H,080H,084H,07DH,000H,000H
		.db   07FH,010H,028H,044H,000H,041H,07FH,040H
		.db   000H,07CH,004H,078H,004H,078H,07CH,008H
		.db   004H,004H,078H,038H,044H,044H,044H,038H
		.db   0FCH,018H,024H,024H,018H,018H,024H,024H
		.db   018H,0FCH,07CH,008H,004H,004H,008H,058H
		.db   054H,054H,054H,024H,004H,03FH,044H,044H
		.db   020H,03CH,040H,040H,03CH,040H,01CH,020H
		.db   040H,020H,01CH,03CH,040H,038H,040H,03CH
		.db   044H,028H,010H,028H,044H,01CH,0A0H,0A0H
		.db   090H,07CH,044H,064H,054H,04CH,044H,000H
		.db   008H,036H,041H,041H,000H,000H,077H,000H
		.db   000H,041H,041H,036H,008H,000H,002H,001H
		.db   002H,004H,002H,000H,000H,000H,000H,000H

; ======================================================
; LCD char generator shape table (80H-FFH)
; ======================================================
lcdgraphics: 	; 78F1H
		.db   0066H,077H,049H,049H,077H,066H,0FCH,086H
		.db   00D7H,0EEH,0FCH,000H,07FH,063H,014H,008H
		.db   014H,000H,078H,076H,062H,04AH,00EH,000H
		.db   0EEH,044H,0FFH,0FFH,044H,0EEH,00CH,04CH
		.db   07FH,04CH,00CH,000H,07CH,056H,07FH,056H
		.db   07CH,000H,07DH,077H,047H,077H,07FH,000H
		.db   000H,000H,07DH,000H,000H,000H,010H,020H
		.db   01CH,002H,002H,002H,054H,034H,01CH,016H
		.db   015H,000H,041H,063H,055H,049H,063H,000H
		.db   024H,012H,012H,024H,012H,000H,044H,044H
		.db   05FH,044H,044H,000H,000H,040H,03EH,001H
		.db   000H,000H,000H,008H,01CH,03EH,000H,000H
		.db   098H,0F4H,012H,012H,0F4H,098H,0F8H,094H
		.db   012H,012H,094H,0F8H,014H,022H,07FH,022H
		.db   014H,000H,0A0H,056H,03DH,056H,0A0H,000H
		.db   04CH,02AH,01DH,02AH,048H,000H,038H,028H
		.db   039H,005H,003H,00FH,000H,016H,03DH,016H
		.db   000H,000H,042H,025H,015H,028H,054H,022H
		.db   004H,002H,03FH,002H,004H,000H,010H,020H
		.db   07EH,020H,010H,000H,008H,008H,02AH,01CH
		.db   008H,000H,008H,01CH,02AH,008H,008H,000H
		.db   01CH,057H,061H,057H,01CH,000H,008H,014H
		.db   022H,014H,008H,000H,01EH,022H,044H,022H
		.db   01EH,000H,01CH,012H,071H,012H,01CH,000H
		.db   000H,004H,002H,001H,000H,000H,020H,055H
		.db   056H,054H,078H,000H,00EH,051H,031H,011H
		.db   00AH,000H,064H,07FH,045H,045H,020H,000H
		.db   000H,001H,002H,004H,000H,000H,07FH,010H
		.db   010H,00FH,010H,000H,000H,002H,005H,002H
		.db   000H,000H,004H,00CH,01CH,00CH,004H,000H
		.db   000H,004H,07FH,004H,000H,000H,018H,0A7H
		.db   0A5H,0E5H,018H,000H,07FH,041H,065H,051H
		.db   07FH,000H,07FH,041H,05DH,049H,07FH,000H
		.db   017H,008H,034H,022H,071H,000H,055H,03FH
		.db   010H,068H,044H,0E2H,017H,008H,004H,06AH
		.db   059H,000H,006H,009H,07FH,001H,07FH,001H
		.db   029H,02AH,07CH,02AH,029H,000H,070H,029H
		.db   024H,029H,070H,000H,038H,045H,044H,045H
		.db   038H,000H,03CH,041H,040H,041H,03CH,000H
		.db   01CH,022H,07FH,022H,014H,000H,008H,004H
		.db   004H,008H,004H,000H,020H,055H,054H,055H
		.db   078H,000H,030H,04AH,048H,04AH,030H,000H
		.db   03CH,041H,040H,021H,07CH,000H,040H,07FH
		.db   049H,049H,03EH,000H,071H,011H,067H,011H
		.db   071H,000H,038H,054H,056H,055H,018H,000H
		.db   03CH,041H,042H,020H,07CH,000H,038H,055H
		.db   056H,054H,018H,000H,000H,004H,000H,004H
		.db   000H,000H,048H,07EH,049H,001H,002H,000H
		.db   040H,0AAH,0A9H,0AAH,0F0H,000H,070H,0AAH
		.db   0A9H,0AAH,030H,000H,000H,002H,0E9H,002H
		.db   000H,000H,030H,04AH,49H,04AH,030H,000H
		.db   038H,042H,041H,022H,078H,000H,008H,004H
		.db   002H,004H,008H,000H,038H,055H,054H,055H
		.db   018H,000H,000H,002H,068H,002H,000H,000H
		.db   020H,054H,056H,055H,07CH,000H,000H,000H
		.db   06AH,001H,000H,000H,030H,048H,04AH,049H
		.db   030H,000H,03CH,040H,042H,021H,07CH,000H
		.db   00CH,050H,052H,051H,03CH,000H,07AH,011H
		.db   009H,00AH,071H,000H,042H,0A9H,0A9H,0AAH
		.db   0F1H,000H,032H,049H,049H,04AH,031H,000H
		.db   0E0H,052H,049H,052H,0E0H,000H,0F8H,0AAH
		.db   0A9H,0AAH,088H,000H,000H,08AH,0F9H,08AH
		.db   000H,000H,070H,08AH,089H,08AH,070H,000H
		.db   078H,082H,081H,082H,078H,000H,000H,045H
		.db   07CH,045H,000H,000H,07CH,055H,054H,055H
		.db   044H,000H,07CH,054H,056H,055H,044H,000H
		.db   0E0H,050H,04AH,051H,0E0H,000H,000H,088H
		.db   0FAH,089H,000H,000H,070H,088H,08AH,089H
		.db   070H,000H,03CH,040H,042H,041H,03CH,000H
		.db   00CH,010H,062H,011H,00CH,000H,03CH,041H
		.db   042H,040H,03CH,000H,07CH,055H,056H,054H
		.db   044H,000H,0E0H,051H,04AH,050H,0E0H,000H
		.db   000H,000H,000H,000H,000H,000H,00FH,00FH
		.db   00FH,000H,000H,000H,000H,000H,000H,00FH
		.db   00FH,00FH,0F0H,0F0H,0F0H,000H,000H,000H
		.db   000H,000H,000H,0F0H,0F0H,0F0H,00FH,00FH
		.db   00FH,0F0H,0F0H,0F0H,0F0H,0F0H,0F0H,00FH
		.db   00FH,00FH,00FH,00FH,00FH,00FH,00FH,00FH
		.db   0F0H,0F0H,0F0H,0F0H,0F0H,0F0H,0FFH,0FFH
		.db   0FFH,000H,000H,000H,000H,000H,000H,0FFH
		.db   0FFH,0FFH,0FFH,0FFH,0FFH,00FH,00FH,00FH
		.db   00FH,00FH,00FH,0FFH,0FFH,0FFH,0FFH,0FFH
		.db   0FFH,0F0H,0F0H,0F0H,0F0H,0F0H,0F0H,0FFH
		.db   0FFH,0FFH,0FFH,0FFH,0FFH,0FFH,0FFH,0FFH
		.db   000H,000H,0F8H,008H,008H,008H,008H,008H
		.db   008H,008H,008H,008H,008H,008H,0F8H,000H
		.db   000H,000H,008H,008H,0F8H,008H,008H,008H
		.db   000H,000H,0FFH,008H,008H,008H,000H,000H
		.db   0FFH,000H,000H,000H,000H,000H,00FH,008H
		.db   008H,008H,008H,008H,00FH,000H,000H,000H
		.db   008H,008H,00FH,008H,008H,008H,008H,008H
		.db   0FFH,000H,000H,000H,008H,008H,0FFH,008H
		.db   008H,008H,03FH,01FH,00FH,007H,003H,001H
		.db   080H,0C0H,0E0H,0F0H,0F8H,0FCH,001H,003H
		.db   007H,00FH,01FH,03FH,0FCH,0F8H,0F0H,0E0H
		.db   0C0H,080H,055H,0AAH,055H,0AAH,055H,0AAH

; ======================================================
; Keyboard conversion matrix
; ======================================================
kbdbascii:  ;7BF1H
		.text "zxcvbnml"
		.text "asdfghjk"
		.text "qwertyui"
		.text "op[;',./"
		.text "12345678"
		.text "90-="
;		.db   07AH,078H,063H,076H,062H,06EH,06DH,06CH
;		.db   061H,073H,064H,066H,067H,068H,06AH,06BH
;		.db   071H,077H,065H,072H,074H,079H,075H,069H
;		.db   06FH,070H,05BH,03BH,027H,02CH,02EH,02FH
;		.db   031H,032H,033H,034H,035H,036H,037H,038H
;		.db   039H,030H,02DH,03DH
		
; Shifted ASCII values
kbdasciishifted:  ;7C1DH
		.text "ZXCVBNML"
		.text "ASDFGHJK"
		.text "QWERTYUI"
		.text "OP]:\"<>?"
		.text "!@#$%^&*"
		.text "()_+"		
;		.db   05AH,058H,043H,056H
;		.db   042H,04EH,04DH,04CH,041H,053H,044H,046H
;		.db   047H,048H,04AH,04BH,051H,057H,045H,052H
;		.db   054H,059H,055H,049H,04FH,050H,05DH,03AH
;		.db   022H,03CH,03EH,03FH,021H,040H,023H,024H
;		.db   025H,05EH,026H,02AH,028H,029H,05FH,02BH

; GRPH values
kbdgraph: 	;7C49H
		.db   000H,083H,084H,000H,095H,096H,081H,09AH
		.db   085H,08BH,000H,082H,000H,086H,000H,09BH
		.db   093H,094H,08FH,089H,087H,090H,091H,08EH
		.db   098H,080H,060H,092H,08CH,099H,097H,08AH
		.db   088H,09CH,09DH,09EH,09FH,0B4H,0B0H,0A3H
		.db   07BH,07DH,05CH,08DH
		
; Shift GRPH values
kbdgraphshifted: ;7C75H
		.db   0E0H,0EFH,0FFH,000H
		.db   000H,000H,0F6H,0F9H,0EBH,0ECH,0EDH,0EEH
		.db   0FDH,0FBH,0F4H,0FAH,0E7H,0E8H,0E9H,0EAH
		.db   0FCH,0FEH,0F0H,0F3H,0F2H,0F1H,07EH,0F5H
		.db   000H,0F8H,0F7H,000H,0E1H,0E2H,0E3H,0E4H
		.db   0E5H,0E6H,000H,000H,000H,000H,07CH,000H
		
; CODE values
kbdcode:  ;7CA1H
		.db   0CEH,0A1H,0A2H,0BDH,000H,0CDH,000H,0CAH
		.db   0B6H,0A9H,0BBH,000H,000H,000H,0CBH,0C9H
		.db   0C8H,000H,0C6H,000H,000H,0CCH,0B8H,0C7H
		.db   0B7H,0ACH,0B5H,0ADH,0A0H,0BCH,0CFH,0AEH
		.db   0C0H,000H,0C1H,000H,000H,000H,0C4H,0C2H
		.db   0C3H,0AFH,0C5H,0BEH

; Shift CODE values
kbdcodeshifted: ;7CCDH
		.db   000H,0DFH,0ABH,0DEH
		.db   000H,000H,0A5H,0DAH,0B1H,0B9H,0D7H,0BFH
		.db   000H,000H,0DBH,0D9H,0D8H,000H,0D6H,0AAH
		.db   0BAH,0DCH,0B3H,0D5H,0B2H,000H,000H,000H
		.db   0A4H,0DDH,000H,000H,0D0H,000H,0D1H,000H
		.db   000H,000H,0D4H,0D2H,0D3H,0A6H,0A7H,0A8H

; NUM LOCK values
kbdnum: ;7CF9H
		.db   06DH,030H,06AH,031H,06BH,032H,06CH,033H
		.db   075H,034H,069H,035H,06FH,036H

; Special Key values (arrow, TAB, fKeys, etc.)
kbdspecial: ;7D07H
		.db   001H,006H
		.db   014H,002H,020H,07FH,009H,01BH,08BH,088H
		.db   08AH,00DH,080H,081H,082H,083H,084H,085H
		.db   086H,087H
		
; Shift special key values
kbdspecialshift: ;7D18H
		.db   01DH,01CH,01EH,01FH,020H,008H
		.db   009H,01BH,08BH,088H,089H,00DH,080H,081H
		.db   082H,083H,084H,085H,086H,087H,051H,052H
		.db   057H,05AH	

 .end
; ======================================================
; External ROM detect image loaded at F605H
; ======================================================
;036FH  DB   3EH,01H,D3H,E8H,21H,40H,00H,11H  ; F605H - MVI A,01H;  OUT E8H; LXI H,0040H;  LXI D,FAA4H
;0377H  DB   A4H,FAH,7EH,12H,23H,13H,7DH,D6H  ; F60DH - MVI A,M;    STAX D;  INX H; INX D; MOV A,L; SUI 48H
;037FH  DB   48H,C2H,0FH,F6H,D3H,E8H,2AH,A4H  ; F515H - JNZ F60FH;  OUT E8H; LHLD FAA4H;   
;0387H  DB   FAH,11H,54H,43H,C3H,18H,00H,F3H  ; F61DH - LXI D,4354H; JMP 0018H;     DI;
;038FH  DB   3EH,01H,D3H,E8H,C7H,00H,01H,00H  ; F625H - MVI A,01H;  OUT E8H; RST 0

; ======================================================
; RAM3 OPTROM Test Code 
; ======================================================
optram3load:
	.db 03EH,009H       ;MVI A,01H
	.db 0D3H,0E8H       ;OUT E8H
	.db 021H,040H,000H  ;LXI H,0040H
	.db 011H,017H,091H  ;LXI D,optram3open ; FAA4H       
	.db 07EH            ;MOV A,M
	.db 012H            ;STAX D
	.db 023H            ;INX H
	.db 013H       	    ;INX D
	.db 07DH            ;MOV A,L
	.db 0D6H,48H        ;SUI 48H
	.db 0C2H,00AH,091H  ;JNZ optram3loop
        .db 0D3H,0E8H       ;OUT E8H
	.db 0C9H	    ;RET

; ======================================================
; RAM2 OPTROM Test Code 
; ======================================================
optram2load:
	.db 03EH,009H       ;MVI A,01H
	.db 0D3H,0E8H       ;OUT E8H
	.db 021H,040H,000H  ;LXI H,0040H
	.db 011H,017H,0B1H  ;LXI D,optram2open ; FAA4H       
	.db 07EH            ;MOV A,M
	.db 012H            ;STAX D
	.db 023H            ;INX H
	.db 013H       	    ;INX D
	.db 07DH            ;MOV A,L
	.db 0D6H,48H        ;SUI 48H
	.db 0C2H,0AH,0B1H   ;JNZ optram2loop
    	.db 0D3H,0E8H       ;OUT E8H
	.db 0C9H	    ;RET

; ======================================================
; RAM1 OPTROM Test Code 
; ======================================================
optram1load:
	.db 03EH,009H       ;MVI A,01H
	.db 0D3H,0E8H       ;OUT E8H
	.db 021H,040H,000H  ;LXI H,0040H
	.db 011H,017H,0D1H  ;LXI D,optram1open ; FAA4H       
	.db 07EH            ;MOV A,M
	.db 012H            ;STAX D
	.db 023H            ;INX H
	.db 013H       		;INX D
	.db 07DH            ;MOV A,L
	.db 0D6H,48H        ;SUI 48H
	.db 0C2H,0AH,0D1H   ;JNZ optram1loop
    .db 0D3H,0E8H       ;OUT E8H
	.db 0C9H	    ;RET
:

; ======================================================
; RAM0 OPTROM Test Code 
; ======================================================
optram0load:
	.db 03EH,09H        ;MVI A,01H
	.db 0D3H,0E8H       ;OUT E8H
	.db 021H,040H,000H  ;LXI H,0040H
	.db 011H,017H,0F1H  ;LXI D,optram0open ; FAA4H       
	.db 07EH            ;MOV A,M
	.db 012H            ;STAX D
	.db 023H            ;INX H
	.db 013H       		;INX D
	.db 07DH            ;MOV A,L
	.db 0D6H,48H        ;SUI 48H
	.db 0C2H,0AH,0F1H  ;JNZ optram0loop
    .db 0D3H,0E8H       ;OUT E8H
	.db 0C9H            ;RET

; ======================================================
; The code below will appear in RAM it is here to generate
; the HEX code for the ROM to load into RAM when running. 
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
		OUT 0E8H; 
		RET
optram3open:
		.db 000H,000H,000H,000H,000H,000H,000H,000H

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
		OUT 0E8H; 
		RET
optram2open:
		.db 000H,000H,000H,000H,000H,000H,000H,000H

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
		OUT 0E8H; 
		RET
optram1open:
		.db 000H,000H,000H,000H,000H,000H,000H,000H

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
		OUT 0E8H; 
		RET
optram0open:
		.db 000H,000H,000H,000H,000H,000H,000H,000H