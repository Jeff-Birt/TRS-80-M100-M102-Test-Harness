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
;			15 Feb 2021 - V3.3. Added OPTROM/Test ROM Checksum
;                         Fixed a couple of bugs
;                         Added code to dump an OPTROM contents
;                         to serial port.  Use only on working boards
;			 1 Mar 2021 - V4.0. Added Serial Log Output.
;
;  Copyright © 2020-21 Stardust. 
;  All rights reserved.
;  Commercial use prohibited
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TASM 
; Processor: 8080/8085
;
#DEFINE VERSION .text "V4.0" ; Change this value to update version
;#DEFINE ROMDUMP   ;uncomment to dump the OPTROM to Serial port
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
		.org 2CH
		DI
		JMP bcrint    ; RST 5.5 RAM Vector	
	
; ==========================================================
; RST 6.5 -- RS232 character pending
; ==========================================================
		.org 34H
		DI
		JMP serint    ; RST 6.5 routine (RS232 receive interrupt)

; ==========================================================
; RST 7.5 -- Timer background task
; ==========================================================
		.org 3CH
		DI
		JMP timint      ; RST 7.5 interrupt routine
	
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
		.text "(c) IS  Mar 2021"
		.db 0
		.text "M100/102 ROM Test "
		VERSION
		.db 0
		.text "Copyright IS Mar 2021"
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

		MVI B, 3      ; Wake UP Diagnostic LCD 3 times	

; ==========================================================
; Configure the diagnostic LCD
; 	Assumes no RAM is available until tested
; ==========================================================
		; INIT diagnostic LDC
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
		MVI M,'C'      ; Load C to diagnostic LCD screen
		
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
; 	Test banks that exist by writing to each byte.
; 	Indicate any failures
; 	Once tested use the stack as part of further testing 
;	if required.
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
;  The Address lines A14-A11 are indirectly used to access the RAMs 
;  via decoders.  Consequently if these lines fail they are limited
;  to the failed CE.
;  The char 'm' is used to indicate a passing block.
;  The test will stop on the first block to fail in the 2K/8K RAM
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
; Collate the RAM pass/fail data to use later in the RAM LOG 
; ==============================================================
ramcheck:
		XRA A
		MOV B,A
		LXI SP,ram0+1000H - 2 ; Module 0				
		POP PSW	
		CPI goodmemch     ; If successful RAM block is OK
		JNZ chkRAM1
		MOV A,B
		ORI 01H
		MOV B,A		
				
chkRAM1:
		LXI SP,ram1+1000H - 2 ; Module 1				
		POP PSW			
		CPI goodmemch     ; If successful RAM block is OK
		JNZ chkRAM2
		MOV A,B
		ORI 02H
		MOV B,A
				
chkRAM2:
		LXI SP,ram2+1000H - 2; Module 2				
		POP PSW			
		CPI goodmemch     ; If successful RAM block is OK
		JNZ chkRAM3
		MOV A,B
		ORI 04H
		MOV B,A
		
chkRAM3:
		LXI SP,ram3+1000H -2 ; Module 3				
		POP PSW			
		CPI goodmemch     ; If successful RAM block is OK
		JNZ chkdone
		MOV A,B
		ORI 08H
		MOV B,A

; ==============================================================				
; Now pick a RAM module to use for the next set of tests. 
; ==============================================================		
chkdone:
;		MVI B,00001000B ; RAM testing
		
;Reg B should contain a bit stream to indicate the failed RAM
		MOV A,B
		ANI 01H          ; Check RAM0 
		JZ RAM1use
		LXI SP,ram0+1000H; Module 0				
		LXI H,logbegin0  ; Store the LOG start address
		SHLD logdatastart+ram0+1000H+2
		LXI H,logptrstart0 
		SHLD logmemptr+ram0+1000H+2
		LXI D,optram0load
		MVI A,'0'
		JMP ramfnd

RAM1use:		
		MOV A,B
		ANI 02H          ; Check RAM1
		JZ RAM2use 
		LXI SP,ram1+1000H; Module 1				
		LXI H,logbegin1  ; Store the LOG start and begin address
		SHLD logdatastart+ram1+1000H+2
		LXI H,logptrstart1
		SHLD logmemptr+ram1+1000H+2
		LXI D,optram1load
		MVI A,'1'	
		JMP ramfnd

RAM2use:
		MOV A,B
		ANI 04H          ; Check RAM2
		JZ RAM3use 
		LXI SP,ram2+1000H; Module 2			
		LXI H,logbegin2  ; Store the LOG start and begin address
		SHLD logdatastart+ram2+1000H+2
		LXI H,logptrstart2
		SHLD logmemptr+ram2+1000H+2
		LXI D,optram2load
		MVI A,'2'
		JMP ramfnd

RAM3use:
		MOV A,B
		ANI 08H          ; Check RAM3
		JZ cpuhlt        ; No useful RAM 
		LXI SP,ram3+1000H; Module 3				
		LXI H,logbegin3  ; Store the LOG start and begin address
		SHLD logdatastart+ram3+1000H+2
		LXI H,logptrstart3
		SHLD logmemptr+ram3+1000H+2
		LXI D,optram3load
		MVI A,'3'
		JMP ramfnd
		
		
; SP is now set up with valid RAM location	
ramfnd:
		PUSH B  ; Save the status of the RAM
		
		CALL stromlocation ; Temp use this location to store A for later use
		MOV M,A

;OPTROM DUMP CODE - DEBUG ROUTINE
; Requires operational RAM 0
#IFDEF ROMDUMP
	   LXI SP,ram0+1000H     ; Set up the stackpointer
	   CALL dumpromrun
#ENDIF		
;OPTROM DUMP CODE - DEBUG ROUTINE
	
	
;Insert OPTROM Detect Code into selected RAM Block	
		LXI H,0
		DAD SP
		MOV A,H
		ANI 0E0H
		ORI 010H
		MOV H,A
		MVI L,0
		LXI B,0100H
		DAD B
		XCHG            ; DE = Free RAM, HL = Code to load	
		MVI B,size      ; Number of bytes to move
		CALL movemem    ; B bytes to (DE) from (HL)
	
		;Finish the ram tests		
		CALL diagscreenpause	
		CALL clrscreen

		MVI A,080H     ; Set diag screen cursor to home
		CALL putins    ; Send to the diag screen			

		
; Set the STROM Resgister Location = 0	
		CALL stromlocation
		
		MOV A,M        ; Recover A
		CALL putch     ; Print out the selected RAM block
		
		MVI A,' '
		CALL putch
				
		; Reset location to 0
		XRA A
		MOV M,A
		OUT CtrlReg
		
; Set the countdown timer for the RST 7.5 interrupt					
		CALL getcountdown
		MVI M,0D7H

;*** RAM LOG Creation	
; Set up RAM log for the failed RAMs		
		POP B   ; Retrieve the RAM stats
		MOV A,B	 
		ANI 01H
		JNZ RAM1test
		;Failed RAM 0 -> 20
		PUSH B
		CALL readlogstartadd
		LXI B,20 ; Point to 'PASS' in RAM LOG
		DAD B  ; Add to the pointer
		XCHG
		LXI H,sysfailstr
		CALL putstrMEM
		POP B
		
RAM1test:
		MOV A,B
		ANI 02H
		JNZ RAM2test
		;Failed RAM 1 -> 32
		PUSH B
		CALL readlogstartadd
		LXI B,32 ; Point to 'PASS' in RAM LOG
		DAD B  ; Add to the pointer
		XCHG
		LXI H,sysfailstr
		CALL putstrMEM
		POP B

RAM2test:
		MOV A,B
		ANI 04H
		JNZ RAM3test
		;Failed RAM 2 -> 44
		PUSH B
		CALL readlogstartadd
		LXI B,44 ; Point to 'PASS' in RAM LOG
		DAD B  ; Add to the pointer
		XCHG
		LXI H,sysfailstr
		CALL putstrMEM
		POP B
		
RAM3test:
		MOV A,B
		ANI 08H
		JNZ RAMtestdone
		;Failed RAM 3 -> 56
		PUSH B
		CALL readlogstartadd
		LXI B,56 ; Point to 'PASS' in RAM LOG
		DAD B  ; Add to the pointer
		XCHG
		LXI H,sysfailstr
		CALL putstrMEM
		POP B

;*** RAM LOG Creation
	
RAMtestdone:	
;DEBUG Used to check loading of OPTROM 
;		CALL initRS ; Init the serial port
;		CALL ramsblockstart
;		XCHG
;		lxi B,1100H
;		DAD B
;		MVI B,192
;		CALL dumpmemRS
:DEBUG
		
		JMP piotest   ; Next test

; ##########################################################
; ##########################################################
; **********************************************************
; Test PIO 81C55
;	Write to the PIO registers and read back. 
;	
; **********************************************************
piotest:
		LXI H,piostr   ; Output test name
		CALL putstr

		; Setup the Control Register
;		MVI A,43H      ; Load configuration for PIO (A=OUT, B=OUT, C=IN, Stop Timer counter)
;		OUT PIOCR      ; Set PIO chip configuration

; ==========================================================
; Test Port A write RAM test data
; ==========================================================
		LXI H,piotstdata ; Reuse RAM byte test data table
tstpioa:
		MOV A,M          ; H = Current RAM byte test data
		CPI 0FEH         ; Check end of table
		JZ tstpioaok       
		OUT PIOA	     ; Write to PIOA Register
		IN  PIOA         ; Read PIOA location
		CMP M		     ; Compare locations
		JNZ tstpioafail  ; Not equal RAM location failed
		INX H            ; Get next RAM test byte
		JMP tstpioa       
		
tstpioaok:
		MVI A,'A'  	 ; Test OK
		JMP exitpioa
tstpioafail:
		MVI A,'F'        ; Test Fail
exitpioa:
		MOV D,A          ; Save status of test
		CALL putch

		;Set the LPT test register it might work
		MVI A,083H       ;Set the BCR, BUSY and BUSY- bits high
		OUT PIOA

		XRA A
		ORI 02H
		OUT CtrlReg 	 ; E8H
		XRA A
		OUT CtrlReg 	 ; E8H		
	
; ==========================================================
; Test Port B write RAM test data do not test power off
; ==========================================================
		LXI H,piotstdata ; Reuse the RAM byte test data table
tstpiob:
		MOV A,M   	     ; H = Current RAM test byte  data
		CPI 0FEH         ; Check end of table
		JZ tstpiobok 
		ANI 0EFH         ; No power off  
		MOV E,A        
		OUT PIOB         ; Write to PIOB Register
		IN  PIOB         ; Read PIOB location		
		CMP E		     ; Compare locations
		JNZ tstpiobfail  ; Not equal RAM location failed
		INX H            ; Get next RAM test byte
		JMP tstpiob       
		
tstpiobok:
		MVI A,'B'  	 ; Test OK
		JMP exitpiob
tstpiobfail:
		MVI A,'F'        ; Test Fail
exitpiob:
		CALL putch		

		MVI A,0E4H       ; PIO B configuration (RTS low, DTR low, SPKR=1, Serial=RS232, Keyscan col 9 enable)
		OUT PIOB         ; Set PIO chip port B configuration	

; ==========================================================
; Check for Port A or B Failure. 
; Jump to REMOTE test on failure.
; ==========================================================
		;Check for failure of PIO B		
		CPI 'F'
		JZ piofail
		
		;Check for failure of PIO A	
		MOV A,D
		CPI 'F'
		JZ piofail
		
		MVI A,0FFH      ; PIO A configuration (Used for Key scan, LCD data, etc.)
		OUT PIOA        ; Initialize PIO chip port A			        
 
; ==========================================================
; Check PIO C
; Ignore modem bits and serial port bits - tested in serial test
; ==========================================================      
tstpioc:        
		IN PIOC     	; Check PIO chip port C
		ANI 006H        ; Ignore top bits they will be tested with the serial port. Modem will not be tested.
		CPI 006H        ; 0x06 magic port C read
		JNZ piofail
		MVI A,'C'  	; Test OK
		CALL putch
	
;		MVI A,'L'
;		OUT PIOT1
;		IN  PIOT1
;	CALL putch
;		CPI 'L'
;		JNZ piofail
;		MVI A,'1'  		; Test OK
;		CALL putch
;
;		MVI A,'H'
;		OUT PIOT2
;		IN  PIOT2
;	CALL putch
;		CPI 'H'
;		JNZ piofail
;		MVI A,'2'  		; Test OK
;		CALL putch

piotestdone:	
		LXI H,passstr
		CALL putstr

		CALL diagscreenpause
	
		JMP chklcd     ; Test the M100/102 main LCD
	
; ==========================================================
; PIO Fail
; If either PIO registers fail test flow needs to change
; ========================================================== 		
piofail:		
		LXI H,piofailstr
		CALL putstr	
		
;Write and read the A & B locations
;Print to the bottom of the screen
		MVI A,0C0H      ; Set Cursor to next line
		CALL putins

; Test the PIO A register		
		XRA A
		CALL prthex
		
		OUT PIOA
		IN PIOA
		
		CALL prthex
		
		MVI A,0FFH
		
		CALL prthex
		
		OUT PIOA
		IN PIOA
		
		CALL prthex		

; Test the PIO B register	

		XRA A	
		CALL prthex
		
		OUT PIOB
		IN PIOB
		
		CALL prthex
		
		MVI A,0EFH  ; Might accidentally power off the board!!!
		
		CALL prthex
		
		OUT PIOB
		IN PIOB
				
		CALL prthex			
		CALL diagscreenpause

		JMP stromtestpiofail
		;CALL buzzer     ; Test the buzzer now the PIO is working		
		JMP cpuhlt      ; Stop here for now

; ##########################################################
; ##########################################################
; **********************************************************
; Screen Verification
;  Determine if real screen or test dongle
; **********************************************************
; Write 00 to PIOA to set CS20-CS27 to 0 
; Set the data register with 00H this will set the dongle to ins1
; Read Instruction register 1 should be 00H 

chklcd:
		XRA A  		   ; Set PIO A port to 0, this sets CSx lines	
		OUT PIOA	
	
		OUT scrdata ; Set the data portion to 0, sets D0 if dongle
		IN scrins
		
		CPI 0
		JNZ lcdtest ; If not 0 then real LCD is present	
					; else continue test using dongle
					
; **********************************************************
; Test Screen dongle
;  Test the LCD interface using the LCD test dongle.
;  Dongle contains registers to perform loopbacks of the data
;  connector bus.  The test also checks the individual CSx
;  lines to the LCD memory chips.
;      Data Read - LCD  scrdata (test Data) 0FFH
;      Inst Read - LCD  scrins  (test CSx)  0FEH
; 
;  There are two Inst registers that are selected by D0
;  of the data register.  
;  
; **********************************************************
dongletest:
		CALL clrscreen  ; Clear the diag screen

		MVI A,080H      ; Set Cursor to top line
		CALL putins
		
		LXI H,donglestrdata ; Indicate this is a dongle test
		CALL putstr
	
; ==========================================================
; Test data lines to LCD screen, AD0-AD7
; ==========================================================
		LXI H,dongletestdata ; Reuse RAM byte test data table
		
tstdongle:
		MOV A,M          ; H = Current RAM byte test data
		CPI 0FEH         ; Check end of table
		JZ tstdongleok       
		OUT scrdata	     ; Write to LCD Data Register
		IN  scrdata      ; Read LCD Data register
		CMP M		     ; Compare locations
		JNZ tstdonglefail; Not equal location failed
		INX H            ; Get next RAM test byte
		JMP tstdongle  
		
tstdongleok:
		LXI H,passstr    ; Output PASS to diag LCD
		CALL putstr      ; Indicate LCD Data bus pass
		JMP dongletestend
			
tstdonglefail:
		PUSH PSW         ; Save the failing data
		PUSH H
		
		LXI H,dongledatafail ; Output FAIL to diag LCD
		CALL putstr
		
		MVI A,0C0H      ; Set Cursor to next line
		CALL putins
		
		LXI H,donglestrstat ; Output WR RD text to diag LCD
		CALL putstr		

		MVI A,0C3H     ; Set Cursor to WR to location
		CALL putins
		
		; Print the data written
		POP H         ; Retrieve the data written
		MOV A,M        						
		CALL prthex

		MVI A,0C9H     ; Set Cursor to RD to location
		CALL putins				
		
		; Print out Read value
		POP PSW        ; Retrieve the data read
		CALL prthex

dongletestend:		
		CALL diagscreenpause 	;Delay to allow review

; ==========================================================
; Test CSx lines to LCD screen, CS20-27 label as INS1
; ==========================================================				
dongletestins:		
		;MVI A,0
		;OUT scrdata     ; Set to read instruction register 1

		CALL clrscreen		
		LXI H,donglestrins1 ; Indicate this is a dongle test
		CALL putstr
			
		LXI H,dongletestdata ; Reuse RAM byte test data table	
tstdongleins:
		MOV A,M          ; H = Current RAM byte test data
		CPI 0FEH         ; Check end of table
		JZ tstdongleinsok       
		OUT PIOA	     ; Write to PIOA Register
		IN  scrins       ; Read the lower instruction register
		CMP M		     ; Compare locations
		JNZ tstdongleinsfail  ; Not equal RAM location failed
		INX H            ; Get next RAM test byte
		JMP tstdongleins  

tstdongleinsok:
		LXI H,passstr ; Output PASS to diag LCD
		CALL putstr         ; Indicate Data bus pass
		JMP dongletestinsend
			
tstdongleinsfail:
		PUSH PSW         ; Save the failing data
		PUSH H
		
		LXI H,dongledatafail ; Output FAIL to diag LCD
		CALL putstr
		
		MVI A,0C0H      ; Set Cursor to next line
		CALL putins
		
		LXI H,donglestrstat ; Output WR RD text to diag LCD
		CALL putstr		

		MVI A,0C3H     ; Set Cursor to WR to location
		CALL putins
		
		; Print the data written
		POP H          ; Retrieve the data written
		MOV A,M        						
		CALL prthex

		MVI A,0C9H     ; Set Cursor to RD to location
		CALL putins				
		
		; Print out Read value
		POP PSW        ; Retrieve the data read
		CALL prthex

dongletestinsend:		
		CALL diagscreenpause 	;Delay to allow review

; ==========================================================
; Test other CSx and ctl lines to LCD screen 
;   1, 1, 1, 1, 1, RST-, C29, C28 (D7-D0) 4 and E
; ==========================================================		
dongletestins2:
		CALL clrscreen		
		LXI H,donglestrins2 ; Indicate this is a dongle test
		CALL putstr

		MVI A,01H
		OUT scrdata ; Set to read instruction register 2
		
		MVI A,0ECH  ; 
		OUT PIOB    ; Set CS28 and CS29 bits to 0.
		IN scrins   ; Read back the settings.
		
		ANI 003H    ;
		JNZ  tstdongleins2fail	
		
		MVI A,0EDH  ; 
		OUT PIOB    ; Set CS28 and CS29 bits to 1 0.
		IN scrins   ; Read back the settings.
		
		ANI 003H    ;
		CPI 001H
		JNZ  tstdongleins2fail	

		MVI A,0EEH  ; 
		OUT PIOB    ; Set CS28 and CS29 bits to 0 1.
		IN scrins   ; Read back the settings.
		
		ANI 003H    ;
		CPI 002H
		JNZ  tstdongleins2fail					
		
		MVI A,0EFH  ; 
		OUT PIOB    ; Set CS28 and CS29 bits to 1.
		
		IN scrins   ; Read back the settings.
		ANI 003H    ;
		CPI 003H
		JNZ  tstdongleins2fail
		
			
		
tstdonglins2ok:
		LXI H,passstr ; Output PASS to diag LCD
		CALL putstr         ; Indicate Data bus pass
		MVI A,0C0H          ; Set Cursor to next line
		CALL putins

		LXI H,dongleins2data ; Reuse RAM byte test data table
		CALL putstr          ; Indicate Data bus pass	
		
		MVI A,0C4H           ; Set Cursor to print out RESET- state	
		CALL putins

		IN scrins            ; Read back the resgister.
		
		ANI 004H				 ; Retrieve the Reset state should always be 1
		RAR
		RAR
		ADI 030H             ; Make into ASCII 0 or 1
		CALL putch
		
		MVI A,0CEH           ; Set Cursor to print out RESET- state	
		CALL putins

		IN scrins            ; Read back the register state.
		
		ANI 003H		     ; Retrieve the CS28 and CS29 states should be 1 if not failed
		ADI 030H             ; Make into ASCII 0 or 1
		CALL putch
				
		JMP dongletestins2end
			
tstdongleins2fail:		
		LXI H,dongledatafail ; Output FAIL to diag LCD
		CALL putstr
		
		MVI A,0C0H           ; Set Cursor to next line
		CALL putins
		
		LXI H,donglestrstat  ; Output WR RD text to diag LCD
		CALL putstr		

		MVI A,0C3H     ; Set Cursor to WR to location
		CALL putins
		
		; Print the data written
		IN PIOB        ; Read the data written to the PIOB  
		ANI 003H       ; Get D1, D0  						
		CALL prthex

		MVI A,0C9H     ; Set Cursor to RD to location
		CALL putins				
		
		; Print out Read value
		IN scrins      ; Retrieve the data from the screen
		ANI 003H       ; Put the CS29 and CS28 bits in D1 and D0
		CALL prthex

dongletestins2end:		

		MVI A,0E4H       ; PIO B configuration (RTS low, DTR low, SPKR=1, Serial=RS232, Keyscan col 9 enable)
		OUT PIOB         ; Set PIO chip port B configuration
		
		MVI A,0FFH
		OUT PIOA         ; Reset PIO to 0FFH
		
		CALL diagscreenpause ; Delay to allow review
		JMP clktest          ; Continue with clock test
		
; **********************************************************
; Test Screen
;  Check the screen can be accessed and write to screen RAM.
;  Write the all 1's pattern and then all 0's
;  Write a set of test chars on all 8 lines.
; **********************************************************
; Write to screen check it can read back status
lcdtest:
		MVI A,00H	    ; Set PIO A port to 0, this sets CSx lines to 1	
		OUT PIOA
		
		MVI A,0C0H      ; Set Cursor to next line
		CALL putins
		
		LXI H,lcdstr    ; Output to diag LCD text
		CALL putstr

		;Init screen 6D02H
		CALL lcdshortdelay ; 7533H Enable LCD drivers after short delay
		XRA A              ; OUT 0 to LCD
		OUT scrins
		CALL lcdshortdelay ; 7533H Enable LCD drivers after short delay
		MVI A,03BH	       ; OUT 3BH to LCD UP mode
		OUT scrins
		CALL lcdsettopline ; 752BH Set the display top line to zero for all LCD controllers
		CALL lcdshortdelay ; 7533H Enable LCD drivers after short delay
		MVI A,39H          ; OUT 39H to LCD Display ON
		OUT scrins

		MVI A,'I'          ; Indicate LCD init complete.  Should see random text on LCD
		CALL putch
		
		IN scrins          ; Check the LCD has completed init
		; If LCD present read back should be X100 0000, else X111 1111

		ANI 07FH
		CPI 040H           ; Check for valid LCD present
		JNZ LCDfail

		MVI A,'U'          ; LCD Success
		JMP LCDprt
		
		;LCD fail or is not present
LCDfail:
		LXI H,lcdfailstr     ; Output fail to diag LCD text
		CALL putstr
		CALL diagscreenpause ;Delay to allow review
		JMP clktest 
LCDprt:
		CALL putch

		;Fill screen with dark pattern
		MVI C,0EFH      ; Clear screen
		CALL fillscreen
	
		CALL longdelay

		;Clear the screen with space ie clear screen
		MVI C,'X'      ; Fill screen with 'X'
		CALL fillscreen

		;Output 8 lines of text.  Text is stored below
		LXI H,tstline0
		LXI D,0101H
		CALL putlcdstr

		LXI H,tstline1
		LXI D,0102H
		CALL putlcdstr

		LXI H,tstline2
		LXI D,0103H
		CALL putlcdstr

		LXI H,tstline3
		LXI D,0104H
		CALL putlcdstr

		LXI H,tstline4
		LXI D,0105H
		CALL putlcdstr

		LXI H,tstline5
		LXI D,0106H
		CALL putlcdstr

		LXI H,tstline6
		LXI D,0107H
		CALL putlcdstr

		LXI H,tstline7
		LXI D,0108H
		CALL putlcdstr

		LXI H,lcddone        ; Output diagnostic LCD text
		CALL putstr
		
		CALL diagscreenpause ;Delay to allow review

		JMP clktest 

; ==========================================================
; PUT String to the main LCD screen. D,E should point to location
; 	Does not word wrap for end of line
; ==========================================================
putlcdstr:		
		PUSH PSW
putlcdloop:
		XRA A           ; A = 0
		MOV C,M   	; H = Current Character
		CMP M           ; Check end of table
		JZ putlcdexit 
		PUSH H
		PUSH D      
		CALL plotchar   ; Write to main screen
		POP D
		POP H
		INX H           ; Get next Char
		INR D           ; Next row entry
		JMP putlcdloop
		
putlcdexit:
		POP PSW
		RET
		
; ======================================================
; Plot character in C on LCD at (D,E)
; All regiters are destroyed
; ======================================================
plotchar: ;73EEH
		DCR D          ; DCR D COL - 1?
		DCR E          ; DCR E ROW - 1?
;		XCHG           ; DE <-> HL 
		PUSH D         ; save COL-1,ROW-1
		MOV A,C        ; A = C Character
		LXI D,lcdasciitable - 1 ; 7710H Load top of Char FONT table - 1
		SUI 20H        ; A = A - 32
		JZ charfound   ; 7410H If char = SPACE
		INX D          ; INC D
		CPI 60H        ; A = 60H => 80H Graphics 
		JC charfound   ; 7410H If less then must be a char
		LXI D,lcdasciitable - 60H ; 76B1H = 7711H - 60H Load top of Graphic FONT table

;Printable Character found. 
charfound: ;7410H
		PUSH PSW       ; Save status from Compare CPI 60H
		MOV L,A        ; L = A
		MVI H,00H      ; H = 0 => HL = char - 32
		MOV B,H        ; B = 0
		MOV C,L        ; C = L => BC = char - 32
		DAD H          ; HL + HL
		DAD H          ; HL + HL  = 4 x HL
		DAD B          ; HL + BC  = 5 x HL
		POP PSW        ; POP A
		PUSH PSW       ; PUSH A  Get status to check for character type ASCII or Graphic
		JC asciichar   ; 741FH  < 60H
		DAD B          ; HL + BC = 6 x HL

;ASCII Char
asciichar: ;741FH
		DAD D          ; HL + DC = Table + Char x 5 or Graphics x 6
		POP PSW        ; POP A
		PUSH H
		
		JNC printlcdchar ; 7430H

;If ASCII Char put into buffer
		POP H
		CALL ramsblockstart  ;Get the start of the RAM in DE
;Move ASCII Char into RAM space
		PUSH D
		MVI B,05H
		CALL movemem ; 2542H Move B bytes from M to (DE)
		XRA A
		STAX D
	
;Graphics char
printlcdchar:   ;7430H
		POP B          ; Either Graphic Font Table Pointer or RAM Table for ASCII Char
		POP H          ; Retrieve the COL-1,ROW-1
		CALL printchar ; 74A2H Byte Plot - Send bit pattern to LCD for character
		XRA A          ; CLR A
		CALL lcdsettopline ; 752BH Set the display top line to zero for all LCD controllers
		RET

; ======================================================
; Send bit pattern to Main LCD for the character
; ======================================================
printchar: ;74A2H
		PUSH B     ; Save Font Table Pointer
		MVI E,06H  ; E = 6  Number of bytes for a Graphic
;74A5H  (3AH) LDA FFF5H  ; COL
		MOV A,H    ; A = H = COL - 1
		CPI 08H    ; A = 8   Col is a bridge
		JZ decfour ; 74B7H E = E - 4
		CPI 10H    ; A = 16  Col is a bridge
		JZ dectwo  ; 74B9H E = E - 2
		CPI 21H    ; A = 33 Col is a bridge E - 4
		JNZ nodec  ; 74BBH E = E  
decfour: ;74B7H
		DCR E      ; E--
		DCR E      ; E--
dectwo:  ;74B9H
		DCR E      ; E--
		DCR E      ; E--
nodec:   ;74BBH
		MOV C,A    ; C = A COL - 1
		ADD C      ; A + A
		ADD C      ; 2A + A
		MOV C,A    ; C = 3*COL - 3  
		MVI B,00H  ; B = 0  
		MOV A,L    ; ROW
		RAR        ; /2
		RAR        ; /4
		RAR        ; /8  0 1 2 3   4 5 6 7 See which section upper or lower RAM modules 
				   ; 2 -> 7 -> C0
				   		
		LXI H,lcdlowercs ; 75C9H 8155 PIO chip bit patterns for Lower LCD drivers
		JC setuptable    ; 74D0H
		LXI H,lcduppercs ; 7551H 8155 PIO chip bit patterns for Upper LCD drivers
		
setuptable:   ;74D0H
		DAD B       ; HL + BC  HL + 3xCOL 75C9 Set pointer to LCD PIO Tables: PIOA, PIOB and RAM Address  

	    MOV B,A     ; Save LCD RAM Address

		CALL enablelcd ; 753BH Enable LCD driver(s) specified by (HL)

		MOV D,H
		MOV C,L    ; Save HL to D,C as PIO address field table pointer
		
		MOV A,B     ; 
		ORA M       ; OR with 3rd entry in PIO table to set address
		MOV B,A     ; Save B = A, Address setting
		
		POP H       ;Get font data bits pointer
		PUSH D		
		CALL writebyteslcd ; 74F7H Send E bytes from HL to the LCD RAM

		POP D
;Finish the print
		MVI A,006H 	; See how many have been missed
		SUB E
		RZ        	; Return if all done
	
;Complete the missing bits on the next RAM Chip
		MOV E,A      ; E = Missing byte cnt  

		PUSH H
		MOV H,D
		MOV L,C      ; Restore HL form D,C as PIO address field table pointer

		INX H          ; Get the next PIO field 
		CALL enablelcd ; 753BH Enable LCD driver(s) specified by (HL) 
		
		MOV A,B
		ANI 0C0H       ; clr lower address leave row address ok
		MOV B,A        ; save 

		POP H
						
;		MOV E,C
 
; Write the selected bytes
writebyteslcd:  ;74F7H
		MOV A,B
		CALL lcdbusy   ; 7548H Wait for LCD driver to be available
		OUT scrins     ; FEH Set Address for the LCD RAM Chip
lcdwrite:       
lcdwait:
		IN scrins 		; FEH Wait for LCD 
		RAL
		JC lcdwait     ; 750BH
		MOV A,M        ; Put font byte to A
		OUT scrdata    ; FFH Output to LCD
		INX H          ; Next byte
		DCR E          ; Byte count
		JNZ lcdwait    ; 750BH      ; Next byte
		RET
			
; ======================================================
; Fill Screen use DE and print char in C
; ======================================================
fillscreen:    ;4601H
		MVI E,1      ; Prepare to point to LCD RAM (1,1) ROW
nextlcdrow:
		MVI D,1      ;   " 				   COL
nextlcdchar:
		PUSH B
		PUSH D
		CALL plotchar  ; 73EEH  or 4566H Call Level 6 Character Draw routine
		POP D
		POP B
		INR D          ; Increment column
		MOV A,D        ; Prepare to test for column 40
		CPI 41         ; Test if beyond column 40
		JNZ nextlcdchar; 4608H Jump if more columns on this line
		INR E          ; Increment line
		MOV A,E        ; Prepare to test if last line refreshed
		CPI 9          ; Test if beyond line 8
		JNZ nextlcdrow ; 4606H Jump back to refresh next line if not on line 9
		RET

; ======================================================
; Set the display top line to zero for all LCD controllers
; This configures the HW scrolling to start displaying
; from the natural ROW 0 (i.e. not scrolled).
; ======================================================
lcdsettopline: ;752BH
		CALL lcdshortdelay    ; 7533H Enable LCD drivers after short delay
		MVI A,03EH            ; Load command to set top line = 0
		OUT scrins            ; FEH Send the command
		RET
	
; ======================================================
; Enable LCD drivers after short delay
; ======================================================
lcdshortdelay: ;7533H
		MVI C,03H            ; Prepare for a short delay 
		CALL shortdelay      ; 7657H Delay routine - decrement C until zero
		LXI H,lcdalldrivers  ; 7641H Point to LCD enable bits to enable all 

; ======================================================
; Enable LCD drivers specified by (HL)
; ======================================================
enablelcd:     ;753BH
		MOV A,M        ; Get Bit pattern for 8 drivers
		OUT PIOA       ; B9H OUTput the bit pattern for 8 drivers
		INX H          ; Increment to bit pattern for next 2 LCD drivers
		IN PIOB        ; BAH Get current value of I/O port with 2 LCD drivers
		ANI 0FCH       ; Mask off LCD driver bit positions
		ORA M          ; OR in selected LCD driver enable bits
		OUT PIOB       ; BAH OUTput selected LCD driver bits
		INX H          ; Increment to next set of LCD driver enable bits
		RET

; ======================================================
; Wait for LCD driver to be available
; ======================================================
lcdbusy:        ;7548H
		PUSH PSW       ; Save A on stack
lcdrdbusy:
		IN scrins      ; FEH Read the LCD driver input port
		RAL            ; Rotate the busy bit into the C flag
		JC lcdrdbusy   ; Jump to keep waiting until not busy
		POP PSW        ; Restore A
		RET

; ##########################################################
; ##########################################################		
; **********************************************************
; Test CLK chip
; Write and read back from the clk chip
; Check the chip is ticking
; **********************************************************
clktest:
		DI
		 
		MVI A,01FH
		SIM

		CALL initclock  ; Set the clock ticking with defaults

		CALL clrscreen

		LXI H,clkteststr
		CALL putstr
 		CALL longdelay  ; Let the LCD Screen
	
		MVI A,080H      ; Set Cursor to top line
		CALL putins   
			
		CALL printclkdata  ; Print the clock data		

		CALL longdelay  ; Let the clock tick
		CALL longdelay  ; Let the clock tick
		CALL longdelay  ; Let the clock tick
		CALL longdelay  ; Let the clock tick

		MVI A,0C0H         ; Set Cursor to bottom line
		CALL putins	

		CALL printclkdata ; Print the clock data

		;MVI A,' '
		;CALL putch

		CALL clocklocation

		MOV A,M

		LXI H,passstr ; Output PASS string to Diag Screen

		CPI 0H
		JZ clkfail

		JMP clkdone

clkfail:		
		LXI H,clkfailstr
clkdone:		
		CALL putstr

		CALL diagscreenpause ;Delay to allow review

		JMP rst75test
		
; ======================================================
; Init the clock chip
; ======================================================
initclock: ;7E9DH
		LXI H,clockinitvalues ; 7F01H Initial clock chip register values
		CALL updatetime       ; 732AH Update clock chip regs from M
		MVI A,05H
		CALL setclkmode       ; 7383H Set clock chip mode
		RET
		
; ======================================================
; Print raw clock data to Test LCD
; ======================================================
; m100_rtc Stucture
; Month/DOW/DayH/DayL/HourH/HourL/MinH/MinL/SecH/SecsL
; All BCD except Month is Hex
; 00 - 0F
; 40 - 4F
printclkdata:
		CALL clocklocation ; HL points to start or clock data RAM
		CALL copytime2mem
	       
		DCX H		
		MOV A,M	       ; Get Month
		CALL prthex    ; Print as HEX
		MVI C,009H	
			
printclkloop:			
		DCX H
		MOV A,M 	;Get Next Value
		ADI '0'   ;Convert to ASCII number
		CALL putch	;Print it out		
		DCR C
		JNZ printclkloop
		RET

; ======================================================
; Update in-memory (F923H) clock values
; ======================================================
updateclock:  ;19A0H
		PUSH H         ; Preserve BASIC string pointer to stack
;		LXI H,F923H    ; Seconds (ones)
		CALL clocklocation

		;DI             ; Disable interrupts during copy
		CALL copytime2mem ; 7329H Copy clock chip regs to M
		;EI             ; Re-enable interrupts
		POP H          ; Restore BASIC string pointer
		RET
	
; ======================================================
; Copy clock chip regs to M pointed to by HL
; ======================================================
copytime2mem: ;7329H
;7329H  (F6H) ORI AFH       
		.db 0F6H        ; Copy Clock   registers to Memory
updatetime:   ;732AH
		XRA A           ; Update clock registers from Memory
		PUSH PSW
		;CALL disablebackground ; 765CH Disable Background task & barcode interrupts
		MVI A,03H       ; Read Time
		CNZ setclkmode  ; 7383H      ; If A! = 0 Read the time registers 7329H
		MVI A,01H       ; Register Shift mode
		CALL setclkmode ; 7383H     ; Set clock chip mode 
	
		MVI C,07H       
		CALL shortdelay ; 7657H     ; Delay routine - decrement C until zero
	
;Init the data read
		MVI B,0AH      ; 10 Digits to read

; Read 4 bits
next4bits: ;7340H
		MVI C,04H      ; Number of bits
		MOV D,M        ; Move memory to D

nextbits:  ;7343H
		POP PSW
		PUSH PSW
		JZ readnextbit ; 7352H Read next bit from Clock Chip
		IN PIOC        ; BBH/B3H Get CLK DataIN
		RAR            ; Put bit into Carry
		MOV A,D        ; A = D
		RAR            ; C -> A -> C 
		MOV D,A        ; D = A
		XRA A          ; CLR
		JMP readclkbit ; 735DH Next bit

; ======================================================
; Read next bit from Clock Chip
; ======================================================
readnextbit: ;7352H
		MOV A,D  ; Put CLK Memory to A
		RRC      ; Get LSB to CY
		MOV D,A  ; Save A to D
		MVI A,10H ;
		RAR      ; 
		RAR
		RAR
		RAR      ; 0000CY001
		OUT PIOA ; B9H Write PIO CLK bit

; Read bit
readclkbit: ;735DH
;Clock the data into the Chip
		ORI 09H  ; CLK and C0 bits
		OUT PIOA ; B9H  ; PIO A
		ANI 0F7H  ; Reset CLK bits
		OUT PIOA ; B9H  ; PIO A

		DCR C    ; Bits -- 
		JNZ nextbits ; 7343H ; 
		MOV A,D  ; A = D Top 4 bits are the new data
		RRC      ; 
		RRC
		RRC
		RRC      ; Move Top 4 bits to bottom 4 bis
		ANI 0FH  ; Get the bottom 4 bits
		MOV M,A  ; Store in Memory 
		INX H    ; Next memory locations
		DCR B    ; Number of loops
		JNZ next4bits   ; 7340H
		POP PSW
		MVI A,02H ; 
		CZ setclkmode   ; 7383H     ; Set clock chip mode
		XRA A
		CALL setclkmode ; 7383H     ; Set clock chip mode
; Enable the background tasks
		;MVI A,09H  ;743CH
		;SIM
		RET

; ======================================================
; Set clock chip mode
; ======================================================
setclkmode: ;7383H
		OUT PIOA     ; B9H PIO A
;7385H  (3AH) LDA FF45H      ; Contents of port E8H
		PUSH H
		CALL stromlocation
		MOV A,M 
		ORI 04H      ; CLK Strobe HI
		OUT CtrlReg  ; E8H 
		ANI 0FBH     ; CLK Strobe LO
		OUT CtrlReg  ; E8H
		POP H
		RET    

; ======================================================
; Initial clock chip register values
; ======================================================
clockinitvalues: ;7F01H
; Order is in reverse:
; SecL/SecH/MinL/MinH/HourL/HourH/DayL/DayH/DOW/Month
;                  M D Dy Ho Mi Se
; Setting below => 1 0 01 11 53 50
	.db 000H,005H,0003H,005H,001H,001H,001H,000H
	.db 000H,001H,0CFH,09DH,0CFH,0DDH	
;	.db 000H,000H,000H,000H,000H,000H,001H,000H
;	.db 000H,001H,0CFH,09DH,0CFH,0DDH

; ##########################################################
; ##########################################################
; **********************************************************
; Test timer interrupt
; 	Initial set up of the test.
; **********************************************************
; Set interrupt.
rst75test:
		CALL clrscreen

		CALL rst75testlocation
		MVI M,0
		
		LXI H,rst75teststr
		CALL putstr
 		CALL longdelay  ; Let the clock tick
 		
 		MVI A,080H         ; Set Cursor to top line
		CALL putins
		CALL printclkdata  ; Print the clock data	
		
		MVI A,1BH      ; Prepare to re-enable RST 7.5 interrupt, Set bit 0
		SIM            ; Re-enable RST 7.5 interrupt
		EI             ; Re-enable interrupts
	
		;Approx 6s delay. Clock should tick ~6s & Test location should reach 2x that
		
		CALL longdelay
		CALL longdelay
		CALL longdelay
		CALL longdelay
		CALL longdelay
		CALL longdelay

		DI
		MVI A,1FH      ; Load to disble interrupts
		SIM            ; Disable all interrupts

 		MVI A,0C0H         ; Set Cursor to bottom line
		CALL putins
		CALL printclkdata  ; Print the clock data
			
		MVI A,08CH         ; Set cursor to top line
		CALL putins
			
		;Check the operation of the interrupt		
		CALL rst75testlocation
		MOV A,M
		CPI 6   	; Make sure it is ticking should > 6s	
				
		PUSH PSW
		
		RAR	       ; /2 This should match the difference in seconds

		CALL prthex    ; Print the count

 		MVI A,0CBH     ; Set Cursor to bottom line
		CALL putins
		
		LXI H,passstr
		
		POP PSW
		JNC rst75testdone
		LXI H,testfailstr

rst75testdone:
		CALL putstr
	
		CALL diagscreenpause ;Delay to allow review
		JMP rst65test

; ======================================================
; RST 7.5 interrupt routine (Background tick)
; ======================================================
timint:
		PUSH H         ; \
		PUSH D         ;  \ Save all registers on stack
		PUSH B         ;  /
		PUSH PSW       ; /
		MVI A,01BH     ; Prepare to re-enable RST 7.5 interrupt
		SIM            ; Re-enable RST 7.5 interrupt
		EI             ; Re-enable interrupts

		CALL getcountdown
		DCR M           ; Decrement the 2Hz count-down counter
		JNZ timintdone  ; 1BAEH Jump if not zero to skip 10Hz background logic
		MVI M,07DH      ; Re-load count-down value for 2 Hz

		CALL rst75testlocation
		INR M

timintdone:
		POP PSW     ; \
		POP B       ;  \ Retrieve all registers on stack
		POP D       ;  /
		POP H		; /
		RET


; ##########################################################
; ##########################################################
; **********************************************************
; Test Serial Port
; Send and receive a character in loopback
; Use interrupt to receive character
; Switch to modem and check serial port does not work.
; **********************************************************
rst65test:
		CALL clrscreen
		
		LXI H,rst65teststr
		CALL putstr

 		MVI A,0C0H     ; Set Cursor to back to start
		CALL putins	
		LXI H,dsrteststr
		CALL putstr	

; ======================================================		
; Test CTS/RTS/DSR/DTS
;  CTS - PC4 10H, DSR - PC5 20H 4E
;  RTS - PB7 80H, DTR - PB6 40H 46
; ======================================================

		MVI B,0        ; Use B for failure count
;Test setting 1		
		IN PIOB        ; Read PIOB port
		ORI 0C0H       ; Set RTS and DTR = 1
		OUT PIOB  
		
;RTS Set 1
 		MVI A,0C6H     ; Set Cursor to back to start
		CALL putins	

		IN PIOC        ; Read Port C	
		ANI 010H       ; Get CTS-RTS	
		
		JZ rtsonefail
		MVI A,'1'
		JMP rtsonedone
rtsonefail:
		MVI A,'F'
		MVI B,1
rtsonedone:		
		CALL putch
		
;DTR Set 1
 		MVI A,0CEH     ; Set Cursor to back to slot
		CALL putins	
		
		IN PIOC
		ANI 020H       ; Get DSR-DTR	

		JZ dtronefail
		MVI A,'1'
		JMP dtronedone
dtronefail:
		MVI A,'F'
		MVI B,1
dtronedone:
		CALL putch

		CALL longdelay
 		CALL longdelay
 		CALL longdelay
		
;Test setting 0	
		IN PIOB        ; Read PIOB port
		ANI 03FH       ; Set RTS and DSR = 0
		OUT PIOB  
		
;RTS Set 0
 		MVI A,0C6H     ; Set Cursor to back to RTS position
		CALL putins	

		IN PIOC        ; Read Port C	
		ANI 010H       ; Get CTS-RTS	
		
		JNZ rtszerofail
		MVI A,'0'
		JMP rtszerodone
rtszerofail:
		MVI A,'F'
		MVI B,1
rtszerodone:
		CALL putch
		
;DTR Set 0
		MVI A,0CEH     ; Set Cursor to DTR position	
		CALL putins	
		
		IN PIOC
		ANI 020H       ; Get DTR	

		JNZ dtrzerofail
		MVI A,'0'
		JMP dtrzerodone
dtrzerofail:
		MVI A,'F'
		MVI B,1
dtrzerodone:
		CALL putch	

		CALL longdelay
 		CALL longdelay
 		CALL longdelay

		MVI A,08BH     ; Set Cursor to back to start
		CALL putins		

		LXI H,passstr
		MOV A,B
		CPI 0
		JZ rst65dtrdone

rst65testfail:		
		LXI H,testfailstr
rst65dtrdone:
		CALL putstr
		
		MVI A,080H     ; Set Cursor to back to start
		CALL putins	
				
 		CALL diagscreenpause ;Delay to allow review
 		
 		CALL rxbuffer  ; Store received character
		MVI M,0

; ======================================================		
; Configure serial port loopback test
; ======================================================		
		CALL clrscreen

		LXI H,loopbackteststr
  		CALL putstr

		MVI A,0C0H     ; Set Cursor to back to start lower line
		CALL putins	  		
  				
  		LXI H,loopbackstatstr
  		CALL putstr
		
		; Reset buffer
		XRA A
		CALL rxbuffer  ; Store received character
		MOV M,A
		INX H
		MOV M,A    
	
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

		MVI A,01DH     ; Prepare to re-enable RST 6.5 interrupt
		SIM  
		EI
; ======================================================
; Begin the loopback test	
; ======================================================
		MVI B,0		
		
txnewchar:
;TX Byte      ;6E3AH
		MVI A,0C2H     ; Set Cursor to C gap
		CALL putins	 
		
		IN UARTmode   ; D8H
		CALL prthex
	
		ANI 010H      ; TX Buffer empty - High buffer empty
		JZ txfail     ; 6E3AH       ; Send character in C to serial port

		MVI A,0C8H     ; Set Cursor  TX gap
		CALL putins			
		MOV A,B
		OUT UARTsend ; C8H
		CALL prthex

		MVI A,0CAH     ; Set Cursor  RX gap
		CALL putins		

		; Short delay to make sure char is there
		MVI D,10
TXdelay:
		MVI C,255   ;
		CALL shortdelay ; Wait ~1ms to allow char to be received
		DCR D
		JNZ TXdelay

;RX Char
;RXwait:		
		CALL rxbuffer  ; Get Received Char
		INX H
		MOV A,M
		CPI 0BFH
		;No character RX failed
		JNZ rxfail
		DCX H
 
		MVI A,0CEH     ; Set Cursor to RX gap
		CALL putins 

		MOV A,M
		CALL prthex
		
		CMP B
		JNZ rxfail
		
		;Reset Char received
		INX H
		MVI M,0
;Next Char				
		DCR B
		JNZ txnewchar

rst65loopbacksuccess:				
		LXI H,loopbackpassstr	
		JMP rst65testdone

txfail:
		LXI H,txfailstr
		JMP rst65testdone

rxfail:
		LXI H,rxfailstr
		JMP rst65testdone
		
rst65loopbackfail:
		LXI H,loopbackfailstr	
			
rst65testdone:
		DI
		MVI A,01FH     ; Prepare to re-enable RST 7.5 interrupt
		SIM            ; Re-enable RST 7.5 interrupt
		
        MVI A,43H      ; Stop timer
		OUT PIOCR      ; B8H Control Reg

		MVI A,080H     ; Set Cursor to upper line
		CALL putins			
		CALL putstr
waiting:
		CALL diagscreenpause ;Delay to allow review
		JMP sysbustest

; ==========================================================
; Serial port (6.5) Interrupt Service routine 	
; ==========================================================	
serint:
		PUSH H         ; \
		PUSH D         ;  \ Save all registers on stack
		PUSH B         ;  /
		PUSH PSW       ; /

		IN UARTread    ; C8H         ; Serial Register

		CALL rxbuffer  ; Store received character
		MOV M,A
		INX H
		MVI M,0BFH     ; Char received 

		POP PSW     ; \
		POP B       ;  \ Retrieve all registers on stack
		POP D       ;  /
		POP H		; /
		
		EI
		RET

; ##########################################################
; ##########################################################
; **********************************************************
; Test Sys BUS @ 80H
;  Read and write to the sysbus
; **********************************************************
;Write to the sys bus register
sysbustest:
		DI 
		CALL clrscreen
		
		LXI H, sysbusteststr
		CALL putstr
		
		MVI A,0C0H     ; Set Cursor to back to start lower line
		CALL putins	
		
		LXI H, systeststr
		CALL putstr

		LXI H,ramtstdata ; Load the RAM byte test data table
sysbus1:
		MVI A,0C3H     ; Set Cursor to WR to location
		CALL putins

		MOV A,M   	     ; Get first entry
		CPI 0FEH         ; Check end of table
		JZ sysbustestdone    
	
 		OUT sysreg     ; Send to the Sys bus port
 		 	
 		CALL prthex    ; Print the written character

		MVI A,0C9H     ; Set Cursor to RD back
		CALL putins	
		 			
 		IN sysreg       ; Read from the sys bus port

 		CALL prthex     ; Print the rd character

		CMP M 
		JNZ sysbustestfail
		CALL longdelay  ; Brief wait

		INX H           ; Get next RAM test byte

		;Print out the settings
		MVI A,0CEH     ; Set Cursor to Settings slot
		CALL putins
		
		IN setreg
		CALL prthex

		JMP sysbus1

sysbustestfail:
		LXI H,sysfailstr
		JMP sysbustestprt
		
sysbustestdone:				
		LXI H,passstr
		
sysbustestprt:
		MVI A,08AH     ; Set Cursor to back to upper line
		CALL putins	
		
		CALL putstr
		CALL diagscreenpause ;Delay to allow review
		JMP lpttest	

; ##########################################################
; ##########################################################	
; **********************************************************
; Test Printer Port
; Write to the LPT port
; Check the strobe works
; Check busy- and busy works.
; **********************************************************
lpttest:
		CALL clrscreen  ; Reset the screen for the new test
		
		LXI H,lptteststr
		CALL putstr

		MVI A,0C0H     ; Set Cursor to back to start lower line
		CALL putins	
		
		LXI H, lptloopteststr ;
		CALL putstr
		
		LXI D,ramtstdata ; Load the RAM byte test data table
lpttest1:
 		MVI A,0C3H     ; Set Cursor to back for new count
		CALL putins	
		
		LDAX D   	 ; D = Current RAM byte test data
		CPI 0FEH     ; Check end of table
		JZ lpttestdone1

		;CMA         ; Complement
		OUT PIOA   	 ; B9H 
		CALL prthex  ; Print the data
			
		MOV C,A      ; Save A
	    
		; Pulse the strobe line to write data to register   
		CALL stromlocation
		MOV A,M
		MOV B,A
		ORI 02H
		OUT CtrlReg 	 ; E8H
		MOV A,B
		OUT CtrlReg 	 ; E8H

		LDAX D

 		MVI A,0C9H     ; Set Cursor to back for new count
		CALL putins		

		IN lptreg      ; Read the LPT reg
		
		;CMA 
		CALL prthex    ; Print the data

		CALL longdelay

		CMP C
		JNZ lpttestfail		
		INX D          ; Get next RAM test byte
		JMP lpttest1
		
		
lpttestdone1:				
		MVI A,08AH     ; Set Cursor to back upper line
		CALL putins	
		
		LXI H,passstr
		
		CALL putstr
		CALL diagscreenpause ;Delay to allow review
		
; *********************************************
; ** Test the two busy lines, busy and busy- **
; *********************************************
lpttestnext:
;Check Busy and Busy- lines PC1 Busy- PC2 Busy
		CALL clrscreen ;Setup the screen for next test
 		MVI A,080H     ; Set Cursor to back to start
		CALL putins	
		LXI H,nbusyteststr
		CALL putstr	


 		MVI A,0C0H     ; Set Cursor to back to start
		CALL putins	
		LXI H,busyteststr
		CALL putstr	

 		MVI A,086H     ; Set Cursor to BUSY-
		CALL putins	

; Set up BUSY line tests
		XRA A         ; Set Busy and Busy- to 0
		MOV C,A
		OUT PIOA   	  ; B9H 

		; Pulse the strobe line to write data to register   
		CALL stromlocation
		MOV A,M
		MOV B,A
		ORI 02H
		OUT CtrlReg 	 ; E8H
		MOV A,B
		OUT CtrlReg 	 ; E8H

		; Read the state of the busy lines				
		IN PIOC        ; Read Port C
	
		MOV B,A
			
		ANI 002H       ; Get Busy-	
		JZ nbusyzero
		
		MVI A,'F'
		MVI C,1
		JMP nbusydone
		
nbusyzero:
		MVI A,'0'
nbusydone:
		CALL putch

 		MVI A,0C6H     ; Set Cursor to BUSY
		CALL putins	
				
		MOV A,B
		
		ANI 004H       ; Get Busy	
		JZ busyzero
		MVI A,'F'
		MVI C,1
		JMP busydone
busyzero:
		MVI A,'0'
busydone:
		CALL putch


 		MVI A,087H     ; Set Cursor to BUSY-
		CALL putins	
		
		MVI A,3       ; Set Busy and Busy- to 1
		OUT PIOA   	  ; B9H 

lpttestbusy:
		; Pulse the strobe line to write data to register   
		CALL stromlocation
		MOV A,M
		MOV B,A
		ORI 02H
		OUT CtrlReg 	 ; E8H
		MOV A,B
		OUT CtrlReg 	 ; E8H

		; Read the state of the busy lines				
		IN PIOC        ; Read Port C
	
		MOV B,A
			
		ANI 002H       ; Get Busy-	
		JNZ nbusyone
		
		MVI A,'F'
		MVI C,1
		JMP nbusydone1
		
nbusyone:
		MVI A,'1'
nbusydone1:
		CALL putch

 		MVI A,0C7H     ; Set Cursor to BUSY
		CALL putins	
				
		MOV A,B
		
		ANI 004H       ; Get Busy	
		JNZ busyone
		MVI A,'F'
		MVI C,1
		JMP busydone1
		
busyone:
		MVI A,'1'
busydone1:
		CALL putch
				
;Check C for failures
		MOV A,C
		CPI 0
		JZ lpttestdone

lpttestfail:  
		LXI H,lptfailstr
		JMP lptprtdone
		
lpttestdone:
		LXI H,passstr

lptprtdone:
		MVI A,089H     ; Set Cursor to upper line
		CALL putins	
		CALL putstr

		;Set up for the BCR test
		MVI A,080H
		OUT PIOA

		; Pulse the strobe line to write data to register   
		CALL stromlocation
		MOV A,M
		MOV B,A
		ORI 02H
		OUT CtrlReg 	 ; E8H
		MOV A,B
		OUT CtrlReg 	 ; E8H

		CALL diagscreenpause ;Delay to allow review
		JMP rst55test	

; ##########################################################
; ##########################################################
; **********************************************************
; Test BCR
; Read the port bit
; Check the interrupt works.
; **********************************************************
bcrloopcnt .equ 250

rst55test:
		CALL clrscreen
		
		LXI H,rst55teststr
		CALL putstr
 		;CALL longdelay  ; Let the clock tick

		MVI A,0C0H     ; Set Cursor to back to start lower line
		CALL putins

		LXI H,rst55loopteststr
		CALL putstr

		CALL rst55testlocation
		MVI M,0        ; Reset button counter
		 				
		MVI A,00EH     ; Prepare to re-enable RST 5.5 interrupt, Set bit 0
		SIM            ; Re-enable RST 5.5 interrupt
		EI             ; Enable Interrupts
 
 		MVI B,bcrloopcnt
rst55wait:

		MVI A,000H    ; Set BCR to 0
		OUT PIOA      ; B9H 

		; Pulse the strobe line to write data to ctrl register   
		CALL stromlocation
		MOV A,M
		MOV C,A
		ORI 02H
		OUT CtrlReg 	 ; E8H
		MOV A,C
		OUT CtrlReg 	 ; E8H
				
		; Short delay to let the interrupt happen
		MVI D,1
BCRdelay:
		MVI C,255   ;
		CALL shortdelay ; Wait ~1ms to allow interrupt
		DCR D
		JNZ BCRdelay

		; Print the loop count and interrupt counts
 		MVI A,0C3H     ; Set Cursor to loop location
		CALL putins

		MOV A,B
		DCR A

		CALL prthex   ; Print the count

 		MVI A,0C9H     ; Set Interrupt count prt location
		CALL putins

		CALL rst55testlocation
		MOV A,M
				
		CALL prthex    ; Print the interrupt count
				
		DCR B		
		JNZ rst55wait


;Complete test the results
		DI              ; Disable interrupts
		MVI A,01FH      ; Prepare to disable all interrupts
		SIM  

		MVI A,087H     ; Set Cursor to top line
		CALL putins	

		CALL rst55testlocation
		MOV A,M
			
		LXI H,passstr
		
		CPI bcrloopcnt				
		JZ rst55testdone  ; If count == loopcount then Pass
		
		LXI H,testfailstr ; Else failed
		
rst55testdone:
		CALL putstr
		CALL diagscreenpause ;Delay to allow review

		JMP stromtest

; ==========================================================
; BCR Interrupt Service routine
; 	BCR PC3. Low level interrupt
; ==========================================================	
bcrint:
		PUSH H      ; \
		PUSH D      ;  \ Save all registers on stack
		PUSH B      ;  /
		PUSH PSW    ; /


 		MVI A,0CEH     ; Set PIOC to count
		CALL putins

		;MVI A,'>'
		;CALL putch		

		CALL rst55testlocation

		;Check the BCR bit is 0
		IN PIOC        ; Read the PIO PC3 
    	CALL prthex
		ANI 008H 	   ; Access the BCR Bit it is inverted so should be '1'
		JZ bcrintdone  ; If not zero do not inc count
				
		INR M				
		
bcrintdone:
		;Clear the interrupt
		MVI A,080H    ; Set BCR to 1
		OUT PIOA      ; B9H 

		; Pulse the strobe line to write data to register   
		CALL stromlocation
		MOV A,M
		MOV C,A
		ORI 02H
		OUT CtrlReg 	 ; E8H
		MOV A,C
		OUT CtrlReg 	 ; E8H

		POP PSW     ; \
		POP B       ;  \ Retrieve all registers on stack
		POP D       ;  /
		POP H		; /		
		EI
		RET
		
; ##########################################################
; ##########################################################
; **********************************************************
; Test STROM register
; The STROM register controls: CLK, LPT, REMOTE, ROM selection
;  CLK bit is tested in Clock test
;  LPT bit is tested in the LPT test
;  REMOTE bit is tested in the cassette test
;
; STROM bit switches to the OPTROM.  During the switch the
; Range 0040-0048H is read to RAM. If the switch works 
; the data will be random based on the absence/presence of an
; OPTROM.  This location contains 'NO OPTROM' in the main test
; ROM.  If 'NO OPTROM' is read then the switch did not work.
; Code is run from the RAM in order to read the ROM contents.
; **********************************************************
stromtestpiofail:
		MVI A,077H
		PUSH PSW
stromtest:
		CALL clrscreen  ; Reset the screen for the new test
		
		LXI H,stromteststr ; 
		CALL putstr	

 		MVI A,0C0H      ; Set Cursor to Bottom line
		CALL putins
		
;Find which memory version is being used
		CALL stackstart	  ;Which RAM is being used 9, B, D, F (80,A0,C0,E0) 
		
		MOV A,D

		ANI 0F0H
;RAM3 is the active one	
		CPI 090H
		JNZ ram2loc
		CALL optram3
		CALL getrex3
		LHLD checksum3t
		PUSH H
		LHLD rexstore3 ;RE
		PUSH H
		LHLD rexstore3+2 ;X!
		PUSH H
		LHLD checksum3  ; Get the checksum value
		PUSH H
		LXI H,ram3used
		LXI D,optram3open
		JMP stromdetect
		
;RAM2 is the active one	
ram2loc:		
		CPI 0B0H
		JNZ ram1loc
		CALL optram2
		CALL getrex2
		LHLD checksum2t
		PUSH H
		LHLD rexstore2 ;RE
		PUSH H
		LHLD rexstore2+2 ;X!
		PUSH H
		LHLD checksum2  ; Get the checksum value
		PUSH H
		LXI H,ram2used	
		LXI D,optram2open
		JMP stromdetect

;RAM1 is the active one	
ram1loc:		
		CPI 0D0H
		JNZ ram0loc
		CALL optram1
		CALL getrex1
		LHLD checksum1t
		PUSH H
		LHLD rexstore1 ;RE
		PUSH H
		LHLD rexstore1+2 ;X!
		PUSH H
		LHLD checksum1  ; Get the checksum value
		PUSH H
		LXI H,ram1used	
		LXI D,optram1open
		JMP stromdetect

;RAM0 is the active one	
ram0loc:	
		CPI 0F0H
		JNZ ramnotfound
		CALL optram0
		CALL getrex0
		LHLD checksum0t
		PUSH H
		LHLD rexstore0 ;RE
		PUSH H
		LHLD rexstore0+2 ;X!
		PUSH H		
		LHLD checksum0  ; Get the checksum value
		PUSH H
		LXI H,ram0used	
		LXI D,optram0open
		JMP stromdetect	

ramnotfound:
		LXI H,stromfailurestr
		JMP stromtestdone
			
stromdetect:	
		CALL putstr
 		MVI A,080H      ; Set Cursor to top line
		CALL putins				
		XCHG
		LXI D,040H      ; Get start of the ROM to compare values
		MVI B,6
optromloop:
		LDAX D
		CMP M
		JNZ optromfound
		INX D
		INX H
		DCR B
		JZ stromtestfail
		JMP optromloop
optromfound:
		LXI H,optromfoundstr
		JMP stromtestdone
				
stromtestfail:
		LXI H,nooptromstr
				
stromtestdone:
		CALL putstr
		
romchecksumchk:         ; Check the checksum
 		MVI A,0C1H      ; Set Cursor to Bottom line
		CALL putins
		LXI H,chksum
		CALL putstr

 		MVI A,0C2H      ; Set Cursor to Bottom line
		CALL putins		
		POP H           ; Get the checksum
 
; Print out the check sum
 		MOV A,H
		CALL prthex
		MOV A,L
		CALL prthex   

		XCHG   ; Save the checksum

;.text "0123456789ABCDEF"
;.text "x yyyy 

 		MVI A,0C7H      ; Set Cursor to Bottom line
		CALL putins		
		
; See if OPTROM is a REX
		POP H
		MOV A,H
		CPI '!'
		JNZ rexidend1
		MOV A,L
		CPI 'X'
		JNZ rexidend1
		POP H
		MOV A,H
		CPI 'E'
		JNZ rexidend
		MOV A,L
		CPI 'R'
		JNZ rexidend
		
		LXI H,idrex
		JMP optromfin	; HL now point to ID string	
		
				
rexidend1:
		POP H ; Clean the stack in the REX! check fails
		
rexidend:
		XCHG  ; Retrieve the checksum
		LXI D,optromidtable
		LXI B, 7
		
findtheoptromid:
		LDAX D
		INX D 
		SUB H
		JNZ nextid1
		LDAX D
		INX D
		SUB L
		JNZ nextid
		XCHG
		JMP optromfin	; HL now point to ID string	
nextid1:
		INX D
nextid:
		XCHG
		DAD B
		XCHG
		MVI A,((endoptromidtable >> 8) & 0FFH)
		SUB D
		JNZ findtheoptromid
		MVI A,endoptromidtable & 0FFH
		SUB E
		JNZ findtheoptromid	
					
		LXI H,optromunknow
		
optromfin:
		CALL putstr
		
 		MVI A,0CCH      ; Set Cursor to Bottom line
		CALL putins		
		POP H           ; Get the checksum for the test rom
		
; Print out the check sum of the test ROM
 		MOV A,H
		CALL prthex
		MOV A,L
		CALL prthex 		

		CALL diagscreenpause ;Delay to allow review
		
		POP PSW
		CPI 077H
		JZ castest					

		JMP chkkbd


; ##########################################################
; ##########################################################
; **********************************************************
; Test Keyboard
;  Determine if there is a real keyboard or loop back.
;  A real keyboard or loopback maybe used to test the port.
;  The loopback connects PA0-PA7 to KR0-KR7, this allows a 
;  direct validation that the connectors are good and that
;  M13 and M15 are functional.  
:  NOTE: PB0 is not tested using the loopback.  It is however
;  exercised in the LCD test.  
;
; **********************************************************
; write 0H to Port A 
; read back the keyboard port. 
;	If keybd = 00H then loopback test 
;	else keyboard test
;   
chkkbd:
		CALL clrscreen ; Clear the diag screen for the test
		
		XRA A  		; Set PIO A port to 0, this sets CSx lines	
		OUT PIOA	
	
		IN readkbd  ; Read the keyboard register
				
		CPI 0FFH
		JZ kbdtest  ; If not 0 then real Keyboard is present	
					; else continue test using loopback

; **********************************************************
; Test keyboard loopback
;  Keyboard ports looped back run through the 256 possible 
;  values check they are valid. 
; **********************************************************
loopbacktest:
		MVI A,080H      ; Set Cursor to top line
		CALL putins
		
		LXI H,loopbackstrdata ; Indicate this is a loopback test
		CALL putstr
	
; ==========================================================
; Test data lines to Keyboard driver KR0-KR7 to PA0-PA7
; ==========================================================
		LXI H,loopbacktstdata ; Reuse RAM byte test data table
		
tstloopback:
		MOV A,M          ; H = Current RAM byte test data
		CPI 0FEH         ; Check end of table
		JZ tstloopbackok       
		OUT PIOA	     ; Write to Keyboard loopback
		IN readkbd       ; Read Keyboard register
		CMP M		     ; Compare locations
		JNZ tstloopbackfail ; Not equal location failed
		INX H            ; Get next RAM test byte
		JMP tstloopback  
		
tstloopbackok:
		LXI H,passstr    ; Output PASS to diag LCD
		CALL putstr      ; Indicate LCD Data bus pass
		JMP loopbacktestend
			
tstloopbackfail:
		PUSH PSW         ; Save the failing data
		PUSH H
		
		LXI H,loopbackfail ; Output FAIL to diag LCD
		CALL putstr
		
		MVI A,0C0H      ; Set Cursor to next line
		CALL putins
		
		LXI H,loopbackstrstat ; Output WR RD text to diag LCD
		CALL putstr		

		MVI A,0C3H     ; Set Cursor to WR to location
		CALL putins
		
		; Print the data written
		POP H         ; Retrieve the data written
		MOV A,M        						
		CALL prthex

		MVI A,0C9H     ; Set Cursor to RD to location
		CALL putins				
		
		; Print out Read value
		POP PSW        ; Retrieve the data read
		CALL prthex
		CALL diagscreenpause ;Delay to allow review

loopbacktestend:		
		CALL diagscreenpause ;Delay to allow review
		JMP castest

; **********************************************************
; Test Keyboard
;  Real keyboard present use this set of tests.
; **********************************************************
; Press key on keyboard
kbdtest:
		; CALL clrscreen
		
		LXI H,kbdteststr
		CALL putstr
		
 		MVI A,0C0H     ; Set Cursor to back to start lower line
		CALL putins	

		LXI H,keypressedstr ; 0C6H
		CALL putstr

		CALL rst75testlocationreset ; Reset the timeout
	
		MVI A,1BH      ; Prepare to re-enable RST 7.5 interrupt, Set bit 0
		SIM            ; Re-enable RST 7.5 interrupt
		EI
			
kbdtestloop:		
		CALL scankeyboard
		
		;Wait for 10s after last key press
		CALL rst75testlocationget
		CPI 20   ; See if 10s is up
		JC kbdtestcont
		JMP kbdtestdone

kbdtestcont:		
		CALL ctrlbreak ; See if CTRL-BREAK pressed
		JNZ kbdtestloop
		
kbdtestdone:
		DI
		MVI A,1FH      ; Prepare to disable RST 7.5 interrupt, Set bit 0
		SIM            ; Disable RST 7.5 interrupt
				
 		MVI A,0C0H     ; Set Cursor to back to start lower line
		CALL putins	
		LXI H,kbdtestdonestr
		CALL putstr
		
		CALL diagscreenpause ;Delay to allow review

		JMP castest
			
; ======================================================
; Check for CTRL-BREAK
;  Z flag set if CTRL-BREAK pressed
; ======================================================
ctrlbreak:
		MVI A,0E4H      ; PIO B configuration (RTS low, DTR low, SPKR=1, Serial=serial, Keyscan col 9 enable)
		OUT PIOB	    ; BAH  Set PIO chip port B configuration
		MVI A,0FFH      ; PIO A configuration (Used for Key scan, LCD data, etc.)
		OUT PIOA	    ; B9H Initialize PIO chip port A
		IN readkbd      ; E8H  Scan Keyboard to test for CTRL-BREAK (cold boot indicator)
		CMA
		ANI 082H        ; Mask all but CTRL-BREAK keys
		CPI 082H
		PUSH PSW
		MVI A,0E5H      ; Load code to disable key-scan col 9 (for CTRL-BREAK)
		OUT PIOB	    ; BAH  Set PIO chip port B configuration
		POP PSW
		RET

; ======================================================
; Check for SHIFT-BREAK
;  Z flag set if CTRL-BREAK pressed
; ======================================================
shiftbreak:
		MVI A,0E4H      ; PIO B configuration (RTS low, DTR low, SPKR=1, Serial=serial, Keyscan col 9 enable)
		OUT PIOB	    ; BAH  Set PIO chip port B configuration
		MVI A,0FFH      ; PIO A configuration (Used for Key scan, LCD data, etc.)
		OUT PIOA	    ; B9H Initialize PIO chip port A
		IN readkbd      ; E8H  Scan Keyboard to test for SHIFT-BREAK (cold boot indicator)
		CMA
		ANI 081H        ; Mask all but SHIFT-BREAK keys
		CPI 081H
		PUSH PSW
		MVI A,0E5H      ; Load code to disable key-scan col 9 (for SHIFT-BREAK)
		OUT PIOB	    ; BAH  Set PIO chip port B configuration
		POP PSW
		RET
		
; ======================================================
; Keyboard scanning management routine
; ======================================================
scankeyboard: ;7055H
		
; ======================================================
; Scan the Mod keys
; ======================================================	
		CALL scancol9  	  ; 72B1H Scan BREAK,CAPS,NUM,CODE,GRAPH,CTRL,SHIFT & set bits in A
		CPI 0
		
		JNZ modkeydecode  ; Print the key type 

; ======================================================
; Scan the regular keyboard
; ======================================================	
;Scan the keyboard for other keys
		XRA A          ; Clear A
		OUT PIOA       ; B9H PORT A
		IN readkbd     ; E8H KBD Check any keys pressed ROWs
		INR A          ; A++ If FFH set to 0
		MVI A,0FFH     ; Reset A to FFH
		OUT PIOA       ; B9H 
		RZ             ; Return no further keys pressed
		
;Carry on with key check				
		MVI A,07FH      ; Keyboard mask   
		MVI C,007H      ; Col Count

		CALL getkeyboardstorage ; ROW, COL 

keyscanloop:
		MOV B,A	       ; Save A 7FH
		OUT PIOA       ; B9H PORT A
		IN readkbd     ; E8H KBD
		CMA            ;     
		CPI 0          ; See if there is a Key in this column
		JZ resetcolscan ;7092H      ; Key different to stored key
		MOV M,A        ; Save ROW 
		CALL prthex
		INX H
		MOV A,C
		MOV M,A        ; Save COLUMN
 	
		JMP keydecode

;Reset port and DEC count
resetcolscan:
		MVI A,0FFH ; Restore port
		OUT PIOA  ; B9H PORT A
		MOV A,B   ; 
		RRC       ; Shift mask to right
		DCR C     ; Shift for 7 bits
		JP keyscanloop ;
		RET

; ======================================================
; Decode the Normal Keys
; ======================================================
keydecode:	
		MOV A,M

		MOV C,A
		INR C

		XRA A		
		MVI D,24   ; Char count per row
mult24:
		DCR C
		JZ rowdone
		ADD D
		JMP mult24
rowdone: 
		; A=C*24
		MOV D,A  ; Save A in D

		DCX H    ; Get ROW
		MOV A,M 
		CALL rowdecode
		DCR C
		CALL mult3  ; COL * 3
		
		ADD D ; A points to string
 
		MOV C,A
		MVI B,00H
		LXI H,kbdchars
		DAD B
		JMP printchars
		
; ======================================================
; Decode the Mod Keys
; ======================================================
modkeydecode:
		CALL rowdecode
		RZ                ;If 0 then false alarm
		
		CALL prthex
		
		DCR C
		CALL mult3  ; COL * 3
	 
modfound:
		LXI H,modkeystr
		MVI B,00H
		MOV C,A
		DAD B

; Print out the 				
printchars:
		
		MVI A,' '
		CALL putch

		MOV A,M
		CALL putch
		INX H 
		MOV A,M
		CALL putch
		INX H 
		MOV A,M
		CALL putch
		CALL rst75testlocationreset ; Reset the timeout
		
		MVI A,0C5H ;0C5H     ; Set Cursor to start of line for new key
		CALL putins
		
		RET

; ======================================================		
; Determine the row number
; 	A = Raw ROW data
; 	C = ROW number
; ======================================================		
rowdecode:
		MVI C,08H
 
modkeydecodeloop:
		RLC
		RC 
		DCR C
		JNZ modkeydecodeloop
		RET

; ======================================================		
; Mult3
;	A = 3*A
; ======================================================		
mult3:
		MOV A,C
		ADD C ; 2A
		ADD C ; 3A
		RET
; ======================================================		
; Key string data
; ======================================================		
			
modkeystr:
		.text "SH CTLGRHCODNUMCAPXXXBRK"
;              012345678901234567890123
		.db 0	
			
kbdchars:  ;7BF1H
;              COL
;              012345678901234567890123  ROWs
		.text " Z  X  C  V  B  N  M  L "   0  - 0
		.text " A  S  D  F  G  H  J  K "   1  - 24
		.text " Q  W  E  R  T  Y  U  I "   2  - 48
		.text " O  P  [  ;  '  ,  .  / "   3  - 72
		.text " 1  2  3  4  5  6  7  8 "   4  - 96
		.text " 9  0  -  = LFTRHTUP DWN"   5  - 120
		.text "SPCDELTABESCPASLABPRICR "   6  - 144
		.text "F1 F2 F3 F4 F5 F6 F7 F8 "   7  - 168 192
			
; ======================================================
; Scan BREAK,CAPS,NUM,CODE,GRAPH,CTRL,SHIFT & set bits in A
; ======================================================
scancol9: ;72B1H
		MVI A,0FFH
		OUT PIOA	;B9H PIO A
		IN PIOB	    ;BAH PIO B
		ANI 0FEH
		MOV B,A
		OUT PIOB   ;BAH
		IN readkbd ;E8H
		PUSH PSW
		MOV A,B
		INR A
		OUT PIOB 	;BAH
		POP PSW
		CMA
		RET

; ##########################################################
; ##########################################################
; **********************************************************
; Test the cassette port
; Check remote clicks the relay
; Read in data from the port using Audacity, check header 
; Write out data to audacity check  
; **********************************************************
castest:
		CALL clrscreen  ; Reset the screen for the new test
		
		LXI H,castestoffstr ; 11 chars
		CALL putstr		

		CALL stromlocation
		MOV A,M
		
;OFF 
		ANI 0F1H
		ORI 08H
		OUT CtrlReg 	; E8H	
		PUSH PSW

 		MVI A,080H      ; Set Cursor to top line
		CALL putins	

		CALL longdelay  

		IN setreg
		ANI 080H
		JNZ remtestfail

;ON		
		LXI H,castestonstr ; 11 chars
		CALL putstr	
		POP PSW
		PUSH PSW		
		ANI 0F1H
		OUT CtrlReg 	; E8H

 		MVI A,080H      ; Set Cursor to top line
		CALL putins

		CALL longdelay  ; Let the clock tick
		
		IN setreg
		ANI 080H
		JZ remtestfail

;OFF 
		LXI H,castestoffstr ; 11 chars
		CALL putstr	
		
		POP PSW
		ANI 0F1H
		ORI 08H
		OUT CtrlReg ; E8H	
	
		CALL longdelay

		IN setreg
		ANI 080H
		JNZ remtestfail
		JMP remtestpass

remtestfail:
		LXI H,castestfailstr
		JMP remtestdone

remtestpass:
		LXI H,castestpassstr

remtestdone:		
		MVI A,080H      ; Set Cursor to top line
		CALL putins
		
		CALL putstr	

		CALL diagscreenpause        ; Delay to allow review	
		CALL rst75testlocationreset ; Reset the timeout

; ***************** Outputting
;Play out header use Grey Lead
		
		MVI A,0C0H      ; Set Cursor to bottom line
		CALL putins
		LXI H,castestrecstr
		CALL putstr
		
		MVI A,1BH      ; Prepare to re-enable RST 7.5 interrupt, Set bit 0
		SIM            ; Re-enable RST 7.5 interrupt
		EI		
				
caswaitrec:		
		CALL shiftbreak       ; See if the cassette tests should be skipped
		JZ castestplayoption

		;Wait for 10s for option to play
		CALL rst75testlocationget
		CPI 30   ; See if 10s is up
		JC caswaitcont

		LXI H,castestaudiodonestr
		MVI A,0C0H        ; Set Cursor to bottom line
		CALL putins
		CALL putstr
		JMP prtdone       ; If timeout then skip cassette tests
		
caswaitcont:
		CALL ctrlbreak	
		JNZ caswaitrec
		
		DI             ; Disable interrupts to write out the header 
		MVI A,1FH      ; Prepare to disable RST 7.5 interrupt, Set bit 0
		SIM  
	
		MVI A,0C0H      ; Set Cursor to bottomline
		CALL putins
		LXI H,castestrecstartedstr
		CALL putstr

		CALL caswriteheader 

; ***************** Listening	
castestplayoption:
        CALL longdelay
	
waitforkeyrelease: 
		CALL scankeyboard
		JNZ waitforkeyrelease
	
;Play in cassette search for header 
		MVI A,0C0H      ; Set Cursor to bottomline
		CALL putins
		LXI H,castestplaystr
		CALL putstr		


caswaitplay:
		CALL shiftbreak
		JZ prtdone		
		CALL ctrlbreak
		JNZ caswaitplay

		MVI A,0C0H      ; Set Cursor to bottomline
		CALL putins
		LXI H,castestplaystartedstr
		CALL putstr
			
		CALL casreadhdrsync
		
		PUSH PSW
		CPI 07FH
		JNZ castestfail
			
		LXI H,castestaudiosyncstr
		JMP castestdone

castestfail:		
		LXI H,castestaudiosyncfailstr

castestdone:
		DI
		MVI A,1FH      ; Prepare to disable RST 7.5 interrupt, Set bit 0
		SIM  
				
		MVI A,0C0H      ; Set Cursor to bottom line
		CALL putins

		CALL putstr
		
		MVI A,0CBH      ; Set Cursor to mid bottom line
		CALL putins
		POP PSW
		CALL prthex

		CALL diagscreenpause ;Delay to allow review
		CALL diagscreenpause ;Delay to allow review
		JMP prtdone
		
; ==========================================================
; Write Header
; ==========================================================	
; ==========================================================
; Write cassette header and sync byte
; ==========================================================
caswriteheader: ;6F46H
		LXI B,0200H
caswriteloop:   ;6F49H
		MVI A,55H
		PUSH B
		CALL casswrite   ;6F5EH
		POP B
		DCX B
		MOV A,B
		ORA C
		JNZ caswriteloop ;6F49H
		MVI A,7FH
		JMP casswrite    ;6F5EH
		
; ======================================================
; Write char in A to cassette w/o checksum
; ======================================================
		CALL caswritebit81 ;6F71H
casswrite: ;6F5EH
		MVI B,08H
caswriteloop1: ;6F60H
		CALL caswritebit8 ;6F6AH Write bit 8 of A to cassette
		DCR B
		JNZ caswriteloop1  ;6F60H
		RET

; ======================================================
; Write bit 8 of A to cassette
; ======================================================
caswritebit8: ;6F6AH
		RLC
		LXI D,1F24H        ;Cassette frequency cycle count for 1 bit
		JC caswritebitbit0  ;6F74H
caswritebit81: ;6F71H
		LXI D,4349H        ; Cassette frequency cycle count for 0 bit
caswritebitbit0: ;6F74H
		DCR D              ; 
		JNZ caswritebitbit0 ;6F74H Wait Low
		MOV D,A        
		MVI A,0D0H
		SIM                 ; Write 1
caswritebitbit1: ;6F7CH
		DCR E
		JNZ caswritebitbit1 ;6F7CH Wait High
		MVI A,50H
		SIM                ; Write 0
		MOV A,D
		RET

; ==========================================================
; Read Header
; ==========================================================	
; ======================================================
; Read cassette header and sync byte
; ======================================================
casreadhdrsync: ;6F85H
		MVI B,80H        ; Load 128
casreadhdrsync1: ;6F87H
		CALL casreadportbit ;6FDBH Read Cassette port data bit
		RC
		MOV A,C
		CPI 08H
		JC casreadhdrsync   ;6F85H Read cassette header and sync byte
		CPI 40H
		JNC casreadhdrsync  ;6F85H Read cassette header and sync byte
		DCR B
		JNZ casreadhdrsync1 ;6F87H
caschecksb: ;6F9AH
;***		CALL 729FH     ; Check if SHIFT-BREAK is being pressed
;		RC
 MVI A,'+'
 STA lcddata
		CALL ctrlbreak
		RZ
		LXI H,0000H
		MVI B,40H
casreadhdrsync2: ;6FA3H
		CALL casgetnextbit ;7016H Get tape bit
		RC
		MOV D,C
		CALL casgetnextbit ;7016H Get tape bit
casreadhdrsync2a: ;6FABH
		RC
		MOV A,D
		SUB C
		JNC casreadhdrsync3 ;6FB3H
		CMA
		INR A
casreadhdrsync3: ;6FB3H
		CPI 0BH
		JC casreadhdrsync5a - 1 ;6FBAH
		INR H
		MVI A,2CH
casreadhdrsync5a:
		DCR B
		JNZ casreadhdrsync2 ;6FA3H
		MVI A,40H
		CMP L
		JZ casreadhdrsync4  ;6FC9H
		SUB H
		JNZ caschecksb      ;6F9AH
casreadhdrsync4: ;6FC9H
;6FC9H  (32H) STA FF8EH   
		CALL putcasstorage   ;Cassette port pulse control
	
		MVI D,00H
casreadhdrsync5: ;6FCEH
		CALL casreadportbit ;6FDBH Read Cassette port data bit
		RC
		CALL countandpackbits ;7023H Count and pack cassette input bits
		CPI 7FH
		JNZ casreadhdrsync5   ;6FCEH
		RET                   ;Return when sync’d
		
; ======================================================
; Read Cassette port data bit
; ======================================================
;Called here
casreadportbit: ;6FDBH
		MVI C,00H
;6FDDH  (3AH) LDA FF8EH    
		CALL getcasstorage   ;Cassette port pulse control
		ANA A
		JZ caswaitzerobit ;6FFAH Go heck for 1 0 1 transition
casreadport: ;6FE4H
;***		CALL 729FH      ; Check if SHIFT-BREAK is being pressed
;		RC
		CALL ctrlbreak
		RZ
		RIM
		RLC             ; Move MSB to carry to check SID
		JNC casreadport ;6FE4H If 0 wait to 1

casreadport1: ;6FEDH
;Called here
		INR C          ; C++
casreadport2: ;6FEEH
		INR C          ; C++
		JZ casreadport ;6FE4H If C=0 try again
		RIM            ; Check SID
		RLC            ; MSB to carry
		JC casreadport2 ;6FEEH If 1 wait to 0
		JMP caswaitnextbit1 ;700DH Once 0 play sound and exit

caswaitzerobit: ; 6FFAH
;***		CALL 729FH     ; Check if SHIFT-BREAK is being pressed
;		RC
		CALL ctrlbreak
		RZ
		RIM	    ; Read SID
		RLC	    ; MSB to carry
		JC caswaitzerobit ;6FFAH If 1 wait to 0
		
; Called here
caswaitnextbit: ;7003H
		INR C          ; C++ Get the timing on the bit
caswaitzerobit1: ;7004H
		INR C          ; C++
		JZ caswaitzerobit ;6FFAH C=0 wait again not a bit
		RIM            ; Check SID
		RLC	        ; MSB to carry
		JNC caswaitzerobit1 ;7004H      ; If 0 wait to 1

; Return here
caswaitnextbit1: ;700DH
;700DH  (3AH) LDA FF44H      ; Sound flag
;7010H  (A7H) ANA A          ; 
;7011H  (CCH) CZ 7676H       ; Click sound port
		XRA A          ; Clear A
		RET            ; Return C = bit width

casgetnextbit: ;7016H
		CALL caswaitnextbit ;7003H Ignore the next bit wait for 0 1 0 transition
		RC                 ; 
		MVI C,00H          ; C = 0
		CALL casreadport1   ;6FEDH Wait for bit next bit
		RC                 ; Check for ctr-break  
		JMP caswaitnextbit ;7003H      ; Get the bit time 

; ======================================================
; Count and pack cassette input bits
; ======================================================
countandpackbits: ;7023H
		MOV A,C
		CPI 15H
		MOV A,D
		RAL
		MOV D,A
		RET
		
; ======================================================
; Read character from cassette w/o checksum
; ======================================================
cascharread: ;702AH
		CALL casreadportbit      ;6FDBH Read Cassette port data bit
		RC
		MOV A,C
		CPI 15H
		JC cascharread        ;702AH Read character from cassette w/o checksum
		MVI B,08H
cascharread1: ;7036H
		CALL casreadportbit      ;6FDBH Read Cassette port data bit
		RC
		CALL countandpackbits ;7023H     ; Count and pack cassette input bits
		DCR B
		JNZ cascharread1      ;7036H
		XRA A
		RET
		
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
;End of test report to user	
		CALL clrscreen  ; Reset the screen for the new text
	
		LXI H,testcompletestr ; 11 chars
		CALL putstr		

		MVI A,0C0H      ; Set Cursor to back for new count
		CALL putins

		LXI H,versioninfostr
		CALL putstr

		CALL dumplogRS   ; Dump the RAM Test Log 

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
; Address calculation routines to deal with unknown RAM Locations
; **********************************************************
casstoragemem  .equ 12 ; Stored at a different location
countdownmem   .equ 14
strommem       .equ countdownmem + 4
rst75testmem   .equ strommem + 4
rxbuffermem    .equ rst75testmem + 4
rst55testmem   .equ rxbuffermem + 4
keyboardmem    .equ rst55testmem + 4
logdatastart   .equ keyboardmem + 4 ;Set up the logging area start
logmemptr      .equ logdatastart + 4
scrmemptr	   .equ logmemptr + 4
scrmemstartptr .equ scrmemptr + 4

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
; Relies on Stack not getting too large and on the 
; 00H boundary
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
; Retrieve Log memory start 16bit address
; ======================================================
readlogstartadd:
		PUSH PSW
		PUSH D
		CALL stackstart ; DE set to SP
		XCHG	        ; HL - DE swap, HL = SP
		MOV A,L         ; 
		ADI logdatastart; Add logdatastart offset to Stack start
		MOV L,A         ; HL points to logdatastart
		MOV E,M
		INX H
		MOV D,M
		XCHG            ; HL = Contents of logdatastart
		POP D
		POP PSW    
		RET
		
; ======================================================
; Retrieve Log memory ptr 16bit address
; ======================================================
readlogmemptr:
		PUSH PSW
		PUSH D
		CALL stackstart ; DE set to SP
		XCHG	        ; HL - DE swap, HL = SP
		MOV A,L         ; 
		ADI logmemptr  ; Add logmemptr offset to Stack start
		MOV L,A         ; HL points to logmemptr
		MOV E,M
		INX H
		MOV D,M
		XCHG            ; HL = Contents of logmemptr
		POP D		
		POP PSW    
		RET

; ======================================================
; Retrieve Screen memory start ptr 16bit address
; ======================================================
readscrstartptr:
		PUSH PSW
		PUSH D
		CALL stackstart ; DE set to SP
		XCHG	        ; HL - DE swap, HL = SP
		MOV A,L         ; 
		ADI scrmemstartptr  ; Add scrmemstartptr offset to Stack start
		MOV L,A         ; HL points to scrmemstartptr
		MOV E,M
		INX H
		MOV D,M
		XCHG            ; HL = Contents of scrmemstartptr
		POP D		
		POP PSW    
		RET
		
; ======================================================
; Retrieve Screen memory ptr 16bit address
; ======================================================
readscrmemptr:
		PUSH PSW
		PUSH D
		CALL stackstart ; DE set to SP
		XCHG	        ; HL - DE swap, HL = SP
		MOV A,L         ; 
		ADI scrmemptr   ; Add scrmemptr offset to Stack start
		MOV L,A         ; HL points to scrmemptr
		MOV E,M
		INX H
		MOV D,M
		XCHG            ; HL = Contents of scrmemptr
		POP D		
		POP PSW    
		RET
				
; ======================================================
; Store Log memory start 16bit address
;   HL Contains the data to be stored.
; ======================================================
savelogstartadd:
		PUSH PSW
		PUSH D
		CALL stackstart ; DE set to SP
		XCHG	        ; HL - DE swap, HL = SP
		MOV A,L         ; 
		ADI logdatastart; Add logdatastart offset to Stack start
		MOV L,A         ; HL points to logdatastart
		MOV M,E
		INX H
		MOV M,D
		POP D
		POP PSW    
		RET
		
; ======================================================
; Store Log memory ptr 16bit address
;   HL Contains the data to be stored.
; ======================================================
savelogmemptr:
		PUSH PSW
		PUSH D
		CALL stackstart ; DE set to SP
		XCHG	        ; HL - DE swap, HL = SP
		MOV A,L         ; 
		ADI logmemptr   ; Add logmemptr offset to Stack start
		MOV L,A         ; HL points to logmemptr
		MOV M,E
		INX H
		MOV M,D
		POP D	
		POP PSW    
		RET
	
; ======================================================
; Store Screen memory star ptr 16bit address
;   HL Contains the data to be stored.
; ======================================================
savescrstartptr:
		PUSH PSW
		PUSH D
		CALL stackstart    ; DE set to SP
		XCHG	           ; HL - DE swap, HL = SP
		MOV A,L            ; 
		ADI scrmemstartptr ; Add scrmemstartptr offset to Stack start
		MOV L,A            ; HL points to scrmemstartptr
		MOV M,E
		INX H
		MOV M,D
		POP D	
		POP PSW    
		RET	
	
; ======================================================
; Store Screen memory ptr 16bit address
;   HL Contains the data to be stored.
; ======================================================
savescrmemptr:
		PUSH PSW
		PUSH D
		CALL stackstart ; DE set to SP
		XCHG	        ; HL - DE swap, HL = SP
		MOV A,L         ; 
		ADI scrmemptr   ; Add scrmemptr offset to Stack start
		MOV L,A         ; HL points to scrmemptr
		MOV M,E
		INX H
		MOV M,D
		POP D	
		POP PSW    
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

; **********************************************************
; Diagnostic LCD basic routines
; **********************************************************
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

; Set up new clear screen in RAM Log
		CALL readlogmemptr
		CALL savescrmemptr  ; Update the screen ptr to start of screen
		CALL readlogmemptr
		CALL savescrstartptr; Update the screen start ptr
		LXI H,clrscreenMEM	
		CALL appendstrMEM   ; Fill Log with blank screen image
		RET
		
; ==========================================================
; PUT Char on Diagnostic LCD. A contains character
; ==========================================================
putch: 
		PUSH B           ; Save BC
		PUSH H
		PUSH D
		
		STA lcddata    	 ; Send char to LCD		
		MVI C,16         ; Counter 16 	
		CALL shortdelay

		CALL readscrmemptr ; Load Mem Screen Ptr
		MOV M,A
		INX H
		CALL savescrmemptr ; Save Mem Screen Ptr

		POP D
		POP H
		POP B	
		RET
		
; ==========================================================
; PUT instruction to Diagnostic LCD.  A contains instruction
; ==========================================================
putins:
		PUSH B           ; Save BC
		PUSH H
		PUSH D
		
		STA lcdins    	 ; Send ins to LCD		
		MVI C,16         ; Counter 16 	
		CALL shortdelay

; Deal with screen ptr moving 08xH, 0CxH
		MOV B,A
		ANI 0F0H
		CPI 080H     ; Check for top line 
		JNZ bottomline
		MOV A,B
		ANI 00FH
		LXI B,0
		MOV C,A
		CALL readscrstartptr
		DAD B
		CALL savescrmemptr
		JMP putinsend
		
bottomline:
		MOV A,B
		ANI 0F0H
		CPI 0C0H     ; Check for bottom line
		JNZ putinsend
		MOV A,B
		ANI 00FH
		ADI 011H
		LXI B,0
		MOV C,A
		CALL readscrstartptr
		DAD B
		CALL savescrmemptr

putinsend:
		POP D
		POP H
		POP B			
		RET	

; ==========================================================
; PUT String on Diagnostic LCD HL points to string
; ==========================================================
putstr:
		PUSH PSW
		PUSH H
putsloop:
		MOV A,M   	     ; H = Current Character
		CPI 0H           ; Check end of table
		JZ putstrexit    
		 
		STA lcddata    	 ; Send char to LCD		
		MVI C,16         ; Counter 16 	
		CALL shortdelay
		  
		INX H            ; Get next Char
		JMP putsloop
		
putstrexit:

; Put same text to memory log
		CALL readscrmemptr ; Load Mem Screen Ptr
		XCHG
		POP H		
		CALL putstrMEM ; HL=Str, DE=RAM ptr
		XCHG
		CALL savescrmemptr ; Save Mem Screen Ptr
		
		POP PSW
		RET

; ==========================================================
; Print A as 2 HEX digits to diagnostic Screen
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
; Diagnostic Screen Text Strings
; ==========================================================
#include "DiagStrings.asm"	

; **********************************************************
; Delays used in timing of tests
; **********************************************************
; ==========================================================
; Short delay.  C = loop counter
; ==========================================================
shortdelay: ;7657H
		DCR C           ; Decrement C: 4
		JNZ shortdelay  ; Loop until C = 0: 7 = 11 = 4.45us
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
; OPTROM IDs used when testing OPTROM to determine installed
; ROM.  Not all ROMs are included and extras can be easily
; added to the list.  
; ==========================================================
optromidtable:
	.db 0C0H,000H ; Empty Socket
	.text "NULL  "
	.db 0
	.db 019H,032H ; UR II
	.text "UR2   "
	.db 0	
	.db 074H,01AH ; Clueseau
	.text "ROM2  "
	.db 0
	.db 03CH,009H ; Sardine
	.text "SAR10 "
	.db 0
	.db 040H,07CH ; 
	.text "SUP10 "
	.db 0	
	.db 044H,038H ; TSDOS
	.text "TSD10 "
	.db 0	
endoptromidtable .equ $

; **********************************************************
;  Serial port routines used to dump Test Log and OPTROMs.
; **********************************************************
; ==========================================================
; Init serial port
; ==========================================================
initRS:
; Configure the serial port, switch to RS232
		MVI A,024H       ; PIO B configuration (RTS low, DTR low, SPKR=1, Serial=RS232, Keyscan col 9 enable)
		OUT PIOB         ; Set PIO chip port B configuration	

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
putchRS:
		PUSH PSW      ; Save A
putchwaitRS:
		IN UARTmode    ; D8H	
		ANI 010H       ; TX Buffer empty - High buffer empty
		JZ putchwaitRS ; Wait for TX buffer to empty		
		POP PSW        ; Restore A
		OUT UARTsend   ; C8H
		RET	

; ==========================================================
; Put string to serial port HL points to string
; ==========================================================
putstrRS:
		PUSH PSW
putsloopRS:
		MOV A,M   	     ; H = Current Character
		CPI 0H           ; Check end of table
		JZ putstrRSexit       
		
		CALL putchRS     ; Write to Serial Port
		INX H            ; Get next Char
		JMP putsloopRS
		
putstrRSexit:
		POP PSW
		RET

; ======================================================
; Dump memory (HL) size of B to Serial port in Binary
; ======================================================
dumpmemRS:
		PUSH PSW
		PUSH B
		PUSH H
dumploopRS:
		MOV A,M
		CALL putchRS     ; Write to Serial Port
		INX H
		DCR B
		JNZ dumploopRS

		POP H
		POP B
		POP PSW
		RET
		
; **********************************************************
;  Routines used to create the RAM test log.
; **********************************************************		
; ==========================================================
; Append Char in A to to RAM Test Log
; ==========================================================
appendchMEM:
		PUSH H
		CALL readlogmemptr     ; Get the END start address
		MOV A,M
		INX H
		CALL savelogmemptr     ; Save the new END start address
		POP H
		RET
		
; ==========================================================
; Append String to RAM Test Log,  HL points to string
; ==========================================================
appendstrMEM:
		PUSH PSW
		PUSH D
		XCHG                 ; Swap HL & DE
		CALL readlogmemptr   ; Get the END log address
		XCHG				 ; Swap HL & DE, HL = String, DE = RAM
		
		CALL putstrMEM
		
		XCHG 
		CALL savelogmemptr   ; Save the new END log address
	
		POP D
		POP PSW
		RET
				
; ==========================================================
; PUT String into RAM Test Log memory file
;  DE points to RAM file
;  HL points to String
; ==========================================================
putstrMEM:
		PUSH PSW
putsloopMEM:
		MOV A,M   	     ; H = Current Character
		CPI 0H           ; Check end of table
		JZ putstrexitMEM       
		STAX D           ; Write to memory
		INX H            ; Get next Char
		INX D            ; Next mem location
		JMP putsloopMEM

		;Add on LF,CR to end of Strings
;		MVI A,0AH
;		STAX D
;		INX D
;		MVI A,0DH
;		STAX D
;		INX D
				
putstrexitMEM:
		POP PSW
		RET
				
; ======================================================
; Dump RAM Test log to Serial port
; ======================================================
dumplogRS:
		PUSH PSW
		PUSH D
		PUSH H
		
		CALL initRS    ; Init RS232 PORT

		; Provide a newline for the Log print out
		MVI A,0AH
		CALL putchRS      ; Write to Serial Port
		MVI A,0DH
		CALL putchRS      ; Write to Serial Port
		MVI A,0AH
		CALL putchRS      ; Write to Serial Port
		MVI A,0DH
		CALL putchRS      ; Write to Serial Port
		
; Get start address for the RAM log
		CALL readlogstartadd  ; Get the LOG start address
		XCHG                  ; Swap HL to DE
				
; Get end address for the RAM log
		CALL readlogmemptr     ; Get the END start address
				
dumplogloopRS:
		LDAX D            ; Get byte from LOG
		CALL putchRS      ; Write to Serial Port
		INX D             ; Point to next byte
		
		MOV A,L           ; See if the end is reached
		SUB E
		JNZ dumplogloopRS 	  ; L = E 	
		MOV A,H
		SUB D
		JNZ dumplogloopRS     ; H = D

		POP H
		POP D
		POP PSW
		RET
		
; ==========================================================
; Diagnostic Serial Strings	
; ==========================================================		
romdumpstart:
	.text "Start ROM dump \r\n>"
	.db 0
romdumpend:
	.text "<\r\n End ROM dump \r\n"
	.db 0
clrscreenMEM:
	.text "                                 \r\n"
;	.text "?????????????????????????????????\r\n"
	.db 0
	
;***********************************************************
; ==========================================================
; Generate the 16-bit summation of the test ROM
; Used as part of the OPTROM test routines	
; ==========================================================		

dumpromtest:
		LXI H, 0  ; Start of ROM
dumpromlooptest:
		MOV A,M   ; Get byte
		CALL putchRS
		INX  H    ; Point to next byte
		MOV A,H   ; Test count for 0: 4
		SBI 080H  ;
		ORA L     ; Test lower byte: 4
        JNZ dumpromlooptest  ; If ~0 continue 
		RET
		
; ==========================================================
; Main LCD Screen Strings for MT100/102
; 	The following strings are used for the LCD test
; ==========================================================
tstline0:
		.text "Model 100 Test ROM Version "
		VERSION
		.text " Feb 2021*"
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

; **********************************************************
; External ROM detect image loaded at F605H
; **********************************************************
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
	.db 011H,04BH,091H  ;LXI D,optram3open ; FAA4H       
	.db 07EH            ;MOV A,M
	.db 012H            ;STAX D
	.db 023H            ;INX H
	.db 013H       	    ;INX D
	.db 07DH            ;MOV A,L
	.db 0D6H,48H        ;SUI 48H
	.db 0C2H,00AH,091H  ;JNZ optram3loop
	
                       ;Calculate Checksum over complete OPTROM
	.db 021H,000H,000H ;LXI H, 0  ; Start of ROM
	.db 001H,000H,000H ;LXI B, 0  ; Counter
	.db 011H,000H,000H ;LXI D, 0  ; storage
            ;optram3checksum:
	.db 04EH           ;MOV C,M   ; Get byte
	.db 0EBH           ;XCHG      ; Swap HL and DE
	.db 009H           ;DAD B     ; Add to form 16bit sum
	.db 0EBH           ;XCHG      ; Swap HL and DE
	.db 023H           ;INX  H    ; Point to next byte
	.db 07CH           ;MOV A,H   ; Test count for 0: 4
	.db 0DEH,080H      ;SBI 080H  ; Check for count to 080H
	.db 0B5H		   ;ORA L     ; Test lower byte: 4
	.db 0C2H,01DH,091H ;JNZ optram3checksum  ; If ~0 continue 
	.db 0EBH           ;XCHG
	.db 022H,053H,091H ;SHLD checksum3 ; Store the checksum 
	.db 03EH,008H      ;MVI A,008H
	.db 0D3H,0E8H      ;OUT 0E8H;  ; Return to main ROM	
         ;Calculate Checksum over complete ROM 0-07FFFH
	.db 021H,00H,00H  ;LXI H, 0  ; Start of ROM
	.db 001H,00H,00H  ;LXI B, 0  ; Counter
	.db 011H,00H,00H  ;LXI D, 0  ; storage
					;optram3tchecksum:
	.db 04EH          ;MOV C,M   ; Get byte
	.db 0EBH          ;XCHG      ; Swap HL and DE
	.db 009H          ;DAD B     ; Add to form 16bit sum
	.db 0EBH          ;XCHG      ; Swap HL and DE
	.db 023H          ;INX  H    ; Point to next byte
	.db 07CH          ;MOV A,H   ; Test count for 0: 4
	.db 0DEH,080H     ;SBI 080H  ;
	.db 0B5H          ;ORA L     ; Test lower byte: 4
	.db 0C2H,03AH,091H;JNZ optram3tchecksum  ; If ~0 continue 			
	.db 0EBH          ;XCHG
	.db 022H,055H,091H;SHLD checksum3t
	.db 0C9H          ;RET
	.db 000H,000H,000H,000H,000H,000H,000H,000H
	.dw 0000H
	.dw 0000H

;getrex3
	.db 03EH,09H        ;MVI A,09H
	.db 0D3H,0E8H       ;OUT E8H
;getrex3loop:
	.db 021H,004H,000H  ;LXI H,0004H
	.db 011H,070H,091H  ;LXI D,rexstore3 ;        
	.db 07EH            ;MOV A,M
	.db 012H            ;STAX D
	.db 023H            ;INX H
	.db 013H       		;INX D
	.db 07DH            ;MOV A,L
	.db 0D6H,008H       ;SUI 8H
	.db 0C2H,061H,091H  ;JNZ getrex3loop             		
	.db 03EH,008H       ;MVI A,008H
	.db 0D3H,0E8H       ;OUT 0E8H;  ; Return to main ROM	
	.db 0C9H            ;RET
;rexstore3:
	.db 00H,00H,00H,00H
	.text "Test Log Data\r\nRAM 0 PASS\r\nRAM 1 PASS\r\nRAM 2 PASS\r\nRAM 3 PASS\r\n"
		
; ======================================================
; RAM2 OPTROM Test Code 
; ======================================================
optram2load:
	.db 03EH,009H       ;MVI A,01H
	.db 0D3H,0E8H       ;OUT E8H
	.db 021H,040H,000H  ;LXI H,0040H
	.db 011H,04BH,0B1H  ;LXI D,optram2open ; FAA4H       
	.db 07EH            ;MOV A,M
	.db 012H            ;STAX D
	.db 023H            ;INX H
	.db 013H       	    ;INX D
	.db 07DH            ;MOV A,L
	.db 0D6H,48H        ;SUI 48H
	.db 0C2H,0AH,0B1H   ;JNZ optram2loop
	
                        ;Calculate Checksum over complete OPTROM
	.db 021H,000H,000H ;LXI H, 0  ; Start of ROM
	.db 001H,000H,000H ;LXI B, 0  ; Counter
	.db 011H,000H,000H ;LXI D, 0  ; storage
            ;optram2checksum:
	.db 04EH           ;MOV C,M   ; Get byte
	.db 0EBH           ;XCHG      ; Swap HL and DE
	.db 009H           ;DAD B     ; Add to form 16bit sum
	.db 0EBH           ;XCHG      ; Swap HL and DE
	.db 023H           ;INX  H    ; Point to next byte
	.db 07CH           ;MOV A,H   ; Test count for 0: 4
	.db 0DEH,080H      ;SBI 080H  ; Check for count to 080H
	.db 0B5H		   ;ORA L     ; Test lower byte: 4
	.db 0C2H,01DH,0B1H ;JNZ optram2checksum  ; If ~0 continue 
	.db 0EBH           ;XCHG
	.db 022H,053H,0B1H ;SHLD checksum2 ; Store the checksum 
	.db 03EH,008H      ;MVI A,008H
	.db 0D3H,0E8H      ;OUT 0E8H;  ; Return to main ROM	
         ;Calculate Checksum over complete ROM 0-07FFFH
	.db 021H,00H,00H  ;LXI H, 0  ; Start of ROM
	.db 001H,00H,00H  ;LXI B, 0  ; Counter
	.db 011H,00H,00H  ;LXI D, 0  ; storage
					;optram2tchecksum:
	.db 04EH          ;MOV C,M   ; Get byte
	.db 0EBH          ;XCHG      ; Swap HL and DE
	.db 009H          ;DAD B     ; Add to form 16bit sum
	.db 0EBH          ;XCHG      ; Swap HL and DE
	.db 023H          ;INX  H    ; Point to next byte
	.db 07CH          ;MOV A,H   ; Test count for 0: 4
	.db 0DEH,080H     ;SBI 080H  ;
	.db 0B5H          ;ORA L     ; Test lower byte: 4
	.db 0C2H,03AH,0B1H;JNZ optram2tchecksum  ; If ~0 continue 			
	.db 0EBH          ;XCHG
	.db 022H,055H,0B1H;SHLD checksum2t
	.db 0C9H          ;RET
	.db 000H,000H,000H,000H,000H,000H,000H,000H
	.dw 0000H
	.dw 0000H
	
;getrex2
	.db 03EH,09H        ;MVI A,09H
	.db 0D3H,0E8H       ;OUT E8H
;getrex2loop:
	.db 021H,004H,000H  ;LXI H,0004H
	.db 011H,070H,0B1H  ;LXI D,rexstore2 ;        
	.db 07EH            ;MOV A,M
	.db 012H            ;STAX D
	.db 023H            ;INX H
	.db 013H       		;INX D
	.db 07DH            ;MOV A,L
	.db 0D6H,08H        ;SUI 8H
	.db 0C2H,61H,0B1H  ;JNZ getre2loop             		
	.db 03EH,008H      ;MVI A,008H
	.db 0D3H,0E8H      ;OUT 0E8H;  ; Return to main ROM	
	.db 0C9H          ;RET
;rexstore2:
	.db 00H,00H,00H,00H
	.text "Test Log Data\r\nRAM 0 PASS\r\nRAM 1 PASS\r\nRAM 2 PASS\r\nRAM 3 PASS\r\n"
	
; ======================================================
; RAM1 OPTROM Test Code 
; ======================================================
optram1load:
	.db 03EH,009H       ;MVI A,01H
	.db 0D3H,0E8H       ;OUT E8H
	.db 021H,040H,000H  ;LXI H,0040H
	.db 011H,04BH,0D1H  ;LXI D,optram1open ; FAA4H       
	.db 07EH            ;MOV A,M
	.db 012H            ;STAX D
	.db 023H            ;INX H
	.db 013H       		;INX D
	.db 07DH            ;MOV A,L
	.db 0D6H,48H        ;SUI 48H
	.db 0C2H,0AH,0D1H   ;JNZ optram1loop
	
                       ;Calculate Checksum over complete OPTROM
	.db 021H,000H,000H ;LXI H, 0  ; Start of ROM
	.db 001H,000H,000H ;LXI B, 0  ; Counter
	.db 011H,000H,000H ;LXI D, 0  ; storage
            ;optram1checksum:
	.db 04EH           ;MOV C,M   ; Get byte
	.db 0EBH           ;XCHG      ; Swap HL and DE
	.db 009H           ;DAD B     ; Add to form 16bit sum
	.db 0EBH           ;XCHG      ; Swap HL and DE
	.db 023H           ;INX  H    ; Point to next byte
	.db 07CH           ;MOV A,H   ; Test count for 0: 4
	.db 0DEH,080H      ;SBI 080H  ; Check for count to 080H
	.db 0B5H		   ;ORA L     ; Test lower byte: 4
	.db 0C2H,01DH,0D1H ;JNZ optram1checksum  ; If ~0 continue 
	.db 0EBH           ;XCHG
	.db 022H,053H,0D1H ;SHLD checksum1 ; Store the checksum 
	.db 03EH,008H      ;MVI A,008H
	.db 0D3H,0E8H      ;OUT 0E8H;  ; Return to main ROM	
         ;Calculate Checksum over complete ROM 0-07FFFH
	.db 021H,00H,00H  ;LXI H, 0  ; Start of ROM
	.db 001H,00H,00H  ;LXI B, 0  ; Counter
	.db 011H,00H,00H  ;LXI D, 0  ; storage
					;optram1tchecksum:
	.db 04EH          ;MOV C,M   ; Get byte
	.db 0EBH          ;XCHG      ; Swap HL and DE
	.db 009H          ;DAD B     ; Add to form 16bit sum
	.db 0EBH          ;XCHG      ; Swap HL and DE
	.db 023H          ;INX  H    ; Point to next byte
	.db 07CH          ;MOV A,H   ; Test count for 0: 4
	.db 0DEH,080H     ;SBI 080H  ;
	.db 0B5H          ;ORA L     ; Test lower byte: 4
	.db 0C2H,03AH,0D1H;JNZ optram1tchecksum  ; If ~0 continue 			
	.db 0EBH          ;XCHG
	.db 022H,055H,0D1H;SHLD checksum1t
	.db 0C9H          ;RET
	.db 000H,000H,000H,000H,000H,000H,000H,000H
	.dw 0000H
	.dw 0000H

;getrex1
	.db 03EH,09H        ;MVI A,09H
	.db 0D3H,0E8H       ;OUT E8H
;getrex1loop:
	.db 021H,004H,000H  ;LXI H,0004H
	.db 011H,070H,0D1H  ;LXI D,rexstore1 ;        
	.db 07EH            ;MOV A,M
	.db 012H            ;STAX D
	.db 023H            ;INX H
	.db 013H       		;INX D
	.db 07DH            ;MOV A,L
	.db 0D6H,08H        ;SUI 8H
	.db 0C2H,61H,0D1H  ;JNZ getrex1loop             		
	.db 03EH,008H      ;MVI A,008H
	.db 0D3H,0E8H      ;OUT 0E8H;  ; Return to main ROM	
	.db 0C9H          ;RET
;rexstore1:
	.db 00H,00H,00H,00H
	.text "Test Log Data\r\nRAM 0 PASS\r\nRAM 1 PASS\r\nRAM 2 PASS\r\nRAM 3 PASS\r\n"
	
; ======================================================
; RAM0 OPTROM Test Code 
; ======================================================
optram0load:
	.db 03EH,09H        ;MVI A,09H
	.db 0D3H,0E8H       ;OUT E8H
	.db 021H,040H,000H  ;LXI H,0040H
	.db 011H,04BH,0F1H  ;LXI D,optram0open ; FAA4H       
	.db 07EH            ;MOV A,M
	.db 012H            ;STAX D
	.db 023H            ;INX H
	.db 013H       		;INX D
	.db 07DH            ;MOV A,L
	.db 0D6H,48H        ;SUI 48H
	.db 0C2H,0AH,0F1H  ;JNZ optram0loop             		
                       ;Calculate Checksum over complete OPTROM
	.db 021H,000H,000H ;LXI H, 0  ; Start of ROM
	.db 001H,000H,000H ;LXI B, 0  ; Counter
	.db 011H,000H,000H ;LXI D, 0  ; storage
            ;optram0checksum:
	.db 04EH           ;MOV C,M   ; Get byte
	.db 0EBH           ;XCHG      ; Swap HL and DE
	.db 009H           ;DAD B     ; Add to form 16bit sum
	.db 0EBH           ;XCHG      ; Swap HL and DE
	.db 023H           ;INX  H    ; Point to next byte
	.db 07CH           ;MOV A,H   ; Test count for 0: 4
	.db 0DEH,080H      ;SBI 080H  ; Check for count to 080H
	.db 0B5H		   ;ORA L     ; Test lower byte: 4
	.db 0C2H,01DH,0F1H ;JNZ optram0checksum  ; If ~0 continue 
	.db 0EBH           ;XCHG
	.db 022H,053H,0F1H ;SHLD checksum0 ; Store the checksum 
	.db 03EH,008H      ;MVI A,008H
	.db 0D3H,0E8H      ;OUT 0E8H;  ; Return to main ROM	
         ;Calculate Checksum over complete ROM 0-07FFFH
	.db 021H,00H,00H  ;LXI H, 0  ; Start of ROM
	.db 001H,00H,00H  ;LXI B, 0  ; Counter
	.db 011H,00H,00H  ;LXI D, 0  ; storage
					;optram0tchecksum:
	.db 04EH          ;MOV C,M   ; Get byte
	.db 0EBH          ;XCHG      ; Swap HL and DE
	.db 009H          ;DAD B     ; Add to form 16bit sum
	.db 0EBH          ;XCHG      ; Swap HL and DE
	.db 023H          ;INX  H    ; Point to next byte
	.db 07CH          ;MOV A,H   ; Test count for 0: 4
	.db 0DEH,080H     ;SBI 080H  ;
	.db 0B5H          ;ORA L     ; Test lower byte: 4
	.db 0C2H,03AH,0F1H;JNZ optram0tchecksum  ; If ~0 continue 			
	.db 0EBH          ;XCHG
	.db 022H,055H,0F1H;SHLD checksum0t
	.db 0C9H          ;RET
	.db 000H,000H,000H,000H,000H,000H,000H,000H
	.dw 0000H
	.dw 0000H
	
;getrex0
	.db 03EH,09H        ;MVI A,09H
	.db 0D3H,0E8H       ;OUT E8H
;getrex0loop:
	.db 021H,004H,000H  ;LXI H,0004H
	.db 011H,070H,0F1H  ;LXI D,rexstore0 ;        
	.db 07EH            ;MOV A,M
	.db 012H            ;STAX D
	.db 023H            ;INX H
	.db 013H       		;INX D
	.db 07DH            ;MOV A,L
	.db 0D6H,08H        ;SUI 8H
	.db 0C2H,061H,0F1H  ;JNZ getrex0loop             		
	.db 03EH,008H      ;MVI A,008H
	.db 0D3H,0E8H      ;OUT 0E8H;  ; Return to main ROM	
	.db 0C9H          ;RET
;rexstore0:
	.db 00H,00H,00H,00H
	.text "Test Log Data\r\nRAM 0 PASS\r\nRAM 1 PASS\r\nRAM 2 PASS\r\nRAM 3 PASS\r\n"

; ======================================================
; Activate DUMP OPTROM Debug Code 
; Easter egg code to dump ROM to serial port.
; Call in the RAM check routines.
; ======================================================
dumpromrun:
		LXI H,dumpromload
		LXI D,dumprom		
		MVI B,sizerom
		CALL movemem    ; B bytes to (DE) from (HL)
		CALL clrscreen
		LXI H,dumpromstr
		CALL putstr
		
		CALL initRS    ; Init RS232 PORT
		
		LXI H,romdumpstart
		CALL putstrRS
	
		CALL dumprom

		LXI H,romdumpend
		CALL putstrRS
		
 		MVI A,0C0H     ; Set Cursor to back to start
		CALL putins
		
		LXI H,dumpromstrend
		CALL putstr
dumpromfin:
		JMP dumpromfin
			
; ======================================================
; DUMP OPTROM Debug Code for RAM
; ======================================================
dumpromload:
;dumprom:
	.db 03EH,009H        ;MVI A,09H;  ; Switch to OPTROM
	.db 0D3H,0E8H        ;OUT 0E8H; 	
            		
	.db 000H,000H,000H   ;NOP
             		
;Calculate Checksum over complete OPTROM 0-07FFFH
	.db 021H,000H,000H   ;LXI H, 0  ; Start of ROM
;dumpromloop:
	.db 07EH          	 ;MOV A,M   ; Get byte
	.db 0CDH,030H,0F2H   ;CALL putchserport
	.db 023H          	 ;INX  H    ; Point to next byte
	.db 07CH          	 ;MOV A,H   ; Test count for 0: 4
	.db 0DEH,080H        ;SBI 080H  ;
	.db 0B5H             ;ORA L     ; Test lower byte: 4
	.db 0C2H,00AH,0F2H   ;JNZ dumpromloop  ; If ~0 continue 
             
	.db 03EH,008H       		 ;MVI A,08H
	.db 0D3H,0E8H        ;OUT 0E8H;  ; Return to main ROM	
	.db 0C9H          	 ;RET
;initserport:
; Configure the serial port, switch to RS232
	.db 03EH,024H        ;MVI A,024H    ; PIO B configuration (RTS hi, DTR hi, SPKR=1, Serial=RS232, Keyscan col 9 enable)
	.db 0D3H,0B2H        ;OUT PIOB      ; Set PIO chip port B configuration	
             
; Set baud rate 19200 using PIO Timer	
	.db 03EH,008H        ;MVI A,008H
	.db 0D3H,0B4H        ;OUT PIOT1     ; BCH Timer 0
	.db 03EH,040H        ;MVI A,040H
	.db 0D3H,0B5H        ;OUT PIOT2	  ; BDH Timer 1
	.db 03EH,0C3H        ;MVI A,0C3H
	.db 0D3H,0B0H        ;OUT PIOCR	  ; B8H Control Reg
             		
; Configure UART Chip
; B11100: 8bits + No Parity + 1 stop
	.db 03EH,01CH        ;MVI A,01CH
	.db 0D3H,0D8H        ;OUT UARTmode  ; D8H
	.db 0C9H          	 ;RET
          		
; Put Char in A to serial port
;putchserport:
	.db 0F5H             ;PUSH PSW       ; Save A
;putchserportwait:
	.db 0DBH,0D8H        ;IN UARTmode    ; D8H	
	.db 0E6H,010H        ;ANI 010H       ; TX Buffer empty - High buffer empty
	.db 0CAH,031H,0F2H   ;JZ putchserportwait ; Wait for TX buffer to empty		
	.db 0F1H             ;POP PSW        ; Restore A
	.db 0D3H,0C8H        ;OUT UARTsend   ; C8H
	.db 0C9H             ;RET	


; ======================================================
; RAM Code include
; ======================================================
#include "RAMdefs.asm"

	.end