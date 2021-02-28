@echo off
REM Tell TASM where to find the correct table to use
REM We are using the 8085 table extended with undocumented opcodes
set TASMTABS=..\..\TASM\tasmTab\
REM Move up to the TASM 'install' directory 
REM call TASM with build args and source file name
@echo on
..\..\TASM\tasm32\tasm.exe -g0 -85 romtstV32tl.8085.asm
@echo off
del *.hex
ren *.obj *.hex
