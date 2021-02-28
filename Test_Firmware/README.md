# TRS-80-M100 Test Harness Firmware
Various versions of the test harness firmware. Firmware written in assembler
using TASM. See https://github.com/Jeff-Birt/TASM_vsCode_Extension for information and extension for using TASM with VS Code.

Folders ("Official" firmware by test harness creator)
romtstv25 - Firmware V2.5 corresponds to manual version 1.0
romtstv32 - Firmware V3.2 corresponds to manual version 3.2
romtstv33 - Firmware V3.3 corresponds to manual version 1.0 (improved optrom test)

Folders ("Unofficial" firmware modified by myself )
romtstv32rl - Continious RAM test. Stops on any RAM failure. Based on V3.2
romtstv32tl - Continous full test up to power off. Does not stop on failure. Based on V3.2
