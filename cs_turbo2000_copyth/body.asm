;*******************************************************************************
;COPY T2/H (C) 1.0 2021 BAKTRA Software
;Mainline code. Assemble with MADS.
;
;This utility automatically copies Czechoslovak Turbo 2000 files to the
;emulator's H1: device. The utility is designed to be used under emulator, not
;the real machine. It was created to ease the process of decoding Turbo 2000
;files from .WAVE files. For each input file, there are two outputs: 1. binary
;load file, 2. tape image.
;
;Emulation of an XL/XE system is required, because the copier uses
;'RAM under ROM' in order to provide a decently sized buffer for input files.
;
;Operation:
;
; Wait for START key
; 1. Load Turbo 2000 header to memory
; 2. Store turbo header in human readable form
; 3. Load rest of the Turbo 2000 file to the memory
; 4. Write loaded file to the H1: device as .xex  (for convenience)
; 5. Write loadef file to the H1: device as .cas  (for multi-part files)
; 6. Flush line to the H1:LISTING.TXT file
; 7. Go to 1
;
; The process can be interrupted by pressing RESET. The copier simply restarts.
;
; The code of the copier is split in two sections. 
; 1st section begins at $0400
; 2nd section begins at $CC00 - in RAM under ROM
;
; Supported emulators
;
; atari800 with modified a8cas
; http://www.arus.net.pl/FUJI/a8cas-util/downloads/
; modified-atari800-emulator.html
;
; atari800 with a8cas 
; http://a8cas.sourceforge.net/features.html#patch
;
; Altirra
; https://www.virtualdub.org/altirra.html
; Note: Use version 4.10-test2 and newer. This version supports long
;       file names for the H1: device.
;
; Maintenance log
; Version 1.1 - Support for 11-character file names, the 11th character
;               is a simple numerical iterator (0-9), which reduces the risk
;               of overwriting files with identical names.
;*******************************************************************************
            ICL 'equates.asm'
            
;-------------------------------------------------------------------------------
; Local equates and definitions
;-------------------------------------------------------------------------------            
            
            COPY_START EQU $0400
            COPY_HIGH  EQU $CC00
            
            ZP_SRCL    EQU 128
            ZP_SRCH    EQU 129
            ZP_TGTL    EQU 130
            ZP_TGTH    EQU 131
            
            MI_START  EQU 0
            MI_HIO    EQU 1
            
            
            HFNAME_LEN  EQU FNAME_LEN
            BUFFER_LEN  EQU 49151-MAIN_BUFFER+1
;-------------------------------------------------------------------------------
; Mainline code
;-------------------------------------------------------------------------------
            ORG COPY_START

;Initialize the copier
L05D2       jsr CASINIT       ;Setup DOS vectors   
            jsr SETSCREEN     ;Setup the screen
                
            lda #MI_START
            jsr DISP_MSG      ;Display the start message
            
            jsr WAIT_START
            jsr MSG_CLEAR
            
            lda #$08          ;Reset listing file mode
            sta PL_WMODE      ;Set it to plain write

;-------------------------------------------------------------------------------
; Decode header
;-------------------------------------------------------------------------------
L05D7       lda #<THEADER     ;Setup address where turbo header will be placed
            sta BUFRLO
            lda #>THEADER
            sta BUFRHI
            
            lda #<TH_END
            sta BFENLO
            lda #>TH_END
            sta BFENHI
            
            lda #0            ;First byte of the header should be 0
            jsr L0631         ;Call Turbo 2000 block decoding subroutine
            bcc L05D7         ;If error occured, try to decode the header again
            
            lda ICAX6Z        ;Keep checksum
            sta TH_CHKSUM
            
;-------------------------------------------------------------------------------
; Prepare for listing
;-------------------------------------------------------------------------------
;Open the listing file
           jsr OPEN_LISTING
           
;Place file name               ;Always 10 characters
           ldx #10
@          lda TH_NAME-1,x
           sta PL_NAME-1,x
           dex
           bne @-
           
;Place return code, assume 'E' until proven otherwise           
           lda #'E'                     
           sta PL_RC
           
;Place file type
           lda TH_TYPE
           cmp #3
           bne @+
           lda #'A'
           sta PL_TYPE
           jmp TYPE_DONE
           
@          lda #'!'
           sta PL_TYPE 
                      
TYPE_DONE
;-------------------------------------------------------------------------------
;Check if the file can be decoded
;-------------------------------------------------------------------------------           
;Check length of the file, if it exceeds maximum, set RC='L' and skip file           
           
           lda TH_LEN+1            ;Check HI byte
           cmp #>BUFFER_LEN        ;Is lower?
           bcc CHK_LEN_OK          ;Yes, length is fine
           lda TH_LEN              ;Check LO byte
           cmp #<BUFFER_LEN        ;Is lower?
           bcc CHK_LEN_OK          ;Yes, length is fine
;
           lda #'L'                ;File too long for the copier. RC='L'
           sta PL_RC
           jsr PLINE_FLUSH         ;Flush the line to the listing
           jmp L05D7               ;Try another file
 
CHK_LEN_OK 
;------------------------------------------------------------------------------- 
;Decode file data
;-------------------------------------------------------------------------------
            lda #<MAIN_BUFFER  ;Setup address where main block will be placed
            sta BUFRLO
            clc
            adc TH_LEN
            sta BFENLO
            lda #>MAIN_BUFFER
            sta BUFRHI
            adc TH_LEN+1
            sta BFENHI

            lda #255          ;First byte should be 255
            jsr L0631         ;Call Turbo 2000 block decoding subroutine
            bcs L0622         ;No error - jump
            
ERRDATA     jsr PLINE_FLUSH   ;Print header line
            jmp L05D7         ;And then try to load another file
                        

;Decoding ok            
L0622       lda ICAX6Z        ;Preserve data block check sum
            sta CHKSUM_D
            lda #$4F          ;Set return code to 'O' as OK
            sta PL_RC              
            jsr PLINE_FLUSH
            
            
;Now store the file both as binary file and as tape image
            ldy #0
            jsr MAKE_FNAME            
            jsr STORE_BINARY
            ldy #1
            jsr MAKE_FNAME
            jsr STORE_TAPEIMAGE
            
;And when done with all this, go and load next file            
            jmp L05D7
          
                      
;-------------------------------------------------------------------------------
; Data area for saving to the H1: device
;-------------------------------------------------------------------------------
HDEV_DEV    dta c'H1:'
HDEV_FILEN  
            .REPT HFNAME_LEN
            dta c' '
            .ENDR
HDEV_FDOT   dta c'.'
HDEV_FEXT   dta c'   '
HDEV_EOL    dta $9B

HDEV_XEX     dta c'XEX'
HDEV_CAS     dta c'CAS'
     
;===============================================================================
;Block decoding subroutine
;Block is placed from: BUFRLO+256*BUFRHI
;                to:   (BFENLO+256*BFENHI)-1
;Register usage
;Input:
;  A - Identification byte - first byte of the block that is not part of the
;      data
;Output:
;  CF - 1 - Block decoding OK
;       0 - Block decoding failed           
;
;Fields used:
; BUFRLO,BUFRHI,BFENLO,BFENHI - Buffer pointer
; LTEMP   - Identification byte
; CHKSUM  - Checksum
; LTEMP+1 - Display mask (0 no display, 255 display)
; ICAX5Z  - Counter of pilot tone pulses
; ICAX6Z  - Byte being decoded
; STATUS  - Prior DATA IN logical value       
;===============================================================================
   
L0631       sta LTEMP         ;Keep the requested first byte value

            lda #52           ;Switch program recorder to turbo mode                                                             
            sta PACTL         ;Motor ON
            sta PBCTL         ;Command ON
            
            lda #128          ;Disable interrupts
            sta POKMSK
            sta IRQEN
            
            clc               ;Clear work fields 
            ldy #0
            sty STATUS
            sty CHKSUM
            sty NMIEN
            sty DMACLT
            php

;-------------------------------------------------------------------------------
; Wait for 256 pilot tone pulses
;-------------------------------------------------------------------------------           
L0650       bne L06C2         ;If not equal, terminate decoding
L0652       jsr L06DB         ;Wait for edge
            bcc L0650         ;If no edge, try again
            
            lda #0            ;Clear           
            sta ICAX5Z        ; Number of pilot tone pulses 
            sta LTEMP+1       ; Display mask (0 No stripes, 255 Stripes)
            
L065D       ldy #180          ;Set pulse width unit counter base value
L065F       jsr L06D6         ;Measure width of the pulse
            bcc L0650         ;If no pulse, start over
            cpy #216          ;Is the pulse too long?
            bcc L0652         ;Yes, start over
            inc ICAX5Z        ;Increment pilot tone pulse counter
            bne L065D         ;If not enoguh pilot tone pulses (255), get next
            dec LTEMP+1       ;More than 255 pilot tone pulses - display stripes

;-------------------------------------------------------------------------------
; Wait for synchronization (very narrow) pulse
;-------------------------------------------------------------------------------
            
L066E       ldy #209          ;Set pulse width unit counter base value
            jsr L06DB         ;Wait for edge
            bcc L0650         ;If no edge, start over
            
            cpy #222          ;Pulse too wide to be a sync pulse?
            bcs L066E         ;Yes, keep waiting
            jsr L06DB         ;Wait for edge
            bcc L06C2         ;If no edge, terminate decoding
            
            ldy #198          ;Set pulse width unit counter base value
            jmp L069D         ;Start decoding the data
            
;-------------------------------------------------------------------------------
; Decode data
;-------------------------------------------------------------------------------            
L0683       plp               ;Is this identification byte?
            bne L068E         ;No, just place the byte to the buffer
            lda LTEMP         ;Yes, please update checksum
            eor ICAX6Z        ;Check identification byte
            bne L06C3         ;Bad - terminate decoding
            beq L069A         ;Good - decode next byte
            
L068E       ldy #0            ;Place byte to the buffer
            lda ICAX6Z
            sta (BUFRLO),Y
            inc BUFRLO        ;Update buffer pointer
            bne L069A
            inc BUFRHI
L069A       ldy #200          ;Set pulse width unit counter base value
                              ;Time compensation
            php
            
L069D       lda #1            ;Prepare bit mask
            sta ICAX6Z         
            
L06A1       jsr L06D6         ;Measure width of the pulse
            bcc L06C2         ;If no pulse, terminate decoding
            cpy #227          ;Determine wide or narrow pulse
            rol ICAX6Z        ;Rotate bit mask
            ldy #198          ;Set pulse width unit counter base value
            bcc L06A1         ;If byte not finished, get next bit
            
            lda CHKSUM        ;Update checksum
            eor ICAX6Z
            sta CHKSUM
            
            lda BUFRLO        ;Check if all bytes decoded
            cmp BFENLO
            lda BUFRHI
            sbc BFENHI
            bcc L0683         ;If not all decoded, place byte to memory   
            lda #0            ;Use CF=0 to indicate bad checksum
            cmp CHKSUM


;-------------------------------------------------------------------------------
; Terminate decoding
;-------------------------------------------------------------------------------            
L06C2       pla               ;Loading complete
L06C3       lda #192          ;Enable interrupts
            sta NMIEN
            sta POKMSK
            sta IRQEN
            
            lda #60           ;Switch program recorder mode to standard           
            sta PACTL         ;Motor OFF
            sta PBCTL         ;Command OFF
            rts               ;Return

;-------------------------------------------------------------------------------
; Detect pulses and edges
;-------------------------------------------------------------------------------            
L06D6       jsr L06DB         ;Wait for edge
            bcc L06FF         ;If no edge, terminate
            
L06DB       ldx #4            ;Delay
L06DD       dex               
            bne L06DD
            
            lda STATUS        ;Get prior status of DATA IN  
            lsr               ;Shift it
            and LTEMP+1       ;Display stripe (if mask on)
            sta COLBK
            
L06E8       iny               ;Increment pulse width unit counter
            beq L06FE         ;If wraparound, terminate
            lda BRKKEY        ;Check BREAK key
            beq L06FC         ;If pressed, terminate
            lda SKSTAT        ;Get SKSTAT
            and #16           ;Determine DATA IN logical value
            cmp STATUS        ;Compare with prior one
            beq L06E8         ;If the same, then no edge has been found
            sta STATUS        ;Otherwise, there was an edge (0-1 or 1-0)
            sec               ;Indicate edge found
            rts               ;And return
L06FC       dec BRKKEY
L06FE       clc
L06FF       rts

;-------------------------------------------------------------------------------
; Turbo header buffer
;-------------------------------------------------------------------------------
THEADER
TH_TYPE     dta 0
TH_NAME     dta c'..........'
TH_LOAD     dta 0,0
TH_LEN      dta 0,0
TH_RUN      dta 0,0
TH_END      EQU *
TH_CHKSUM   dta 0
TH_FULL_LEN EQU *-THEADER
;-------------------------------------------------------------------------------
; Other turbo data
;-------------------------------------------------------------------------------
CHKSUM_D    dta 0

;===============================================================================
; File listing data area and routines
;===============================================================================
; One output line
PLINE       dta c' '
PL_NAME     dta c'..........'
            dta c','
PL_TYPE     dta c'x'
            dta c','
PL_RC       dta c'.'
PL_EOL      dta $0D,$0A
PL_LENGTH   EQU *-PLINE            
PDEV        dta c'H1:LISTING.TXT',$9B
;Other data
PL_WMODE    dta 0        

;-------------------------------------------------------------------------------
; Flush one listing line - and close the file
;-------------------------------------------------------------------------------
PLINE_FLUSH pha
            ldx #(2*16)       ;IOCB multiplied by 16
            lda #$0B          ;Put bytes command
            sta IOCB0+ICCOM,X
            lda #<PLINE
            sta IOCB0+ICBAL,X
            lda #>PLINE
            sta IOCB0+ICBAH,X
            lda #<PL_LENGTH
            sta IOCB0+ICBLL,X
            lda #>PL_LENGTH
            sta IOCB0+ICBLH,X
            jsr CIOV
            
            ldx #(2*16)
            lda #$0C
            sta IOCB0+ICCOM,X
            jsr CIOV
            
PL_F_OK
            pla
            rts
;-------------------------------------------------------------------------------
; Open listing file for append or creation
;-------------------------------------------------------------------------------
OPEN_LISTING            
            pha
            ldx #(2*16)       ;IOCB multiplied by 16
            lda #$03          ;OPEN command
            sta IOCB0+ICCOM,X
            lda PL_WMODE      ;Write mode, write or append
            sta IOCB0+ICAX1,X   
            lda #<PDEV
            sta IOCB0+ICBAL,X
            lda #>PDEV
            sta IOCB0+ICBAH,X
            lda #0
            sta IOCB0+ICAX2,X
            sta IOCB0+ICBLH,X
            sta IOCB0+ICBLL,X
            jsr CIOV
            
            lda PL_WMODE      ;Check write mode
            cmp #$09          ;Is that apppend?
            beq OL_DONE       ;If yes, leave as it is
            lda #$09          ;If it was other than append
            sta PL_WMODE      ;Change to append
OL_DONE   
            pla
            rts                    

;===============================================================================
;Display list and display memory
;===============================================================================

DLIST       dta AEMPTY8,AEMPTY8,AEMPTY8
            dta ALMS+2,<DISPLINE1,>DISPLINE1
            dta AEMPTY1
            dta 2
            dta AVB+AJMP
            dta <DLIST,>DLIST
                 ;1234567890123456789012345678901234567890
DISPLINE1   dta d'COPY T/H 1.1-11 (C) 2025 BAKTRA Software'
DISPLINE2   dta d'                                        '     

SETSCREEN    sei
             lda #<DLIST
             sta SDLSTL 
             lda #>DLIST 
             sta SDLSTH 
             cli
             rts

;-------------------------------------------------------------------------------
; Wait for VBLANK
;-------------------------------------------------------------------------------
WAITVBL     pha
            lda RTCLOK+2
@           cmp RTCLOK+2
            beq @-
            pla
            rts
;===============================================================================
; Messaging system
;===============================================================================

;Message table
MSG_START   dta a(M_START-1),(M_START_L),0
MSG_HIO     dta a(M_HIO-1),(M_HIO_L),0

;Message texts
M_START     dta d'START'*
            dta d' to copy'
M_START_L   EQU *-M_START
     

M_HIO      dta d'Err H1: '
           dta d'START'*
M_HIO_L    EQU *-M_HIO

;-------------------------------------------------------------------------------
;Display message subroutine
;Inputs:    A: Index of the message
;-------------------------------------------------------------------------------
DISP_MSG    pha                      ;Save A
            jsr MSG_CLEAR            ;Clear message area
            asl                      ;Multiply A by 4
            asl
            tax
            lda MSG_START,X          ;Put message start address to the ZP cells
            sta ZP_SRCL
            lda MSG_START+1,X
            sta ZP_SRCH
            
            lda MSG_START+2,X        ;Put message length to reg X
            tay
            
MS_LOOP     lda (ZP_SRCL),y
            sta DISPLINE2-1,y
            dey
            bne MS_LOOP            
            pla
            rts

;-------------------------------------------------------------------------------            
;Clear the messaging area completely
;-------------------------------------------------------------------------------
MSG_CLEAR   pha
            lda #0
            ldx #40
MC_LOOP     sta DISPLINE2-1,x
            dex
            bne MC_LOOP            
            pla
            rts
;-------------------------------------------------------------------------------            
;Wait for the START key
;-------------------------------------------------------------------------------            
WAIT_START  pha
S_LOOP1     lda CONSOL
            cmp #6
            beq S_LOOP1
S_LOOP2     lda CONSOL
            cmp #6
            bne S_LOOP2
            pla
            rts
                                
;===============================================================================
;Initialization of DOS vectors
;===============================================================================
CASINIT     lda #0                        ;Warm start
            sta COLDST                    
            lda #02                       ;Cassette boot successfull
            sta BOOT
            lda #<COPY_START             ;CASINI to loader entry
            sta CASINI
            lda #>COPY_START
            sta CASINI+1
            lda #$FE       ;Get OS ROM MASK
            and $D301      ;Prepare new PORTB 
            sta $D301      ;Now we have OS ROM off
            rts
MAIN_BUFFER EQU *            


;===============================================================================
; HIGH AREA
;===============================================================================
            ORG COPY_HIGH

;-------------------------------------------------------------------------------
;Store the file as a binary load file
;-------------------------------------------------------------------------------
STORE_BINARY
            pha  
            
;Open the H1 device for output, channel #1
            ldx #(1*16)       ;IOCB multiplied by 16
            lda #$03          ;OPEN command
            sta IOCB0+ICCOM,X
            lda #$08          ;Write mode
            sta IOCB0+ICAX1,X   
            lda #<HDEV_DEV
            sta IOCB0+ICBAL,X
            lda #>HDEV_DEV
            sta IOCB0+ICBAH,X
            lda #0
            sta IOCB0+ICAX2,X
            sta IOCB0+ICBLH,X
            sta IOCB0+ICBLL,X
            jsr CIOV
            bpl SB_OPENED
            
;Handle error
            jmp HDEVICE_HANDLE_ERR
            
SB_OPENED
       
; Prepare and output segment header
            lda TH_LOAD
            sta HBUF_SEGF
            lda TH_LOAD+1
            sta HBUF_SEGF+1
            
            lda TH_LOAD
            clc
            adc TH_LEN
            sta HBUF_SEGL
            lda TH_LOAD+1
            adc TH_LEN+1
            sta HBUF_SEGL+1
            
            lda HBUF_SEGL
            beq SH1
            dec HBUF_SEGL
            jmp SH2
SH1         dec HBUF_SEGL
            dec HBUF_SEGL+1            
SH2         
            ldx #(1*16)       ;IOCB multiplied by 16
            lda #$0B          ;PUT BYTES command
            sta IOCB0+ICCOM,X
            lda #<HBUF_SEGH
            sta IOCB0+ICBAL,X
            lda #>HBUF_SEGH
            sta IOCB0+ICBAH,X
            lda #<HBUF_SEG_LEN
            sta IOCB0+ICBLL,X
            lda #>HBUF_SEG_LEN
            sta IOCB0+ICBLH,X
            jsr CIOV
            bpl SD1
            jmp HDEVICE_HANDLE_ERR           
            
               
;Prepare and output the main data segment
SD1
            ldx #(1*16)       ;IOCB multiplied by 16
            lda #$0B          ;PUT BYTES command
            sta IOCB0+ICCOM,X
            lda #<MAIN_BUFFER
            sta IOCB0+ICBAL,X
            lda #>MAIN_BUFFER
            sta IOCB0+ICBAH,X
            lda TH_LEN
            sta IOCB0+ICBLL,X
            lda TH_LEN+1
            sta IOCB0+ICBLH,X
            jsr CIOV
            bpl RS1
            jmp HDEVICE_HANDLE_ERR
            
;Prepare and output the RUN segment
RS1
            lda TH_RUN
            sta HBUF_RUN
            lda TH_RUN+1
            sta HBUF_RUN+1
;            
            ldx #(1*16)       ;IOCB multiplied by 16
            lda #$0B          ;PUT BYTES command
            sta IOCB0+ICCOM,X
            lda #<HBUF_RH
            sta IOCB0+ICBAL,X
            lda #>HBUF_RH
            sta IOCB0+ICBAH,X
            lda #<HBUF_R_LEN
            sta IOCB0+ICBLL,X
            lda #>HBUF_R_LEN
            sta IOCB0+ICBLH,X
            jsr CIOV
            bpl SB_CLOSE
            jmp HDEVICE_HANDLE_ERR
 
SB_CLOSE    jsr HDEVICE_CLOSE
;
            pla 
            rts
;            

;            
HBUF_SEGH      dta $FF,$FF
HBUF_SEGF      dta $0,$0
HBUF_SEGL      dta $0,$0
HBUF_SEG_LEN   EQU *-HBUF_SEGH

HBUF_RH       dta a(RUNAD),a(RUNAD+1)
HBUF_RUN      dta $0,$0
HBUF_R_LEN    EQU *-HBUF_RH

;-------------------------------------------------------------------------------
;Store the file as a tape image
;1. FUJI block
;2. pwmc block, sync pulse
;3. pwmd block with turbo header, pwml block with safety pulse
;4. pwmc block, sync pulse
;5. pwmd block with turbo data, pwml blok with safety pulse
;-------------------------------------------------------------------------------
STORE_TAPEIMAGE
            pha  
            
;Open the H1 device for output, channel #1
TI_OPEN     ldx #(1*16)       ;IOCB multiplied by 16
            lda #$03          ;OPEN command
            sta IOCB0+ICCOM,X
            lda #$08          ;Write mode
            sta IOCB0+ICAX1,X   
            lda #<HDEV_DEV
            sta IOCB0+ICBAL,X
            lda #>HDEV_DEV
            sta IOCB0+ICBAH,X
            lda #0
            sta IOCB0+ICAX2,X
            sta IOCB0+ICBLH,X
            sta IOCB0+ICBLL,X
            jsr CIOV
            bpl TI_OPENED
            
;Handle error
            jmp HDEVICE_HANDLE_ERR
            
TI_OPENED
; Write the cas file header
            ldx #(1*16)       ;IOCB multiplied by 16
            lda #$0B          ;PUT BYTES command
            sta IOCB0+ICCOM,X
            lda #<CAS_HEAD
            sta IOCB0+ICBAL,X
            lda #>CAS_HEAD
            sta IOCB0+ICBAH,X
            lda #<CAS_HEAD_L
            sta IOCB0+ICBLL,X
            lda #>CAS_HEAD_L
            sta IOCB0+ICBLH,X
            jsr CIOV
            bpl TI_WRITTEN_1
            jmp HDEVICE_HANDLE_ERR

; Write turbo header data to the .cas file                       
TI_WRITTEN_1
            ldx #(1*16)       ;IOCB multiplied by 16
            lda #$0B          ;PUT BYTES command
            sta IOCB0+ICCOM,X
            lda #<THEADER
            sta IOCB0+ICBAL,X
            lda #>THEADER
            sta IOCB0+ICBAH,X
            lda #<TH_FULL_LEN
            sta IOCB0+ICBLL,X
            lda #>TH_FULL_LEN
            sta IOCB0+ICBLH,X
            jsr CIOV
            bpl TI_WRITTEN_2
            jmp HDEVICE_HANDLE_ERR
            
;Setup and write pwmd chunk for data            
TI_WRITTEN_2
            lda TH_LEN
            sta CAS_F_LEN
            lda TH_LEN+1
            sta CAS_F_LEN+1
            clc
            lda CAS_F_LEN
            adc #2
            sta CAS_F_LEN
            bcc @+
            inc CAS_F_LEN+1           
@            
            ldx #(1*16)       ;IOCB multiplied by 16
            lda #$0B          ;PUT BYTES command
            sta IOCB0+ICCOM,X
            lda #<CAS_FILE
            sta IOCB0+ICBAL,X
            lda #>CAS_FILE
            sta IOCB0+ICBAH,X
            lda #<CAS_FILE_L
            sta IOCB0+ICBLL,X
            lda #>CAS_FILE_L
            sta IOCB0+ICBLH,X
            jsr CIOV
            bpl TI_WRITTEN_3
            jmp HDEVICE_HANDLE_ERR            
            
;Write the file buffer
TI_WRITTEN_3
            ldx #(1*16)       ;IOCB multiplied by 16
            lda #$0B          ;PUT BYTES command
            sta IOCB0+ICCOM,X
            lda #<MAIN_BUFFER
            sta IOCB0+ICBAL,X
            lda #>MAIN_BUFFER
            sta IOCB0+ICBAH,X
            lda TH_LEN
            sta IOCB0+ICBLL,X
            lda TH_LEN+1
            sta IOCB0+ICBLH,X
            jsr CIOV
            bpl TI_WRITTEN_4
            jmp HDEVICE_HANDLE_ERR            

;Write the checksum byte
TI_WRITTEN_4
            ldx #(1*16)       ;IOCB multiplied by 16
            lda #$0B          ;PUT BYTES command
            sta IOCB0+ICCOM,X
            lda #<CHKSUM_D
            sta IOCB0+ICBAL,X
            lda #>CHKSUM_D
            sta IOCB0+ICBAH,X
            lda #1
            sta IOCB0+ICBLL,X
            lda #0
            sta IOCB0+ICBLH,X
            jsr CIOV
            bpl TI_WRITTEN_5
            jmp HDEVICE_HANDLE_ERR 
                       
;Write safety pulse
TI_WRITTEN_5
            ldx #(1*16)       ;IOCB multiplied by 16
            lda #$0B          ;PUT BYTES command
            sta IOCB0+ICCOM,X
            lda #<CAS_FILE
            sta IOCB0+ICBAL,X
            lda #>CAS_FILE
            sta IOCB0+ICBAH,X
            lda #<CAS_SAFP_L
            sta IOCB0+ICBLL,X
            lda #>CAS_SAFP_L
            sta IOCB0+ICBLH,X
            jsr CIOV
            bpl TI_CLOSEH
            jmp HDEVICE_HANDLE_ERR
                         
TI_CLOSEH   jsr HDEVICE_CLOSE
            pla 
            rts       
;
CAS_HEAD    dta c'FUJI'                                   ;FUJI header
            dta 0,0,0,0                                   ;Empty
            dta c'pwms'                                   ;Setup
            dta $02,$00,$06,$00,$44,$AC
            dta c'pwmc'                                   ;Pilot tone
            dta $03,$00,$00,$00,$20,$00,$0C
            dta c'pwml'                                   ;Sync pulse
            dta $04,$00,$00,$00,$05,$00,$05,$00
            dta c'pwmd'                                   ;Header data
            dta $13,$00,$0C,$1A                           ;Len+duration
            dta $00                                       ;Id byte
CAS_HEAD_L  equ *-CAS_HEAD            
            
CAS_FILE    dta c'pwml'
            dta $04,$00,$00,$00,$06,$00,$06,$00
CAS_SAFP_L  EQU *-CAS_FILE            
            dta c'pwmc'                                   ;Pilot tone                    
            dta $03,$00,$00,$00,$20,$00,$0C
            dta c'pwml'                                   ;Sync pulse
            dta $04,$00,$00,$00,$05,$00,$05,$00
            dta c'pwmd'                                   ;File data
CAS_F_LEN   dta $FF,$FF                                   ;Data length
            dta $0C,$1A
            dta $FF                                       ;Id byte
CAS_FILE_L  equ *-CAS_FILE
                             
NAME_ITER   dta c'0'                                      ;Name iterator
NAME_CURR   dta c' '                                      ;Name character
NAME_BADC   dta c'<>|*?\/:"'                              ;table of bad chars
NAME_BADC_L equ *-NAME_BADC                               ;length of the table

;===============================================================================
;H: device general routines
;===============================================================================
;-------------------------------------------------------------------------------            
;Routine do display H: device I/O error
;-------------------------------------------------------------------------------
HDEVICE_HANDLE_ERR 
            lda #MI_HIO
            jsr DISP_MSG
            jsr WAIT_START
            jmp WARMSV
            
;-------------------------------------------------------------------------------            
;Routine to close H: device
;-------------------------------------------------------------------------------
HDEVICE_CLOSE
            pha
            ldx #(1*16)       ;IOCB multiplied by 16
            lda #$0C          ;Close
            sta IOCB0+ICCOM,X
            jsr CIOV
            pla 
            rts           

;-------------------------------------------------------------------------------
;Routine ta make output file name
;Input: Y=0 for XEX, Y=1 for CAS
;-------------------------------------------------------------------------------
MAKE_FNAME
            pha
            
;Move the filename
            ldx #(HFNAME_LEN-1)
FN1         lda TH_NAME-1,X
            sta NAME_CURR
            
            cmp #$20          ;IS char <$20
            bcc FNX           ;It is, place 'X'

;Fiter out undesired characters using tabke
FNBC        txa               ;Backup The X register
            pha
            ldx #NAME_BADC_L  ;Get the length of bad chars table
FNBC_L      lda NAME_CURR     ;Get the current letter
            cmp NAME_BADC-1,X ;Compare with table entry
            beq FNBC_E1       ;If bad char, quit and replace with X
            dex               ;Decrement x
            bne FNBC_L        ;Continue loop
FNBC_E2     pla               ;Restore the X register
            tax
            jmp FN2           ;Continue to further checks            
            
FNBC_E1     pla               ;Restore the X register
            tax             

;Force X              
FNX         lda #$58          ;Force 'X'
            jmp FN5

;Other checks
FN2
            lda NAME_CURR
                        
            cmp #$3A          ;Is char <$3A?         
            bcc FN5           ;Yes, then use the char (it is a number)
            
            cmp #$41          ;Is char <$41?
            bcc FNX           ;Indeed, then force 'X'
            
            cmp #$5B          ;Is char <$5B?
            bcc FN5           ;Then it is a capital letter, use it
            
            cmp #$61          ;Is char <$61?
            bcc FNX           ;Then replace with 'X'
            
            cmp #$7B          ;Is char <$7B
            bcs FN3           ;No, skip  
            sec               ;Otherwise capitalize
            sbc #$20
            jmp FN5
            
FN3         jmp FNX           ;In other cases, just replace with 'X'      

;Place the character                     
FN5         sta HDEV_FILEN-1,X
            dex
            bne FN1
            
;Place iterating character
            ldx #HFNAME_LEN
            lda NAME_ITER
            sta HDEV_FILEN-1,X

;Iterate the character
            inc NAME_ITER     ;Iterate
            lda NAME_ITER     ;Check what it is
            cmp #':'          ;Over 9?
            bne @+            ;No, just skip
            lda #'0'          ;Back to 0
            sta NAME_ITER     ;Write back
@            
;Place extension - either XEX or CAS 
            ldx #3
            cpy #0
            bne FNCAS
FNXEX       lda HDEV_XEX-1,x
            sta HDEV_FEXT-1,x
            dex
            bne FNXEX
            pla
            rts                 
FNCAS       lda HDEV_CAS-1,x
            sta HDEV_FEXT-1,x
            dex 
            bne FNCAS            
            pla
            rts
