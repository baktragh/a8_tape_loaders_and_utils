;*******************************************************************************
;TURGEN - BACKUP T/D Utility Disk (C) 2024 BAKTRA Software
;Mainline code. Assemble with MADS.
;
;This utility disk automatically copies Czechoslovak Turbo 2000 files to disk
;without a file system. The files can be extracted by the BACKUP T/D Extractor. 
;
;Minimum of 128 KB of total RAM is required, because the turbo block is loaded
;into the banks of the extended memory. This allows to process files that
;are up to 64 KB long.
;
;Disk layout:
; The disk is always 8 MB, sector size 128 bytes.
; Sector  Description
; 1..32   Program code and data, not all are used.
; 33      Eye-catcher sector
; 34      Pristine indicator. Pristine disk has all $55s in the sector.
;         Non-pristine disk has all $C0s in the sector. This sector is used
;         to test if the disk is writable.
; 35..    Sectors for data.

; Header sector:
; Begins with 'H',length, continues with header data. The header always
; occupies only one sector.
;
; Data sectors:
; The first sector begins with 'D',legnth, data follows. Then sectors
; with pure data follow. The number of sectors can be calculated using the
; length field.
;
; End of filesystem marker/Non-commited header.
; The sector begins with 'E'
;*******************************************************************************
            ICL 'asminc/equates.asm'
            ICL 'asminc/auxmacs.mac'

            OPT H-,F-
;-------------------------------------------------------------------------------
; Local equates and definitions
;-------------------------------------------------------------------------------            
            PROGRAM_START EQU $1000
            MAIN_BUFFER   EQU $4000
            MAIN_BUFPAGE  EQU $40
            ZP_BASEBANK   EQU 128
            ZP_CURRBANK   EQU 129
            ZP_LASTBANK   EQU 130
            ZP_RETCODE    EQU 131
 
            SEC_BUFFER    EQU $2000

            ZP_D_BUFPTR   EQU 132
            ZP_D_SECLO    EQU 133
            ZP_D_SECHI    EQU 134

            ZP_W_BFENLO   EQU 135
            ZP_W_BFENHI   EQU 136
            ZP_W_LASTBANK EQU 137
            ZP_W_BYTE     EQU 138
            ZP_W_BUFRLO   EQU 139
            ZP_W_BUFRHI   EQU 140
            ZP_W_MAXRC    EQU 141

            ZP_WK_LENLO   EQU 142
            ZP_WK_LENHI   EQU 143
            ZP_WK_LO      EQU 144
            ZP_WK_HI      EQU 145  

            SNO_MARKING   EQU 33
            SNO_PRISTINE  EQU 34
            SNO_DATA      EQU 35

            VBI_VCOUNT    EQU 124

            MENU_CODE_BACKUP  EQU 0
            MENU_CODE_LISTING EQU 1
            MENU_CODE_RECORD  EQU 2
            MENU_CODE_EXIT    EQU 3
;-------------------------------------------------------------------------------
; Mainline code
;-------------------------------------------------------------------------------
            ORG PROGRAM_START
            .byte $00
            .byte [[PROGRAM_END-PROGRAM_START]/128]+1
            .word PROGRAM_START
            .word PROGRAM_INIT
PROGRAM_BOOT_CONTINUATION
            lda #<PROGRAM_BEGIN
            sta DOSVEC
            lda #>PROGRAM_BEGIN
            sta DOSVEC+1
            clc
            rts
;Initialize the copier
PROGRAM_INIT 
            rts
;Main program
PROGRAM_BEGIN
            jmp PROGRAM_CODE
            dta '** TURGEN - BACKUP T/D (c) 2024 BAKTRA Software **'
PROGRAM_CODE            
            lda W_FIRST_RUN   ;Check for first run, $FF indicates that
            bne PC_FIRST      ;If so, skip to the PORTB setup

PC_NOT_FIRST                  ;Restore the PORTB setting.
            sei
            lda #0
            sta NMIEN
            sta DMACLT
            lda W_FIRST_BANK
            sta PORTB
            lda #$40
            sta NMIEN
            lda #34
            sta DMACLT
            cli
PC_FIRST
            ;Prepare the base PORTB value
            lda PORTB         ;Get current settings
            and #($FF-$4-$8-$10) ;CPU eRAM + BANK 0
            sta ZP_BASEBANK
            sta W_FIRST_BANK
            lda #0            ;No more first run.
            sta W_FIRST_RUN
            jmp PC_DO_MENU


PC_DO_MENU
            jsr SHOWMENU

            lda ZP_RETCODE    ;What was selected?
            cmp #MENU_CODE_BACKUP
            bne @+
            jmp OP_BACKUP

@           cmp #MENU_CODE_LISTING
            bne @+
            jmp OP_LISTING

@           cmp #MENU_CODE_RECORD
            bne @+
            jmp OP_RECORD

@           cmp #MENU_CODE_EXIT
            bne @+
            jmp COLDSV

@           jmp WARMSV           

;===============================================================================
; Mainline Backup
;===============================================================================
OP_BACKUP   
            jsr DISPLAY_TITLE ;Display the title
            jsr DISPLAY_START_TO_LOAD ;Press START prompt
            jsr WAIT_START    ;Wait for the START key

;Initialize the sector writer
            jsr WRITE_INIT  
            
;Verify if the eye-catcher is present
            jsr DISK_VERIFY_EYE 
            lda ZP_RETCODE
            beq VERIFY1_OK

            jsr DISPLAY_VERIFY1_FAILED
VERIFY1_BAD lda #$12
            sta COLOR4
            jmp ENDLESS                

;Verify if the disk is pristine or not
VERIFY1_OK  jsr DISK_VERIFY_PRISTINE
            lda ZP_RETCODE
            beq VERIFY2_OK

            jsr DISPLAY_VERIFY2_FAILED
            jsr DISPLAY_START_OR_RESET
            jsr WAIT_START
            
;Verify if the disk is writable.
VERIFY2_OK  jsr DISK_VERIFY_WRITABLE
            lda ZP_RETCODE
            beq VERIFY3_OK

            jsr DISPLAY_VERIFY3_FAILED
            jsr DISPLAY_START_OR_RESET
            jsr WAIT_START

VERIFY3_OK
;Write the 'End of filesystem marker'
            jsr WRITE_CLRBUF
            lda #'E'
            sta SEC_BUFFER
            jsr WRITE_FORCE

;-------------------------------------------------------------------------------
; Decode header
;-------------------------------------------------------------------------------
DEC_HEADER  lda #<THEADER     ;Setup address where turbo header will be placed
            sta BUFRLO
            lda #>THEADER
            sta BUFRHI
            
            lda #<TH_END
            sta BFENLO
            lda #>TH_END
            sta BFENHI

            jsr RESET_POKEY
            lda #0            ;First byte of the header should be 0
            jsr L0631         ;Call Turbo 2000 block decoding subroutine
            bcc DEC_HEADER    ;If error occured, try to decode the header again
            jsr RESET_POKEY
;-------------------------------------------------------------------------------
; Process the header
;-------------------------------------------------------------------------------
PROC_HEADER jsr DISPLAY_FOUND
            jsr DELAY_3S
;------------------------------------------------------------------------------- 
;Calculate the buffer range
;Start: BANK,BUFRHI,BUFRLO
;End: BANK, BFENHI, BFENLO 
;-------------------------------------------------------------------------------
            lda ZP_BASEBANK   ;Buffer start - base bank, 16384
            sta ZP_CURRBANK
            sta ZP_LASTBANK
            sta PORTB
            lda #0
            sta BUFRLO
            lda #MAIN_BUFPAGE
            sta BUFRHI

            lda TH_LEN
            sta ZP_WK_LENLO
            lda TH_LEN+1
            sta ZP_WK_LENHI

BUF_B0      lda ZP_WK_LENHI  ;Check length (number of pages)
            cmp #16384/256   ;Is that <16384
            bcc BUF_REM      ;Yes, done

BUF_B1      lda ZP_WK_LENHI  ;Check length (number of pages)
            cmp #32768/256   ;Is that <32767
            bcs BUF_B2       ;No, try other amount
            beq BUF_B2

            clc              ;Calculate the last bank
            lda ZP_LASTBANK
            adc #4
            sta ZP_LASTBANK

            lda ZP_WK_LENHI   ;Calculate length remainder.
            sec
            sbc #16384/256
            sta ZP_WK_LENHI 
            jmp BUF_REM

BUF_B2      lda ZP_WK_LENHI     ;Check length (number of pages)
            cmp #49152/256   ;Is that <49152
            bcs BUF_B3       ;No, try other amount
            beq BUF_B3

            clc
            lda ZP_LASTBANK  ;Calculate the last bank
            adc #8
            sta ZP_LASTBANK

            lda ZP_WK_LENHI     ;Calculate length remainder.
            sec
            sbc #32768/256
            sta ZP_WK_LENHI 
            jmp BUF_REM

BUF_B3      clc               ;Calculate the last bank               
            lda ZP_LASTBANK
            adc #12
            sta ZP_LASTBANK

            lda ZP_WK_LENHI     ;Calculate length remainder
            sec
            sbc #49152/256
            sta ZP_WK_LENHI 
            jmp BUF_REM

BUF_REM     lda ZP_WK_LENLO       ;Count number of bytes
            sta BFENLO
            sta ZP_W_BFENLO  ;Backup the value for later writing

            clc              ;Count page number relative to 16384
            lda ZP_WK_LENHI
            adc BUFRHI
            sta BFENHI
            sta ZP_W_BFENHI  ;Backup the value for later writing
;------------------------------------------------------------------------------- 
;Decode file data
;-------------------------------------------------------------------------------
            jsr RESET_POKEY
            lda #255          ;First byte should be 255
            jsr L0631         ;Call Turbo 2000 block decoding subroutine
            bcs L0622         ;No error - jump
            
ERRDATA     jsr RESET_POKEY
            jsr DISPLAY_LOAD_ERROR
            jsr DELAY_3S
            jmp DEC_HEADER    ;And then try to load another file

;Decoding ok            
L0622       jsr RESET_POKEY
            jsr DISPLAY_LOADED_OK
;-------------------------------------------------------------------------------
; Write the header data 
;-------------------------------------------------------------------------------
            jsr DISPLAY_WRITING_TO_DISK   ;Display message
            jsr WRITE_CLRBUF

            lda ZP_D_SECLO                ;Remember sector number for commit
            sta W_WRITE_LASTH_SECLO
            lda ZP_D_SECHI
            sta W_WRITE_LASTH_SECHI

;Reset the MAXRC
            lda #0
            sta ZP_W_MAXRC           

;Write byte indicating 'E' as header of unfinished file. The 'E' value
;will be changed to 'H' during write commit. If the writing fails, this will
;become end of filesystem indicator.

            lda #'E'
            jsr WRITE_BYTE
;Write word indicating length of the header (17 bytes)
            lda #<THEADER_LEN
            jsr WRITE_BYTE
            lda #>THEADER_LEN
            jsr WRITE_BYTE

;Write data of the header
            ldx #0
@           lda THEADER,X
            jsr WRITE_BYTE
            inx 
            cpx #THEADER_LEN
            bne @-
            jsr WRITE_FLUSH
            UPDATERC
            jsr WRITE_CLRBUF
;-------------------------------------------------------------------------------
; Write the data block 
;-------------------------------------------------------------------------------
;Write byte indicating 'D' as data            
            lda #'D'
            jsr WRITE_BYTE

;Write word indicating length of the file
            lda TH_LEN
            jsr WRITE_BYTE
            lda TH_LEN+1
            jsr WRITE_BYTE

;Write data of the block
            lda ZP_BASEBANK
            sta ZP_CURRBANK
            sta PORTB
            lda #0
            sta ZP_W_BUFRLO
            lda #16384/256
            sta ZP_W_BUFRHI

WD_ONEBYTE
            ldy #0                     ;Get one byte
            lda (ZP_W_BUFRLO),Y         
            jsr WRITE_BYTE

;Increment all the pointers
            inc ZP_W_BUFRLO        ;Update buffer pointer (low)
            bne WD_DONEINC         ;No wraparound, skip

            inc ZP_W_BUFRHI        ;Update buffer pointer (high)
            lda ZP_W_BUFRHI        ;Check buffer pointer (high)
            cmp #32768/256         ;BUFRHI reached end of bank?
            bne WD_DONEINC         ;No, just skip

            lda ZP_CURRBANK        ;Get current bank
            clc                    ;Increment by 4
            adc #4
            sta ZP_CURRBANK        ;Store it
            sta PORTB              ;Update PORTB
            lda #(16384/256)       ;Wrap BUFRHI
            sta ZP_W_BUFRHI 
WD_DONEINC     
;Check if all bytes have been processed
            lda ZP_CURRBANK 
            cmp ZP_LASTBANK
            bne WD_ONEBYTE

            lda ZP_W_BUFRLO       
            cmp ZP_W_BFENLO
            lda ZP_W_BUFRHI
            sbc ZP_W_BFENHI
            bcc WD_ONEBYTE

;Ensure all data is flushed, and writer advances to the next sector
            jsr WRITE_FLUSH
            UPDATERC

;Write 'End of file system marker' temporarily to the sector.
            jsr WRITE_CLRBUF
            lda #'E'
            sta SEC_BUFFER
            jsr WRITE_FORCE
            UPDATERC

;Commit
            jsr WRITE_COMMIT;
            UPDATERC

;Display message
            lda ZP_W_MAXRC
            beq WD_MSG_OK
            jsr DISPLAY_WRITING_FAILED
            jsr DISPLAY_START_OR_RESET
            jsr WAIT_START
            jmp WD_DONE 
 
WD_MSG_OK   jsr DISPLAY_WRITTEN_TO_DISK
WD_DONE     jsr DELAY_TENTH

;And when done with all this, go and load next file            
            jmp DEC_HEADER
     
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
L0650       beq L0652         ;If not equal, terminate decoding
            jmp L06C2
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

            inc BUFRLO        ;Update buffer pointer (low)
            bne L069A         ;No wraparound, skip

            inc BUFRHI        ;Update buffer pointer (high)

            lda BUFRHI        ;Check buffer pointer (high)
            cmp #32768/256    ;BUFRHI reached end of bank?
            bne L069A         ;No, just skip

            lda ZP_CURRBANK   ;Get current bank
            clc               ;Increment by 4
            adc #4
            sta ZP_CURRBANK   ;Store it
            sta PORTB         ;Update PORTB
            lda #(16384/256)  ;Wrap BUFRHI
            sta BUFRHI 
           
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
            
            lda ZP_CURRBANK    ;Check if all bytes decoded
            cmp ZP_LASTBANK
            bne L0683

            lda BUFRLO       
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
L06C3       lda #$40          ;Enable interrupts, just VBI
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

;===============================================================================
; Disk operations
;===============================================================================
;-------------------------------------------------------------------------------
; Verify the marking sector (nr. 25)
;-------------------------------------------------------------------------------
DISK_VERIFY_EYE
            SUBENTRY
            lda #0
            sta ZP_RETCODE

            lda #1
            sta DUNIT 
            lda #<SNO_MARKING
            sta DAUX1     
            lda #>SNO_MARKING
            sta DAUX2
            lda #$52     ; "R"
            sta DCOMND
            lda #>SEC_BUFFER
            sta DBUFHI  
            lda #<SEC_BUFFER 
            sta DBUFLO
            jsr DSKINV

            lda DSTATS                ;Check the status
            sta W_DSKIO_CODE          ;Keep the status for later
            cmp #1                    ;Is status OK (==1)?
            bne DVE_BAD               ;No, return 8

DVE_COMPARE    
            ldx #DVE_T_EYE_L
@           lda SEC_BUFFER-1,X
            cmp DVE_T_EYE-1,X
            bne DVE_BAD2
            dex
            bne @-
DVE_DONE
            SUBEXIT

DVE_BAD     jsr MSG_DISK_ERROR
DVE_BAD2    lda #8                    ;Set RC=8
            sta ZP_RETCODE
            jmp DVE_DONE


DVE_T_EYE     dta c'TURGEN BACKUP T/D 1.00'
DVE_T_EYE_L   equ *-DVE_T_EYE
;-------------------------------------------------------------------------------
; Verify if the disk is pristine 
;-------------------------------------------------------------------------------
DISK_VERIFY_PRISTINE
            SUBENTRY
            lda #0
            sta ZP_RETCODE

            lda #1
            sta DUNIT 
            lda #<SNO_PRISTINE
            sta DAUX1     
            lda #>SNO_PRISTINE
            sta DAUX2
            lda #$52     ; "R"
            sta DCOMND
            lda #>SEC_BUFFER
            sta DBUFHI  
            lda #<SEC_BUFFER 
            sta DBUFLO
            jsr DSKINV

            lda DSTATS                ;Check the status
            sta W_DSKIO_CODE          ;Keep for later
            cmp #1                    ;Is status OK (==1)?
            bne DVP_BAD               ;No, return 8

DVP_COMPARE                           ;Check for all $55s
            ldx #128
@           lda SEC_BUFFER-1,X
            cmp #$55
            bne DVP_BAD2              ;If value not expected, bad
            dex
            bne @-
DVP_DONE
            SUBEXIT

DVP_BAD     jsr MSG_DISK_ERROR
DVP_BAD2    lda #8                    ;Set RC=8
            sta ZP_RETCODE
            jmp DVP_DONE
;-------------------------------------------------------------------------------
; Verify if the disk is writable
;-------------------------------------------------------------------------------
DISK_VERIFY_WRITABLE
            SUBENTRY

            lda #0
            sta ZP_RETCODE

            ldx #128
            lda #$C0
@           sta SEC_BUFFER-1,X
            dex
            bne @-             
 
            lda #1
            sta DUNIT 
            lda #<SNO_PRISTINE
            sta DAUX1     
            lda #>SNO_PRISTINE
            sta DAUX2
            lda #$57     ; Put sector, with verification
            sta DCOMND
            lda #>SEC_BUFFER
            sta DBUFHI  
            lda #<SEC_BUFFER 
            sta DBUFLO
            jsr DSKINV

            lda DSTATS                ;Check the status
            sta W_DSKIO_CODE          ;Keep for later
            cmp #1                    ;Is status OK (==1)?
            bne DVW_BAD               ;No, return 8

DVW_DONE    SUBEXIT

DVW_BAD     lda #8                    ;Set RC=8
            sta ZP_RETCODE
            jsr MSG_DISK_ERROR
            jmp DVW_DONE            


;===============================================================================
; Data writing subroutines
;===============================================================================
;-------------------------------------------------------------------------------
;Initialize thr writing system
;Position to sector 27, buffer offset is zero
;-------------------------------------------------------------------------------
WRITE_INIT  SUBENTRY
            lda #SNO_DATA
            sta ZP_D_SECLO
            lda #0
            sta ZP_D_SECHI
            sta ZP_D_BUFPTR
            SUBEXIT
;-------------------------------------------------------------------------------
;Write one byte, present in the A register
;-------------------------------------------------------------------------------
WRITE_BYTE  sta  ZP_W_BYTE                ;First, store the parameter
            SUBENTRY
            ldx  ZP_D_BUFPTR              ;Get buffer offset
            lda  ZP_W_BYTE
            sta  SEC_BUFFER,X             ;Store to the sector buffer
            inx                           ;Increment buffer offset
            stx ZP_D_BUFPTR               ;Update buffer pointer
            cpx  #128                     ;Sector complete?
            bne  WB_DONE                  ;No, then skip
            
            jsr  WRITE_FLUSH              ;Flush the sector
            UPDATERC
            jsr  WRITE_CLRBUF             ;And clear the buffer.
 
WB_DONE     SUBEXIT
;-------------------------------------------------------------------------------
;Flush written data
;-------------------------------------------------------------------------------
WRITE_FLUSH SUBENTRY
            lda #0
            sta ZP_RETCODE

            ldx ZP_D_BUFPTR
            beq WF_DONE
            bne WF_0

WF_0        lda #1
            sta DUNIT 
            lda ZP_D_SECLO
            sta DAUX1     
            lda ZP_D_SECHI
            sta DAUX2
            lda #$57     ; Put sector, with verification
            sta DCOMND
            lda #>SEC_BUFFER
            sta DBUFHI  
            lda #<SEC_BUFFER 
            sta DBUFLO
            jsr DSKINV

            lda DSTATS                ;Check the status
            sta W_DSKIO_CODE          ;Keep for later
            cmp #1                    ;Is status OK (==1)?
            bne WF_BAD               ;No, return 8

            ldx #0                       ;Reset the buffer pointer
            stx ZP_D_BUFPTR
            inc ZP_D_SECLO               ;Increment lo sector number
            bne WF_DONE                  ;When no wraparound, skip
            inc ZP_D_SECHI               ;Increment hi sector counter

WF_DONE     SUBEXIT

WF_BAD      lda #8                    ;Set RC=8
            sta ZP_RETCODE
            jsr MSG_DISK_ERROR
            jmp WF_DONE
;-------------------------------------------------------------------------------
;Write the buffer to the current sector, but do not advance to the next sector.
;-------------------------------------------------------------------------------
WRITE_FORCE SUBENTRY
            lda #0
            sta ZP_RETCODE

            lda #1
            sta DUNIT 
            lda ZP_D_SECLO
            sta DAUX1     
            lda ZP_D_SECHI
            sta DAUX2
            lda #$57     ; Put sector, with verification
            sta DCOMND
            lda #>SEC_BUFFER
            sta DBUFHI  
            lda #<SEC_BUFFER 
            sta DBUFLO
            jsr DSKINV

            lda DSTATS                ;Check the status
            sta W_DSKIO_CODE          ;Keep for later
            cmp #1                    ;Is status OK (==1)?
            bne WFO_BAD              ;No, return 8

WFO_DONE    SUBEXIT

WFO_BAD     lda #8                    ;Set RC=8
            sta ZP_RETCODE
            jsr MSG_DISK_ERROR
            jmp WFO_DONE          
;-------------------------------------------------------------------------------
; Clear sector buffer
;-------------------------------------------------------------------------------
WRITE_CLRBUF SUBENTRY
            lda #$FF
            ldx #128
@           sta SEC_BUFFER-1,X
            dex
            bne @-
            SUBEXIT
;-------------------------------------------------------------------------------
; Commit write
; Read the recent header sector, change its type to 'H'
;-------------------------------------------------------------------------------
WRITE_COMMIT SUBENTRY
;Read the sector with header
            lda #0
            sta ZP_RETCODE

            lda #1
            sta DUNIT 
            lda W_WRITE_LASTH_SECLO
            sta DAUX1     
            lda W_WRITE_LASTH_SECHI
            sta DAUX2
            lda #$52     ; Read sector
            sta DCOMND
            lda #>SEC_BUFFER
            sta DBUFHI  
            lda #<SEC_BUFFER 
            sta DBUFLO
            jsr DSKINV

            lda DSTATS                ;Check the status
            sta W_DSKIO_CODE          ;Keep for later
            cmp #1                    ;Is status OK (==1)?
            bne WC_BAD                ;No, return 8

;Check the sector buffer for 'E', change to 'H'
            lda SEC_BUFFER
            cmp #'E'
            bne WC_BAD
            lda #'H'
            sta SEC_BUFFER
 
;Write the sector back.
            lda #0
            sta ZP_RETCODE

            lda #1
            sta DUNIT 
            lda W_WRITE_LASTH_SECLO
            sta DAUX1     
            lda W_WRITE_LASTH_SECHI
            sta DAUX2
            lda #$57     ; Put sector, with verification
            sta DCOMND
            lda #>SEC_BUFFER
            sta DBUFHI  
            lda #<SEC_BUFFER 
            sta DBUFLO
            jsr DSKINV

            lda DSTATS                ;Check the status
            sta W_DSKIO_CODE          ;Keep for later
            cmp #1                    ;Is status OK (==1)?
            bne WC_BAD              ;No, return 8

WC_DONE     SUBEXIT

WC_BAD      lda #8                    ;Set RC=8
            sta ZP_RETCODE
              
            lda W_WRITE_LASTH_SECLO
            sta ZP_D_SECLO
            lda W_WRITE_LASTH_SECHI
            sta ZP_D_SECHI        
            lda #0
            sta ZP_D_BUFPTR     

            jsr MSG_DISK_ERROR
            jmp WC_DONE          

 
;===============================================================================
; Display messages
;===============================================================================
           CIO0_OP    EQU $0342
           CIO0_STAT  EQU $0343
           CIO0_BUFLO EQU $0344
           CIO0_BUFHI EQU $0345
           CIO0_LENLO EQU $0348
           CIO0_LENHI EQU $0349
           CIO0_AUX1  EQU $034A
           CIO0_AUX2  EQU $034B

;-------------------------------------------------------------------------------
; Display Found: filename
;-------------------------------------------------------------------------------
DISPLAY_FOUND
           SUBENTRY
           jsr MSG_CLR
           jsr MSG_DISPLAY

           ldx #M_FOUND_L
@          lda M_FOUND-1,X
           sta MSG_BUF-1,X
           dex
           bne @-

           ldx #10
@          lda TH_NAME-1,X
           sta MSG_BUF-1+M_FOUND_L,X
           dex
           bne @-
           jsr MSG_DISPLAY
           SUBEXIT

M_FOUND    dta c'Found: '
M_FOUND_L  equ *-M_FOUND
;-------------------------------------------------------------------------------
; Display program title
;-------------------------------------------------------------------------------
DISPLAY_TITLE SUBENTRY
           jsr MSG_CLR

           ldx #M_TITLE_L
@          lda M_TITLE-1,X
           sta MSG_BUF-1,X
           dex
           bne @-

           jsr MSG_DISPLAY

           SUBEXIT
M_TITLE    dta 125,c'Backup Tape'
M_TITLE_L  equ *-M_TITLE
;-------------------------------------------------------------------------------
; Display PRESS START to begin backup
;-------------------------------------------------------------------------------
DISPLAY_START_TO_LOAD SUBENTRY
           jsr MSG_CLR

           ldx #M_START_TO_LOAD_L
@          lda M_START_TO_LOAD-1,X
           sta MSG_BUF-1,X
           dex
           bne @-
           jsr MSG_DISPLAY

           SUBEXIT
M_START_TO_LOAD    dta c'Press START to begin backup'
M_START_TO_LOAD_L  equ *-M_START_TO_LOAD

;-------------------------------------------------------------------------------
; Display START or RESET
;-------------------------------------------------------------------------------
DISPLAY_START_OR_RESET SUBENTRY
           jsr MSG_CLR

           ldx #M_START_OR_RESET_L
@          lda M_START_OR_RESET-1,X
           sta MSG_BUF-1,X
           dex
           bne @-
           jsr MSG_DISPLAY

           SUBEXIT
M_START_OR_RESET    dta c'START to continue or RESET to abort'
M_START_OR_RESET_L  equ *-M_START_OR_RESET

;-------------------------------------------------------------------------------
; Display 'File loaded OK'
;-------------------------------------------------------------------------------
DISPLAY_LOADED_OK 
           SUBENTRY
           jsr MSG_CLR

           ldx #M_LOADED_OK_L
@          lda M_LOADED_OK-1,X
           sta MSG_BUF-1,X
           dex
           bne @-
           jsr MSG_DISPLAY

           SUBEXIT
M_LOADED_OK    dta c'File loaded OK'
M_LOADED_OK_L  equ *-M_LOADED_OK

;-------------------------------------------------------------------------------
; Display 'Load error'
;-------------------------------------------------------------------------------
DISPLAY_LOAD_ERROR 
           SUBENTRY
           jsr MSG_CLR

           ldx #M_LOAD_ERROR_L
@          lda M_LOAD_ERROR-1,X
           sta MSG_BUF-1,X
           dex
           bne @-
           jsr MSG_DISPLAY

           SUBEXIT
M_LOAD_ERROR    dta c'Load Error'
M_LOAD_ERROR_L  equ *-M_LOAD_ERROR


;-------------------------------------------------------------------------------
; Display 'Disk identification failed'
;-------------------------------------------------------------------------------
DISPLAY_VERIFY1_FAILED
           SUBENTRY
           jsr MSG_CLR
           jsr MSG_DISPLAY

           ldx #M_VERIFY1_FAILED_L
@          lda M_VERIFY1_FAILED-1,X
           sta MSG_BUF-1,X
           dex
           bne @-
           jsr MSG_DISPLAY

           SUBEXIT
M_VERIFY1_FAILED    dta c'Disk identification failed. Reboot.'
M_VERIFY1_FAILED_L equ *-M_VERIFY1_FAILED

;-------------------------------------------------------------------------------
; Display 'Disk not pristine'
;-------------------------------------------------------------------------------
DISPLAY_VERIFY2_FAILED
           SUBENTRY
           jsr MSG_CLR
           jsr MSG_DISPLAY

           ldx #M_VERIFY2_FAILED1_L
@          lda M_VERIFY2_FAILED1-1,X
           sta MSG_BUF-1,X
           dex
           bne @-
           jsr MSG_DISPLAY

           jsr MSG_CLR
           ldx #M_VERIFY2_FAILED2_L
@          lda M_VERIFY2_FAILED2-1,X
           sta MSG_BUF-1,X
           dex
           bne @-
           jsr MSG_DISPLAY
           jsr MSG_CLR
           jsr MSG_DISPLAY

           SUBEXIT
M_VERIFY2_FAILED1    dta c'Disk not pristine. '
                     dta c'If you continue,'*
M_VERIFY2_FAILED1_L equ *-M_VERIFY2_FAILED1
M_VERIFY2_FAILED2    dta c'all existing data will be lost.'*
M_VERIFY2_FAILED2_L equ *-M_VERIFY2_FAILED2

;-------------------------------------------------------------------------------
; Display 'Disk not writable'
;-------------------------------------------------------------------------------
DISPLAY_VERIFY3_FAILED
           SUBENTRY
           jsr MSG_CLR
           jsr MSG_DISPLAY

           ldx #M_VERIFY3_FAILED_L
@          lda M_VERIFY3_FAILED-1,X
           sta MSG_BUF-1,X
           dex
           bne @-
           jsr MSG_DISPLAY

           SUBEXIT
M_VERIFY3_FAILED   dta c'Disk not writable'
M_VERIFY3_FAILED_L equ *-M_VERIFY3_FAILED

;-------------------------------------------------------------------------------
; Display 'Writing data to disk'
;-------------------------------------------------------------------------------
DISPLAY_WRITING_TO_DISK
           SUBENTRY
           jsr MSG_CLR

           ldx #M_WRITING_TO_DISK_L
@          lda M_WRITING_TO_DISK-1,X
           sta MSG_BUF-1,X
           dex
           bne @-
           jsr MSG_DISPLAY

           SUBEXIT
M_WRITING_TO_DISK   dta c'Writing data to disk...'
M_WRITING_TO_DISK_L equ *-M_WRITING_TO_DISK
;-------------------------------------------------------------------------------
; Display 'Writing data to disk OK'
;-------------------------------------------------------------------------------
DISPLAY_WRITTEN_TO_DISK
           SUBENTRY
           jsr MSG_CLR

           ldx #M_WRITTEN_TO_DISK_L
@          lda M_WRITTEN_TO_DISK-1,X
           sta MSG_BUF-1,X
           dex
           bne @-
           jsr MSG_DISPLAY

           SUBEXIT
M_WRITTEN_TO_DISK   dta c'Data written to disk'
M_WRITTEN_TO_DISK_L equ *-M_WRITTEN_TO_DISK

;-------------------------------------------------------------------------------
; Display 'Writing to disk fuiled'
;-------------------------------------------------------------------------------
DISPLAY_WRITING_FAILED
           SUBENTRY
           jsr MSG_CLR

           ldx #M_WRITING_FAILED_L
@          lda M_WRITING_FAILED-1,X
           sta MSG_BUF-1,X
           dex
           bne @-
           jsr MSG_DISPLAY

           SUBEXIT
M_WRITING_FAILED   dta c'Writing to disk failed'
M_WRITING_FAILED_L   equ *-M_WRITING_FAILED
;===============================================================================
; Messaging support
;===============================================================================
;-------------------------------------------------------------------------------
; Message buffer
;-------------------------------------------------------------------------------
MSG_BUF     dta c'123456789012345678901234567890123456',$9B
MSG_BUF_L   EQU *-MSG_BUF-1

;-------------------------------------------------------------------------------
; Clear message
;-------------------------------------------------------------------------------
MSG_CLR    SUBENTRY
           lda #32 
           ldx #MSG_BUF_L
MSG_C_L1   sta MSG_BUF-1,X
           dex
           bne MSG_C_L1
           SUBEXIT              

;-------------------------------------------------------------------------------
;Specialized message - Disk error
;-------------------------------------------------------------------------------
MSG_DISK_ERROR
           SUBENTRY
           jsr  MSG_CLR           ;Clear buffer
           COPYMSG MDE_TEXT MDE_TEXT_L

           lda  W_DSKIO_CODE      ;Get the code
           and  #$F0              ;Mask high nibble
           lsr                    ;Shift to position
           lsr
           lsr
           lsr
           tax                    ;Get index to table
           lda C_HEXTAB,X
           ora #$80               ;Make it inverse
           sta MSG_BUF+MDE_TEXT_L+1 ;Store to the message buffer

           lda W_DSKIO_CODE       ;Get the code again
           and #$0F               ;Mask the low nibble
           tax
           lda C_HEXTAB,X
           ora #$80
           sta MSG_BUF+MDE_TEXT_L+2          

           jsr MSG_DISPLAY        ;Display the message
           SUBEXIT 
MDE_TEXT   dta c'I/O Error: '
MDE_TEXT_L equ *-MDE_TEXT
;-------------------------------------------------------------------------------
; Display message buffer using CIO, channel 0.
;-------------------------------------------------------------------------------
MSG_DISPLAY SUBENTRY
           lda #11                  ;Requesting PUTCHAR
           sta CIO0_OP
           lda #<MSG_BUF
           sta CIO0_BUFLO
           lda #>MSG_BUF
           sta CIO0_BUFHI
           lda #MSG_BUF_L+1
           sta CIO0_LENLO
           ldx #0                  ;Channel 0
           stx CIO0_LENHI
           jsr CIOV                ;Call CIO
           SUBEXIT
;===============================================================================
; Miscellaneous routines
;===============================================================================
;-----------------------------------------------------------------------
; Wait for VBLANK
;-----------------------------------------------------------------------
WAIT_FOR_VBLANK    lda #VBI_VCOUNT             ;Get the desired value
WFV_1              cmp VCOUNT                  ;Check
                   bne WFV_1                   ;If equal, keep checking
WFV_2              cmp VCOUNT
                   beq WFV_2                   
                   rts             
;-----------------------------------------------------------------------
; Short delay
;-----------------------------------------------------------------------
DELAY_3S           ldy #3*50               ;Delay for name display
                   jmp DELAY_WAIT                   
DELAY_TENTH        ldy #5                  ;Short delay, 0.1 sec
DELAY_CUSTOM_Y     jmp DELAY_WAIT
 
DELAY_WAIT         jsr WAIT_FOR_VBLANK     ;Wait for VBLANK
                   dey                     ;Decrement counter
                   bne DELAY_WAIT          ;Repeat until not zero
DELAY_END          rts
;-------------------------------------------------------------------------------
;Reset POKEY
;-------------------------------------------------------------------------------
RESET_POKEY        SUBENTRY 
                   ldx #$0F
                   lda #0
@                  sta $D200-1,X
                   dex
                   bne @-
                   SUBEXIT ,
;-------------------------------------------------------------------------------            
;Wait for the START key
;-------------------------------------------------------------------------------            
WAIT_START  SUBENTRY
S_LOOP1     lda CONSOL
            cmp #6
            beq S_LOOP1
S_LOOP2     lda CONSOL
            cmp #6
            bne S_LOOP2
            SUBEXIT
;-------------------------------------------------------------------------------
; Endless loop
;-------------------------------------------------------------------------------
ENDLESS     jmp ENDLESS 
;===============================================================================
;Initialization of DOS vectors
;===============================================================================
DOSINIT     SUBENTRY
            lda #0                        ;Warm start
            sta COLDST                    
            lda #01                       ;Disk boot successfull
            sta BOOT
            lda #<PROGRAM_BEGIN           ;DOSINI to loader entry
            sta DOSINI
            lda #>PROGRAM_BEGIN
            sta DOSINI+1
            SUBEXIT
;===============================================================================
; Main menu
;===============================================================================
SHOWMENU    SUBENTRY

            jsr MSG_CLR
            COPYMSG SM_M_TITLE1 SM_M_TITLE1_L
            jsr MSG_DISPLAY
            
            jsr MSG_CLR
            COPYMSG SM_M_TITLE2 SM_M_TITLE2_L
            jsr MSG_DISPLAY

            jsr MSG_CLR
            jsr MSG_DISPLAY

            jsr MSG_CLR
            COPYMSG SM_M_ITEM0 SM_M_ITEM0_L
            jsr MSG_DISPLAY

            jsr MSG_CLR
            COPYMSG SM_M_ITEM1 SM_M_ITEM1_L
            jsr MSG_DISPLAY

            jsr MSG_CLR
            COPYMSG SM_M_ITEM2 SM_M_ITEM2_L
            jsr MSG_DISPLAY

SM_KEY      lda #255              ;Clear key
            sta CH
            jsr WAIT_FOR_VBLANK

SM_KEY_L    lda CH
SM_KEY0     cmp #$00                 ;Is that 'L'?
            bne SM_KEY1              ;No, try another one
            lda #MENU_CODE_LISTING   ;Set code to 'listing'
            jmp SM_DONE              ;And be done

SM_KEY1     cmp #$15                 ;Is that 'B'?
            bne SM_KEY2
            lda #MENU_CODE_BACKUP
            jmp SM_DONE

SM_KEY2     cmp #$28                 ;Is that 'R'
            bne SM_KEY3
            lda #MENU_CODE_RECORD
            jmp SM_DONE

SM_KEY3     cmp #92                  ;Is that SHIFT-ESC?
            bne SM_KEY_L            
            lda #MENU_CODE_EXIT
            jmp SM_DONE

SM_DONE     sta ZP_RETCODE 
            SUBEXIT

SM_M_TITLE1   dta 125,c'BACKUP T/D Utility Disk 1.01'
SM_M_TITLE1_L equ *-SM_M_TITLE1
SM_M_TITLE2   dta c'(c) 2024 BAKTRA Software'
SM_M_TITLE2_L equ *-SM_M_TITLE2

SM_M_ITEM0    dta c'(L) List files on disk'
SM_M_ITEM0_L  equ *-SM_M_ITEM0   

SM_M_ITEM1    dta c'(B) Backup tape'
SM_M_ITEM1_L  equ *-SM_M_ITEM1

SM_M_ITEM2    dta c'(R) Record on tape'
SM_M_ITEM2_L  equ *-SM_M_ITEM2
 
;===============================================================================
; Listing mainline
;===============================================================================
OP_LISTING 
;Display an empty line
              jsr MSG_CLR
              jsr MSG_DISPLAY
              lda #1
              sta DSPFLG

;Read the pristine indicator
              lda #<SNO_PRISTINE
              sta ZP_D_SECLO
              lda #>SNO_PRISTINE
              sta ZP_D_SECHI
              jsr DISK_READSECT
              lda ZP_RETCODE
              beq @+
              jmp OPL_FAILURE
@
              lda SEC_BUFFER
              cmp #$55                     
              bne @+ 
              jmp OPL_COMPLETE
@
              lda #<SNO_DATA
              sta ZP_D_SECLO
              lda #>SNO_DATA
              sta ZP_D_SECHI

;Read the next sector
OPL_LOOP      jsr DISK_READSECT
              lda ZP_RETCODE
              beq @+
              jmp OPL_FAILURE
@
;Check if it is 'H' or 'E'
              lda SEC_BUFFER
              cmp #'E'
              beq OPL_COMPLETE
              cmp #'H'
              beq @+
              jmp OPL_FAILURE
@
;Process the 'H' block - display message
              jsr MSG_CLR
              ldx #10
@             lda SEC_BUFFER+TH_NAME-THEADER-1+1+2,X
              sta MSG_BUF-1,X
              dex
              bne @-
              jsr MSG_DISPLAY

;Advance to the following 'D' block
              inc ZP_D_SECLO
              bne @+
              inc ZP_D_SECHI
              beq OPL_FAILURE
@             jsr DISK_READSECT
              lda ZP_RETCODE
              bne OPL_FAILURE

;Check if really 'D' block
              lda SEC_BUFFER
              cmp #'D'
              bne OPL_FAILURE

;Calculate the total length of the data
              lda SEC_BUFFER+1          ;Get the value from sector
              sta ZP_WK_LENLO
              lda SEC_BUFFER+2
              sta ZP_WK_LENHI

              clc                 ;Increment by 3 to get total bytes to skip 
              lda ZP_WK_LENLO
              adc #3
              sta ZP_WK_LENLO
              lda ZP_WK_LENHI
              adc #0
              sta ZP_WK_LENHI

;Calculate the number of sectors to skip - high value
              ldx #2
@             lda ZP_WK_LENHI
              clc
              adc ZP_D_SECLO
              sta ZP_D_SECLO
              lda ZP_D_SECHI
              adc #0
              sta ZP_D_SECHI
              dex
              bne @-   

;Continue calculation - 0,1,2 additional sectors
              ldy #0
              lda ZP_WK_LENLO
              beq OPL_REMSEC_3
              ldy #1
              cmp #128
              bcc OPL_REMSEC_3
              ldy #2

;Finalize the calculation
OPL_REMSEC_3  tya
              clc
              adc ZP_D_SECLO
              sta ZP_D_SECLO
              lda ZP_D_SECHI
              adc #0
              sta ZP_D_SECHI

;Ready to process the next file
              jmp OPL_LOOP

OPL_COMPLETE  jsr MSG_CLR
              COPYMSG OPL_M_COMPLETE OPL_M_COMPLETE_L
              jsr MSG_DISPLAY
              jsr WAIT_START
OPL_EXIT      lda #0
              sta DSPFLG 
              jmp PROGRAM_BEGIN

OPL_FAILURE   jsr MSG_CLR
              COPYMSG OPL_M_NOREAD OPL_M_NOREAD_L
              jsr MSG_DISPLAY
              jsr WAIT_START
              jmp OPL_EXIT

OPL_M_NOREAD dta c'Unable to read disk. Press START.'
OPL_M_NOREAD_L equ *-OPL_M_NOREAD
OPL_M_COMPLETE dta c'Listing complete. Press START.'
OPL_M_COMPLETE_L equ *-OPL_M_COMPLETE

;-------------------------------------------------------------------------------
; Disk - readbyte, returns byte in A
;-------------------------------------------------------------------------------
DISK_READBYTE SUBENTRY
             lda #0
             sta ZP_RETCODE
            
             ldx ZP_D_BUFPTR            ;Get current buffer pointer
             lda SEC_BUFFER,X           
             sta ZP_W_BYTE              ;Get the next byte
             inx                        ;Increment the buffer pointer
             stx ZP_D_BUFPTR            ;Store updated buffer pointer  
             cpx #128                   ;Is there a wraparound ?
             bne DRB_DONE               ;No, skip

             ldx #0                     ;Reset buffer pointer to zero
             stx ZP_D_BUFPTR            ;Store buffer pointer
             inc ZP_D_SECLO             ;Increment sector counter
             bne @+
             inc ZP_D_SECHI
@            jsr DISK_READSECT          ;Read next sector

DRB_DONE     pla                        ;Special exit linkage
             tax
             pla
             tay
             pla
             lda ZP_W_BYTE
             rts       

DISK_READ_DRAIN SUBENTRY
             ldx ZP_D_BUFPTR             ;If pointer at zero, done
             beq DRD_DONE

             inc ZP_D_SECLO              ;Increment the sector counter
             bne @+
             inc ZP_D_SECHI
@            ldx #0                      ;Reset the buffer pointer
             stx ZP_D_BUFPTR
DRD_DONE     SUBEXIT
;-------------------------------------------------------------------------------
; Read sector, given by ZP_D_SECLO and ZP_D_SECHI
;-------------------------------------------------------------------------------
DISK_READSECT SUBENTRY
           
            lda #0
            sta ZP_RETCODE

            lda #1
            sta DUNIT 
            lda ZP_D_SECLO
            sta DAUX1     
            lda ZP_D_SECHI
            sta DAUX2
            lda #$52     ; "R"
            sta DCOMND
            lda #>SEC_BUFFER
            sta DBUFHI  
            lda #<SEC_BUFFER 
            sta DBUFLO
            jsr DSKINV

            lda DSTATS               ;Check the status
            sta W_DSKIO_CODE         ;Keep for later 
            cmp #1                   ;Is status OK (==1)?
            bne DR_BAD               ;No, return 8

DR_DONE
            SUBEXIT

DR_BAD      lda #8                    ;Set RC=8
            sta ZP_RETCODE
            jsr MSG_DISK_ERROR
            jmp DR_DONE
            SUBEXIT

;===============================================================================
; Operation record tape
;===============================================================================
OP_RECORD 
;Display message and wait for the START key
            jsr MSG_CLR
            COPYMSG OPR_M_RECORD1,OPR_M_RECORD1_L
            jsr MSG_DISPLAY
            jsr MSG_CLR
            jsr MSG_DISPLAY
            COPYMSG OPR_M_RECORD2,OPR_M_RECORD2_L
            jsr MSG_DISPLAY
            jsr MSG_CLR
            COPYMSG OPR_M_RECORD3,OPR_M_RECORD3_L
            jsr MSG_DISPLAY
            jsr MSG_CLR
            COPYMSG OPR_M_RECORD4,OPR_M_RECORD4_L
            jsr MSG_DISPLAY
 
            lda #255
            sta CH
OP_RECORD_WAIT_KEY
            lda CH
            cmp #$3F                    ;Is that 'A'
            bne @+                      ;Nope, try other
            lda #10                     ;Yes, set 10 VBLs (0.2 s)
            bne OP_RECORD_STORE_SEP     ;And go store it

@           cmp #$15                    ;Is that 'B'
            bne @+                      ;Nope, try other
            lda #50                     ;Yes, set 50 VBLs (1.0 s)
            bne OP_RECORD_STORE_SEP

@           cmp #$12                    ;Is that 'C'
            bne OP_RECORD_WAIT_KEY      ;Nope, try again
            lda #150                    ;Yes, set 150 VBLs (3.0 s)                  

OP_RECORD_STORE_SEP
            sta W_REC_SEPDURATION

            jsr MSG_CLR
            COPYMSG OPR_M_RECORD5,OPR_M_RECORD5_L
            jsr MSG_DISPLAY
            jsr WAIT_START

;Check if disk is pristine
            lda #<SNO_PRISTINE
            sta ZP_D_SECLO
            lda #>SNO_PRISTINE
            sta ZP_D_SECHI
            jsr DISK_READSECT
            lda ZP_RETCODE
            beq OP_VERPRIST
            jmp OP_BADREAD
OP_VERPRIST
            lda SEC_BUFFER        ;Check the pristine indicator
            cmp #$55              ;It is present, nothing to record
            bne OP_READ1SEC
            jmp OP_COMPLETE

;Begin reading at the beginning of the data sectors
OP_READ1SEC
            lda #<SNO_DATA        
            sta ZP_D_SECLO
            lda #>SNO_DATA
            sta ZP_D_SECHI
            lda #0
            sta ZP_D_BUFPTR

;Read the sector - prospective header or end marker
OP_NEWFILE  jsr DISK_READSECT
            lda ZP_RETCODE
            beq OP_1SECOK
            jmp OP_BADREAD  

OP_1SECOK
            lda SEC_BUFFER            ;Check where are we
            cmp #'E'                  ;If end of filesystem, we are done.
            bne @+
            jmp OP_COMPLETE
@           cmp #'H'                  ;If header, it is good
            beq OP_NEWHEADER
            jmp OP_BADREAD            ;Found something unexpected, over

;Process the header
OP_NEWHEADER 
            ldx #THEADER_LEN         ;Copy the header data to header buffer
@           lda SEC_BUFFER+3-1,X
            sta THEADER-1,X
            dex
            bne @-

            jsr MSG_CLR             ;Display file name
            COPYMSG THEADER+1 10
            jsr MSG_DISPLAY

;Reset all disk and memory pointers 
OP_READRST  lda #0                  ;Reset sector buffer pointer
            sta ZP_D_BUFPTR
            inc ZP_D_SECLO          ;Advance to the next sector
            bne @+
            inc ZP_D_SECHI

@           lda ZP_BASEBANK         ;Reset pointers to the extended RAM
            sta ZP_CURRBANK
            sta PORTB
            lda #0
            sta ZP_W_BUFRLO
            lda #16384/256
            sta ZP_W_BUFRHI

            jsr DISK_READSECT      ;Read first data sector
            lda ZP_RETCODE
            beq OP_READRST_OK
            jmp OP_BADREAD

OP_READRST_OK
            jsr DISK_READBYTE     ;Get next byte
            ldy ZP_RETCODE        ;Check if OK
            beq OP_DH_00          
            jmp OP_BADREAD

OP_DH_00    cmp #'D'              ;Check if Data block
            beq OP_DH_10
            jmp OP_BADREAD

OP_DH_10    jsr DISK_READBYTE     ;Get the total length
            sta ZP_WK_LENLO
            jsr DISK_READBYTE
            sta ZP_WK_LENHI

            lda #0                ;We have a length and counter
            sta ZP_WK_LO
            sta ZP_WK_HI
            
            lda #0
            sta ZP_W_BUFRLO
            lda #16384/256
            sta ZP_W_BUFRHI 

OP_FILELOOP 
            jsr DISK_READBYTE     ;Get next byte

            ldy #0                ;Store the byte to the bufer
            sta (ZP_W_BUFRLO),Y

            inc ZP_W_BUFRLO      ;Increment lo pointer
            bne OP_FL_BUF10      ;No wraparound, skip
            inc ZP_W_BUFRHI      ;Increment hi pointer
            lda ZP_W_BUFRHI      ;Is that beyond the bank?
            cmp #32768/256
            bne OP_FL_BUF10       ;It is not, just skip

            lda #16384/256        ;Reset hi pointer to the bank beginning
            sta ZP_W_BUFRHI
            clc                   ;Advance to the next bank
            lda ZP_CURRBANK
            adc #4
            sta ZP_CURRBANK      
            sta PORTB             ;Tell MMU to get to the new bank
OP_FL_BUF10 

            inc ZP_WK_LO          ;Just increment counter
            bne @+
            inc ZP_WK_HI

@           lda ZP_WK_HI
            cmp ZP_WK_LENHI
            bne OP_FILELOOP_CONT
            lda ZP_WK_LO
            cmp ZP_WK_LENLO
            beq OP_FILELOOP_END
OP_FILELOOP_CONT
            jmp OP_FILELOOP

OP_FILELOOP_END
            jsr DISK_READ_DRAIN

;Keep information on the range for further processing
            lda ZP_CURRBANK
            sta W_REC_BANK
            lda ZP_W_BUFRLO
            sta W_REC_PTRLO
            lda ZP_W_BUFRHI
            sta W_REC_PTRHI

;Set the default banks
            lda ZP_BASEBANK
            sta ZP_CURRBANK
            sta ZP_LASTBANK
            sta PORTB

;Record the file
OP_RECORD_START
            ldy #40                      ;Allow disk to calm down (0.8 s)
            jsr DELAY_CUSTOM_Y
 
            lda #52                      ;Motor on
            sta PACTL

            ldy W_REC_SEPDURATION        ;How long the separator is?
            jsr DELAY_CUSTOM_Y           ;Write silence accordingly.

;Write the header
OP_RECORD_HEADER
            lda #<(THEADER-1)
            sta BUFRLO
            lda #>(THEADER-1)
            sta BUFRHI
            lda #<(TH_END)
            sta BFENLO
            lda #>(TH_END)
            sta BFENHI
            lda #0
            jsr RESET_POKEY
            jsr WRITE_BLOCK

;Write the data block. The data of the data block was loaded to $4000
;and continues to further banks if needed.
OP_RECORD_RANGE

;Restore the range
            lda W_REC_BANK
            sta ZP_CURRBANK
            lda W_REC_PTRLO
            sta ZP_W_BUFRLO
            lda W_REC_PTRHI
            sta ZP_W_BUFRHI

OP_RECORD_RANGE_DONE

            lda #255                      ;Point before the bank
            sta BUFRLO
            lda #[16384-1]/256 
            sta BUFRHI

            lda ZP_W_BUFRLO               ;End range + last bank
            sta BFENLO
            lda ZP_W_BUFRHI
            sta BFENHI
            lda ZP_CURRBANK
            sta ZP_LASTBANK

;Reset the bank settings
            lda ZP_BASEBANK
            sta ZP_CURRBANK
            sta PORTB

;Record the turbo block
            lda #255
            jsr WRITE_BLOCK

            lda #60                       ;Motor off
            sta PACTL
            jsr RESET_POKEY

;Reset the bank settings again
            lda ZP_BASEBANK
            sta ZP_CURRBANK
            sta PORTB

;Continue to the next file
            jmp OP_NEWFILE

;Recording completion
OP_COMPLETE jsr MSG_CLR
            COPYMSG OPR_M_REC_COMPLETE,OPR_M_REC_COMPLETE_L
            jsr MSG_DISPLAY
            jsr WAIT_START
OP_EXIT     jmp PROGRAM_BEGIN

;Handle bad sector read
OP_BADREAD
            jsr MSG_CLR
            COPYMSG OPL_M_NOREAD,OPL_M_NOREAD_L
            jsr MSG_DISPLAY
            jsr WAIT_START
            jmp OP_EXIT


OPR_M_RECORD1 dta 125,'Record on tape'
OPR_M_RECORD1_L equ *-OPR_M_RECORD1

OPR_M_RECORD2 dta 'Press A for (0.2 s) gaps'
OPR_M_RECORD2_L equ *-OPR_M_RECORD2
OPR_M_RECORD3 dta 'Press B for (1.0 s) gaps'
OPR_M_RECORD3_L equ *-OPR_M_RECORD3
OPR_M_RECORD4 dta 'Press C for (3.0 s) gaps'
OPR_M_RECORD4_L equ *-OPR_M_RECORD4

OPR_M_RECORD5 dta 'Press START to begin recording'
OPR_M_RECORD5_L equ *-OPR_M_RECORD5

OPR_M_REC_COMPLETE dta 'Recording complete. Press START'
OPR_M_REC_COMPLETE_L equ *-OPR_M_REC_COMPLETE
;=======================================================================
; Write Turbo 2000 block
; BUFRLO,BUFRHI  - Pointer right before first byte
; BFENLO,BFENHI  - Pointer past the last byte of the block
; A              - Identification byte
;=======================================================================
WRITE_BLOCK    pha                    ;Keep A in the stack

               jsr WR_RESET_ALL       ;Reset coder
               pla                    ;Restore A
               sta ICAX6Z             ;Keep identification byte
               sta CHKSUM             ;Use as base for check sum
            
               lda #192
               sta AUDCTL
               
               ldx #$19               ;Prepare pilot duration for header                 
               lda ICAX6Z
               beq WR_PILOTLEN        ;If ID zero, skip
               ldx #$0D               ;Prepare pilot tone duration for data
WR_PILOTLEN    stx STATUS             ;Keep status
WR_PILOTL1     dey
               bne WR_PILOTL1         ;Wait
               lda #3                 
               sta SKCTL              ;Generate half of pulse
               ldy #119
WR_PILOTL2     dey
               bne WR_PILOTL2
               lda #11
               sta SKCTL              ;Generate half of pulse
               ldy #118
               dex
               bne WR_PILOTL1         ;Continue with part of pilot tone  
               dey
               dey
               dec STATUS              
               bne WR_PILOTL1         ;Continue with part of pilot tone

            
               ldy #32                ;Generate SYNC pulse
WR_SYNC1       dey
               bne WR_SYNC1
               lda #3
               sta SKCTL
               ldy #39
WR_SYNC2       dey
               bne WR_SYNC2
               lda #11
               sta SKCTL
            
               ldy #43                ;Generate bytes
               sec
               jmp WR_GBYTE
            
WR_PICKBYTE    lda BFENLO             ;Check buffer range (lo)
               cmp BUFRLO
               bne WR_DOBYTE          ;No match, continue

               lda BFENHI             ;Check buffer range (hi)
               cmp BUFRHI
               bne WR_DOBYTE          ;No match, continue

               lda ZP_CURRBANK        ;Check if the last bank
               cmp ZP_LASTBANK        
               beq WR_GBYTE_CSM       ;Last bank, go for checksum
WR_DOBYTE      sec           
               lda (BUFRLO,X)
               sta ICAX6Z
               eor CHKSUM
               sta CHKSUM
            
WR_GBYTE       jmp WR_GBYTE_NBIT      ;Go and generate byte
WR_GBYTE_CSM   lda CHKSUM             
               sta ICAX6Z
               dex
               sec
               bcs WR_GBYTE
WR_GBYTE_W1    dey
               bne WR_GBYTE_W1
               bcc WR_GBYTE_HI
               ldy #48
WR_GBYTE_W2    dey
               bne WR_GBYTE_W2
WR_GBYTE_HI    lda #3
               sta SKCTL
               ldy #46
               bcc WR_GBYTE_W3
               ldy #94
WR_GBYTE_W3    dey
               bne WR_GBYTE_W3
               lda #11
               sta SKCTL
               clc
               ldy #44
            
WR_GBYTE_NBIT  rol ICAX6Z             ;Still bits to go
               bne WR_GBYTE_W1        ;Yes, write bit

WR_ADVBUF_00   ldy #32
               inc BUFRLO             ;Increment lo pointer
               bne WR_ADVBUF_90       ;No wraparound, done
               ldy #30  
               inc BUFRHI             ;Increment hi pointer
               lda BUFRHI             ;Check if beyond the bank
               cmp #32768/256         ;Is beyond the bank?
               bne WR_ADVBUF_90       ;No, we are done

               ldy #26
               clc                    ;Advance to the next bank
               lda ZP_CURRBANK
               adc #4
               sta ZP_CURRBANK
               sta PORTB
               lda #16384/256         ;Reset the hi pointer
               sta BUFRHI             ;And store it

WR_ADVBUF_90   txa
               beq WR_PICKBYTE        ;Get other byte from buffer
            
WR_GBYTE_W4    dey                    ;Keep waiting 
               bne WR_GBYTE_W4             
            
               lda #3                 ;Write safety pulse
               sta SKCTL
               lda #0
               sta AUDCTL
               jmp WR_TERM            ;Terminate
               
WR_TERM        lda #64
               sta NMIEN
               sta IRQEN
               jsr WAIT_FOR_VBLANK   
               lda #34                ;New
               sta DMACLT             ;New
               rts
               
WR_RESET_ALL   ldy #0
               sty STATUS
               sty CHKSUM
               sty NMIEN
               sty DMACLT
               sty IRQEN
               clc
               rts

;===============================================================================
; Global static data area
;===============================================================================
               dta c'@CDATA'
C_HEXTAB       dta c'0123456789ABCDEF'     
;===============================================================================
; Data areas
;===============================================================================
               dta c'@WDATA'
;Turbo header buffer
THEADER
TH_TYPE     dta 0
TH_NAME     dta c'..........'
TH_LOAD     dta 0,0
TH_LEN      dta 0,0
TH_RUN      dta 0,0
TH_END      EQU *
THEADER_LEN EQU *-THEADER

;Disk operation error code backup
W_DSKIO_CODE dta 0

;Turbo data recording
W_REC_BANK   dta 0
W_REC_PTRLO  dta 0
W_REC_PTRHI  dta 0
W_REC_SEPDURATION dta 10

;Miscellaneous
W_FIRST_RUN  dta $FF
W_FIRST_BANK dta 0

;Writing data to disk
W_WRITE_LASTH_SECLO dta 0
W_WRITE_LASTH_SECHI dta 0
 
;===============================================================================
; Filler
;===============================================================================
PROGRAM_END EQU *
            .REPT 4096-[PROGRAM_END-PROGRAM_START]
            .byte 0
            .ENDR
