;*******************************************************************************
;BACKUP T/D (C)  2024 BAKTRA Software
;Mainline code. Assemble with MADS.
;
;This utility automatically copies Czechoslovak Turbo 2000 files to a raw disk
;format. The raw disk format is then processed by the TURGEN utility, to
;create binary load files or TSFXes.
;
;Minimum of 128 KB of total RAM is required, because the turbo block is loaded
;into the banks of the extended memory. This allows to process files that
;are up to 64 KB long.
;*******************************************************************************
            ICL 'equates.asm'
            ICL 'auxmacs.mac'

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

            SNO_MARKING   EQU 25
            SNO_PRISTINE  EQU 26
            SNO_DATA      EQU 27

            VBI_VCOUNT    EQU 124

            
;-------------------------------------------------------------------------------
; Mainline code
;-------------------------------------------------------------------------------
            ORG PROGRAM_START
            .byte $00
            .byte [[PROGRAM_END-PROGRAM_START]/128]+1
            .word PROGRAM_START
            .word L05D2

;Initialize the copier
L05D2       jsr CASINIT       ;Setup DOS vectors   
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

;Prepare the base PORTB value
            lda PORTB         ;Get current settings
            and #($FF-$4-$8-$10) ;CPU eRAM + BANK 0
            sta ZP_BASEBANK
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
            
            lda #0            ;First byte of the header should be 0
            jsr L0631         ;Call Turbo 2000 block decoding subroutine
            bcc DEC_HEADER         ;If error occured, try to decode the header again
;-------------------------------------------------------------------------------
; Process the header
;-------------------------------------------------------------------------------
PROC_HEADER jsr DISPLAY_FOUND
            jsr SHORT_DELAY
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

BUF_B0      lda TH_LEN+1     ;Check length (number of pages)
            cmp #16384/256   ;Is that <16384
            bcc BUF_REM      ;Yes, done

BUF_B1      lda TH_LEN+1     ;Check length (number of pages)
            cmp #32768/256   ;Is that <32767
            bcs BUF_B2       ;No, try other amount
            beq BUF_B2

            clc              ;Calculate the last bank
            lda ZP_LASTBANK
            adc #4
            sta ZP_LASTBANK

            lda TH_LEN+1    ;Calculate length remainder.
            sec
            sbc #16384/256
            sta TH_LEN+1 
            jmp BUF_REM

BUF_B2      lda TH_LEN+1     ;Check length (number of pages)
            cmp #49152/256   ;Is that <49152
            bcs BUF_B3       ;No, try other amount
            beq BUF_B3

            clc
            lda ZP_LASTBANK  ;Calculate the last bank
            adc #8
            sta ZP_LASTBANK

            lda TH_LEN+1     ;Calculate length remainder.
            sec
            sbc #32768/256
            sta TH_LEN+1 
            jmp BUF_REM

BUF_B3      clc               ;Calculate the last bank               
            lda ZP_LASTBANK
            adc #12
            sta ZP_LASTBANK

            lda TH_LEN+1     ;Calculate length remainder
            sec
            sbc #49152/256
            sta TH_LEN+1 
            jmp BUF_REM

BUF_REM     lda TH_LEN       ;Count number of bytes
            sta BFENLO
            sta ZP_W_BFENLO  ;Backup the value for later writing

            clc              ;Count page number relative to 16384
            lda TH_LEN+1
            adc BUFRHI
            sta BFENHI
            sta ZP_W_BFENHI  ;Backup the value for later writing
;------------------------------------------------------------------------------- 
;Decode file data
;-------------------------------------------------------------------------------
            lda #255          ;First byte should be 255
            jsr L0631         ;Call Turbo 2000 block decoding subroutine
            bcs L0622         ;No error - jump
            
ERRDATA     jsr DISPLAY_LOAD_ERROR
            jsr SHORT_DELAY
            jmp DEC_HEADER         ;And then try to load another file
                        

;Decoding ok            
L0622       jsr DISPLAY_LOADED_OK
;-------------------------------------------------------------------------------
; Write the header data 
;-------------------------------------------------------------------------------
            jsr DISPLAY_WRITING_TO_DISK   ;Display message

;Reset the MAXRC
            lda #0
            sta ZP_W_MAXRC           

;Write byte indicating 'H' as header
            lda #'H'
            jsr WRITE_BYTE
;Write word indicating length of the header (17 bytes)
            lda #<17
            jsr WRITE_BYTE
            lda #>17
            jsr WRITE_BYTE

;Write data of the header
            ldx #0
@           lda THEADER,X
            jsr WRITE_BYTE
            inx 
            cpx #17
            bne @-
            jsr WRITE_FLUSH
            UPDATERC
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
            jsr WRITE_FORCE_ADVANCE

;Display message
            lda ZP_W_MAXRC
            beq WD_MSG_OK
            jsr DISPLAY_WRITING_FAILED
            jsr DISPLAY_START_OR_RESET
            jsr WAIT_START
 
WD_MSG_OK   jsr DISPLAY_WRITTEN_TO_DISK
            jsr SHORT_DELAY

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
            cmp #1                    ;Is status OK (==1)?
            bne DVE_BAD               ;No, return 8

DVE_COMPARE    
            ldx #DVE_T_EYE_L
@           lda SEC_BUFFER-1,X
            cmp DVE_T_EYE-1,X
            bne DVE_BAD
            dex
            bne @-
DVE_DONE
            SUBEXIT

DVE_BAD     lda #8                    ;Set RC=8
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
            cmp #1                    ;Is status OK (==1)?
            bne DVP_BAD               ;No, return 8

DVP_COMPARE                           ;Check for all $55s
            ldx #128
@           lda SEC_BUFFER-1,X
            cmp #$55
            bne DVP_BAD               ;If value not expected, bad
            dex
            bne @-
DVP_DONE
            SUBEXIT

DVP_BAD     lda #8                    ;Set RC=8
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
            cmp #1                    ;Is status OK (==1)?
            bne DVW_BAD               ;No, return 8

DVW_DONE    SUBEXIT

DVW_BAD     lda #8                    ;Set RC=8
            sta ZP_RETCODE
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
            bne  WB_DONE                  ;When no wraparound, skip

            jsr  WRITE_FLUSH              ;Flush the sector

            UPDATERC
           
            inc  ZP_D_SECLO               ;Increment lo sector number
            bne  WB_DONE                  ;When no wraparound, skip
            inc  ZP_D_SECHI               ;Increment hi sector counter

WB_DONE     stx ZP_D_BUFPTR           
            SUBEXIT
;-------------------------------------------------------------------------------
;Flush written data
;-------------------------------------------------------------------------------
WRITE_FLUSH SUBENTRY
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
            cmp #1                    ;Is status OK (==1)?
            bne WF_BAD               ;No, return 8

WF_DONE     SUBEXIT

WF_BAD      lda #8                    ;Set RC=8
            sta ZP_RETCODE
            jmp WF_DONE       
;-------------------------------------------------------------------------------
;Force advancement to the next sector
;-------------------------------------------------------------------------------
WRITE_FORCE_ADVANCE
            SUBENTRY
            ldx ZP_D_BUFPTR           ;Is pointer zero?
            beq WFA_DONE              ;No need to do anything
            ldx #0                    ;Reset the pointer to zero
            stx ZP_D_BUFPTR            
            inc ZP_D_SECLO            ;Increment the sector counter
            bne WFA_DONE
            inc ZP_D_SECHI
WFA_DONE    SUBEXIT 
     

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
M_TITLE    dta 125,c'TURGEN - BACKUP T/D 0.02'
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
M_LOADED_OK    dta c'  File loaded OK'
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
M_LOAD_ERROR    dta c'  Load Error'
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

           ldx #M_VERIFY2_FAILED_L
@          lda M_VERIFY2_FAILED-1,X
           sta MSG_BUF-1,X
           dex
           bne @-
           jsr MSG_DISPLAY

           SUBEXIT
M_VERIFY2_FAILED    dta c'Disk not pristine'
M_VERIFY2_FAILED_L equ *-M_VERIFY2_FAILED

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
M_WRITING_TO_DISK   dta c'  Writing data to disk...'
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
M_WRITTEN_TO_DISK   dta c'  Data written to disk'
M_WRITTEN_TO_DISK_L equ *-M_WRITTEN_TO_DISK

;-------------------------------------------------------------------------------
; Display 'Writing data failed'
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
M_WRITING_FAILED   dta c'  Writing to disk failed'
M_WRITING_FAILED_L   equ *-M_WRITING_FAILED

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
; Display message buffer using CIO, channel 0.
;-------------------------------------------------------------------------------
MSG_DISPLAY SUBENTRY
           lda #9                  ;Requesting PRINT
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
;-------------------------------------------------------------------------------
; Wait for VBLANK (using VCOUNT)
;-------------------------------------------------------------------------------
WAIT_FOR_VBLANK    SUBENTRY
                   lda #VBI_VCOUNT
WFV_1              cmp VCOUNT
                   bne WFV_1
                   SUBEXIT 
;-------------------------------------------------------------------------------
; Short delay 
;-------------------------------------------------------------------------------                   
SHORT_DELAY        SUBENTRY
                   ldy #220
DELAY_LOOP_E       ldx #255            
DELAY_LOOP_I       stx WSYNC
                   dex
                   bne DELAY_LOOP_I
                   dey 
                   bne DELAY_LOOP_E
                   SUBEXIT
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
CASINIT     SUBENTRY
            lda #0                        ;Warm start
            sta COLDST                    
            lda #02                       ;Cassette boot successfull
            sta BOOT
            lda #<PROGRAM_START           ;CASINI to loader entry
            sta CASINI
            lda #>PROGRAM_START
            sta CASINI+1
            SUBEXIT

PROGRAM_END EQU *
            .REPT 3072-[PROGRAM_END-PROGRAM_START]
            .byte 0
            .ENDR
