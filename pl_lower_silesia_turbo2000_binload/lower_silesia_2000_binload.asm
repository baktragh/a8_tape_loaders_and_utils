;===============================================
;Lower Silesia Turbo 2000 Binary Loader
;===============================================

.INCLUDE equates.asm

;
; Start of code
;
            *= 1792
;
;----------------------------------------------
;Initialization
;----------------------------------------------
            jmp PREINIT
            
START       lda #0
            sta FLG_FIRST     ;Reset to 0 
            sta COLDST
            lda #1            ;Indicate boot successful
            sta BOOT        
 
            lda #<CMN_RTS     ;Set default INITAD and RUNAD
            sta INITAD
            sta RUNAD
            lda #>CMN_RTS
            sta RUNAD+1
            sta INITAD+1

;----------------------------------------------
;Load dummy block (2 bytes + checksum)
;----------------------------------------------            
RD_DUMMY    lda #0
            sta BUFRLO
            lda #2           
            sta BFENLO
            
            lda #4            ;Set buffer (PAGE 4)
            sta BUFRHI
            sta BFENHI
            
            jsr READBLOCK     ;Decode block
            bcs START         ;Try again if failed
            
            lda #1            ;Indicate boot successful
            sta BOOT          
;----------------------------------------------
;Load segment header (4 bytes)
;----------------------------------------------                        
RD_SEGHDR   lda #<SGH_00      ;Set buffer
            sta BUFRLO
            lda #>SGH_00
            sta BUFRHI
            
            lda #<[SGH_03+1]
            sta BFENLO
            lda #>[SGH_03+1]
            sta BFENHI
            
            jsr READBLOCK    ;Decode block
            bcs RD_ERROR     ;If error, handle it
            
            jsr PROC_HD      ;Process segment header

;----------------------------------------------
;Load segment data 
;----------------------------------------------                                    
            lda SGH_00       ;Setup buffer using segment header
            sta BUFRLO
            lda SGH_01
            sta BUFRHI
            lda SGH_02
            sta BFENLO
            lda SGH_03
            sta BFENHI
                             
            clc              ;Fine tune buffer end 
            lda BFENLO
            adc #1
            sta BFENLO
            lda BFENHI
            adc #0
            sta BFENHI
SETINI            
            lda #<CMN_RTS
            sta INITAD       ;Setup default INITAD
            lda #>CMN_RTS
            sta INITAD+1
            
            jsr READBLOCK    ;Decode block
            bcs RD_ERROR     ;Handle error
            
L0867       jsr L086D        ;Emulate JSR(INITAD)
            jmp RD_SEGHDR    ;Proceed to the next header
L086D       jmp (INITAD)

FLG_FIRST   .byte 0          ;First segment indicator

;----------------------------------------------
; Common return from subroutine
;----------------------------------------------
CMN_RTS       rts


;----------------------------------------------
; Load error
;----------------------------------------------
RD_ERROR    jsr LEF8E
            jsr LC63E
            jmp START
            
;----------------------------------------------
; Process segment header
;----------------------------------------------
PROC_HD     lda SGH_00      ;Check if 0 0 0 0
            bne CHKFIRST
            lda SGH_01
            bne CHKFIRST
            lda SGH_02
            bne CHKFIRST
            lda SGH_03
            beq RUNIT       ;If yes, run program
    
CHKFIRST    lda FLG_FIRST   ;Is that first segment
            bne CMN_RTS     ;If no, header is processed
            
            lda SGH_00      ;First segment. Set RUNAD
            sta RUNAD
            lda SGH_01
            sta RUNAD+1
            inc FLG_FIRST   ;Indicate not first segment
            jmp CMN_RTS     ;Header is processed

;---------------------------------------------
;Run loaded program
;---------------------------------------------
RUNIT       jmp (RUNAD)
            
;--------------------------------------------
; Read single block
; BUFRLO,BFENLO - Point to the first byte
; BFENLO,BFENHI - Point past the last byte
; No identification byte
;--------------------------------------------
READBLOCK    lda NMIEN          ;Backup NMIEN
             sta BK_NMIEN      
             lda #52
             sta PACTL
             sta PBCTL
             jsr REABLOCKIMPL   ;Read block
             rts                ;Return
            
REABLOCKIMPL lda BUFRLO         ;Duplicate lower buffer address
             sta $00F0
             lda BUFRHI
             sta $00F1
            
             sec                ;Calculate length of the block
             lda BFENLO    
             sbc BUFRLO
             sta BLK_LENLO           
             lda BFENHI
             sbc BUFRHI
             sta BLK_LENHI
            
             jsr INIT_READ      ;Go Disable DMA and interrupts
WFORPILOT    jsr WFOREDGE       ;Go wait for edge
             bcs WFORPILOT      ;No edge, try again
            
             lda #248           ;Wait for some time and expect 255-248 pilot tone pulses
             sta PILOT_CNTR
PILOTDELAY   dex
             bne PILOTDELAY
            
             jsr MEASWIDTH      ;Measure width of pulse 
             bcs WFORPILOT      ;No pulse, start over
            
WFORSYNC     ldx #74            ;Set initial counter value   
             jsr MEASWIDTH      ;Measure width of pulse
             bcs WFORPILOT      ;No pulse, start over
             cpx #98            ;Check width
             bcc WFORPILOT      ;Bad width
             inc PILOT_CNTR     ;Count pilot tone pulses
             bne WFORSYNC       ;If not enough, keep waiting for pilot tone pulses
            
WFORSYNC2    ldx #162           ;Set initial counter value
             jsr WFOREDGE       ;Wait for edge
             bcs WFORPILOT      ;No edge, start over
             cpx #179           ;Check width
             bcs WFORSYNC2      ;Still pilot tone, continue waiting for sync
             jsr WFOREDGE       ;Wait for edge
             bcc GETDATA        ;If edge, continue 
            
             ldy #140           ;Go enable DMA and interrupts
             jmp TERM_READ      ;Terminate processing
            
GETDATA      ldx #188           ;Set initial counter value  
RESBYTE      lda #1             ;Start with bit 0           
             sta WKBYTE
CHKPULSE     jsr MEASWIDTH      ;Measure width
             bcc GETBYTE        ;If ok, go get byte
             ldy #141           ;If not ok, error
             jmp TERM_READ
GETBYTE      cpx #208           ;Determine 0 or 1
             rol WKBYTE         ;Place bit into byte
             ldx #188           ;Set initial counter value
             bcc CHKPULSE       ;Continue bit loop 
             
             lda PILOT_CNTR     ;Update checksum
             sta LOMEM
             eor WKBYTE
             sta PILOT_CNTR
             
             lda BLK_LENHI      ;Advance in buffer
             ora BLK_LENLO
             beq ENDBLK
             lda WKBYTE
             sta ($00F0),Y
             lda $00F0
             clc
             adc #1
             sta $00F0
             lda $00F1
             adc #0
             sta $00F1
             sec
             lda BLK_LENLO
             sbc #1
             sta BLK_LENLO
             lda BLK_LENHI
             sbc #0
             sta BLK_LENHI
             ldx #191
             bne RESBYTE        ;Go for next byte
             
ENDBLK       lda PILOT_CNTR     ;End of reading
             beq ENDBLK2
             ldy #143
             bne TERM_READ
ENDBLK2      ldy #1
             bne TERM_READ
MEASWIDTH    jsr WFOREDGE
             bcc WFOREDGE
ENDBIT       rts

WFOREDGE     lda SKSTAT
             and #16
             lsr A
             ora #16
             sta AUDC1
             ldy #20
BITDELAY1    dey
             bne BITDELAY1
             sec
BITCOUNT     inx
             beq ENDBIT
             lda SKSTAT
             eor WKCHSUM
             and #16
             beq BITCOUNT
             lda SKSTAT
             sta WKCHSUM
             clc
             lda WKBYTE2
             eor #15
             sta WKBYTE2
             sta COLBK
             rts
            
INIT_READ    tsx                 ;Backup stack pointer
             stx BK_STACK
             lda BRKKY
             sta BK_BRKKY_LO     ;Backup BRKKY
             nop
             nop
             lda BRKKY+1         ;Backup BRKKY+1
             sta BK_BRKKY_HI
            
             nop
             nop
             lda #217            ;Set BRKKY vector
             sta BRKKY
             lda #8
             sta BRKKY+1
            
             lda #0              ;Disable NMI and DMA
             sta NMIEN
             sta DMACLT
             lda #128            ;Enable BREAK only
             sta IRQEN
             rts                 ;Return

TERM_READ    sty DSTATS
             lda BK_BRKKY_LO
             sta BRKKY
             lda BK_BRKKY_HI
             sta BRKKY+1
             lda SDMCTL
             sta DMACLT
             lda BK_NMIEN
             sta NMIEN
             ldx BK_STACK
             inx
             inx
             txs
             cli
             lda POKMSK
             sta IRQEN
             lda #60
             sta PACTL
             sta PBCTL
             ldy DSTATS
             clc
             bmi RSETCARRY
             rts
RSETCARRY    sec
             rts
;--------------------------------------------------
; Read block data area
;--------------------------------------------------
BK_BRKKY_LO .BYTE 0
BK_BRKKY_HI .BYTE 0            
BLK_LENHI   .BYTE 0
BLK_LENLO   .BYTE 0
PILOT_CNTR  .BYTE 0
WKBYTE      .BYTE 0
WKCHSUM     .BYTE 0
WKBYTE2     .BYTE 0
BK_NMIEN    .BYTE 0
BK_STACK    .BYTE 0
SGH_00      .BYTE 0
SGH_01      .BYTE 0
SGH_02      .BYTE 0
SGH_03      .BYTE 0

;-----------------------------------------
; Preinitialization - FORCED WARM START.
; This part of the loader can be overwritten
; by the binary file being loaded
;-----------------------------------------
PREINIT     lda #0           ;Disable cold start
            sta COLDST
            
            lda #1           ;Indicate boot successful
            sta BOOT
            
            lda #255         ;Turn BASIC off  
            sta 54017
            sta 1016         
            
            lda #<DINI
            sta DOSINI       ;Setup DOSINI
            lda #>DINI
            sta DOSINI+1
            
            jsr WARMSV
            
DINI        lda #<SHOWTITLE  ;Setup DOSVEC
            sta DOSVEC
            lda #>SHOWTITLE
            sta DOSVEC+1            
            clc
            rts

SHOWTITLE   ldx #<TITLE       ;Show program name
            ldy #>TITLE
            jsr LC642
            
            lda #1            ;Tape beep + key press
            sta 764
            jsr LFDFC         
            jmp START

TITLE       .BYTE 125            
PROGNAME    .BYTE "LST2000BL "
            .BYTE 155
            
            