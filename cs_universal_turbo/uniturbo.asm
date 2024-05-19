;==============================================
;Universal turbo (Czechoslovakia) loader
;==============================================


            *= 1280
            .INCLUDE "equates.asm"


L049F       jsr DOSINIT         ;Setup DOS vector

SHOWTITLE   lda #<TITLE         ;Display title
            ldy #>TITLE
            ldx #<TITLE_L
            jsr PRINT_LINE
    
RESTART     jsr BEEP            ;Beep

            lda #52             ;Tape motor switched on
            sta PACTL
            sta PBCTL

;----------------------------------------------
;Turbo header decoding
;----------------------------------------------

GET_HEADER  jsr CLR_NOINT       ;Call subroutine for clearing status variables. Disable interrupts.
            jsr MEAS_SPEED      ;Call subroutine for speed detection (T2K or ST)
                            
            jsr ENABLE_INT      ;Enable interrupts

L04BA       lda BUFRFL
            cmp #180            ;Check T2K/ST
            bcc SET_T2K         ;T2K - jump, ST - continue

            lda #155            ;Setup address where ST header will be placed
            sta BFENLO          ;ST header has 28 bytes
            lda #128
            sta BUFRLO
            asl A
            sta BUFRHI
            sta BFENHI
            lda #20
            sta W_NAME_LEN

            lda #183            ;First byte if the block should be 183
            jsr DECODE_BLOCK    ;Call subroutine for block decoding

            bcc GET_HEADER      ;If error occured, try to decode the header again

            lda L0095           ;Setup address where main block will be placed
            sta BUFRLO
            clc
            adc L0097
            sta BFENLO
            lda L0096
            sta BUFRHI
            adc L0098
            sta BFENHI
            lda #155
            sta L0095          ;Augment the file name string with EOL
            jsr DISPLAY_NAME   ;Call subroutine that displays the file name

;----------------------------------------------
;Main block decoding (Super turbo)
;----------------------------------------------

            lda #237           ;First byte of block should be 237
            jsr DECODE_BLOCK   ;Call subroutine for block decoding

            bcc DISPLAY_ERROR  ;If error occured, jump (issue error message)

            lda L0099          ;Setup RUN address
            sta CASINI
            lda L009A
            sta CASINI+1
RUN_PROGRAM lda #60            ;Switch off tape motor
            sta PACTL
            sta PBCTL
            lda #0
            sta WARMST
            sta CRSINH
            jmp (CASINI)       ;Jump to RUN address

DISPLAY_ERROR jsr ERROR_MSG    ;Issue error message
              jmp RESTART      ;Return to the beginning


;----------------------------------------------
;Turbo 2000 handling
;----------------------------------------------

SET_T2K     lda #10
            sta W_NAME_LEN
            lda #145           ;Setup address where T2K header will be placed
            sta BFENLO
            lda #128
            sta BUFRLO
            asl A
            sta BUFRHI
            sta BFENHI

            jsr GET_T2K_BLOCK  ;Call subroutine for block decoding that will be set up for T2K
            bcc GET_HEADER     ;If error occured, try to decode the header again

            lda L008B          ;Setup address where the main block will be placed
            sta BUFRLO
            clc
            adc L008D
            sta BFENLO
            lda L008C
            sta BUFRHI
            adc L008E
            sta BFENHI
            lda #155          ;Augment the file name string with EOL
            sta L008B
            jsr DISPLAY_NAME  ;Call subroutine that displays file name

            lda #255          ;First byte of the block should be 255
            jsr GET_T2K_BLOCK ;Call subroutine for block decoding that will be set up for T2K  
            bcc DISPLAY_ERROR ;If error occured, jump (issue error message)

            lda L008F         ;Prepare RUN address
            sta CASINI
            lda L0090
            sta CASINI+1
            jmp RUN_PROGRAM   ;Run program

;----------------------------------------------
;Beep
;----------------------------------------------
BEEP        lda #30
            sta AUDF1
            lda #174
            sta AUDC1
            lda #10
            jsr DELAY
            lda #0
            sta AUDF1
            sta AUDC1
            rts
;----------------------------------------------
;Display text
;----------------------------------------------
DISPLAY_NAME lda #125
             sta LOMEM
             lda #128
             ldy #0
             ldx W_NAME_LEN
             jsr PRINT_LINE
             lda #50
DELAY        adc RTCLOK+2
DELAY_LOOP   cmp RTCLOK+2
             bne DELAY_LOOP
             rts

ERROR_MSG   lda #<ERRORTEXT
            ldy #>ERRORTEXT
            ldx #<ERRORTEXT_L
            jsr PRINT_LINE
            lda LE425
            pha
            lda LE424
            pha
            rts

PRINT_LINE  stx IOCB0+ICBLL		
            sta IOCB0+ICBAL
            sty IOCB0+ICBAH
            lda #9
            sta IOCB0+ICCOM
            ldx #0
            stx IOCB0+ICBLH
            jmp CIOV

;==============================================
;Block decoding subroutine
;Block is placed from: BUFRLO+256*BUFRHI
;                to:   (BFENLO+256*BFENHI)-1
;First byte of the block should be in
;acumulator register before subroutine
;is called        
;==============================================

DECODE_BLOCK  sta LTEMP
              jsr CLR_NOINT     ;Call subroutine for clearing status variables. Disable interrupts.
              php

L05A2       jsr MEAS_WIDTH      ;Measure length of pulse
L05A5       ldy RECVDN          ;Count from MED
            jsr WFOREDGE        ;Wait for edge
            bcc L05A2           ;If Y wrapped, repeat
            cpy XMTDON
            bcs L05A5           ;If Y>HIGH repeat
            jsr WFOREDGE        ;Wait for edge
            bcc DB_RETURN       ;If Y wrapped, end block decoding
            ldy BUFRFL
            jmp L05D6

L05BA       plp                 ;Store data to memory
            bne L05C5
            lda LTEMP
            eor NOCKSM
            bne ENABLE_INT
            beq L05D1
L05C5       ldy #0
            lda NOCKSM
            sta (BUFRLO),Y
            inc BUFRLO
            bne L05D1
            inc BUFRHI
L05D1       ldy BUFRFL
            iny
            iny
            php

L05D6       lda #1
            sta NOCKSM
L05DA       jsr MEAS_WIDTH          ;Measure length of pulse
            bcc DB_RETURN          ;If pulse is too long, end block decoding
            cpy RECVDN         ;Compate to MED
            rol NOCKSM         ;Rotate NOCKSM with carry, length >MED "1", length <MED "0"
            ldy BUFRFL
            bcc L05DA          ;If bit 7 was 0, repeat
            lda CHKSUM         ;Update checksum
            eor NOCKSM
            sta CHKSUM

            lda BUFRLO         ;Write number to buffer
            cmp BFENLO
            lda BUFRHI
            sbc BFENHI
            bcc L05BA
            lda #0
            cmp CHKSUM
DB_RETURN   pla


ENABLE_INT  lda #64            ;Enable interrupts
            sta NMIEN
            sta IRQEN
            rts
;----------------------------------------------
;Turbo 2000 setup
;----------------------------------------------
GET_T2K_BLOCK sta LTEMP          ;Setup LO,MED,HI
              jsr CLR_NOINT
              php
              lda #176
              sta BUFRFL
              lda #214
              sta RECVDN
              jsr MEAS_WIDTH
              bcc DB_RETURN
              ldy #0
              jsr MEAS_WIDTH
              bcc DB_RETURN
L061F         ldy #200
              jsr WFOREDGE
              bcc DB_RETURN
              cpy #215
              bcs L061F
              jsr WFOREDGE
              bcc DB_RETURN
              ldy #176
              jmp L05D6

;-----------------------------------------------
;Clear status variables, disable interrupts
;-----------------------------------------------
CLR_NOINT   ldy #0
            sty STATUS
            sty CHKSUM
            sty NMIEN
            sty DMACLT
            sty IRQEN
            clc
            rts
;-----------------------------------------------
;ST speed detection
;-----------------------------------------------
MEAS_SPEED  jsr MEAS_WIDTH           ;Measure length of pulse
            bcc MEAS_SPEED           ;If no CF, repeat
            lda #3
            sta LTEMP+1

L064E       ldy #0
            sty FREQ            ;{{

            ldx #3
L0654       jsr MEAS_WIDTH           ;Measure length of 3 pulses {
            bcc MEAS_SPEED           ;No CF, => return to  MEAS_SPEED
            dex
            bne L0654           ;}
            tya                 ;A now holds number of measuring loop passes
            lsr A               ;A=A/2
            pha
            eor #255            ;A = 255-A
            sta BUFRFL          ;LO
            pla
            lsr A
            pha
            eor #255
            sta RECVDN          ;MED
            pla
            lsr A
            eor #255
            sta XMTDON          ;HI
            ldy #0
            jsr WFOREDGE           ;Wait for edge
            bcc MEAS_SPEED
L0677       ldy BUFRFL          ;Star from LO
            jsr MEAS_WIDTH           ;Measure length of pulse
            bcc MEAS_SPEED
            cpy RECVDN          ;Compare with MED
            bcc MEAS_SPEED           ;If Y<MED, measure speed again
            inc FREQ
            bne L0677           ;256 pulses must have appropriate length
            dec LTEMP+1
            bpl L064E           ;Repeat 4 times }}

            ldy #0
            jsr WFOREDGE           ;Wait for edge
            bcc MEAS_SPEED         ;No CF, measure speed again
            rts


;-----------------------------------------------
;Measure length of pulse
;-----------------------------------------------
MEAS_WIDTH  jsr WFOREDGE        ;Call wait for edge
            bcc L06B4           ;No CF, return

;-----------------------------------------------
;Wait for edge + colour effect
;-----------------------------------------------
WFOREDGE    lda #255         ;Was Random
            and #228         ;Was and #228
            ora STATUS
            lsr A
            and LTEMP+1
            sta COLBK

L06A4       iny              ;Measuring loop
            beq L06B4
            lda SKSTAT
            and #16
            cmp STATUS
            beq L06A4
            sta STATUS
            sec
            rts
L06B4       clc
            rts
;-----------------------------------------------
;Strings
;-----------------------------------------------
TITLE	    .byte "Universal Turbo"
TITLE_L      =*-TITLE
ERRORTEXT   .byte "BOOT ERROR"
ERRORTEXT_L  =*-ERRORTEXT_L
W_NAME_LEN  .byte 0
;-----------------------------------------------
;Initialization of DOS vectors
;-----------------------------------------------
DOSINIT     ldx #<SHOWTITLE
            stx DOSINI
            ldx #>SHOWTITLE
            stx DOSINI+1
            ldx #1
            stx BOOT
            dex
            stx COLDST
            rts 