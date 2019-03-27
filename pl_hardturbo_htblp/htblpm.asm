;===============================================
;HTBL+ Mini (Hard Turbo, Poland)
;===============================================
;
;Enhanced binary loader for Hard Turbo, minuature bootable version
;
;Enhancements (compared with original loader)
;1. Data recorder motor is switched off after segment data is loaded. This
;   enables loading of binary files that execute lot of code from INIT
;   segments.
;2. RUNAD is set in a way that allows RUN segment to be the first segment
;   of the binary file.
;3. The loader never waits for any key under normal circumstances. This
;   makes the loader easier to use with devices do not support MOTOR CTRL
;   signal

.INCLUDE equates.asm

;
; Start of code
;
            *= 1999
;------------------------------------------------
; Boot header
;------------------------------------------------
BOOTHEAD  .BYTE 0                 ;Boot sign
          .BYTE 3                 ;4-1 blocks           
          .WORD 1999              ;Cassette boot file start
          .WORD CMN_RTS           ;Fake RTS
          
;-------------------------------------------------------------
; Data in EOF block trick
;-------------------------------------------------------------          
RELO_P2     ldx  #128                    ;Relocate 2nd portion
RELO_P2_L   lda  [1024-1],X              ;From cassette buffer
            sta  [1999+[128*3]-1],X          ;past the first portion
            dex  
            bne  RELO_P2_L
;----------------------------------------------
;Initialization
;----------------------------------------------
START       lda #0
            sta L0871         ;Reset to 0 
            sta COLDST        ;Do not force cold start
            lda #1            ;Indicate boot successful
            sta BOOT        
 
            lda #<CMN_RTS     ;Set default INITAD and RUNAD
            sta INITAD
            sta RUNAD
            lda #>CMN_RTS
            sta RUNAD+1
            sta INITAD+1
;----------------------------------------------
;Load main header (41 bytes)
;----------------------------------------------            
L07F1       lda #39           ;Set buffer (PAGE 4)
            sta BFENLO
            lda #4
            sta BUFRHI
            sta BFENHI
            lda #0
            sta BUFRLO
            
            jsr L0897         ;Decode block
            bcc L07F1         ;Try again if failed
            
            ldx #0            ;Display file name
            ldy #4
            jsr LC642
            
            lda #60           ;Tape motor off
            sta PACTL
            
            lda #1            ;Indicate boot successful
            sta 764
            sta BOOT          
            jsr LFDFC         ;Tape beep + key press

;----------------------------------------------
;Load segment header (6 bytes)
;----------------------------------------------                        
L0815       lda #0           ;Set buffer
            sta L0870
            sta BUFRLO
            lda #4
            sta BFENLO
            sta BUFRHI
            sta BFENHI
            
            lda #255         ;Expect 255 as first byte 
            jsr L0897        ;Decode block
            bcc L0873        ;If error, handle it
            
            jsr L087C        ;Process segment header

;----------------------------------------------
;Load segment data 
;----------------------------------------------                                    
            lda L0400        ;Setup buffer using segment header
            sta BUFRLO
            lda L0401
            sta BUFRHI
            lda L0402
            sta BFENLO
            lda L0403
            sta BFENHI
            
            lda #<CMN_RTS
            sta INITAD       ;Setup default INITAD
            lda #>CMN_RTS
            sta INITAD+1
            
            lda #255         ;Expect 255 as first byte
            jsr L0897        ;Decode block
            bcc L0873        ;Handle error
            
            lda #60          ;Tape motor off @@
            sta PACTL        ;               @@ 
            
L0867       jsr L086D        ;Emulate JSR(INITAD)
            jmp L0815        ;Proceed to the next header
L086D       jmp (INITAD)

L0870       .byte 111        ;
L0871       .byte 0          ;First segment indicator

;----------------------------------------------
; Common return from subroutine
;----------------------------------------------
CMN_RTS     rts

;----------------------------------------------
; Load error
;----------------------------------------------
L0873       jsr LEF8E
            jsr LC63E
            jmp START
;----------------------------------------------
; Process segment header
;----------------------------------------------
L087C       lda L0400       ;Check if 255 255
            cmp #255
            bne CHKFIRST
            lda L0401
            cmp #255
            beq RUNIT       ;If yes, run program
    
CHKFIRST    lda L0871       ;Is that first segment
            bne CMN_RTS     ;If no, header is processed
            
            lda L0400       ;First segment. Set RUNAD
            sta RUNAD
            lda L0401
            sta RUNAD+1
            inc L0871       ;Indicate not first segment
            jmp CMN_RTS     ;Header is processed
;---------------------------------------------
;Run loaded program
;---------------------------------------------
RUNIT       lda #60         ;End of file
            sta PACTL
            lda #60
            sta PACTL
            jmp (RUNAD)
;----------------------------------------------
; Decode block
;----------------------------------------------
L0897       sta LTEMP
            lda #52
            sta PACTL
            sta PBCTL
            lda #128
            sta POKMSK
            sta IRQEN
            clc
            ldy #0
            sty STATUS
            sty CHKSUM
            sty NMIEN
            sty DMACLT
            php
L08B6       bne L0928
L08B8       jsr L0939
            bcc L08B6
            lda #0
            sta ICAX5Z
            sta LTEMP+1
L08C3       ldy #180
            jsr L0934
            bcc L08B6
            cpy #216
            bcc L08B8
            inc ICAX5Z
            bne L08C3
            dec LTEMP+1
L08D4       ldy #219
            jsr L0939
            bcc L08B6
            cpy #230
            bcs L08D4
            jsr L0939
            bcc L0928
            ldy #198
            jmp L0903
L08E9       plp
            bne L08F4
            lda LTEMP
            eor ICAX6Z
            bne L0929
            beq L0900
L08F4       ldy #0
            lda ICAX6Z
            sta (BUFRLO),Y
            inc BUFRLO
            bne L0900
            inc BUFRHI
L0900       ldy #198
            php
L0903       lda #1
            sta ICAX6Z
L0907       jsr L0934
            bcc L0928
            cpy #222
            rol ICAX6Z
            ldy #198
            bcc L0907
            lda CHKSUM
            eor ICAX6Z
            sta CHKSUM
            lda BUFRLO
            cmp BFENLO
            lda BUFRHI
            sbc BFENHI
            bcc L08E9
            lda #0
            cmp CHKSUM
L0928       pla
L0929       lda #192
            sta NMIEN
            sta POKMSK
            sta IRQEN
            rts
L0934       jsr L0939
            bcc L095D
L0939       ldx #4
L093B       dex
            bne L093B
            lda STATUS
            lsr A
            and LTEMP+1
            sta COLBK
L0946       iny
            beq L095C
            lda BRKKEY
            beq L095A
            lda SKSTAT
            and #16
            cmp STATUS
            beq L0946
            sta STATUS
            sec
            rts
L095A       dec BRKKEY
L095C       clc
L095D       rts