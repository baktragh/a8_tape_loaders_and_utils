;===============================================================================
;SuperBlock (Super Turbo, Czechoslovakia)


;Binary load emulation loader
;Binary file being loaded is required to have
;a RUN segment

;This loader loads turbo blocks according to 
;table that is located at the end of the loader.
;Each table entry contains first and last+1 address
;of the block. 

;Before turbo block is loaded, INITAD is set to
;subroutine that does nothing. After loading block,
;emulation of JSR(INITAD) is performed.

;The table begins with label TABLE and should be
;filled by some external program. (This table should
;be a result of analysis of binary file segments)

;TABLEMAX must be set according to the following
;expression. Let X be number of segments of the
;binary file. Then TABLEMAX=(X+1)*4

;===============================================================================

.INCLUDE "includes.asm"
            *= 2048


L049F       jsr BOOTSETUP       ;Show title

NEXTBLOCK   lda CURRENTBLOCK    ;End of table reached
            cmp TABLEMAX
            bne LOAD            ;No, load next block

            jmp (736)           ;RUN loaded program 

LOAD        lda #<FAKEINIT      ;Setup INITAD to NOP subroutine
            sta 738
            lda #>FAKEINIT
            sta 739

            ldy CURRENTBLOCK    ;Setup buffer according to the table
            lda TABLE+0,Y
            sta BUFRLO
            lda TABLE+1,Y
            sta BUFRHI
            lda TABLE+2,Y
            sta BFENLO
            lda TABLE+3,Y
            sta BFENHI


            lda #52             ;Swtich on tape motor
            sta PACTL
            sta PBCTL

            jsr L04B1           ;Jump to super turbo block decoding 
                                ;subroutine

            lda #60	        ;Switch of tape motor
            sta PACTL
            sta PBCTL

            bcs BLOCKDONE       ;If no error, jump to handle loaded block
            jmp HANDLEERROR     ;Otherwise handle the error

;-------------------------------------------------------------------------------
;Handling loaded block
;-------------------------------------------------------------------------------
BLOCKDONE   jsr DOINIT          ;Emulate JSR(INITAD)

            inc CURRENTBLOCK
            inc CURRENTBLOCK
            inc CURRENTBLOCK
            inc CURRENTBLOCK

            jmp NEXTBLOCK       ;Jump and load next block

DOINIT      jmp (738)           


;===============================================================================
;Block decoding subroutine
;Block is placed from: BUFRLO+256*BUFRHI
;                to:   (BFENLO+256*BFENHI)-1
;First byte of the block should be in
;acumulator register before subroutine
;is called        
;===============================================================================

L04B1       jsr L0634
            jsr L0645
                     
            jsr L05FC

L04BA       lda BUFRFL
            cmp #180
            bcc L04B1
            lda #124      ;Expected ID byte - 124
            jsr L059C
            rts


L059C       sta LTEMP
            jsr L0634
            php
L05A2       jsr L0692
L05A5       ldy RECVDN
            jsr L0697
            bcc L05A2
            cpy XMTDON
            bcs L05A5
            jsr L0697
            bcc L05FB
            ldy BUFRFL
            jmp L05D6
L05BA       plp
            bne L05C5
            lda LTEMP
            eor NOCKSM
            bne L05FC
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
L05DA       jsr L0692
            bcc L05FB
            cpy RECVDN
            rol NOCKSM
            ldy BUFRFL
            bcc L05DA
            lda CHKSUM
            eor NOCKSM
            sta CHKSUM
            lda BUFRLO
            cmp BFENLO
            lda BUFRHI
            sbc BFENHI
            bcc L05BA
            lda #0
            cmp CHKSUM
L05FB       pla

L05FC       lda #64
            sta NMIEN
            sta IRQEN
            rts


L0634       ldy #0
            sty STATUS
            sty CHKSUM
            sty NMIEN
            sty DMACLT
            sty IRQEN
            clc
            rts

L0645       jsr L0692
            bcc L0645
            lda #3
            sta LTEMP+1
L064E       ldy #0
            sty FREQ
            ldx #3
L0654       jsr L0692
            bcc L0645
            dex
            bne L0654
            tya
            lsr A
            pha
            eor #255
            sta BUFRFL
            pla
            lsr A
            pha
            eor #255
            sta RECVDN
            pla
            lsr A
            eor #255
            sta XMTDON
            ldy #0
            jsr L0697
            bcc L0645
L0677       ldy BUFRFL
            jsr L0692
            bcc L0645
            cpy RECVDN
            bcc L0645
            inc FREQ
            bne L0677
            dec LTEMP+1
            bpl L064E
            ldy #0
            jsr L0697
            bcc L0645
            rts

L0692       jsr L0697
            bcc L06B4


L0697       lda RANDOM
            and #228
            ora STATUS
            lsr A
            and LTEMP+1
            sta COLBK

L06A4       iny
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
;-------------------------------------------------------------------------------
; Data area
;-------------------------------------------------------------------------------
CURRENTBLOCK .BYTE $0          ;Number of block expected to be
                               ;loaded

FAKEINIT    rts                ;Init routine doing nothing


TABLESEP    .BYTE 255          ;Table eyecatcher
TABLEMAX    .BYTE 0            ;Max. ofset in the table. External program
                               ;must set this according to formula above

TABLE       .REPT 256          ;Table of blocks. This table must be filled
            .BYTE 0            ;by external program
            .ENDR

VOLATILE    = [*-1]            

;-------------------------------------------------------------------------------
; Handle error
;-------------------------------------------------------------------------------
HANDLEERROR lda #1              ;Cold start after RESET
            sta COLDST     
            lda #88             ;Red/Purple screen
            sta COLBK
            sta COLOR4
ERRLOOP     jmp ERRLOOP         ;Wait for RESET   


;-------------------------------------------------------------------------------
; Emulate successful disk boot
;-------------------------------------------------------------------------------
BOOTSETUP   ldx #0            ;Reset cold start flag
            stx COLDST
            inx               ;Indicate disk boot is OK (1)
            stx BOOT
            rts


