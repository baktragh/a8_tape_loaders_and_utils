;==============================================
;MINIBLOCK (TURBO 2000 , Czechoslovakia) 
;==============================================             
;
;Minimalistic single block loader. This loader
;is intended to be base for creating special
;single purpose loaders (e.g these who
;reside under ROM etc.)
;

            .INCLUDE "equates.asm" 
;==============================================
;Block to be loaded
;==============================================                                            
BLOCK_FIRST=1183               ;First address
BLOCK_LAST=1183+586-1          ;Last address
BLOCK_RUN=1183                 ;RUN address


;-----------------------------------------------
; Boot header
;-----------------------------------------------
            *=32768
            
BOOTHEAD   .BYTE 0                       ;Unused
BH_NBLK    .BYTE 2                       ;Number of blocks.
BH_LADDR   .WORD 32768                   ;Load address
BH_IADDR   .BYTE <[BH_RTS],>[BH_RTS]     ;Initialization address            
            
BH_SETUP    lda  #60                     ;Motor off
            sta  PACTL
            sta  PBCTL
            jmp  L05D2                   ;Star loader
BH_RTS      rts

L05D2       lda #<[BLOCK_FIRST]  ;Setting up buffer for single block loading
            sta BUFRLO
            lda #>[BLOCK_FIRST]
            sta BUFRHI

            lda #<[BLOCK_LAST+1]
            sta BFENLO
            lda #>[BLOCK_LAST+1]
            sta BFENHI


            lda #255           ;First byte in the block should be 128
            jsr L0631          ;Call block decoding subroutine

            bcs L0622          ;If there was no error, jump to RUN

            jsr $C63E          ;Error occured, call BOOT ERROR OS routine
            jmp L05D2          ;Back to the beginning

L0622	    jmp BLOCK_RUN      ;RUN


;==============================================
;Block decoding subroutine
;Block is placed from: BUFRLO+256*BUFRHI
;                to:   (BFENLO+256*BFENHI)-1
;First byte of the block should be in
;acumulator register before subroutine
;is called        
;==============================================
L0631       sta LTEMP
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
L0650       bne L06C2
L0652       jsr L06DB
            bcc L0650
            lda #0
            sta ICAX5Z
            sta LTEMP+1
L065D       ldy #180
L065F       jsr L06D6
            bcc L0650
            cpy #216
            bcc L0652
            inc ICAX5Z
            bne L065D
            dec LTEMP+1
L066E       ldy #209
            jsr L06DB
            bcc L0650
            cpy #222
            bcs L066E
            jsr L06DB
            bcc L06C2
            ldy #198
            jmp L069D
L0683       plp
            bne L068E
            lda LTEMP
            eor ICAX6Z
            bne L06C3
            beq L069A
L068E       ldy #0
            lda ICAX6Z
            sta (BUFRLO),Y
            inc BUFRLO
            bne L069A
            inc BUFRHI
L069A       ldy #200
            php
L069D       lda #1
            sta ICAX6Z
L06A1       jsr L06D6
            bcc L06C2
            cpy #227
            rol ICAX6Z
            ldy #198
            bcc L06A1
            lda CHKSUM
            eor ICAX6Z
            sta CHKSUM
            lda BUFRLO
            cmp BFENLO
            lda BUFRHI
            sbc BFENHI
            bcc L0683
            lda #0
            cmp CHKSUM
L06C2       pla
L06C3       lda #192
            sta NMIEN
            sta POKMSK
            sta IRQEN
            lda #60
            sta PACTL
            sta PBCTL
            rts
L06D6       jsr L06DB
            bcc L06FF
L06DB       ldx #4
L06DD       dex
            bne L06DD
            lda STATUS
            lsr A
            and LTEMP+1
            sta COLBK
L06E8       iny
            beq L06FE
            lda BRKKEY
            beq L06FC
            lda SKSTAT
            and #16
            cmp STATUS
            beq L06E8
            sta STATUS
            sec
            rts
L06FC       dec BRKKEY
L06FE       clc
L06FF       rts  