;===============================================================================
;TURGEN SYSTEM - BlockLoader
;Special Binary loader for Turbo 2000 (Czechoslovakia)


;Binary file being loaded is required to have
;a RUN vector

;This loader loads turbo blocks according to 
;table that is located at the end of the loader.
;Each table entry contains first and last+1 address
;of the block. 

;The table begins with label TABLE and must be
;populated by some external program. 

;TABLEMAX must be set according to the following
;expression. Let X be number of segments of the
;binary file. Then TABLEMAX=(X+1)*4
;=============================================================================== 
 
.INCLUDE equates.asm

            *=2048
            JMP START         ;Jump to the mainline code

;===============================================================================
;Block decoding subroutine
;Block is placed from: BUFRLO+256*BUFRHI
;                to:   (BFENLO+256*BFENHI)-1
;First byte of the block (identification byte) must be in
;the accumulator register before the subroutine is called
;===============================================================================           

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
BARCOLOR    ora #$80         ;Color of the color bar
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

;===============================================================================
; Mainline code
;===============================================================================
START       jsr BOOTSETUP     ;Fake successful boot

            lda #0            ;Current block in table is 0
            sta CURRENTBLOCK

NEXTBLOCK   lda CURRENTBLOCK  ;End of table reached ?
            cmp TABLEMAX       
            bne LOAD          ;No, load next block
 
            jmp (736)         ;RUN loaded program


LOAD        lda #<FAKEINIT    ;Setup dummy INIT vector
            sta 738
            lda #>FAKEINIT
            sta 739

            ldy CURRENTBLOCK  ;Setup buffer according to the table
            lda TABLE+0,Y
            sta BUFRLO
            lda TABLE+1,Y
            sta BUFRHI
            lda TABLE+2,Y
            sta BFENLO
            lda TABLE+3,Y
            sta BFENHI

            lda #124         ;Call Turbo 2000 block decoding subroutine. ID=124
            ldx #0            
            ldy #0
            jsr L0631          

            bcs BLOCKDONE    ;Block loaded OK. Handle next
            jmp HANDLEERROR  ;Handle error


;-------------------------------------------------------------------------------
;Handling loaded block
;-------------------------------------------------------------------------------
BLOCKDONE   jsr DOINIT        ;Emulate JSR(INITAD)

            inc CURRENTBLOCK  ;Move forward in the table
            inc CURRENTBLOCK
            inc CURRENTBLOCK
            inc CURRENTBLOCK

            jmp NEXTBLOCK     ;Jump and load next block 

DOINIT      jmp (738)


CURRENTBLOCK .BYTE $0         ;Number of block expected to be
                              ;loaded

FAKEINIT    rts               ;INIT routine doing nothing

TABLESEP    .BYTE 255         ;Table eye-catcher
TABLEMAX    .BYTE 0           ;Max. offset in the table. External program
                              ;must set this according to formula above

TABLE       .REPT 256         ;Table of blocks. This table must be filled
            .BYTE 0           ;by external program
            .ENDR

VOLATILE    = [*-1]             
;-------------------------------------------------------------------------------
; Handle load error - red/purple screen and endless loop, cold start
;-------------------------------------------------------------------------------

HANDLEERROR lda #1            ;Cold start after RESET
            sta COLDST     
            lda #88           ;Red/Purple screen
            sta COLBK
            sta COLOR4
ERRLOOP     jmp ERRLOOP       ;Wait for RESET     
            

BOOTSETUP   ldx #0            ;Reset cold start flag
            stx COLDST
            inx               ;Indicate disk boot is OK (1)
            stx BOOT
            rts
