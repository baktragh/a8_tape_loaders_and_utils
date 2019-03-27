;===============================================================================
;Omicron Turbo - Blockloader
;===============================================================================

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

.INCLUDE "system_equates.asm"
            *= 2048


L049F       jsr BOOTSETUP       ;Setup boot flags
            jsr SWITCH_SIG      ;Switch signal source

NEXTBLOCK   lda CURRENTBLOCK    ;End of table reached
            cmp TABLEMAX
            bne LOAD            ;No, load next block

            jmp (736)           ;RUN loaded program 

LOAD        lda #<FAKEINIT      ;Setup INITAD to NOP subroutine
            sta 738
            lda #>FAKEINIT
            sta 739

            ldy CURRENTBLOCK    ;Setup buffer according to the table
            lda TABLE,Y
            sta BUFRLO
            iny
            lda TABLE,Y
            sta BUFRHI
            iny
            lda TABLE,Y
            sta BFENLO
            iny
            lda TABLE,Y
            sta BFENHI

;--------------------------------------------------------------------------------
; Set-up turbo mode
;--------------------------------------------------------------------------------
            lda #52       ;Group A - COMMAND active
            sta PBCTL
            
            lda #$83      ;Group B - DATA-OUT forced to zero
            sta SKCTL     
            
            lda #$38      ;Group C - Input/Output from joystick port
            sta PACTL     ;Direction control mode
            lda #$60      ;Set direction contol bits
            sta PORTA
            lda #$34      ;PORTA addressing mode
            sta PACTL     ;Motor on

;--------------------------------------------------------------------------------
; Decode block
;--------------------------------------------------------------------------------            
            jsr L04B1           ;Jump to super turbo block decoding 
                                ;subroutine

;--------------------------------------------------------------------------------
; Reset turbo mode
;--------------------------------------------------------------------------------                                            
            lda #60      ;Group A - COMMAND Inactive
            sta PBCTL
            
            lda #$03     ;Group B - Do not force DATA-OUT to 0
            sta SKCTL    
            
            lda #$38     ;Group C - Reset joystick port         
            sta PACTL              
            lda #$00               
            sta PORTA              
            
            lda #$3C     ;Motor off
            sta PACTL              
;--------------------------------------------------------------------------------
; Evaluate result
;--------------------------------------------------------------------------------                                            
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
            cmp #180    ;Speed - 2270 bd or higher?
            bcc LT2K    ;Only 2270 bd, perform special preparation
LCONT       lda #183
            jsr L059C
            rts
            
LT2K        lda #176    ;Special preparation for 2270 bd
            sta BUFRFL  
            lda #214    
            sta RECVDN  
            jmp LCONT   

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


L0697       lda STATUS    ; 4 cycles   (16/128 or 0)
            lsr A         ; 2 cycles   (8/64 or 0)
            ora #$B2      ; 2 cycles   (Base color - green)
            nop           ; 2 cycles   (Filler)
            
            and LTEMP+1    ; 255 or nothing
            sta COLBK

L06A4       iny
            beq L06B4
L_DET       lda SKSTAT
L_AND       and #16
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
            sta DMACLT
            sta SDMCTL          ;No image
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
;-----------------------------------------------------------------------------
; Switch signal source
;-----------------------------------------------------------------------------
SWITCH_SIG  pha
            lda ICSTAZ         ;Check indicator         
            bne SW_TOSIO       ;If not zero, switch to SIO
            
SW_TOJOY    lda #$00           ;$D300
            sta L_DET+1
            lda #$D3
            sta L_DET+2
            lda #128           ;Bit 7
            sta L_AND+1
            bne SW_END
        
SW_TOSIO    lda #$0F
            sta L_DET+1
            lda #$D2
            sta L_DET+2
            lda #16
            sta L_AND+1
            
SW_END      pla                ;Restore everything
            rts                ;Return
