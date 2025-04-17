;=======================================================================
; Common auxiliary subroutines
;=======================================================================
;-----------------------------------------------------------------------
; Wait for START 
;-----------------------------------------------------------------------
WAIT_FOR_START     lda #8
                   sta CONSOL
WFS_LOOP           lda CONSOL             ;What keys?  
                   cmp #6                 ;Is that START?
                   beq WFS_DONE           ;Yes, we are done
                   bne WFS_LOOP
WFS_DONE           rts
;-----------------------------------------------------------------------
; Beep
;-----------------------------------------------------------------------                   
BEEP 	           lda #0
                   sta AUDCTL
                   lda #$AF
                   sta AUDC1
                   lda #$10
                   sta AUDF1
                   ldx #20
BELL_1             jsr WAIT_FOR_VBLANK
                   dex
                   bne BELL_1
                   stx AUDC1                     ;Reset AUDC1 and AUDF1
                   stx AUDF1
                   rts
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
; Delay (0.1) seconds
;-----------------------------------------------------------------------
DELAY_TENTHS       jsr DELAY_TENTH
                   dey
                   bne DELAY_TENTHS
                   rts

DELAY_TENTH        ldx #5
@                  jsr DELAY_TICK
                   dex 
                   bne @-
                   rts

DELAY_TICK         jsr WAIT_FOR_VBLANK     ;Wait for VBLANK
DELAY_END          rts

;-----------------------------------------------------------------------
; Delay - custom number of vblanks
;-----------------------------------------------------------------------
DELAY_CUSTOM_Y     jsr WAIT_FOR_VBLANK
                   dey
                   bne DELAY_CUSTOM_Y
                   rts                              