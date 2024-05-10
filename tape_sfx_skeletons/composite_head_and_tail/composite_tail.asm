;=======================================================================
; Composite Self-extractor tail   
; Assemble with the MADS assembler
;=======================================================================
           
                 ICL "equates.asm" 
                 OPT H-,F-

                 START_ADDR=0x2000
                 VBI_VCOUNT      = 124

;=======================================================================
; MAINLINE CODE
;=======================================================================
                   .WORD START_ADDR
                   .WORD LAST-1
                   ORG START_ADDR
                   jmp BEGIN
;-----------------------------------------------------------------------
; Configuration
;------------------------------------------------------------------------
CFG_ALARM          .BYTE 0
;-----------------------------------------------------------------------
;Initialization
;-----------------------------------------------------------------------
BEGIN              lda #1               ;Reset = cold start
                   sta COLDST

                   jsr WAIT_FOR_VBLANK

                   sei                                 
                   lda #<DLIST          ;Setup display list
                   sta SDLSTL
                   lda #>DLIST
                   sta SDLSTH
                   cli

                   jsr WAIT_FOR_VBLANK ;Enable the screen
                   lda #34
                   sta SDMCTL
                   lda #15
                   sta COLOR2
                   sta COLOR4
                   lda #0
                   sta COLOR1

                   lda CFG_ALARM       ;Is alarm set?
                   beq ENDLESS         ;No, just wait for RESET

ALARM_LOOP         jsr BELL
                   jsr WAIT_FOR_VBLANK
                   dec CFG_ALARM
                   bne ALARM_LOOP

ENDLESS            jmp ENDLESS
;-----------------------------------------------------------------------
; Wait for VBLANK
;-----------------------------------------------------------------------
WAIT_FOR_VBLANK    php
                   lda #VBI_VCOUNT
WFV_1              cmp VCOUNT
                   bne WFV_1
                   plp
                   rts            
;-----------------------------------------------------------------------
; Audibles
;-----------------------------------------------------------------------
BELL 	           lda #0
                   sta AUDCTL
                   lda #$AF
                   sta AUDC1
                   lda #$30
                   sta AUDF1

                   ldx #40
BELL_1             jsr WAIT_FOR_VBLANK
                   dex
                   bne BELL_1

                   lda #$00
                   sta AUDC1

                   ldx #40
BELL_2             jsr WAIT_FOR_VBLANK
                   dex
                   bne BELL_2

                   stx AUDC1
                   stx AUDF1

                   rts
;=======================================================================
; DISPLAY DATA
;=======================================================================
;-----------------------------------------------------------------------
; Display list
;-----------------------------------------------------------------------
DLIST      .BYTE 112,112,112,112,112,112,112,112,112
           .BYTE 2+64,<LINE_INSTR,>LINE_INSTR,2
           .BYTE 65,<DLIST,>DLIST
;-----------------------------------------------------------------------
; Screen lines
;-----------------------------------------------------------------------
LINE_INSTR  .BYTE         "Recording complete                      "
            .BYTE         "Press RESET for cold start              "      
LAST        EQU *               
;-----------------------------------------------------------------------
; INIT Vector
;-----------------------------------------------------------------------
            .WORD RUNAD
            .WORD RUNAD+1
            .WORD START_ADDR
