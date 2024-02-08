;=======================================================================
; Composite Self-extractor header   
; Assemble with the MADS assembler
;=======================================================================
           
                 ICL "equates.asm" 
                 OPT H+,F-

                 START_ADDR=0x2000
                 VBI_VCOUNT      = 124

;=======================================================================
; MAINLINE CODE
;=======================================================================
                   ORG START_ADDR
;-----------------------------------------------------------------------
;Initialization
;-----------------------------------------------------------------------
                   lda #$20
                   sta COLOR2
                   sta COLOR4
                   lda #12
                   sta COLOR1

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
                                       
                   jsr WAIT_FOR_START     ;Wait for START key

                   lda #0
                   sta COLOR4 
                   jsr WAIT_FOR_VBLANK

                   jsr BEEP
                   rts                    ;Back to the binary loader
;=======================================================================
; KEYBOARD SUBROUTINES
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
                   
;=======================================================================
; OTHER SUBROUTINES
;=======================================================================                   
;-----------------------------------------------------------------------
; Beep
;-----------------------------------------------------------------------                   
BEEP               ldx #200
BEEP_LOOP          lda #8
                   sta CONSOL
                   sta WSYNC
                   sta WSYNC
                   lda #0
                   sta CONSOL
                   sta WSYNC
                   sta WSYNC
                   dex
                   bne BEEP_LOOP
                   rts
                   
;-----------------------------------------------------------------------
; Wait for VBLANK
;-----------------------------------------------------------------------
WAIT_FOR_VBLANK    php
                   lda #VBI_VCOUNT
WFV_1              cmp VCOUNT
                   bne WFV_1
                   plp
                   rts                                                   
;=======================================================================
; DISPLAY DATA
;=======================================================================
;-----------------------------------------------------------------------
; Display list
;-----------------------------------------------------------------------
DLIST      .BYTE 112,112,112
           .BYTE 112,112
           .BYTE 112,112,112,112
           .BYTE 2+64,<LINE_TITLE,>LINE_TITLE
           .BYTE 112,112,112,112
           .BYTE 2+64,<LINE_INSTR,>LINE_INSTR,2,2
           .BYTE 65,<DLIST,>DLIST
;-----------------------------------------------------------------------
; Screen lines
;-----------------------------------------------------------------------
LINE_TITLE  .BYTE         "tttttttttttttttttttttttttttttttttttttttt"

LINE_INSTR  .BYTE         "1. Insert blank tape                    " 
            .BYTE         "2. Press PLAY+RECORD                    "
            .BYTE         "3. Press START to begin recording       " 
;-----------------------------------------------------------------------
; INIT Vector
;-----------------------------------------------------------------------
            ORG INITAD
            .WORD START_ADDR
