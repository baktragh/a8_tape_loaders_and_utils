;=======================================================================
; Composite Self-extractor tail   
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
                   lda #1               ;Reset = cold start
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
                   lda #0
                   sta COLOR2
                   lda #$1A
                   sta COLOR0
                   sta COLOR1

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
;=======================================================================
; DISPLAY DATA
;=======================================================================
;-----------------------------------------------------------------------
; Display list
;-----------------------------------------------------------------------
DLIST      .BYTE 112,112,112
           .BYTE 2+64,<LINE_INSTR,>LINE_INSTR,2
           .BYTE 65,<DLIST,>DLIST
;-----------------------------------------------------------------------
; Screen lines
;-----------------------------------------------------------------------
LINE_INSTR  .BYTE         "Recording complete                      "
            .BYTE         "Press RESET for cold start              "                     
;-----------------------------------------------------------------------
; INIT Vector
;-----------------------------------------------------------------------
            ORG RUNAD
            .WORD START_ADDR
