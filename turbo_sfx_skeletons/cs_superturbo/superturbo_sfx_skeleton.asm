;=======================================================================
; Super Turbo Self-extractor skeleton   
; Assemble with the MADS assembler
;
;
;=======================================================================
           
                 ICL "equates.asm" 
                 OPT H+,F-

;=======================================================================
; Private constants
;=======================================================================
                ZP_TAB_PTR_LO   = 128
                ZP_TAB_PTR_HI   = 129
                VBI_VCOUNT      = 124
                START_ADDR      = 2944
                ZP_BLOCKFLAG    = 130
                ZP_ZAP_PTRLO    = 131
                ZP_ZAP_PTRHI    = 132
                ZP_ZAP_PTR      = ZP_ZAP_PTRLO 
;
                BF_NOSILENCE    = 0x80
                BF_LONGPILOT    = 0x40
                BF_HIGH_SPEED   = 0x01
;=======================================================================
; INITITALIZATION CODE - Switches off the display, so that
; loading data into the screen memory does no harm. Also ensure
; that RESET results in cold start.
;=======================================================================
                   ORG  START_ADDR      
                   ldy #0
                   sty SDMCTL
                   iny
                   sty COLDST
                   jsr IN_WBV
                   jsr IN_WBV

IN_WBV             lda #VBI_VCOUNT
IN_WBV_L           cmp VCOUNT
                   bne IN_WBV_L
                   rts     
                   
                   ORG INITAD
                   .WORD START_ADDR
            
;=======================================================================
; MAINLINE CODE
;=======================================================================
                   ORG START_ADDR
;-----------------------------------------------------------------------
;Initialization
;-----------------------------------------------------------------------
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
;-----------------------------------------------------------------------
; Prepare to save
;-----------------------------------------------------------------------
READY_SAVE         lda #<DATA_TABLE        ;Reset the table and counter
                   sta ZP_TAB_PTR_LO
                   lda #>DATA_TABLE
                   sta ZP_TAB_PTR_HI

                   jsr WAIT_FOR_START     ;Wait for START key
                   jsr BEEP

;From now on, disable interrupts and DMA, keep motor ON until the contents
;is fully recorded.
;-----------------------------------------------------------------------
                   jsr RECENV_INIT
                   lda #52
                   sta PACTL
                   jsr SHORT_DELAY
;-----------------------------------------------------------------------      
SAVE_LOOP          ldy #0                 ;Get buffer range
                   lda (ZP_TAB_PTR_LO),Y
                   sta BUFRLO
                   iny
                   lda (ZP_TAB_PTR_LO),Y
                   sta BUFRHI
                   iny 
                   lda (ZP_TAB_PTR_LO),Y
                   sta BFENLO
                   iny
                   lda (ZP_TAB_PTR_LO),Y
                   sta BFENHI
                   iny
                   lda (ZP_TAB_PTR_LO),Y
                   sta ZP_BLOCKFLAG 

                   lda BUFRLO
                   and BUFRHI
                   and BFENLO
                   and BFENHI
                   cmp #$FF
                   beq SAVE_TERM
    
SAVE_DOBLOCK       ldy #0                       ;Get ID byte
                   lda (BUFRLO),Y
                   sta ID_BYTE
                   
                   jsr WRITE_BLOCK

                   clc                          ;Increment table pointer
                   lda #5
                   adc ZP_TAB_PTR_LO
                   sta ZP_TAB_PTR_LO
                   bcc SAVE_CONT
                   inc ZP_TAB_PTR_HI  

;Add some gaps between blocks
SAVE_CONT          bit ZP_BLOCKFLAG             ;Check block flag
                   bmi SAVE_NODELAY             ;If 0x80, skip the delay
                   jsr SHORT_DELAY              ;Otherwise add a gap
SAVE_NODELAY
SAVE_NEXTBLOCK     jmp SAVE_LOOP                ;Continue saving.
;-----------------------------------------------------------------------
SAVE_TERM          jsr RECENV_TERM              ;Back with DMA and INTRs
                   lda #60                      ;Motor off
                   sta PACTL
; 
                   jmp READY_SAVE                   
                   
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
;-----------------------------------------------------------------------
; Short delay
;-----------------------------------------------------------------------                   
SHORT_DELAY        ldy #32
DELAY_LOOP_E       ldx #255            
DELAY_LOOP_I       stx WSYNC
                   dex
                   bne DELAY_LOOP_I
                   dey 
                   bne DELAY_LOOP_E
                   rts

;=======================================================================
; Write block of data
; BUFRLO,BUFRHI  - Right before first byte
; BFENLO,BFENHI  - Last byte
; A              - Identification byte
;=======================================================================
WRITE_BLOCK    pha                    ;Keep A in the stack

               pla                    ;Restore A
               sta ICAX6Z             ;Keep identification byte
               sta CHKSUM             ;Use as base for check sum
            
               lda #192
               sta AUDCTL

Z00            ldx #14                ;Presume normal pilot tone
               bit ZP_BLOCKFLAG       ;Check longer pilot flag (0x40)
               bvc WR_PILOT           ;If not set, skip
Z01            ldx #22                ;Setup elongated pilot tone

WR_PILOT       
WR_PILOTLEN    stx STATUS             ;Keep status
WR_PILOTL1     dey
               bne WR_PILOTL1         ;Wait
               lda #3                 
               sta SKCTL              ;Generate half of pulse
Z02            ldy #119
WR_PILOTL2     dey
               bne WR_PILOTL2
               lda #11
               sta SKCTL              ;Generate half of pulse
Z03            ldy #118
               dex
               bne WR_PILOTL1         ;Continue with part of pilot tone  
               dey
               dey
               dec STATUS              
               bne WR_PILOTL1         ;Continue with part of pilot tone
;            
Z04            ldy #32                ;Generate SYNC pulse
WR_SYNC1       dey
               bne WR_SYNC1
               lda #3
               sta SKCTL
Z05            ldy #39
WR_SYNC2       dey
               bne WR_SYNC2
               lda #11
               sta SKCTL
            
Z06            ldy #43                ;Generate bytes
               sec
               jmp WR_GBYTE
            
WR_PICKBYTE    lda BFENLO
               cmp BUFRLO
               lda BFENHI
               sbc BUFRHI
               bcc WR_GBYTE_CSM
               lda (BUFRLO,X)
               sta ICAX6Z
               eor CHKSUM
               sta CHKSUM
            
WR_GBYTE       jmp WR_GBYTE_NBIT      ;Go and generate byte
WR_GBYTE_CSM   jmp WR_SAFPULSE        ;Just end the writing
WR_GBYTE_W1    dey
               bne WR_GBYTE_W1
               bcc WR_GBYTE_HI
Z07            ldy #48
WR_GBYTE_W2    dey
               bne WR_GBYTE_W2
WR_GBYTE_HI    lda #3
               sta SKCTL
Z08            ldy #46
               bcc WR_GBYTE_W3
Z09            ldy #94
WR_GBYTE_W3    dey
               bne WR_GBYTE_W3
               lda #11
               sta SKCTL
               clc
Z10            ldy #44
            
WR_GBYTE_NBIT  rol ICAX6Z             ;Still bits to go
               bne WR_GBYTE_W1        ;Yes, write bit
               inc BUFRLO             ;No, advance in the buffer
               beq WR_ADVBUF             
               bne WR_CHANI1
WR_CHANI1      bne WR_CHAIN2
WR_ADVBUF      inc BUFRHI

WR_CHAIN2      
Z11            ldy #32
               txa
               beq WR_PICKBYTE        ;Get other byte from buffer
            
WR_GBYTE_W4    dey                    ;Keep waiting 
               bne WR_GBYTE_W4             
            
WR_SAFPULSE    lda #3                 ;Write safety pulse
               sta SKCTL
               lda #0
               sta AUDCTL
               rts                    ;Terminate writing
;-------------------------------------------------------------------------------
; Terminate recording environment
;-------------------------------------------------------------------------------               
RECENV_TERM    lda #64
               sta NMIEN
               sta IRQEN
               jsr WAIT_FOR_VBLANK   
               lda #34                
               sta DMACLT             
               rts

;-------------------------------------------------------------------------------
; Initiate recording environment;
;-------------------------------------------------------------------------------               
RECENV_INIT    ldy #0
               sty STATUS
               sty CHKSUM
               sty NMIEN
               sty DMACLT
               jsr WAIT_FOR_VBLANK
               sty IRQEN
               clc
               rts      
;-------------------------------------------------------------------------------
; Zap the routines for base speed
;-------------------------------------------------------------------------------
ZAP_BASE_SPEED
              lda #<BASE_SPEED_TABLE
              ldx #>BASE_SPEED_TABLE
              jmp ZAP_DO
ZAP_HIGH_SPEED
              lda #<HIGH_SPEED_TABLE
              ldx #>HIGH_SPEED_TABLE
ZAP_DO        sta ZP_ZAP_PTRLO
              stx ZP_ZAP_PTRHI

              
;------------------------------------------------------------------------------
; 
BASE_SPEED_TABLE
HIGH_SPEED_TABLE
;=======================================================================
; DISPLAY DATA
;=======================================================================
;-----------------------------------------------------------------------
; Display list
;-----------------------------------------------------------------------
DLIST      .BYTE 112,112,112
           .BYTE 7+64,<LINE_NAME,>LINE_NAME
           .BYTE 112,112
           .BYTE 2+64,<LINE_TITLE,>LINE_TITLE
           .BYTE $30
           .BYTE 2+64,<LINE_INSTR,>LINE_INSTR,2
           .BYTE 65,<DLIST,>DLIST
;-----------------------------------------------------------------------
; Screen lines
;-----------------------------------------------------------------------
;                          0123456789012345678901234567890123456789   
LINE_NAME   .BYTE         "nnnnnnnnnnnnnnnnnnnn"

LINE_TITLE  .BYTE         "tttttttttttttttttttttttttttttttttttt ppp"

LINE_INSTR  .BYTE         "Insert blank tape. Press PLAY+RECORD.   "
            .BYTE         "Then press START to begin recording.    "                     
;=======================================================================
; DATA AREAS
;=======================================================================
ID_BYTE          .BYTE 0       
;=======================================================================
; Segment data table
;=======================================================================
            DATA_TABLE=*  
