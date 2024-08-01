;=======================================================================
; Super Turbo Self-extractor skeleton   
; Assemble with the MADS assembler
;=======================================================================
           
                 ICL "equates.asm" 
                 OPT H+,F-

;=======================================================================
; Private constants
;=======================================================================
                START_ADDR      = 2760 

                ZP_TAB_PTR_LO   = 128
                ZP_TAB_PTR_HI   = 129
                VBI_VCOUNT      = 124
                
                ZP_BLOCKFLAG    = 130
                ZP_ID_BYTE      = 131
                ZP_ZAP_PTRLO    = 132
                ZP_ZAP_PTRHI    = 133
                ZP_ZAP_PTR      = ZP_ZAP_PTRLO 
              
;Block flag constants
                CS_PILOT_HEADER = 0x80;
                CS_PILOT_DATA   = 0x40;
                CS_PILOT_LOOSE  = 0x20;
    
                CS_GAP_NONE = 0x08;
                CS_GAP_ELONGATED = 0x04;
                 
                CS_HIGH_SPEED_BLOCK = 0x01
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
                   jmp ENTRY_ADDR
;-----------------------------------------------------------------------
; Screen lines
;-----------------------------------------------------------------------
;                          0123456789012345678901234567890123456789   
LINE_NAME   .BYTE         "nnnnnnnnnnnnnnnnnnnn"

LINE_TITLE  .BYTE         "tttttttttttttttttttttttttttttttttttt ppp"

LINE_INSTR  .BYTE         "Insert blank tape. Press PLAY+RECORD.   "
            .BYTE         "Then press START to begin recording.    "      
;-----------------------------------------------------------------------
; Configuration
;-----------------------------------------------------------------------
CFG_FLAGS  .BYTE  0
           CFG_F_COMPOSITE = $80
           CFG_F_LONGSEP   = $40
           CFG_F_ALARM     = $20
CFG_SEP_DURATION .BYTE (3*50)
CFG_SAFETY_DELAY .BYTE 5
;------------------------------------------------------------------------------
; Speed tables
; First two values - standard, elongated pilot tone
; Other values - timing constants to control pulse width
;------------------------------------------------------------------------------
BASE_SPEED_TABLE
             .BYTE 14,22
             .BYTE 119,118,32,39,43,48,46,94,44,32
HIGH_SPEED_TABLE
             .BYTE $14,$1C
             .BYTE $4F,$4E,$22,$27,$23,$28,$26,$4E,$24,$18
;-----------------------------------------------------------------------
;Initialization
;-----------------------------------------------------------------------
ENTRY_ADDR         jsr WAIT_FOR_VBLANK

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
                   lda #$3A
                   sta COLOR0
                   sta COLOR1                                       
;-----------------------------------------------------------------------
; Prepare to save
;-----------------------------------------------------------------------
READY_SAVE         lda #<DATA_TABLE        ;Reset the table and counter
                   sta ZP_TAB_PTR_LO
                   lda #>DATA_TABLE
                   sta ZP_TAB_PTR_HI

                   bit CFG_FLAGS
                   bmi SKIP_START         ;If $80 (composite)
                   jsr WAIT_FOR_START     ;Wait for START key
SKIP_START         jsr BEEP

;-----------------------------------------------------------------------
;From now on, disable interrupts and DMA, keep motor ON until the
;contents is fully recorded.
                   jsr RECENV_INIT
                   lda #52
                   sta PACTL

                   ldy CFG_SAFETY_DELAY   ;Presume just safety delay
                   bit CFG_FLAGS          ;Check if long separator requested
                   bvc NORM_SEP           ;No, stick with safety delay
                   ldy CFG_SEP_DURATION   ;Use delay for long separator
NORM_SEP           jsr DELAY_CUSTOM_Y     ;Make the delay
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

                   lda BUFRLO             ;Check for termination indicator
                   and BUFRHI
                   and BFENLO
                   and BFENHI
                   cmp #$FF
                   beq SAVE_TERM
                                          ;Determine the speed

                   lda ZP_BLOCKFLAG             ;Check block flag
                   and #CS_HIGH_SPEED_BLOCK     ;Check for high speed
                   beq SAVE_BASE_SPEED          ;Just low speed, skip
                   jsr ZAP_HIGH_SPEED           ;Setup high speed
                   jmp SAVE_DOBLOCK             ;And jump over

SAVE_BASE_SPEED    jsr ZAP_BASE_SPEED   
    
SAVE_DOBLOCK       ldy #0                       ;Get ID byte
                   lda (BUFRLO),Y
                   sta ZP_ID_BYTE
                   
                   jsr WRITE_BLOCK

                   clc                          ;Increment table pointer
                   lda #5
                   adc ZP_TAB_PTR_LO
                   sta ZP_TAB_PTR_LO
                   bcc SAVE_CONT
                   inc ZP_TAB_PTR_HI  

;Add some gaps between blocks
SAVE_CONT          jsr DELAY_BLOCK

SAVE_NEXTBLOCK     jmp SAVE_LOOP                ;Continue saving.
;-----------------------------------------------------------------------
SAVE_TERM          jsr RECENV_TERM              ;Back with DMA and INTRs
                   lda #60                      ;Motor off
                   sta PACTL

                   bit CFG_FLAGS                ;Check for composite($80)
                   bmi SAVE_QUIT                ;If composite, quit
                   lda CFG_FLAGS                ;Check for alarm
                   and #CFG_F_ALARM                
                   beq SAVE_AGAIN               ;No alarm, just skip

SAVE_ALARM         jsr BEEP                     ;Three beeps for alarm
                   jsr BEEP
                   jsr BEEP
SAVE_AGAIN         jmp READY_SAVE               ;Otherwise start over    
SAVE_QUIT          rts          
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
; Short delay
;-----------------------------------------------------------------------                   
DELAY_SHORT        ldy #5                  ;Short delay, 0.1 sec
DELAY_CUSTOM_Y     jmp DELAY_WAIT

DELAY_BLOCK        ldy #5                  ;Default is 0.1 sec
                   lda ZP_BLOCKFLAG        ;Check block flag
                   and #CS_GAP_NONE        ;Skip gap?
                   bne DELAY_END           ;Yes, done
                   lda ZP_BLOCKFLAG        ;Check again
                   and #CS_GAP_ELONGATED   ;Is it elongated?
                   beq DELAY_WAIT          ;No, stick to default
                   ldy #50                 ;Yes, set to 1 second.

DELAY_WAIT         jsr WAIT_FOR_VBLANK     ;Wait for VBLANK
                   dey                     ;Decrement counter
                   bne DELAY_WAIT          ;Repeat until not zero
DELAY_END          rts
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

WR_PILOT_LEN_DET
Z00A           ldx #12                ;Presume "loose" pilot tone.
               lda ZP_BLOCKFLAG       ;Check what pilot we have?
               and #CS_PILOT_HEADER   ;Is that header pilot?
               beq @+                 ;No skip
Z01            ldx #30                ;Yes, header, 30*256
               bne WR_PILOT           ;Then continue
@              lda ZP_BLOCKFLAG       ;Check again
               and #CS_PILOT_DATA     ;Is that pilot tone for data block
               beq WR_PILOT           ;No, skip
Z00B           ldx #12                ;Yes, have it 12*256 pulses

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
            
WR_SAFPULSE    ldy #12                ;Ensure safety pulse is long enough.
               jsr DELAY_CUSTOM_Y
               lda #3                 ;End the safety pulse
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

              ldy #0
;Zap00
              lda (ZP_ZAP_PTR),Y
              sta Z00A+1
              sta Z00B+1
              iny
;Zap01
              lda (ZP_ZAP_PTR),Y
              sta Z01+1
              iny
;Zap02
              lda (ZP_ZAP_PTR),Y
              sta Z02+1
              iny
;Zap03
              lda (ZP_ZAP_PTR),Y
              sta Z03+1
              iny
;Zap04
              lda (ZP_ZAP_PTR),Y
              sta Z04+1
              iny
;Zap05
              lda (ZP_ZAP_PTR),Y
              sta Z05+1
              iny
;Zap06
              lda (ZP_ZAP_PTR),Y
              sta Z06+1
              iny
;Zap07
              lda (ZP_ZAP_PTR),Y
              sta Z07+1
              iny
;Zap08
              lda (ZP_ZAP_PTR),Y
              sta Z08+1
              iny
;Zap09
              lda (ZP_ZAP_PTR),Y
              sta Z09+1
              iny
;Zap10
              lda (ZP_ZAP_PTR),Y
              sta Z10+1
              iny
;Zap11
              lda (ZP_ZAP_PTR),Y
              sta Z11+1
              iny
;
              rts
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
;=======================================================================
; DATA AREAS
;=======================================================================
;=======================================================================
; Segment data table
;=======================================================================
            DATA_TABLE=*
            SFX_CAPACITY = 49152-DATA_TABLE-5-5-1
            START = START_ADDR
