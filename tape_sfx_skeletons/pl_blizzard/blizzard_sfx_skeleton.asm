;=======================================================================
; Turbo Blizzard Self-extractor skeleton   
; Assemble with the MADS assembler
;=======================================================================
                 ICL "equates.asm" 
                 OPT H+,F-
;=======================================================================
; Private constants
;=======================================================================
                START_ADDR      = 2800

                ZP_TAB_PTR_LO   = 128
                ZP_TAB_PTR_HI   = 129
                VBI_VCOUNT      = 124
                
                ZP_BLOCKFLAG    = 130
                CIOCHR          = $2F

                BLIZZARD_BLOCK_SYNC = $80
                BLIZZARD_BLOCK_HEADER = $40
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
           CFG_F_COMPOSITE = $80       ;Part of composite
           CFG_F_LONGSEP   = $40       ;Long separator
           CFG_F_ALARM     = $20       ;Alarm after saving
CFG_SEP_DURATION .BYTE (3*45)
;------------------------------------------------------------------------
; Silence configuration
;------------------------------------------------------------------------
CFG_S_BEFORE_SYNC  .BYTE $88
CFG_S_AFTER_SYNC   .BYTE $88
CFG_S_AFTER_HEADER .BYTE $88
;------------------------------------------------------------------------
;Initialization
;------------------------------------------------------------------------
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
                   lda #$8A
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
;From now on, disable interrupts and DMA, keep motor ON until the contents
;is fully recorded.
;-----------------------------------------------------------------------
                   jsr RECENV_INIT
                   lda #52
                   sta PACTL
                   
                   bit CFG_FLAGS           ;Is this composite ? ($80)
                   bpl SAVE_LOOP           ;No, skip initial delay
   
                   ldy #5                 ;Presume normal separation (0.1)
                   bit CFG_FLAGS          ;Check if long sep requested ($40)
                   bvc NORM_SEP           ;No, skip to normal sep
                   ldy CFG_SEP_DURATION   ;Long sep
NORM_SEP           jsr DELAY_CUSTOM_Y     ;Make long sep
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
    
SAVE_DOBLOCK       
;Add gap before the block
                  jsr DELAY_BLOCK_BEFORE

;Write the block  
                   
                   jsr WRITE_BLOCK

;Continue to the next block

                   clc                          
                   lda #5
                   adc ZP_TAB_PTR_LO
                   sta ZP_TAB_PTR_LO
                   bcc SAVE_CONT
                   inc ZP_TAB_PTR_HI  

;Add gap after the block
SAVE_CONT          jsr DELAY_BLOCK_AFTER

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
; Delay after block
;-----------------------------------------------------------------------                   
DELAY_BLOCK_AFTER  lda ZP_BLOCKFLAG            ;Check block type
                   beq DBA_NORM                ;Ordinary block, norm. delay

                   and #BLIZZARD_BLOCK_SYNC    ;Is that sync block?
                   beq @+                      ;No, skip
                   ldy CFG_S_AFTER_SYNC        ;Yes, load # of tenths
                   jmp DBA_WAIT

@                  lda ZP_BLOCKFLAG            ;Check again
                   and #BLIZZARD_BLOCK_HEADER  ;Is that header block?
                   beq @+                      ;No, skip
                   ldy CFG_S_AFTER_HEADER      ;Yes, load # of tenths 
                   jmp DBA_WAIT 
@
DBA_NORM           ldy #2
DBA_WAIT           jsr DELAY_TENTHS
                   rts
;-----------------------------------------------------------------------
; Delay before block
;-----------------------------------------------------------------------                   
DELAY_BLOCK_BEFORE lda ZP_BLOCKFLAG            ;Check block type
                   beq DBB_END

                   and #BLIZZARD_BLOCK_SYNC    ;Is that sync block?
                   beq DBB_END                 ;No, skip
                   ldy CFG_S_BEFORE_SYNC       ;Yes, load # of tenths

DBB_WAIT           jsr DELAY_TENTHS
DBB_END            rts
;-----------------------------------------------------------------------
; Delay atoms
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

DELAY_CUSTOM_Y     jsr WAIT_FOR_VBLANK
                   dey
                   bne DELAY_CUSTOM_Y
                   rts
;=======================================================================
; Write block of data, turbo blizzard
; BUFRLO,BUFRHI - BFENHO,BFENHI - Buffer Range
; 
;=======================================================================
WRITE_BLOCK       
j0DAB             JSR DO_BUFFER_SETUP    ;Setup the buffer range
                  JSR NUL_SPACE          

sk466             LDY #$02               ;Presume short pilot
                  LDX #$2E               ;Timing constant
                  lda ZP_BLOCKFLAG       ;Check block flag 
                  and #BLIZZARD_BLOCK_SYNC  ;Is it SYNC block
                  beq WR_PILOT           ;No, then go with the short pilot
                  ldy #$20               ;Yes, have longer pilot
WR_PILOT          JSR DO_PILOT

                  DEC CIOCHR ;$2F

sk52              LDX #8                 ;Get byte from buffer
                  LDA ($2A,X)
                  STA $30                ;Store it to temporary location

sk50              JSR j0E4A              
                  ASL $30
                  BCC sk49

                  LDA #$D1
                  STA AUDF3 
                  BCS sk499

sk49              LDA #$87
                  STA AUDF3 

sk499             JSR j0DFD
                  DEX
                  BNE sk50

                  JSR DO_ADVANCE_BUF 
                  BCS sk52

                  INC CIOCHR ;$2F
                  BEQ sk52

                  JSR VBI_IRQ ;$0D4F

                  LDY #1
                  JMP TERM_OK
sk44              JMP TERM_ERROR  ;$0CBD

;-------------------------------------------------------------------------------
; Generate edge
;-------------------------------------------------------------------------------
j0DFD             LDA #4

sk43              BIT IRQST ;$D20E
                  BNE sk43

                  STY STIMER ;$D209

                  LDA #$83
                  STA SKCTL ;$D20F

                  LDA #$FB
                  STA IRQEN ;$D20E
                  STY IRQEN ;$D20E
                  RTS

;-------------------------------------------------------------------------------
; Genereate pilot tone
;-------------------------------------------------------------------------------
DO_PILOT          LDA #1
                  STA AUDF4 ;$D206

                  LDA #$AE
                  STA AUDF3 ;$D204

                  STY CIOCHR ;$2F

                  LDY #$84
                  STY STIMER ;$D209
                  STY IRQEN ;$D20E

sk46              JSR j0DFD
                  JSR j0E4A

                  DEX
                  BNE sk46

                  DEC CIOCHR ;$2F
                  BNE sk46

                  LDA #0
                  STA AUDF4 ;$D206

                  LDA #$87
                  STA AUDF3 ;$D204

                  JSR j0DFD
                  JSR j0E4A
                  JSR j0DFD
                  RTS

;-------------------------------------------------------------------------------
; Generate edges
;-------------------------------------------------------------------------------
j0E4A             LDA #4

sk45              BIT IRQEN ;$D20E
                  BPL sk44
                  BNE sk45

                  LDA #3
                  STA SKCTL ;$D20F

                  LDA #$FB
                  STA IRQEN ;$D20E
                  STY IRQEN ;$D20E
                  RTS

;-------------------------------------------------------------------------------
; Buffer setup for write routine
;-------------------------------------------------------------------------------
DO_BUFFER_SETUP   lda BUFRLO
                  sta LTEMP
                  lda BUFRHI
                  sta BUFRFL 
                  rts 
;-------------------------------------------------------------------------------
; Pause
;-------------------------------------------------------------------------------
PAUSE             LDX #0
                  LDA #3
                  JSR $E45C

                  LDA #$FF
                  STA CDTMF3 ;$22A

sk42              LDA CDTMF3 ;$22A
                  BNE sk42
                  RTS

;-------------------------------------------------------------------------------
; Disable interrupts and display
;-------------------------------------------------------------------------------
NUL_SPACE         lda #0

                  sta AUDC4  ;$D207
                  sta AUDC3  ;$D205
                  sei

                  lda #$83  ;%10000011
                  sta SKCTL ;$D20F

                  lda #$28  ;%00101000
                  sta AUDCTL ;$D208
                  rts
;-------------------------------------------------------------------------------
; Advance the buffer pointer and check if whole buffer recorded
;-------------------------------------------------------------------------------
DO_ADVANCE_BUF    inc $32
                  bne sk51

                  inc $33

sk51              sec
                  lda $34
                  sbc $32

                  lda $35
                  sbc $33
                  rts
;-------------------------------------------------------------------------------
;Restore VBI and IRQ to its original values 
;-------------------------------------------------------------------------------
VBI_IRQ           LDA #0
                  STA IRQEN ;$D20E
                  STA AUDC4 ;$D207

;                  LDA #$40
;                  STA NMIEN ;$D40E

                  LDA $10
                  STA IRQEN ;$D20E

                  CLI

                  LDA #3
                  STA SKCTL ;$D20F
                  RTS
;-------------------------------------------------------------------------------
; Error handling, termination
;-------------------------------------------------------------------------------
TERM_OK           RTS
TERM_ERROR        JMP COLDSV  

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
