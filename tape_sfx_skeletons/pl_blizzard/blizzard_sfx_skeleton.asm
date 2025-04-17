;=======================================================================
; Turbo Blizzard Self-extractor skeleton   
; Assemble with the MADS assembler
;=======================================================================
                 ICL "equates.asm" 
                 OPT H+,F-
;=======================================================================
; Global constants
;=======================================================================
                 ICL "../commons/gconstants.asm"                 
;=======================================================================
; Private constants
;=======================================================================
                START_ADDR      = 2700

                ZP_TAB_PTR_LO   = 128
                ZP_TAB_PTR_HI   = 129
                
                ZP_BLOCKFLAG    = 130
                CIOCHR          = $2F

                BLIZZARD_BLOCK_SYNC = $80
                BLIZZARD_BLOCK_HEADER = $40
                BLIZZARD_BLOCK_HASINIT = $20
;=======================================================================
; INITITALIZATION CODE 
;=======================================================================
                ICL "../commons/preinit.asm"            
;=======================================================================
; MAINLINE CODE
;=======================================================================
                   ORG START_ADDR
                   jmp ENTRY_ADDR
;-----------------------------------------------------------------------
; Screen lines
;-----------------------------------------------------------------------
                ICL "../commons/screen.asm"
;-----------------------------------------------------------------------
; Configuration
;-----------------------------------------------------------------------
                ICL "../commons/gconfig.asm"
;------------------------------------------------------------------------
; Silence configuration
;------------------------------------------------------------------------
CFG_S_BEFORE_SYNC  .BYTE $88
CFG_S_AFTER_SYNC   .BYTE $88
CFG_S_AFTER_HEADER .BYTE $88
CFG_S_AFTER_BLOCK  .BYTE $88
;------------------------------------------------------------------------
;Initialization
;------------------------------------------------------------------------
ENTRY_ADDR         
                ICL "../commons/screeni.asm"           
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
; Common Auxiliary Subroutines
;=======================================================================
                   ICL "../commons/aux.asm"                   
;=======================================================================
; Private Auxiliary Subroutines
;=======================================================================
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
 
@                  lda ZP_BLOCKFLAG            ;Check again
                   and #BLIZZARD_BLOCK_HASINIT ;Block with INIT?                  
                   beq DBA_NORM                ;Nope, just ordinary one
                   ldy #20                     ;Force 2 seconds
                   bne DBA_WAIT                ;And do it.

DBA_NORM           ldy CFG_S_AFTER_BLOCK       ;Ordindary block
DBA_WAIT           cpy #0                      ;Is there zero delay?
                   beq DBA_EXIT                ;Yes, skip it.
                   jsr DELAY_TENTHS
DBA_EXIT           rts
;-----------------------------------------------------------------------
; Delay before block
;-----------------------------------------------------------------------                   
DELAY_BLOCK_BEFORE lda ZP_BLOCKFLAG            ;Check block type
                   beq DBB_END

                   and #BLIZZARD_BLOCK_SYNC    ;Is that sync block?
                   beq DBB_END                 ;No, skip
                   ldy CFG_S_BEFORE_SYNC       ;Yes, load # of tenths

DBB_WAIT           cpy #0                      ;Is there zero delay?
                   beq DBB_END                 ;Yes, skip it.
                   jsr DELAY_TENTHS
DBB_END            rts

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
                  and #BLIZZARD_BLOCK_SYNC ;Is it SYNC block?
                  beq WR_PILOT            ;No, then go with the short pilot
                  ldy #$0D                ;Yes, have longer pilot (1.5 s)
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
; DISPLAY list
;=======================================================================
               ICL "../commons/dlist.asm"
;=======================================================================
; DATA AREAS
;=======================================================================
;=======================================================================
; Segment data table
;=======================================================================
            DATA_TABLE=*
            SFX_CAPACITY = 49152-DATA_TABLE-5-5-1
            START = START_ADDR
