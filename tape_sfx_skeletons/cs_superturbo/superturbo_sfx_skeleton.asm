;=======================================================================
; Super Turbo Self-extractor skeleton   
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
                ZP_ID_BYTE      = 131
                ZP_ZAP_PTRLO    = 132
                ZP_ZAP_PTRHI    = 133
                ZP_ZAP_PTR      = ZP_ZAP_PTRLO 
                ZP_SILENCE      = 134
              
;Block flag constants
                CS_PILOT_HEADER = 0x80
                CS_PILOT_DATA   = 0x40
                CS_PILOT_LOOSE  = 0x20
                 
                CS_HIGH_SPEED_BLOCK = 0x01
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
SAVE_LOOP          ldx #4
                   ldy #0                 ;Get buffer range
@                  lda (ZP_TAB_PTR_LO),Y
                   sta BUFRLO,Y
                   iny
                   dex
                   bne @-
                   
                   lda (ZP_TAB_PTR_LO),Y
                   sta ZP_BLOCKFLAG 
                   iny 
                   lda (ZP_TAB_PTR_LO),Y
                   sta ZP_SILENCE
                   iny

                   lda BUFRLO
                   and BUFRHI
                   and BFENLO
                   and BFENHI
                   cmp #$FF
                   beq SAVE_TERM

SAVE_DOSPEED
                   jsr ZAP_BASE_SPEED           ;Zap for base speed
                   lda ZP_BLOCKFLAG             ;Check block flag
                   and #CS_HIGH_SPEED_BLOCK     ;Check for high speed
                   beq SAVE_DOBLOCK             ;Just low speed, skip
                   jsr ZAP_HIGH_SPEED           ;Setup high speed
                   
SAVE_DOBLOCK 
SAVE_DOSILENCE                   
                   ldy ZP_SILENCE               ;Silence before the block
                   beq SAVE_DOWRITE             ;Yes, skip
                   jsr DELAY_TENTHS             ;Otherwise, do silence   
    
SAVE_DOWRITE       ldy #0                       ;Get ID byte
                   lda (BUFRLO),Y
                   sta ZP_ID_BYTE
                   
                   jsr WRITE_BLOCK

                   clc                          ;Increment table pointer
                   lda #6
                   adc ZP_TAB_PTR_LO
                   sta ZP_TAB_PTR_LO
                   bcc SAVE_CONT
                   inc ZP_TAB_PTR_HI  
SAVE_CONT          

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
                   ICL "../commons/routines.asm"                   
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
            SFX_CAPACITY = 49152-DATA_TABLE-6-6-1
            START = START_ADDR
            