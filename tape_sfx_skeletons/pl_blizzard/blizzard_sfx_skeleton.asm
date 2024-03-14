;=======================================================================
; Turbo Blizzard Self-extractor skeleton   
; Assemble with the MADS assembler
;=======================================================================
           
                 ICL "equates.asm" 
                 OPT H+,F-

;=======================================================================
; Private constants
;=======================================================================
                ZP_TAB_PTR_LO   = 128
                ZP_TAB_PTR_HI   = 129
                VBI_VCOUNT      = 124
                START_ADDR      = 2840
                ZP_BLOCKFLAG    = 130
                ZP_ID_BYTE      = 131
                CIOCHR = $2F
;
                BF_NOSILENCE    = 0x80
                BF_LONGPILOT    = 0x40
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
           CFG_F_LONGGAP   = $40       ;Long gap
           CFG_F_ALARM     = $20       ;Alarm after saving
CFG_LGAP_DURATION .BYTE (3*45)
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

                   bit CFG_FLAGS          ;Check if long gap requested
                   bvc NORM_GAP           ;No, skip to normal delay
  
                   ldy CFG_LGAP_DURATION  ;Long gap
                   jsr DELAY_LOOP_E       ;Make long gap
NORM_GAP           jsr SHORT_DELAY
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
                   sta ZP_ID_BYTE
                   
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
                   lda #$00
                   sta AUDC1
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
SHORT_DELAY        ldy #44
DELAY_LOOP_E       ldx #255            
DELAY_LOOP_I       stx WSYNC
                   dex
                   bne DELAY_LOOP_I
                   dey 
                   bne DELAY_LOOP_E
                   rts

;=======================================================================
; Write block of data, turbo blizzard
; X,Y Buffer end range
;=======================================================================
WRITE_BLOCK       ldx #<DATA_TABLE+8
                  ldy #>DATA_TABLE+8
j0DAB             JSR SET_34_32_36 ;$0D68
                  JSR NUL_SPACE    ;$0D19

                  LDA SOUNDR ;$41
                  BEQ sk466

                  LDA #$A8
                  STA AUDC4 ;$D207

sk466             JSR j0D36

                  LDX #0
                  STA ($34,X)

                  LDY #$02
                  LDX #$2E
                  JSR j0E15

                  DEC CIOCHR ;$2F

sk52              LDX #8
                  LDA ($2A,X)
                  STA $30

sk50              JSR j0E4A
                  ASL $30
                  BCC sk49

                  LDA #$D1
                  STA AUDF3 ;$D204
                  BCS sk499

sk49              LDA #$87
                  STA AUDF3 ;$D204

sk499             JSR j0DFD
                  DEX
                  BNE sk50

                  JSR INC_32 ;$0D9B
                  BCS sk52

                  INC CIOCHR ;$2F
                  BEQ sk52

                  JSR VBI_IRQ ;$0D4F

                  LDY #1
                  JMP ERR_Y
sk44              JMP ERR   ;$0CBD

* ;$0DFD

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
* ;$0E15

j0E15 LDA #1
      STA AUDF4 ;$D206

     LDA #$AE
     STA AUDF3 ;$D204

     STY CIOCHR ;$2F

     LDY #$84
     STY STIMER ;$D209
     STY IRQEN ;$D20E

sk46 JSR j0DFD
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

* ;$0E4A

j0E4A LDA #4

sk45 BIT IRQEN ;$D20E
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
SET_34_32_36 ;STX $34
             ;STY $35

             ;LDX <BUFOR
             ;STX $32
             ;STX $36

             ;LDX >BUFOR
             ;STX $33
             ;STX $37

             lda BUFRLO
             sta LTEMP
             lda BUFRHI
             sta BUFRFL 
           
             RTS
;-------------------------------------------------------------------------------
; Pause
;-------------------------------------------------------------------------------
PAUSE LDX #0
      LDA #3
      JSR $E45C

     LDA #$FF
     STA CDTMF3 ;$22A

sk42 LDA CDTMF3 ;$22A
     BNE sk42
     RTS

;-------------------------------------------------------------------------------
; Disable interrupts and display
;-------------------------------------------------------------------------------
NUL_SPACE LDA #0

     STA NMIEN  ;$D40E
     STA DMACLT ;$D400
     STA IRQEN  ;$D20E
     STA AUDC4  ;$D207
     STA AUDC3  ;$D205
     SEI

     LDA #$83  ;%10000011
     STA SKCTL ;$D20F

     LDA #$28  ;%00101000
     STA AUDCTL ;$D208
     RTS
;-------------------------------------------------------------------------------
;???
;-------------------------------------------------------------------------------
j0D36 LDY $36
      LDA #0
      STA $36

sk48 CLC
     ADC ($36),Y
     INY
     BNE sk47

     INC $37

sk47 CPY $34
     BNE sk48

     LDX $37
     CPX $35
     BNE sk48
     RTS
;-------------------------------------------------------------------------------
; ???
;-------------------------------------------------------------------------------
INC_32 INC $32
       BNE sk51

     INC $33

sk51 SEC
     LDA $34
     SBC $32

     LDA $35
     SBC $33
     RTS

;-------------------------------------------------------------------------------
;???
;-------------------------------------------------------------------------------
VBI_IRQ LDA #0
        STA IRQEN ;$D20E
        STA AUDC4 ;$D207

     LDA #$40
     STA NMIEN ;$D40E

     LDA $10
     STA IRQEN ;$D20E

     CLI

     LDA #3
     STA SKCTL ;$D20F
     RTS

;-------------------------------------------------------------------------------
; Error handling
;-------------------------------------------------------------------------------
ERR_Y  EQU *
sk70   TYA
       RTS

* ;$0CBD

ERR  JSR VBI_IRQ ;$0D4F

     LDX $0318
     TXS

     ;JSR OFF_MOTOR ;$109C   @@

     LDY #0
     STY $11
     LDY #$80
     STY NOPS
     RTS

;-------------------------------------------------------------------------------
; Data area
;-------------------------------------------------------------------------------
NOPS .BYTE 0

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

