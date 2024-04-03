;=======================================================================
; Atari Super Turbo TSFX Skeleton   
; Assemble with the MADS assembler
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
                START_ADDR      = 2840
                ZP_BLOCKFLAG    = 130
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
    
SAVE_DOBLOCK       jsr WRITE_BLOCK

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
                   stx AUDC1                     ;Reset AUDC1 and AUDF1
                   stx AUDF1
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
; Write block of data
; ZP Variables used
; $CB,$CC - Buffer pointer (current byte)
; $CD,$CE - Number of bytes to write 
; $CF,$D0 - Byte counter
; $D1 - Checksum, EOR based

;=======================================================================
WRITE_BLOCK        lda BUFRLO  ;Set buffer pointer
                   sta $CB
                   lda BUFRHI
                   sta $CC

                   sec         ;Count number of bytes
                   lda BFENLO
                   sbc BUFRLO
                   sta $CD
                   lda BFENHI
                   sbc BUFRHI
                   sta $CE

                   inc $CD
                   bne WB_BUF_ADJ0
                   inc $CE
WB_BUF_ADJ0                     
                  
                   JSR LCC0A   ;Prepare for writing
                   JSR LCCD8   ;Write pilot tone
                   JSR LCD52   ;Write sync pulse
                   JSR LCCFF   ;Write a block of data
                   JSR LCD72   ;Write termination signal
                   RTS

;Write a block of data
LCCFF             LDY #$00
             STY $CF
             STY $D0
             STY $D1
LCD07             LDY #$00
                 LDA ($CB),Y
                 PHA
                 EOR $D1
                 STA $D1
                 PLA
LCD11            LDX #$50
                 LSR
                 BCS LCD18
                 LDX #$28
LCD18            PHA
              TXA
              PHA
              LDA #$34
              STA PBCTL
              STA COLBK
LCD23         DEX
              BNE LCD23
              LDA #$3C
              STA PBCTL
              STA COLBK
              PLA
              TAX
LCD30          DEX
                 BNE LCD30
                 PLA
                 INY
                 CPY #$08
                 BNE LCD11
                 INC $CB
                 BNE LCD3F
                 INC $CC
LCD3F            INC $CF
                 BNE LCD45
                 INC $D0
LCD45            LDA $CF
                 CMP $CD
                 BNE LCD07
                 LDA $D0
                 CMP $CE
                 BNE LCD07
                 RTS

;Write sync pulse
LCD52              LDX #$50
LCD54              DEX
                   BNE LCD54
                   LDX #$28
                   LDA #$34
                   STA PBCTL
                   STA COLBK
LCD61              DEX
                   BNE LCD61
                   LDA #$3C
                   STA PBCTL
                   STA COLBK
                   LDX #$28
LCD6E              DEX
                   BNE LCD6E
                   RTS


;Write pilot tone
LCCD8              LDY #$3C
LCCDA              TYA
                   PHA
LCCDC              LDX #$50
LCCDE              DEX
                   BNE LCCDE
                   LDA #$34
                   STA PBCTL
                   STA COLBK
                   LDX #$50
LCCEB              DEX
                   BNE LCCEB
                   LDA #$3C
                   STA PBCTL
                   STA COLBK
                   DEY
                   BNE LCCDC
                   PLA
                   TAY
                   DEY
                   BNE LCCDA
                   RTS

;Write termination signal
LCD72              LDY #$64
LCD74              LDX #$C8
                   LDA #$34
                   STA PBCTL
LCD7B              DEX
                   BNE LCD7B
                   LDA #$3C
                   STA PBCTL
                   LDX #$C8
LCD85              DEX
                   BNE LCD85
                   DEY
                   BNE LCD74
                   RTS

;Prepare for recording of the block
LCC0A              SEI
;                  LDA #$00
;                  STA NMIEN
;                  STA DMACTL
                   LDA #$03
                   STA SKCTL
                   LDA #$34
                   STA PBCTL
;                  LDA #$34
;                  STA PACTL
                   RTS

;Terminate recording of the block
LCC96              CLI
                   LDA #$3C
                   STA PBCTL
;                  STA PACTL
;                  LDA #$40
;                  STA NMIEN
                   RTS
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
