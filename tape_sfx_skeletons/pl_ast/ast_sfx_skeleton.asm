;=======================================================================
; Atari Super Turbo TSFX Skeleton (AST,ATT,UM)  
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
                START_ADDR      = 2840

                ZP_TAB_PTR_LO   = 128
                ZP_TAB_PTR_HI   = 129
                ZP_BLOCKFLAG    = 130
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
    
SAVE_DOBLOCK       jsr WRITE_BLOCK

                   clc                          ;Increment table pointer
                   lda #5
                   adc ZP_TAB_PTR_LO
                   sta ZP_TAB_PTR_LO
                   bcc SAVE_CONT
                   inc ZP_TAB_PTR_HI  

;Add some gaps between blocks
SAVE_CONT          ldy #2
                   jsr DELAY_TENTHS

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
                   JSR LCC96   ;Terminate writing

                   RTS

;Write a block of data
LCCFF              LDY #$00
                   STY $CF
                   STY $D0
                   STY $D1
LCD07              LDY #$00
                   LDA ($CB),Y
                   PHA
                   EOR $D1
                   STA $D1
                   PLA
LCD11              LDX #$50
                   LSR
                   BCS LCD18
                   LDX #$28
LCD18              PHA
                   TXA
                   PHA
                   LDA #$34
                   STA PBCTL
                   STA COLBK
LCD23              DEX
                   BNE LCD23
                   LDA #$3C
                   STA PBCTL
                   STA COLBK
                   PLA
                   TAX
LCD30              DEX
                   BNE LCD30
                   PLA
                   INY
                   CPY #$08
                   BNE LCD11
                   INC $CB
                   BNE LCD3F
                   INC $CC
LCD3F              INC $CF
                   BNE LCD45
                   INC $D0
LCD45              LDA $CF
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
                   LDA #0                ;Reset background to black
                   STA COLBK
                   RTS
;-------------------------------------------------------------------------------
; Terminate recording environment
;-------------------------------------------------------------------------------               
RECENV_TERM        lda #64
                   sta NMIEN
                   sta IRQEN
                   jsr WAIT_FOR_VBLANK   
                   lda #34                
                   sta DMACLT             
                   rts
;-------------------------------------------------------------------------------
; Initiate recording environment;
;-------------------------------------------------------------------------------               
RECENV_INIT        ldy #0
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
