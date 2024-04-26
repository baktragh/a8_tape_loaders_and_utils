;=======================================================================
; Standard Self-extractor skeleton   
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
                
                ZP_SIO_BUFRLO   = 131
                ZP_SIO_BUFRHI   = 132
                ZP_SIO_LENLO    = 133
                ZP_SIO_LENHI    = 134

;Block flag constants
                
                STANDARD_PILOT_LONG     = 0x80;         ;20 s
                STANDARD_PILOT_MEDIUM   = 0x40;         ;3  s
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
CFG_SEP_DURATION .BYTE (3*50)
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
                   lda #52
                   sta PACTL

                   bit CFG_FLAGS          ;Check if long separator requested
                   bvc NORM_SEP           ;No, skip to normal delay
  
                   ldy CFG_SEP_DURATION   ;Long separator
                   jsr DELAY_CUSTOM_Y     ;Make long separator
NORM_SEP           jsr DELAY_SHORT        ;Make normal separator
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
SAVE_CONT

SAVE_NEXTBLOCK     jmp SAVE_LOOP                ;Continue saving.
;-----------------------------------------------------------------------
SAVE_TERM          lda #60
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

DELAY_WAIT         jsr WAIT_FOR_VBLANK     ;Wait for VBLANK
                   dey                     ;Decrement counter
                   bne DELAY_WAIT          ;Repeat until not zero
DELAY_END          rts

;=======================================================================
; Write block of data
; Inputs:
; BUFRLO,BUFRHI  - Right before first byte
; BFENLO,BFENHI  - Last byte
;=======================================================================
WRITE_BLOCK        

;We are going to use SIO to write raw tape blocks.
;We need to adjust the buffer range, so that we have buffer start
;and length, for the SIO call

;First calculate length.
                   sec
                   lda BFENHI
                   sbc BUFRHI
                   sta ZP_SIO_LENHI
                   lda BFENLO
                   sbc BUFRLO
                   sta ZP_SIO_LENLO

;Then increment the buffer pointer
                   lda BUFRLO
                   sta ZP_SIO_BUFRLO
                   lda BUFRHI
                   sta ZP_SIO_BUFRHI

                   inc ZP_SIO_BUFRLO
                   bne @+
                   inc ZP_SIO_BUFRHI
@
;Now prepare the SIO call
                   lda #$60               ;Cassette
                   sta DDEVIC
                   lda #0                 
                   sta DUNIT              ;Zero unit
                   sta DCOMND             ;No command
                   sta DUNUSE             ;Zero unused byte
                   sta DAUX1              ;Zero AUX1 byte
          
                   lda #$80               ;Indicate Write
                   sta DSTATS
           
                   lda ZP_SIO_BUFRLO     ;Set buffer address
                   sta DBUFLO
                   lda ZP_SIO_BUFRHI
                   sta DBUFHI
          
                   lda ZP_SIO_LENLO      ;Set buffer length
                   sta DBYTLO
                   lda ZP_SIO_LENHI
                   sta DBYTHI
          
                   lda #$00               ;Short gaps between records
                   sta DAUX2
          
                   jsr SIOV               ;Call SIO 

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
            dta <(BLOCK1-1),>(BLOCK1-1),<BLOCK1_e,>BLOCK1_E,$00
            dta $FF,$FF,$FF,$FF,$FF 
            SFX_CAPACITY = 49152-DATA_TABLE-5-5-1
            START = START_ADDR


BLOCK1      dta 0x55,0x55,0xFC,1,2,3,4,5,7,8,9,10,11,12,13,14,15,16
BLOCK1_E    equ *-1
         
