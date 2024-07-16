;=======================================================================
; Turbo ROM TSFX Skeleton (AST,ATT,UM)  
; Assemble with the MADS assembler
;=======================================================================
           
                 ICL "equates.asm" 
                 OPT H+,F-

;=======================================================================
; Private constants
;=======================================================================
                START_ADDR      = 2840

                ZP_TAB_PTR_LO   = 128
                ZP_TAB_PTR_HI   = 129
                VBI_VCOUNT      = 124
                ZP_BLOCKFLAG    = 130
                CIOCHR          = $2F
                DMACTL          = $D400
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
           CFG_F_LONGSEP   = $40       ;Long gap
           CFG_F_ALARM     = $20       ;Alarm after saving
CFG_SEP_DURATION .BYTE (3*45)
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

                   bit CFG_FLAGS          ;Check if long sep requested
                   bvc NORM_SEP           ;No, skip to normal sep
  
                   ldy CFG_SEP_DURATION   ;Long sep
                   jsr DELAY_CUSTOM_Y     ;Make long sep
NORM_SEP           jsr DELAY_SHORT        ;Make short sep
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

DELAY_BLOCK        ldy #10                 ;Default is 0.2 sec

DELAY_WAIT         jsr WAIT_FOR_VBLANK     ;Wait for VBLANK
                   dey                     ;Decrement counter
                   bne DELAY_WAIT          ;Repeat until not zero
DELAY_END          rts

;=======================================================================
; Write block of data
; ZP Variables used
; $CB,$CC - Buffer pointer (current byte)
; $CD,$CE - Number of bytes to write 
; $CF,$D0 - Byte counter
; BUFRLO,BUFRHI,BFENLO,BFENHI - Counters
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
                  
LA17C   LDA #$00                     ;Disable interrupts
        STA IRQEN
LA181   STA AUDC4                    ;Set POKEY channels   
        STA AUDC3
        LDA #$28
        STA AUDCTL

        LDX #$00                     ;Delay before block
        LDY #$00
LA190   DEY
        BNE LA190
        DEX
        BNE LA190
        STX $CF                      ;Reset byte counter to zero
        STX $D0
LA19A   LDY CIOCHR                   ;Write pilot tone
        STY BUFRHI
        STY BFENLO
        LDA #$84
        STA BUFRLO
        LDA #$01
        STA AUDF4
        LDX #$29
        LDY #$3C
        JSR LA218
LA1B0   JSR LA20A
        DEC BUFRHI
        BNE LA1B0
        DEC BFENLO
        BNE LA1B0

        LDA #$00                      ;Write synchronization pulse
        STA AUDF4
        LDX #$69
        JSR LA20A

LA1C5   LDY #$00                      ;Reset bit counter to 0 
        LDA ($CB),Y                   ;Get byte in the buffer
        STA BFENHI                    ;Store to work byte
LA1CB   LDX #$EB                      ;Set timing constant            
        LSR BFENHI                    ;Check if bit 0 or 1
        BCS LA1D3                     ;Set timing constant
        LDX #$69
LA1D3   TYA                           ;Push the bit counter
        PHA                           
        JSR LA20A                     ;Write pulse 
        PLA                           ;Pop the bit counter
        TAY                           
        INY                           ;Increment the bit counter
        CPY #$08                      ;Check if 8 bits written
        BNE LA1CB                     ;Not yet, do next bit

        INC $CB                       ;Increment buffer pointer
        BNE LA1E5
        INC $CC

LA1E5   INC $CF                       ;Increment byte counter
        BNE LA1EB
        INC $D0

LA1EB   LDA $CF                       ;Check if all bytes written
        CMP $CD
        BNE LA1C5                     ;No, keep writing bytes
        LDA $D0
        CMP $CE
        BNE LA1C5                     ;No, keep writing bytes

        LDA #$04                      ;Write termination signal
        STA BUFRHI
        LDA #$01
LA1FD   STA AUDF4
        LDX #$29
        JSR LA20A
LA202   DEC BUFRHI
        BNE LA202
        RTS

LA20A   LDY #$34                      ;Write one pulse
        JSR LA211
        LDY #$3C
LA211   LDA #$04
LA213   BIT IRQST
        BNE LA213
LA218   STX AUDF3
        STY COLBK
        STY PBCTL
        LDY BUFRLO
        STY STIMER
        LDA #$FB
        STA IRQEN
        STY IRQEN
        RTS
;-------------------------------------------------------------------------------
; Terminate recording environment
;-------------------------------------------------------------------------------               
RECENV_TERM
        LDA #$FF
        STA NMIEN
        CLI
;       LDA #$3C
;       STA PACTL
        LDA #$3C
        STA PBCTL
        LDA #$03
        STA SSKCTL
        STA SOUNDR
        STA SKCTL
        RTS
;-------------------------------------------------------------------------------
; Initiate recording environment
;-------------------------------------------------------------------------------               
RECENV_INIT
;       LDA #$34
;       STA PACTL
        LDA #$00
        STA NMIEN
        STA DMACTL
        STA AUDCTL
        LDA #$03
        STA SKCTL
        SEI
        LDA #$34
        STA PBCTL
        RTS
;=======================================================================
; DISPLAY DATA
;=======================================================================
;-----------------------------------------------------------------------
; Display list
;-----------------------------------------------------------------------
DLIST              .BYTE 112,112,112
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
