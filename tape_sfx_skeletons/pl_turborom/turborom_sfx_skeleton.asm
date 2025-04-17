;=======================================================================
; Turbo ROM TSFX Skeleton  
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
                CIOCHR          = $2F
                DMACTL          = $D400
                TROM_BLOCK_HEADER = $80
                TROM_BLOCK_DATA   = $40
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
; Common Auxiliary Subroutines
;=======================================================================
                   ICL "../commons/aux.asm"             
;=======================================================================
; Private auxiliary subroutines
;=======================================================================                   
;-----------------------------------------------------------------------
; Block delay
;-----------------------------------------------------------------------                   
DELAY_BLOCK        ldy #10                 ;Default is 0.2 sec
                   jsr DELAY_CUSTOM_Y
                   rts
;=======================================================================
; Write block of data
; ZP Variables used
; $CB,$CC - Buffer pointer (current byte)
; $CD,$CE - Number of bytes to write 
; $CF,$D0 - Byte counter
; BUFRLO,BUFRHI,BFENLO,BFENHI - Counters
; CIOCHR - Pilot tone duration
;=======================================================================
WRITE_BLOCK        ldx #$0C          ;Presume default pilot tone duration
                   lda ZP_BLOCKFLAG  ;Check block flag
                   and #TROM_BLOCK_HEADER  ;Is that header?
                   beq WB_PILOT_01         ;No, try other
                   ldx #$14                ;Otherwise set long duration
                   bne WB_PILOT_02         ;And skip

WB_PILOT_01        lda ZP_BLOCKFLAG        ;Check block flag again
                   and #TROM_BLOCK_DATA    ;Is that data block?
                   beq WB_PILOT_02         ;No, just skip
                   ldx #$06                ;Set short duration
WB_PILOT_02        stx CIOCHR      
                   

                   lda BUFRLO  ;Set buffer pointer
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
LA19A   LDY CIOCHR                   ;Write pilot tone, CIOCHR=duration
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
LA202   JSR LA20A
        DEC BUFRHI
        BNE LA202
        RTS

LA20A   LDY #$34                      ;Write one pulse
        JSR LA211
        LDY #$3C
LA211   LDA #$04                      ;Delay until INTR triggers
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
        LDA #$40
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
