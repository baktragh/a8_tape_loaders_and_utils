;=======================================================================
; Turbo 6000 Self-extractor skeleton   
; Assemble with the MADS assembler
;
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
                ZP_ID_BYTE      = 131

;Block flag constants
                
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
CFG_SAFETY_DELAY .BYTE 5               ;Safety delay (VBLs)
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

                   ldy CFG_SAFETY_DELAY   ;Presum just safety delay
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
BEEP               lda #0
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

DELAY_WAIT         jsr WAIT_FOR_VBLANK     ;Wait for VBLANK
                   dey                     ;Decrement counter
                   bne DELAY_WAIT          ;Repeat until not zero
DELAY_END          rts

;=======================================================================
; Write block of data
; Inputs:
; $32,$33 - Buffer start
; $34,$35 - Buffer end
; $3E     - Duration of the pilot tone (1 unit=256 pulses)
;
; Work bytes
; $2F     - Currently written byte
; $31     - Checksum
;
; Pulses are written by altering BREAK on SKCTL
;
; Special notes
; - Buffer range adjustment is needed
; - Checksum is calculated, but since it is embedded in the block,
;   zero is written instead as a padding byte,
; - The ID  byte is taken from the beginning of the block 
;=======================================================================
WRITE_BLOCK               
;Correct the buffer range
; - The table entry points to the ID byte, we need it to point past the ID byte
; - The buffer end must be incremented by 1

                   inc BUFRLO          ;Increment LO by 1
                   bne @+              ;If not zeroed, skip
                   inc BUFRHI          ;Increment HI by 1
@
                   inc BFENLO          ;Increment LO by 1
                   bne @+              ;If not zeroed, skip
                   inc BFENHI          ;Increment HI by 1
@

;Set pilot tone duration
                   LDX #$05               ;Pilot tone 5*256 pulses
                   STX $3E
;Output pilot tone
                   JSR MAA0               ;Write pilot tone
                   lda ZP_ID_BYTE      ;Identification byte
                   JSR MAC3               ;Write identification byte
                   STY $31               
                   LDX #$07            ;Set delay
                   NOP
;
MA5E               LDA ($32),Y               ;Get byte to write
                   JSR MAC3               ;Write byte
                   LDX #$03               ;Delay
                   INC $32               ;Increment buffer pointer (LO)
                   BNE MA6D            ;Not overflow, skip
                   INC $33             ;Increment buffer pointer (HI)
                   DEX               ;Delay compensation               
                   DEX                 ;Delay compensation

MA6D               LDA $32               ;Check EOF
                   CMP $34             ;Check if BURLO,HI == BFENLO,HI
                   LDA $33
                   SBC $35
                   BCC MA5E            ;Not EOF, continue loop
                   NOP               ;EOF reached                
;
MA78               LDA #0              ;Take the checksum ;Was LDA $31
                   JSR MAC3            ;Write padding byte
;Writing is over
                   RTS

;Write pilot tone
;Pilot tone begins with sequence of $02s
;And ends with $09,$08,$07...
MAA0               NOP                
                   LDY #$00               ; Clear error flag
                   STY $30                
MAA5               LDA #$02               ; Ready value $02
                   JSR MAC3            ; Do write byte
                   LDX #$07               ; Set delay
                   DEY                 ; Decrement Y
                   CPY #$09            ; Did Y reach $09
                   BNE MAA5            ; No, keep outputting $02s

                   LDX #$05               ; Set delay
                   DEC $3E             ; Decrement pilot tone unit counter
                   BNE MAA5            ; If not all units written, repeat

;When enough $02 written, write the pilot tone end sequence
MAB7               TYA               ; #$09-->A
               JSR MAC3               ; Do write byte
               LDX #$07            ; Set delay
               DEY                 ; Decrement Y ($09,$08,...)
               BNE MAB7            ; If not zero, continue sequence
               DEX               ; Timing correction               
               DEX                 ; 
               RTS                 ; Done with pilot tone

;Write one byte (the byte is in A)
MAC3               STA $2F               ; Store A to working byte
               EOR $31               ; Update check sum
               STA $31             ; Write checksum back
               LDA #$08               ; Set bit counter to 8
               STA $01             ; Store bit counter
MACD           ASL $2F               ; Shift with carry ;Was ASL $2F,X
               LDA $30               ; Invert the output bit
               EOR #$07
               JSR MAE5               ; Do half pulse
               LDX #$11               ; Set timing
               NOP                 ; Micro-delay
               EOR #$80               ; Invert the outptut bit
               JSR MAE5               ; Do half pulse
               LDX #$0E            ; Set delay
               DEC $01             ; Decrement bit counter
               BNE MACD            ; If not all bits, loop
               RTS                 ; Done, all 8 bits written

;Write half pulse
MAE5           DEX               ; Delay loop 1
               BNE MAE5                
               LDX #$0E            ; Delay loop 2
MAEA           DEX                   
               BNE MAEA
               BCC MAF4               ; Bit is 0, skip
               LDX #$13               ; Set delay
MAF1           DEX               ; Delay loop 3
               BNE MAF1
MAF4           STA SKCTL           ; Modify SKCTL
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
            SFX_CAPACITY = 49152-DATA_TABLE-5-5-1
            START = START_ADDR
