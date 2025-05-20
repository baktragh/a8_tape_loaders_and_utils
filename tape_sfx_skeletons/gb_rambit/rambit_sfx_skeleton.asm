;=======================================================================
; Rambit Turbo Tape Self-extractor skeleton   
; Assemble with the MADS assembler
;
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
                ZP_SILENCE      = 131
                
                ZP_CURR         = 132
                ZP_CHSUM        = 133

;Block flag constants
                
                RAMBIT_PILOT_HEADER = 0x80;
                RAMBIT_PILOT_DATA   = 0x40;
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

                   ldy CFG_SAFETY_DELAY   ;Presum just safety delay
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
                   
SAVE_DOSILENCE                   
                   ldy ZP_SILENCE               ;Silence before the block
                   beq SAVE_DOWRITE             ;Yes, skip
                   jsr DELAY_TENTHS             ;Otherwise, do silence
SAVE_DOWRITE			
                   jsr WRITE_BLOCK

SAVE_TONEXT
                   clc                          ;Increment table pointer
                   lda #6                       ;By 6 bytes
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
;===============================================================================
; Common Auxiliary Subroutines
;===============================================================================
                   ICL "../commons/routines.asm"                   
;===============================================================================
; Write block of data
; ==============================================================================
WRITE_BLOCK
;-------------------------------------------------------------------------------
; Buffer range correction
;-------------------------------------------------------------------------------
            inc BFENLO
            bne @+
            inc BFENHI
            
@           inc BFENLO
            bne @+
            inc BFENHI
;-------------------------------------------------------------------------------
;Prepare for writing
; - Disable interrupts
; - Set serial port for writing
; - Switch on the motor
; - Set interrupt handler for writing signal
; - Set pokey frequencies
;-------------------------------------------------------------------------------
@           sei
            ldx #$00
            stx NMIEN
            lda #$87
            sta SKCTL
            lda #$34
            sta PACTL
            lda #<WRITE_INTR
            sta VIMIRQ
            lda #>WRITE_INTR
            sta VIMIRQ+1
            ldy #$80                     ;Was $40, we prefer longer pilot
            stx DMACLT
            stx IRQEN
            inx
            stx IRQEN
            ldx #$FF                     ; Set to $FF for pilot tone
            stx ZP_CURR                  
            ldx #$17
            lda #$0B
            stx AUDF1
            sta AUDF2
            cli
;-------------------------------------------------------------------------------
; Write pilot tone ($FFs)
;-------------------------------------------------------------------------------           
            ldx #0                       ;Reset bit counter to 0
LB8B0       dey                          ;Still pilot tone ?
            beq LB8C3                    ;No, continue with sync byte
            lda #$FF                     ;Pilot tone value
LB8B5       cpx #$00                     ;Done with current byte?
            bne LB8B5                    ;No, then wait
            sta ZP_CURR                  ;Store new value - FF
            ldx #$08                     ;Do 8 bits
            cpy #$00                     ;Are pilot and sync byte done?
            beq LB8C7                    ;Yes, continue to data 
            bne LB8B0                    ;No, continue pilot tone
;-------------------------------------------------------------------------------
; Write synchronization sequence ($5A)
;-------------------------------------------------------------------------------            
LB8C3       lda #$5A                     ;Prepare sync byte
            bne LB8B5                    ;Go write sync byte
;-------------------------------------------------------------------------------
; Write data block (buffer pointer at BUFRLO/HI, buffer end at BFENLO/HI)
;-------------------------------------------------------------------------------            
LB8C7       lda (BUFRLO),Y
LB8C9       cpx #$00
            bne LB8C9
            sei
            sta ZP_CURR
            ldx #$08
            cli
            eor ZP_CHSUM
            sta ZP_CHSUM
            inc BUFRLO
            bne LB8DD
            inc BUFRHI
LB8DD       lda BUFRLO
            cmp BFENLO
            bne LB8C7
            lda BUFRHI
            cmp BFENHI
            bne LB8C7
;-------------------------------------------------------------------------------
; Write checksum (checksum is in the block buffer, we just write termination
; signal instead)
;-------------------------------------------------------------------------------            
;            lda ZP_CHSUM
;LB8EB       cpx #$00
;            bne LB8EB
;            sta ZP_CURR
;            ldx #$FF
;LB8F3       cpx #$00
;           bne LB8F3

@           cpx #$00
            bne @-
            lda #$00
            sta ZP_CURR
            ldx #$02
@           cpx #$00
            bne @- 
;-------------------------------------------------------------------------------
; Terminate block writing - restore IRQs etc.
;-------------------------------------------------------------------------------            
LB531       sei
            lda #$B1
            sta MEMTOP+1
LB537       lda #$30
            sta VIMIRQ
LB53C       lda #$C0
            sta VIMIRQ+1
            jsr INTINV
            lda #$3C
;            sta PACTL                  ;Do not stop the motor
            sta PBCTL
            lda #$00
            sta IRQEN
;            ldx #$80                   ;Do not zap zero page
;LB553       sta L0000,X
;            inx
;            bne LB553
            lda POKMSK
            sta IRQEN
            
;            lda #$09                   ;Do not set any vectors
;            sta DOSVEC
;            lda #$B2
;            sta DOSVEC+1
;            lda #$86
;LB567       sta CASINI
;            lda #$B1
;            sta CASINI+1
;            sta DOSINI+1
             cli
             rts
;-------------------------------------------------------------------------------
; Interrupt handler - writing signal
;-------------------------------------------------------------------------------
WRITE_INTR  pha                           ;Push registers
            tya
            pha
            sta STIMER                    ;Reset timers
LB775       lda #$87                      ;Set forced break
            sta SKCTL
            lda #$00
            sta IRQEN                     ;Disable IRQs
            asl ZP_CURR                   ;Get next bit
            lda #$01                      ;Timing for log. 1
LB783       ldy #$30
            bcs LB78B                     ;If log. 1, skip
            lda #$02                      ;Timing for log. 0
            ldy #$16
LB78B       dey                           ;Delay loop
            bne LB78B
            dex                           ;Decrement bit counter
            sta IRQEN                     ;Enable selected timers (1 or 2)
LB792       lda #$07                      ;Reset forced break
            sta SKCTL
            pla                           ;Pop registers
            tay
            pla
            rti
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
            SFX_CAPACITY = 49152-DATA_TABLE-6-6-1
            START = START_ADDR