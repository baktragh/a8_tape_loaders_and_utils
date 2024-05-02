;===============================================================================
; Standard Self-extractor skeleton   
; Assemble with the MADS assembler
;===============================================================================
           
                 ICL "equates.asm" 
                 OPT H+,F-

;===============================================================================
; Private constants
;===============================================================================
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
                STANDARD_PILOT_20_0     = 0x80;         ;20 s
                STANDARD_PILOT_16_0     = 0x40;         ;16 s
                STANDARD_PILOT_08_0     = 0x20;         ;8  s
                STANDARD_PILOT_03_0     = 0x10;         ;3  s
;===============================================================================
; INITITALIZATION CODE - Switches off the display, so that
; loading data into the screen memory does no harm. Also ensure
; that RESET results in cold start.
;===============================================================================
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
            
;===============================================================================
; MAINLINE CODE
;===============================================================================
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
;-------------------------------------------------------------------------------
;Initiate recording
; - Motor ON
; - Silence for file separation
; - Setup POKEY for writing cassette frames with customized IRGs
;-------------------------------------------------------------------------------
                   lda #52
                   sta PACTL

                   bit CFG_FLAGS          ;Check if long separator requested
                   bvc NORM_SEP           ;No, skip to normal delay
  
                   ldy CFG_SEP_DURATION   ;Long separator
                   jsr DELAY_CUSTOM_Y     ;Make long separator
NORM_SEP           jsr DELAY_SHORT        ;Make normal separator
                   jsr TAPE_INIT_POKEY
;-------------------------------------------------------------------------------      
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

                   lda BUFRLO             ;Check for terminator (all $FFs)
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
                   bcc @+
                   inc ZP_TAB_PTR_HI  
@
SAVE_NEXTBLOCK     jmp SAVE_LOOP                ;Continue saving.
;-------------------------------------------------------------------------------
; Terminate the recording
; - Motor off
; - RESET POKEY
;-------------------------------------------------------------------------------
SAVE_TERM          lda #60                      ;Motor OFF
                   sta PACTL

                   jsr TAPE_TERM_POKEY          ;Terminate POKEY

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
; KEYBOARD SUBROUTINES
;===============================================================================                   
;-------------------------------------------------------------------------------
; Wait for START 
;-------------------------------------------------------------------------------
WAIT_FOR_START     lda #8
                   sta CONSOL
WFS_LOOP           lda CONSOL             ;What keys?  
                   cmp #6                 ;Is that START?
                   beq WFS_DONE           ;Yes, we are done
                   bne WFS_LOOP
WFS_DONE           rts
                   
;===============================================================================
; OTHER SUBROUTINES
;===============================================================================                   
;-------------------------------------------------------------------------------
; Beep
;-------------------------------------------------------------------------------                   
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
;-------------------------------------------------------------------------------
; Wait for VBLANK
;-------------------------------------------------------------------------------
WAIT_FOR_VBLANK    lda #VBI_VCOUNT             ;Get the desired value
WFV_1              cmp VCOUNT                  ;Check
                   bne WFV_1                   ;If equal, keep checking
WFV_2              cmp VCOUNT
                   beq WFV_2                   
                   rts             
;-------------------------------------------------------------------------------
; Short delay
;-------------------------------------------------------------------------------                   
DELAY_SHORT        ldy #5                  ;Short delay, 0.1 sec
DELAY_CUSTOM_Y     jmp DELAY_WAIT

DELAY_WAIT         jsr WAIT_FOR_VBLANK     ;Wait for VBLANK
                   dey                     ;Decrement counter
                   bne DELAY_WAIT          ;Repeat until not zero
DELAY_END          rts
;===============================================================================
; Write block of data
; Inputs:
; BUFRLO,BUFRHI  - First byte
; BFENLO,BFENHI  - Last byte
;===============================================================================
WRITE_BLOCK        

;Ensure correct IRG
                   lda ZP_BLOCKFLAG        ;Check block flag
                   beq WB_RANGE            ;Code 0, minimum IRG
                   jsr TAPE_IRG            ;Other code, specific IRG
WB_RANGE
;Calculate the length of the block (BFEN-BUFR+1)
                   sec
                   lda BFENLO
                   sbc BUFRLO
                   sta ZP_SIO_LENLO
                   lda BFENHI
                   sbc BUFRHI
                   sta ZP_SIO_LENHI

                   inc ZP_SIO_LENLO
                   bne @+
                   inc ZP_SIO_LENHI
@
;Setup the buffer pointer for the SIO call.
                   lda BUFRLO
                   sta ZP_SIO_BUFRLO
                   lda BUFRHI
                   sta ZP_SIO_BUFRHI
;Now prepare the SIO call
                   lda #$60               ;Cassette
                   sta DDEVIC
                   lda #0                 
                   sta DUNIT              ;Zero unit
                   sta DUNUSE             ;Zero unused byte
                   sta DAUX1              ;Zero AUX1 byte
 
                   lda #$80               ;Indicate
                   sta DSTATS             ;Write
                   sta DAUX2              ;Short tape IRG

                   lda #$05               ;Set some bogus timeout         
                   sta DTIMLO             

                   lda #$50               ;Command = put sector
                   sta DCOMND   

                   lda ZP_SIO_BUFRLO      ;Set buffer address
                   sta DBUFLO
                   lda ZP_SIO_BUFRHI
                   sta DBUFHI
          
                   lda ZP_SIO_LENLO       ;Set buffer length
                   sta DBYTLO
                   lda ZP_SIO_LENHI
                   sta DBYTHI

                   jsr SIOV               ;Call SIO 

                   rts
;===============================================================================
; Tape related subroutines
;===============================================================================
;-------------------------------------------------------------------------------
;Initialize tape recording
;-------------------------------------------------------------------------------
TAPE_INIT_POKEY   lda SSKCTL              ;Get shadow          
                  and #$0f                ;Reset serial control
                  ora #$20                ;Set mode sync/timing mode
                  ora #$08                ;Set two-tone mode (tape)
                  sta SSKCTL              ;Set shadow and real port
                  sta SKCTL

                  ldx #9                  ;Set POKEY regs for tape writing
TIP_LOOP_REG      lda TIP_REGDATA-1,X
                  sta AUDF1-1,X
                  dex
                  bne TIP_LOOP_REG
                    
TIP_NOISY_AUDIO                          ;Ensure "noisy I/O"
                  lda #$a8
                  sta audc4
                  lda #$10
                  bit sskctl
                  bne TIP_NOISY_DONE
                  sta audc1
                  sta audc2
TIP_NOISY_DONE

TIP_RESET_SERIAL_STATUS                    ;Reset serial port
                  sta SKREST
                  rts
TIP_REGDATA
                  dta$05;audf1
                  dta$a0;audc1
                  dta$07;audf2
                  dta$a0;audc2
                  dta$cc;audf3
                  dta$a0;audc3
                  dta$05;audf4
                  dta$a0;audc4
                  dta$28;audctl
;-------------------------------------------------------------------------------
; Terminate POKEY setup for tape writing
;-------------------------------------------------------------------------------
TAPE_TERM_POKEY   lda #0
                  sta audc1
                  sta audc2
                  sta audc4
                  rts 
;-------------------------------------------------------------------------------
; Tape IRG for PAL/NTSC
; Input: ZP_BLOCKFLAG holds IRG duration code.
;-------------------------------------------------------------------------------
TAPE_IRG         lda ZP_BLOCKFLAG            ;Double the code           
                 clc
                 adc ZP_BLOCKFLAG
                 tax

                 lda PAL                     ;Check GTIA region
                 and #$0F                    ;Are we PAL, pals?
                 bne TIRG_PAL                ;Yes, use PAL timing

TIRG_NTSC        lda TIRG_TABLE_NTSC,X
                 sta W_IRG_LO
                 lda TIRG_TABLE_NTSC+1,X
                 sta W_IRG_HI
                 jmp TIRG_CORE  

TIRG_PAL         lda TIRG_TABLE_PAL,X
                 sta W_IRG_LO
                 lda TIRG_TABLE_PAL+1,X
                 sta W_IRG_HI
;-------------------------------------------------------------------------------
TIRG_CORE
                 mwa #TAPE_COUNTDOWN_HANDLER CDTMA1
                 ldy W_IRG_LO
                 lda W_IRG_HI
                 tax
                 lda #1
                 sta timflg
                 jsr SETVBV
                 lda:rne timflg
                 rts
;-------------------------------------------------------------------------------
; Countdown handler, just kills the TIMFLG
;-------------------------------------------------------------------------------
TAPE_COUNTDOWN_HANDLER
                 mva #0 timflg
                 rts
;-------------------------------------------------------------------------------
; IRG TABLES
;-------------------------------------------------------------------------------
; PAL IRG Table
TIRG_TABLE_PAL  .WORD 0                ;0.25
                .WORD 1                ;0.27
                .WORD 1                ;0.29
                .WORD 2                ;0.30
                .WORD 4                ;0.35
                .WORD 7                ;0.4
                .WORD 12               ;0.5
                .WORD 27               ;0.8
                .WORD 37               ;1.0
                .WORD 62               ;1.5
                .WORD 87               ;2.0
                .WORD 107              ;2.4
                .WORD 112              ;2.5
                .WORD 137              ;3.0
                .WORD 237              ;5.0
                .WORD 387              ;8.0
                .WORD 487              ;10.0
                .WORD 587              ;12.0
                .WORD 687              ;14.0
                .WORD 787              ;16.0
                .WORD 987              ;20.0

; NTSC IRG Table
TIRG_TABLE_NTSC .WORD 0                 ;0.25
                .WORD 1                 ;0.27
                .WORD 2                 ;0.29
                .WORD 2                 ;0.30
                .WORD 5                 ;0.35
                .WORD 8                 ;0.4
                .WORD 14                ;0.5
                .WORD 32                ;0.8
                .WORD 44                ;1.0
                .WORD 74                ;1.5
                .WORD 104               ;2.0
                .WORD 128               ;2.4
                .WORD 134               ;2.5
                .WORD 164               ;3.0
                .WORD 284               ;5.0
                .WORD 464               ;8.0
                .WORD 584               ;10.0
                .WORD 704               ;12.0
                .WORD 824               ;14.0
                .WORD 944               ;16.0
                .WORD 1184              ;20.0
;===============================================================================
; DISPLAY DATA
;===============================================================================
;-------------------------------------------------------------------------------
; Display list
;-------------------------------------------------------------------------------
DLIST      .BYTE 112,112,112
           .BYTE 7+64,<LINE_NAME,>LINE_NAME
           .BYTE 112,112
           .BYTE 2+64,<LINE_TITLE,>LINE_TITLE
           .BYTE $30
           .BYTE 2+64,<LINE_INSTR,>LINE_INSTR,2
           .BYTE 65,<DLIST,>DLIST
;===============================================================================
; DATA AREAS
;===============================================================================
W_IRG_LO   .BYTE 0
W_IRG_HI   .BYTE 0
;===============================================================================
; Block data table
;===============================================================================
            DATA_TABLE=*
            SFX_CAPACITY = 49152-DATA_TABLE-5-5-1
            START = START_ADDR     
