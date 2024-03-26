;=======================================================================
; KSO Turbo 2000 Self-Extractor skeleton
; Connection using joystick port.
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
                BF_LONGGAP      = 0x02
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
.IF SIO=1
                   lda #52                ;For SIO variant, motor ON
                   sta PACTL
.ENDIF

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
    
SAVE_DOBLOCK       jsr ADJUST_BUFFER            ;Adjust buffer
                   
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

                   lda #$02                     ;Check if 0x02 (long delay)
                   and ZP_BLOCKFLAG              
                   beq SAVE_SHORTDELAY          ;If not, continue with short

SAVE_LONGDELAY     ldy #200                     ;Set longer delay
                   jsr DELAY_LOOP_E             ;Do the long delay

SAVE_SHORTDELAY
                   jsr SHORT_DELAY              ;Otherwise add a gap
SAVE_NODELAY
SAVE_NEXTBLOCK     jmp SAVE_LOOP                ;Continue saving.
;-----------------------------------------------------------------------
SAVE_TERM          jsr RECENV_TERM              ;Back with DMA and INTRs
.IF SIO=1
                   lda #60                      ;For SIO variant, motor OFF
                   sta PACTL
.ENDIF

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

;-----------------------------------------------------------------------
; Adjust buffer
;-----------------------------------------------------------------------
ADJUST_BUFFER     inc BFENLO
                  bne AB_SKIP
                  inc BFENHI
AB_SKIP           rts 

;=======================================================================
; Write block of data
; BUFRLO,BUFRHI  - Buffer start
; BFENLO,BFENHI  - Byte right after buffer end
; 
; Pulses are written by alternating porta between $00 and $DF
; 00000000  11011111                                                    
;=======================================================================
WRITE_BLOCK  jmp L0790    

; Timing table
L0703       .byte $00
L0704       .byte $0D,$21,$35,$49,$91,$99,$36,$1B

;Delay routine
L070C       dex                    ; CA
            bne L070C              ; D0 FD
            rts                    ; 60

;Write one pulse, type of the pulse is given by contents of Y
L0710       ldx L0704,Y            ; Get the timing constant
            jsr L070C              ; Do delay
.IF SIO=0
            lda #$00               ; Set signal output to 0 - JS
            sta PORTA              ; 
.ELSE
            lda #3                 ; Set signal output to 0 - SIO
            sta SKCTL
.ENDIF
            lda #$00               ; Set border to black
            sta COLBK              ; 
            lda CONSOL             ; Check for console keys - disabled
            nop                    ; Was beq L0736     
            ldx L0704+1,Y          ; Get the timing constant
            jsr L070C              ; Do delay
.IF SIO=0
            lda #$DF               ; Set signal output to 1 - JS
            sta PORTA              ;
.ELSE
            lda #11                ; Set signal output to 1 - SIO
            sta SKCTL
.ENDIF 
            lda #$02               ; Set border to dark gray
            sta COLBK              ; 
            rts                    ; 

;Terminate writing (trap) 
L0736       jmp (COLDSV)

;Write 8 bits of a byte
L073B       ldx #$08               ; A2 08
            stx STATUS             ; 86 30
L073F       asl                    ; 0A
            pha                    ; 48
            ldy #$00               ; A0 00
            bcc L0747              ; 90 02
            ldy #$02               ; A0 02
L0747       jsr L0710              ; 20 10 07
            pla                    ; 68
            dec STATUS             ; C6 30
            beq L0757              ; F0 08
            ldx #$0C               ; A2 0C
            jsr L070C              ; 20 0C 07
            jmp L073F              ; 4C 3F 07
L0757       rts                    ; 60

;Write pilot tone (4*256 or 8*256 pulses)

L0790       ldy #$04               ;Presume standard pilot tone
            bit ZP_BLOCKFLAG       ; Check block flag for 0x40
            bvc L0795              ; Not present, skip
            ldy #$08               ; Make long pilot tone

L0795       lda #$00               ; A9 00
;           ldy #$04               ; A0 04
            sta STATUS             ; 85 30
L0799       tya                    ; 98
            pha                    ; 48
            ldy #$04               ; A0 04
            jsr L0710              ; 20 10 07
            pla                    ; 68
            tay                    ; A8
            dec STATUS             ; C6 30
            bne L0799              ; D0 F3
            dey                    ; 88
            bne L0799              ; D0 F0

;Setup buffer range.
            ldy BUFRLO             ; A4 32
            lda #$00               ; A9 00
            sta BUFRLO             ; 85 32
            ldx #$02               ; A2 02
            jsr L070C              ; 20 0C 07

L07B4       tya                    ; 98
            pha                    ; 48
            lda (BUFRLO),Y         ; B1 32
            pha                    ; 48
            clc                    ; 18
            adc CHKSUM             ; 65 31
            sta CHKSUM             ; 85 31
            pla                    ; 68
            jsr L073B              ; Write byte
            pla                    ; 68
            tay                    ; A8
            iny                    ; C8
            beq L07CC              ; F0 05
            cpy #$00               ; C4 00  ??

;Advance in the buffer
            jmp L07CE              ; 4C CE 07
L07CC       inc BUFRHI             ; E6 33
L07CE       cpy BFENLO             ; C4 34
            bne L07DB              ; D0 09
            lda BFENHI             ; A5 35
            cmp BUFRHI             ; C5 33
            bne L07DE              ; D0 06
            jmp L07E1              ; Buffer end, writing complete

L07DB       nop                    ; EA
            nop                    ; EA
            nop                    ; EA

L07DE       jmp L07B4              ; Continue with next bytes

;Write the checksum byte (need to disable this)
L07E1       lda CHKSUM             ; A5 31
            ldx #$01               ; A2 01
;           jsr L070C              ; 20 0C 07
;           jsr L073B              ; 20 3B 07


;Be done with recording of a block
            ldy #$04               ; Record safety pulse
            jsr L0710               

;Return to the caller
            rts
;-------------------------------------------------------------------------------
; Terminate recording environment
;-------------------------------------------------------------------------------               
RECENV_TERM    lda #$40               
               sta NMIEN              
               lda #$22               
               sta DMACLT     
.IF SIO=0        
               lda #$38               
               sta PACTL              
               lda #$00               
               sta PORTA              
               lda #$3C               
               sta PACTL     
.ENDIF         
               cli                    
               rts                    
;-------------------------------------------------------------------------------
; Initiate recording environment;
;-------------------------------------------------------------------------------               
RECENV_INIT    lda #$00               ; Disable NMIs
               sta NMIEN              ; 
               sta CHKSUM             ; Reset checksum
               sta DMACLT             ; Disable screen DMA
               lda #$08               ; Reset CONSOLE keys
               sta CONSOL             ; 

.IF SIO=0
               lda #$38               ; PORT A - Direction control
               sta PACTL              ; 
               lda #$60               ; Setup direction controls
               sta PORTA              ; 
               lda #$3C               ; PORT A - Back to normal mode
               sta PACTL              ;
.ELSEIF SIO=1
               lda #192
               sta AUDCTL
.ENDIF 
               sei                    ; Disable IRQs.
               rts                    ;                    
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
