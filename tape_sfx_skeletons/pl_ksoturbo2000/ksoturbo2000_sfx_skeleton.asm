;=======================================================================
; KSO Turbo 2000 Self-Extractor skeleton
; Connection using joystick port.
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
                ZP_SILENCE      = 131

                KSO_T2000_PILOT_LONG = $80
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
.IF SIO=1
                   lda #52                ;For SIO variant, motor ON
                   sta PACTL
.ENDIF

                   ldy CFG_SAFETY_DELAY   ;Presume just safety delay
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
    
SAVE_DOWRITE       jsr ADJUST_BUFFER            ;Adjust buffer
                   
                   jsr WRITE_BLOCK

                   clc                          ;Increment table pointer
                   lda #6
                   adc ZP_TAB_PTR_LO
                   sta ZP_TAB_PTR_LO
                   bcc SAVE_CONT
                   inc ZP_TAB_PTR_HI  
SAVE_CONT          

SAVE_NEXTBLOCK     jmp SAVE_LOOP                ;Continue saving.
;-----------------------------------------------------------------------
SAVE_TERM          jsr RECENV_TERM              ;Back with DMA and INTRs
.IF SIO=1
                   lda #60                      ;For SIO variant, motor OFF
                   sta PACTL
.ENDIF
                   bit CFG_FLAGS                ;Check for composite ($80)
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
                   ICL "../commons/routines.asm"                   
;=======================================================================
; Private Auxiliary Subroutines
;=======================================================================        
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
            lda ZP_BLOCKFLAG       ;Check if the block flag says othewrise
            and #KSO_T2000_PILOT_LONG ; Request for elongated pilot?
            beq L0795              ; No, skip         
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
