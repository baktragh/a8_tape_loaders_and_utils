;===============================================
;Turbo ROM Loader (Poland)
;Loader resides in "RAM under ROM", namely
;in the area normally occupied by the
;international charecter set.
;===============================================
.INCLUDE equates.asm
           @HDR = 960       ;Header address - printer buffer
           ZP_BUFRLO = $CB         
           ZP_BUFRHI = $CC 
           ZP_BFENLO = $CD
           ZP_BFENHI = $CE
           ZP_BYTESLO = $CF
           ZP_BYTESHI = $D0


             
;
; Start of code
;
            *= $CC00        ;International character set
;
;----------------------------------------------
;Initialization
;----------------------------------------------
START       lda #0
            sta COLDST
            lda #1            ;Indicate boot successful
            sta BOOT        
;----------------------------------------------
;Load header to printer buffer
;----------------------------------------------                        
RD_HEADER   jsr SET_HDBUF
            jsr READBLOCK    ;Decode block

            lda ZP_BYTESLO   ;Copy number of bytes
            sta ZP_BFENLO
            lda ZP_BYTESHI
            sta ZP_BFENHI

            jsr SET_HDBUF    ;Prepare for check sum

            inc ZP_BUFRLO    ;Ignore the first byte
            inc ZP_BYTESLO

            jsr CHSUM0       ;Calculate check sum
            cmp @HDR         ;Match? 
            beq DISP_NAME    ;Yes, continue
            jmp RD_HEADER    ;No try again
            
;------------------------------------------------
; Disply program name
;------------------------------------------------
DISP_NAME   ldy #20
DN_LOOP     lda [@HDR+15-1],Y
            sta (88),Y
            dey
            bne DN_LOOP
            
            lda #0               ;Wait for some time
            sta 20
RDNMWL      lda 20
            cmp #75
            bne RDNMWL
            
;-------------------------------------------------
; Read data block
;-------------------------------------------------
RD_DATA     jsr SET_DATABUF  ;Setup buffer for main block
            
            
SETINI      jsr READBLOCK    ;Decode block
            jsr SET_DATABUF  ;Setup buffer for main block
           
            jsr CHSUM0       ;Check sum
            cmp [@HDR+5]        ;Match?
            beq RUNPROG      ;Yes, go to run the progrem
            jmp RD_ERROR     ;No, BOOT ERROR

;-------------------------------------------------------------------------------
; RUN program
;-------------------------------------------------------------------------------
RUNPROG     ldx #[RS_END-RUNSKEL] ;Move RUN skeleton
RP_LOOP     lda  RUNSKEL-1,X        
            sta  [@HDR+61-1],X
            dex
            bne  RP_LOOP
            
            lda  #<[@HDR+61+[RS_INIT-RUNSKEL]]       ;Prepare INIT
            sta  [@HDR+61+[RS_INISKEL+1-RUNSKEL]]
            lda  #>[@HDR+61+[RS_INIT-RUNSKEL]]
            sta  [@HDR+61+[RS_INISKEL+2-RUNSKEL]]
            
            lda  [@HDR+36]             ;Do we have INIT?
            beq  RP_GORUN              ;Yes, so be it
            
            lda  #<[@HDR+61+RS_RTS-RUNSKEL] ;No, RTS init  
            sta  [@HDR+8]
            lda  #>[@HDR+61+RS_RTS-RUNSKEL]
            sta  [@HDR+9]
            
RP_GORUN    jmp  [@HDR+61]             ;Go to the RUN routine

;-------------------------------------------------------------------------------
; Run program routine skeleton
;-------------------------------------------------------------------------------
RUNSKEL     sei              ;Disable interrupts
            lda #[1+2+128+32+16] ;OS ROM enabled, BASIC disabled ST ROM disabled
                                 ;130XE banks in compatibility mode
            sta PORTB        
            cli              ;Enable interrupts 
RS_INISKEL  jsr RS_INIT      ;Perform INIT (skeleton)
RS_RUN      jmp ([@HDR+6])   ;Run program
RS_INIT     jmp ([@HDR+8])
RS_RTS      rts    
RS_END              

;-------------------------------------------------------------------------------
; Header buffer setting 
; Set address to @HDR and byte counters to zero
;-------------------------------------------------------------------------------
SET_HDBUF   lda #<@HDR
            sta ZP_BUFRLO
            lda #>@HDR
            sta ZP_BUFRHI
            jsr RESET_COUNTER
            rts 
;-------------------------------------------------------------------------------
; Data buffer setting
;-------------------------------------------------------------------------------
SET_DATABUF lda [@HDR+10]
            sta ZP_BUFRLO
            lda [@HDR+11]
            sta ZP_BUFRHI
            lda [@HDR+12]
            sta ZP_BFENLO
            lda [@HDR+13]
            sta ZP_BFENHI
            jsr RESET_COUNTER
            rts
;-------------------------------------------------------------------------------
; Reset counter
;-------------------------------------------------------------------------------
RESET_COUNTER
            lda #0 
            sta ZP_BYTESLO
            sta ZP_BYTESHI
            rts 
;===============================================================================
; Read block of data
;===============================================================================
READBLOCK   lda #1               ; Short pilot tone
            sta ICAX6Z
            jsr RD_PREP          ; Go prepare
            jsr WPILOT           ; Wait for pilot tone
            jsr GETBYTES         ; Go read bytes, ends with RTS
            jsr RD_TERM
            rts 
;-------------------------------------------------------------------------------            
; Calculate check sum      
; ZP_BUFRLO,ZP_BUFRHI   - Data start
; ZP_BYTESLO,ZP_BYTESHI - Counting bytes
; ZP_BFENLO,ZP_BFENHI   - Bytes to process, ZP_BFENLO has check sum at the end
;-------------------------------------------------------------------------------
CHSUM0      
CHSUM1      ldy #0               
            eor (ZP_BUFRLO),Y
            inc ZP_BUFRLO
            bne CHSUM2
            inc ZP_BUFRHI
CHSUM2      inc ZP_BYTESLO
            bne CHSUM3
            inc ZP_BYTESHI
CHSUM3      ldy ZP_BFENLO
            cpy ZP_BYTESLO
            bne CHSUM1
            ldy ZP_BFENHI
            cpy ZP_BYTESHI
            bne CHSUM1
            sta ZP_BFENLO
            lda ZP_BFENLO
            rts
;---------------------------------------- 
;Prepare for reading - motor + interrupts            
;-----------------------------------------
RD_PREP     lda #52
            sta PACTL
            lda NMIEN
            sta D_NMISTORE
            lda #0
            sta NMIEN
            sta DMACLT
            sta AUDCTL
            lda #3
            sta SKCTL
            sei
            lda #52
            sta PBCTL
            rts            
;---------------------------------------- 
;Terminate reading - motor + interrupts            
;-----------------------------------------
RD_TERM     lda #60
            sta PACTL
            lda D_NMISTORE
            sta NMIEN
            cli
            rts                        

; Get bytes of data            
GETBYTES    jsr GETPULSE          ; Go get pulse
            cpx #20
            bcs GETBYTES          ; Still pilot tone, continue
            cpx #8                 
            bcc GB_EXIT           ; Too narrow - exit
GB_NBYTE    ldy #8                ; Init bit counter
            lda #0                ; Init work byte
GB_NBIT     pha                   ; Stack work byte
            jsr GETPULSE          ; Go get pulse
            pla                   ; Unstack work byte
            cpx #8
            bcc GB_EXIT           ; Too narrow - exit
            cpx #43
            bcs GB_EXIT           ; Too wide - exit
            cpx #20               ; Determine 0 or 1
            ror A                 ; Place bit into work byte
            dey                   ; Decrement bit counter
            beq GB_BTOBUF         ; If byte complete, go place to buffer
            ldx #6                ; Delay
GB_DELAY    dex
            bne GB_DELAY
            beq GB_NBIT           ; Continue with next bit   
GB_BTOBUF   sta (ZP_BUFRLO),Y         ; Store byte to buffer
            inc ZP_BYTESLO             ; Increment length
            bne GB_BTOBUFLH
            inc ZP_BYTESHI
GB_BTOBUFLH inc ZP_BUFRLO             ;Increment buffer pointer
            bne GB_BTOBUFH
            inc ZP_BUFRHI
GB_BTOBUFH  bne GB_NBYTE          ;Continue with next byte
GB_EXIT     rts

; Wait for pilot tone
WPILOT      lda ICAX6Z     ;Initialize pilot tone pulse counter (HI)
            sta STATUS
            ldy #0         ;Initialize pilot tone pulse counter (LO)
WPILOT_L1   jsr GETPULSE   ;Go get pulse
            cpx #20
            bcc WPILOT      ;Too narrow - try again
            cpx #43                                
            bcs WPILOT      ;Too wide - try agian
            iny             ;Increment pilot tone pulse counter (LO)
            bne WPILOT_L1   ;If not 0, continue
            dec STATUS      ;Decrement pilot tone pulse counter (HI)
            bne WPILOT_L1   ;If not enough pulses, continue
            rts             ;Pilot tone detected
; Get pulse            
GETPULSE    ldx #0
            lda #16
GPULSE1     bit SKSTAT
            beq GPULSE1
            sta COLBK
GPULSE2     inx
            bit SKSTAT
            bne GPULSE2
            lsr A
            sta COLBK
            rts
;-----------------------------------------------
; Data area
;-----------------------------------------------
D_NMISTORE       .BYTE 0
;-----------------------------------------------
; Load error
;-----------------------------------------------
RD_ERROR    jsr $EF8E
            jsr $C63E
            lda #255
            sta 764
WKEY        lda 764
            cmp #255
            beq WKEY
            jmp START
