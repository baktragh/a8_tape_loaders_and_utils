;===============================================
;Turbo ROM Loader (Poland)
;Loader resides in "RAM under ROM", namely
;in the area normally occupied by the
;international charecter set.
;===============================================
           @HDR = 960       ;Header address - printer buffer           
.INCLUDE equates.asm 
            
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
            
            jsr SET_HDBUF    ;Prepare for check sum
            inc BUFRLO
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

;----------------------------------------------
; RUN program
;----------------------------------------------
RUNPROG     ldx #[RS_END-RUNSKEL] ;Move RUN skeleton
RP_LOOP     lda  RUNSKEL-1,X        
            sta  [@HDR+41-1],X
            dex
            bne  RP_LOOP
            
            lda  #<[@HDR+41+[RS_INIT-RUNSKEL]]       ;Prepare INIT
            sta  [@HDR+41+[RS_INISKEL+1-RUNSKEL]]
            lda  #>[@HDR+41+[RS_INIT-RUNSKEL]]
            sta  [@HDR+41+[RS_INISKEL+2-RUNSKEL]]
            
            lda  [@HDR+36]             ;Do we have INIT?
            beq  RP_GORUN              ;Yes, so be it
            
            lda  #<[@HDR+40]           ;No, RTS init  
            sta  [@HDR+8]
            lda  #>[@HDR+40]
            sta  [@HDR+9]
            
            
RP_GORUN    jmp  [@HDR+41]             ;Go to the RUN routine

;----------------------------------------------
; Run program routine skeleton
;----------------------------------------------
RUNSKEL     sei              ;Disable interrupts
            lda #[1+2+128+32+16] ;OS ROM enabled, BASIC disabled ST ROM disabled
                                 ;130XE banks in compatibility mode
            sta PORTB        
            cli              ;Enable interrupts 
RS_INISKEL  jsr RS_INIT      ;Perform INIT (skeleton)
RS_RUN      jmp ([@HDR+6])   ;Run program
RS_INIT     jmp ([@HDR+8])   
RS_END              

;-----------------------------------------------
; Header buffer setting 
; 41 bytes from address @HDR
;-----------------------------------------------
SET_HDBUF   lda #<@HDR
            sta BUFRLO
            lda #>@HDR
            sta BUFRHI
            lda #41
            sta BFENLO
            lda #0
            sta BFENHI
            sta @HDR+41     ;Clear one byte past header
            rts 
;-----------------------------------------------
; Data buffer setting
;-----------------------------------------------
SET_DATABUF lda [@HDR+10]
            sta BUFRLO
            lda [@HDR+11]
            sta BUFRHI
            lda [@HDR+12]
            sta BFENLO
            lda [@HDR+13]
            sta BFENHI
            rts
;==============================================
; Read block of data
;==============================================
READBLOCK   lda #1               ; Short pilot tone
            sta ICAX6Z
            jsr RD_PREP          ; Go prepare
            jsr WPILOT           ; Wait for pilot tone
            jsr GETBYTES         ; Go read bytes, ends with RTS
            jsr RD_TERM
            rts 
            
;----------------------------------------------            
; Calculate check sum      
; BUFRLO,BUFRHI - Data start
; L0CF,L0D0 - Bytes processed
; BFENLO,BFENHI - Bytes to process, BFENLO has check sum at the end
;------------------------------------------------------------
CHSUM0      lda #0               ;Zero length
            sta D_BYTESLO
            sta D_BYTESHI
CHSUM1      ldy #0               
            eor (BUFRLO),Y
            inc BUFRLO
            bne CHSUM2
            inc BUFRHI
CHSUM2      inc D_BYTESLO
            bne CHSUM3
            inc D_BYTESHI
CHSUM3      ldy BFENLO
            cpy D_BYTESLO
            bne CHSUM1
            ldy BFENHI
            cpy D_BYTESHI
            bne CHSUM1
            sta BFENLO
            lda BFENLO
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
GB_BTOBUF   sta (BUFRLO),Y         ; Store byte to buffer
            inc D_BYTESLO             ; Increment length
            bne GB_BTOBUFLH
            inc D_BYTESHI
GB_BTOBUFLH inc BUFRLO             ;Increment buffer pointer
            bne GB_BTOBUFH
            inc BUFRHI
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
D_BYTESLO        .BYTE 0
D_BYTESHI        .BYTE 0
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