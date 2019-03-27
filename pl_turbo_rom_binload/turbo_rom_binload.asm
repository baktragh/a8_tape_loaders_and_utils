;===============================================
;Turbo ROM Binary Loader (Poland)
;===============================================

.INCLUDE equates.asm 
;
; Code equates
;

;
; Start of code
;
            *= 2048
;
;----------------------------------------------
;Initialization
;----------------------------------------------
            jmp PREINIT
            
START       lda #0
            sta FLG_FIRST     ;Reset to 0 
            sta COLDST
            lda #1            ;Indicate boot successful
            sta BOOT        
;----------------------------------------------
;Load segment header (6 bytes)
;----------------------------------------------                        
RD_SEGHDR   jsr SET_HDBUF
            jsr READBLOCK    ;Decode block
            jsr SET_HDBUF
            inc BUFRLO
            
            jsr CHSUM0       ;Check sum
            cmp D_HEADER_CHS
            beq RD_HDROK;
            jmp RD_ERROR
RD_HDROK    jsr PROC_HD      ;Process segment header

;----------------------------------------------
;Load segment data 
;----------------------------------------------                                    
RD_SEGDATA  jsr SET_DATABUF
            
            
SETINI      lda #<CMN_RTS
            sta INITAD         ;Setup default INITAD
            lda #>CMN_RTS
            sta INITAD+1      
            jsr READBLOCK      ;Decode block
            jsr SET_DATABUF
            
            jsr CHSUM 0         ;Check sum
            cmp D_HEADER_BCHS
            beq RD_DATOK
            jmp RD_ERROR
            
RD_DATOK            
L0867       jsr L086D        ;Emulate JSR(INITAD)
            jmp RD_SEGHDR    ;Proceed to the next header
L086D       jmp (INITAD)

FLG_FIRST   .byte 0          ;First segment indicator

;----------------------------------------------
; Common return from subroutine
;----------------------------------------------
CMN_RTS       rts

;-----------------------------------------------
; Header buffer setting
;-----------------------------------------------
SET_HDBUF   lda #<D_HEADER
            sta BUFRLO
            lda #>D_HEADER
            sta BUFRHI
            lda #5
            sta BFENLO
            lda #0
            sta BFENHI
            rts 
;-----------------------------------------------
; Data buffer setting
;-----------------------------------------------
SET_DATABUF lda D_HEADER_SGLO
            sta BUFRLO
            lda D_HEADER_SGHI
            sta BUFRHI            
            lda D_HEADER_SGLLO
            sta BFENLO
            lda D_HEADER_SGLHI
            sta BFENHI
            rts
            
;----------------------------------------------
; Process segment header
;----------------------------------------------
PROC_HD     lda D_HEADER_SGLLO    ;Check if 0 0 0 0
            bne CHKFIRST
            lda D_HEADER_SGLHI
            bne CHKFIRST
            lda D_HEADER_SGLO
            bne CHKFIRST
            lda D_HEADER_SGHI
            beq RUNIT            ;If yes, run program
    
CHKFIRST    lda FLG_FIRST        ;Is that first segment
            bne CMN_RTS          ;If no, header is processed
            
            lda D_HEADER_SGLO    ;First segment. Set RUNAD
            sta RUNAD
            lda D_HEADER_SGHI
            sta RUNAD+1
            inc FLG_FIRST        ;Indicate not first segment
            jmp CMN_RTS          ;Header is processed

;---------------------------------------------
;Run loaded program
;---------------------------------------------
RUNIT       jmp (RUNAD)
        
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
;-----------------------------------------
; Data area
;-----------------------------------------
D_HEADER_CHS    .BYTE 0
D_HEADER_BCHS   .BYTE 0
D_HEADER_SGLO   .BYTE 0
D_HEADER_SGHI   .BYTE 0
D_HEADER_SGLLO  .BYTE 0
D_HEADER_SGLHI  .BYTE 0
D_HEADER        = D_HEADER_CHS
D_HEADER_TRAIL  .BYTE 0 
D_BYTESLO       .BYTE 0
D_BYTESHI       .BYTE 0
D_NMISTORE      .BYTE 0


;-----------------------------------------------
; Load error - This part of the loader
; can be overwritten by the binary file
; being loaded.
;----------------------------------------------
RD_ERROR    jsr $EF8E
            jsr $C63E
            lda #255
            sta 764
WKEY        lda 764
            cmp #255
            beq WKEY
            jmp START

;-----------------------------------------
; Preinitialization - FORCED WARM START.
; This part of the loader can be overwritten
; by the binary file being loaded
;-----------------------------------------
PREINIT     jmp PREINIT2     ;TS will override this with NOP if it is needed
                             ;to display a title
            ldx #<TITLE      ;Show program name
            ldy #>TITLE
            jsr $C642
            
            lda #1           ;Tape beep + key press
            sta 764
            jsr $FDFC       

PREINIT2    lda #0           ;Disable cold start
            sta COLDST
            
            lda #1           ;Indicate boot successful
            sta BOOT
            
            lda #255         ;Turn BASIC off  
            sta 54017
            sta 1016         
            
            lda #<DINI
            sta DOSINI       ;Setup DOSINI
            lda #>DINI
            sta DOSINI+1
            
            lda #<CMN_RTS    ;Set default INITAD and RUNAD
            sta INITAD
            sta RUNAD
            lda #>CMN_RTS
            sta RUNAD+1
            sta INITAD+1
            
            jsr WARMSV
            
DINI        lda #<SHOWTITLE  ;Setup DOSVEC
            sta DOSVEC
            lda #>SHOWTITLE
            sta DOSVEC+1            
            clc
            rts

SHOWTITLE   jmp START

TITLE       .BYTE 125            
PROGNAME    .BYTE "                    "
            .BYTE 155            