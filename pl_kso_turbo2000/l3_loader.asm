;===============================================================================
;KSO Turbo 2000 - L3 Binary Loader
;The L3 binary loader is a special binary loader for turbo systems based
;on the Polish KSO Turbo 2000.
;
;The L3 loader and its customized file format allow for loading of binary
;files with extensive memory requirements. While the natural file format
;(3 KB blocks) requires 3 KB buffer and also memory for the loader, the L3
;loader requires only about 700 bytes.
;
;The L3 loader works with both signal inputs (joystick port and SIO DATA IN).
;
;The file format is the following:
;Segment header block  
; ID BYTE: 128
; BUFFER : 4 bytes of buffer range ($FF,$FF,$00,$00) indicates EOF
; CHSUM  : Check sum
;Segment data
; ID BYTE: 64
; Segment data
; CHSUM  : Check sum
;===============================================================================

            .INCLUDE "equates.asm"

;
; Start of code
;
            *= 2048
            
;-------------------------------------------------------------------------------
; Preinitialization
;-------------------------------------------------------------------------------
            jsr BOOTINIT
;-------------------------------------------------------------------------------
; Binary load 
;-------------------------------------------------------------------------------
BL_START    lda #0                             ;Reset signal source
            sta ICSTAZ
            
;Decode segment header
BL_SHEADER  lda #<SEG_HEADER_BUFFER            ;Set buffer
            sta BUFRLO
            lda #>SEG_HEADER_BUFFER
            sta BUFRHI
            lda #<[SEG_HEADER_BUFFER+4]
            sta BFENLO
            lda #>[SEG_HEADER_BUFFER+4]
            sta BFENHI
            lda #$80
            sta EXP_BYTE
            
            jsr DEC_BLOCK                      ;Decode block
            cpy #1                             ;Check if correct
            beq BL_CHKHEAD                     ;If so, decode segment
            jmp HANDLE_ERR                     ;If not, signal error

;Check segment header            
BL_CHKHEAD  lda #<FAKEINIT                     ;No more signal switching
            sta L_J_SIG+1
            lda #>FAKEINIT
            sta L_J_SIG+2
            
            lda SEG_HEADER_BUFFER              ;Termination header?
            cmp #255
            bne DEC_SDATA
            lda SEG_HEADER_BUFFER+1
            cmp #255
            bne DEC_SDATA
            lda SEG_HEADER_BUFFER+2
            cmp #0
            bne DEC_SDATA
            
            jmp (RUNAD)                        ;Run the program

;Decode segment data            
DEC_SDATA   lda #<FAKEINIT                     ;Reset INIT vector
            sta INITAD
            lda #>FAKEINIT
            sta INITAD+1

            lda SEG_HEADER_BUFFER              ;Set buffer range
            sta BUFRLO
            lda SEG_HEADER_BUFFER+1
            sta BUFRHI
            lda SEG_HEADER_BUFFER+2
            sta BFENLO
            lda SEG_HEADER_BUFFER+3
            sta BFENHI
            
            lda #$40
            sta EXP_BYTE
            
            jsr DEC_BLOCK                      ;Decode block
            cpy #1                             ;Check if correct
            beq BL_DOINIT                      ;If so, process segment
            jmp HANDLE_ERR                     ;If not, signal error
            
BL_DOINIT   jsr GOINIT                         ;Execute INIT vector
            jmp BL_SHEADER                     ;And get another segment
            
GOINIT      jmp (INITAD)            
            
;-------------------------------------------------------------------------------
; First expected byte
;-------------------------------------------------------------------------------            
EXP_BYTE      .byte $00
SP_BACKUP     .byte $00

;-------------------------------------------------------------------------------
; Pulse width table   
;-------------------------------------------------------------------------------
L0704    .byte $0D       
L0705    .byte $21
L0706    .byte $35
L0707    .byte $49
L0708    .byte $91
L0709    .byte $99
L070A    .byte $36
L070B    .byte $1B
FAKEINIT     rts                   
;-------------------------------------------------------------------------------            
; User break
;-------------------------------------------------------------------------------            
USRBREAK    ldy #$80               ; Set Y=$80    (user break)
            jmp TERMINATE              ; And terminate processing
;-------------------------------------------------------------------------------
; Decoding 8 bits of a byte.
; The decoded byte is stored to the STATUS.
;-------------------------------------------------------------------------------
DEC_1BYTE   ldy #$08               ; Y=8 - bit counter
L075A       lda CONSOL             ; Check console keys
            beq USRBREAK           ; If all pressed, indicate user break
            
L_DET3      lda SKSTAT             ; Check port A or DATA IN
L_AND3      and #16
L_CMP3      bne L075A
            lda #$00               ; Black background
            sta COLBK              ; 
            tax                    ; Transfer A to X (X=0)
L076A       inx                    ; Increment X
            bmi L078B              ; If negative, then indicate error
L_DET4      lda SKSTAT             ; Check PORTA
L_AND4      and #16
L_CMP4      beq L076A
            lda #$B8               ; Light background $B8
            sta COLBK              ; 
            cpx L070A              ; Compare X with entry in pulse width table
            bcs L0787              ; If carry set, then check if error or not
            ror FMSZPG+6           ; Rotate FMSZPG+6 to indicate end of pilot
            cpx L070B              ; Compare X with entry in pulse width table
            rol STATUS             ; Rotate STATUS
            dey                    ; Decrementy Y (bit counter)
            bne L075A              ; If not zero, continue lopp
            rts                    ; Otherwise return

; Check if too long pulse indicates that we are still decoding the pilot tone            
L0787       bit FMSZPG+6           ; Check FMSZPG+6
            bmi L075A              ; If still pilot tone, then just continue
L078B       ldy #$8C               ; Otherwise indicate error
            jmp TERMINATE          ; And terminate reading
            
           
L07F0       jmp USRBREAK           ; Handle interruption


;===============================================================================
; Decode a block
;===============================================================================

; Prepare
DEC_BLOCK   jsr PREP_SYS           ; Disable interrupts, motor ON
            ldy #$00               ; Y=0
            lda #$FF               ; Set mask
            sta FMSZPG+6           ; Store $FF to FMSZPG+6
            
; Wait for pilot tone (at least 256 pulses)            
L_J_SIG     jsr SWITCH_SIG         ; Switch signal sourceldx #$00               ; X=0
L07FD       ldx #0
L07FE       lda CONSOL             ; Check console keys (no effect)
L_DET1      lda SKSTAT             ; Check PORT A or DATA IN
L_AND1      and #16                ; Check if log.1 on input
L_CMP1      bne L07FE              ; If yes, keep waiting
            lda #$00               ; White background
            sta COLBK              ; 
L080D       inx                    ; Increment X
            bmi L_J_SIG            ; If waiting too long, repeat
            
L_DET2      lda PORTA              ; Check PORTA or DATA IN
L_AND2      and #16
L_CMP2      beq L080D              ; If positive, loop until not positive
L_CBK       lda #$18               ; Brown Background
            sta COLBK              ; 
            cpx L070A              ; Check pulse width
            bcc DEC_BLOCK          ; If not acceptable, start over
            
            iny                    ; Increment Y
            bne L07FD              ; If not enough (256), keep looping ;WAS 7FC
            
; Pilot tone was found, check ID byte
            jsr DEC_1BYTE          ;
            lda STATUS
            cmp EXP_BYTE
            bne TERMINATE          ;
            clc 
            adc CHKSUM
            sta CHKSUM

;Identification byte found, keep placing data to the buffer
            ldy BUFRLO             ; Y=BUFRLO
            lda #$00               ; Clear BUFRLO
            sta BUFRLO             ;
             
L0828       tya                    ; A=BUFRLO
            pha                    ; Push A
            jsr DEC_1BYTE          ; Decode 8 bits of a byte
            pla                    ; Pop A
            tay                    ; Y=A
            lda STATUS             ; Get the decoded byte
            nop
            nop
            sta (BUFRLO),Y         ; And place A to the buffer
L0838       clc                    ; Clear C
            adc CHKSUM             ; Update checksum
            sta CHKSUM             ;

;Update buffer pointer and check for end of file             
            iny                    ; Update buffer pointer
            bne L0842              ; If not wraparound, just skip
            inc BUFRHI             ; Increment high byte
L0842       cpy BFENLO             ; Y Equal to buffer end low byte?
            bne L0828              ; No, continue
            lda BUFRHI             ; Yes, check high bytes
            cmp BFENHI             ; 
            bne L0828              ; If not equal then continue
            jsr DEC_1BYTE          ; If equal, then decode a byte (checksum)
            ldy #$01               ; Set Y=1
            lda STATUS             ; Get byte just decoded (checksum)
            cmp CHKSUM             ; Compare with calculated checksum
            beq L0859              ; If equal, then terminate processing
            ldy #$8F               ; If not, Set Y to indicate bad checksum
L0859       jmp TERMINATE          ; And terminate processing
            
;-------------------------------------------------------------------------------
; Terminate processing and return return code in register Y            
;-------------------------------------------------------------------------------
TERMINATE   ldx SP_BACKUP          ; Get stack pointer
            txs                    ; Restore stack pointer
            lda #192     
            sta NMIEN
            sta POKMSK
            sta IRQEN
            
            lda #60                ;Group A - COMMAND Inactive
            sta PBCTL
            
            lda #$38               ;Group C - Reset joystick port         
            sta PACTL              
            lda #$00               
            sta PORTA              
            
            lda #$3C               ;Motor off
            sta PACTL          
            
            cli                    ;Re-enable interrupts
            rts                    ;Return
;-------------------------------------------------------------------------------
; Prepare the system for decoding
;-------------------------------------------------------------------------------
PREP_SYS    lda #$00               ; Disable NMI and DMA
            sta NMIEN              ; 
            sta DMACLT             ; 
            sta CHKSUM             ; Clear checksum
            lda #$08               ; Clear CONSOLE keys register
            sta CONSOL             ;

            lda #52                ;Group A - COMMAND active
            sta PBCTL
            
            lda #$38               ;Group C - Input/Output from joystick port
            sta PACTL              ;Direction control mode
            lda #$60               ;Set direction contol bits
            sta PORTA
            lda #$34               ;PORTA addressing mode
            sta PACTL              ;Motor on            

            sei                    ; Disable interrupts
            tsx                    ; SP to X
            inx                    ; X+=2
            inx                    ; 
            stx SP_BACKUP          ; Store stack pointer
            rts                    ; And return            
;-------------------------------------------------------------------------------
; Data area
;-------------------------------------------------------------------------------
SEG_HEADER_BUFFER .BYTE 0,0,0,0    

;===============================================================================
; Volatile code and data
;===============================================================================
            VOLATILE = [*-1] 
;-------------------------------------------------------------------------------
; Error - purple screen
;-------------------------------------------------------------------------------
HANDLE_ERR  lda #88
            sta COLOR4
            sta COLBK
            sta COLDST 
L_ERR_L     jmp L_ERR_L            
;-----------------------------------------------------------------------------
; Switch signal source
;-----------------------------------------------------------------------------
SWITCH_SIG  php
            pha
            lda ICSTAZ         ;Check indicator         
            beq SW_TOSIO       ;If zero, switch to SIO
            
SW_TOJOY    dec ICSTAZ         ;Switch indicator
            lda #$00           ;$D300
            sta L_DET1+1
            sta L_DET2+1
            sta L_DET3+1
            sta L_DET4+1
            lda #$D3
            sta L_DET1+2
            sta L_DET2+2
            sta L_DET3+2
            sta L_DET4+2
            lda #128           ;Bit 7
            sta L_AND1+1
            sta L_AND2+1
            sta L_AND3+1
            sta L_AND4+1
            lda #$F0            ;Place BEQ
            sta L_CMP1
            sta L_CMP3
            lda #$D0            ;Place BNE
            sta L_CMP2
            sta L_CMP4
            lda #$58
            sta L_CBK+1
            bne SW_END
        
SW_TOSIO    inc ICSTAZ         ;Switch indicator
            lda #$0F
            sta L_DET1+1
            sta L_DET2+1
            sta L_DET3+1
            sta L_DET4+1
            lda #$D2
            sta L_DET1+2
            sta L_DET2+2
            sta L_DET3+2
            sta L_DET4+2
            lda #16
            sta L_AND1+1
            sta L_AND2+1
            sta L_AND3+1
            sta L_AND4+1
            lda #$D0            ;Place BNE
            sta L_CMP1
            sta L_CMP3
            lda #$F0            ;Place BEQ
            sta L_CMP2
            sta L_CMP4
            lda #$18
            sta L_CBK+1
            
SW_END      pla                ;Restore everything
            plp
            rts                ;Return            
            
;-------------------------------------------------------------------------------
; Preinitialization
;-------------------------------------------------------------------------------            
BOOTINIT    ldx #0            ;Reset coldstart flag
            sta COLDST
            inx               ;Indicate disk boot ok (1)
            stx BOOT
            rts
