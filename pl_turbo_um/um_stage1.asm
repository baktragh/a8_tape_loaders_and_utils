;==============================================================================
; Unerring Master Atari Turbo Tape
; Stage 1 Binary loader
;==============================================================================

    .INCLUDE "equates.asm"

;
; Start of code
;
            *= $0794
;
L0794       .byte $00              ;Segment header (first address)
L0795       .byte $00              
L0796       .byte $00              ;Segment header (last address)
L0797       .byte $00
L0798       .byte $00              ;First segment flag 
L0799       .byte $00              ;Calculated Checksum
L079A       .byte $00              ;Checksum
            .byte $00          

;------------------------------------------------------------------------------
; Load file
;------------------------------------------------------------------------------

; Decode pilot tone and wait for sync pulse
L079C       ldx #$00               ; Clear INITAD vector
            stx INITAD             ; 
            stx INITAD+1           ; 
            jsr L086E              ; Detect pilot tone and sync pulse
            
; Decode first two segment header bytes            
L07A7       jsr L0841              ; Decode segment header


; If first two bytes are $FF,$FF - Binary file header
            lda L0794              ; Check if segment header begins with $FF,$FF
            and L0795              ; 
            cmp #$FF               ; 
            beq L07A7              ; If yes, then decode another header
            
; If first two bytes are $DE,$DE - EOF flag
            lda L0794              ; Check if segment header begins with $DE,$DE
            cmp #$DE               ; 
            bne L07CB              ; If not, skip
            lda L0795              ; 
            cmp #$DE               ; If not, skip
            bne L07CB              ; 
            
            jsr L0816              ; Decode checksum byte
            jsr L080A              ; And check for INIT vector
            
            jmp (RUNAD)            ; RUN program

; Get the two remaining bytes of the segment header
L07CB       ldx #$96               ; Set X to $96
            jsr L0843              ; Decode some (2) bytes

;Increment segment end pointer by 1            
            inc L0796              ; Increment pointer at $0796
            bne L07D8              ; 
            inc L0797              ; 

;Set new pointers for segment data 
L07D8       ldx #$03               ; Set new DSKFMS and DSKUTIL pointers
L07DA       lda L0794,X            ; 
            sta DSKFMS,X           ; 
            dex                    ; 
            bpl L07DA              ; 

;Check if first segment. If so, set RUNAD            
            lda L0798              ; Check L0798 for zero
            bne L07F6              ; If not zero, skip
            inc L0798              ; Set L0798 for non-zero
            lda L0794              ; Set RUNAD
            sta RUNAD              ; 
            lda L0795              ; 
            sta RUNAD+1            ; 

;Decode segment data, determine what is next            
L07F6       jsr L0899              ; Decode byte
            lda INITAD             ; Check for INIT vector
            ora INITAD+1           ; 
            beq L07A7              ; No INIT, back and decode another header
            jsr L0816              ; There is INIT, decode checksum
            jsr L0813              ; Execute INIT code
            jmp L079C              ; And start decoding next BLOCK
            
L080A       lda INITAD             ; Check for INIT vector
            ora INITAD+1           ; 
            bne L0813              ; If present, execute INIT code
            rts                    ; Otherwise return
L0813       jmp (INITAD)           ; Execute INIT code


L0816       lda ABUFPT             ; Get checksum
            sta L0799              ; Copy the checksum
            ldx #$9A               ; X=$9A
            stx DSKFMS             ; Store to DSKFMS
            jsr L0846              ; Decode some bytes
            lda L0799              ; Get checksum copy
            cmp L079A              ; Compare with bytes decoded
            beq L083E              ; If equal, then terminate decoding and all is OK
;------------------------------------------------------------------------------
; Load error
;------------------------------------------------------------------------------
L082A       jsr L08CF              ; Otherwise just stop the motor
            lda VCOUNT             ; And 
            rol A                  ; Display rainbow
            sta WSYNC              ; 
            sta COLBK              ; 
            lda CONSOL             ; Check CONSOLE keys
            cmp #$06               ; Start pressed?
            bne L082A              ; No, keep displaying rainbow
L083E       jmp L08C9              ; Yes, terminate decoding


;------------------------------------------------------------------------------
; Prepare DSMFMS and DSKUTL pointers
;------------------------------------------------------------------------------
L0841       ldx #$94               ; X=$94
L0843       stx DSKFMS             ; Store to DSKFMS
            inx                    ; Increment X
L0846       inx                    ; Increment X
            stx DSKUTL             ; Store to DSKUTL
            ldx #$07               ; X=$07
            stx DSKFMS+1           ; Store to DSKFMS+1
            stx DSKUTL+1           ; Store to DSKUTL+1
            jmp L0899              ; And decode some bytes

;------------------------------------------------------------------------------
;Measure pulse            
;------------------------------------------------------------------------------
L0852       ldx #$00               ; Counter=0

;Detect 0-->1 transition
L0854       lda SKSTAT             ; Check serial port status
            and #$10               ; DATA IN signal
            beq L0854              ; If logical 0, loop
            lda #$00               ; Black background
            sta COLBK              ; 
;Detect 1-->0 transition            
L0860       inx                    ; Count loop passes
            lda SKSTAT             ; Check serial port status
            and #$10               ; DATA IN signal
            bne L0860              ; If logical 1, loop
            lda #$06               ; Light Gray background
            sta COLBK              ; 
            rts                    ; Return

;------------------------------------------------------------------------------
; Detect beginning of a block
;------------------------------------------------------------------------------
;Prepare system for decoding
L086E       sei                    ; Disable interrupts
            lda #$00               ; 
            sta ABUFPT             ; Clear ABUFPT
            sta NMIEN              ; 
            sta DMACLT             ; Disable DMA
            lda #$34               ; 
            sta PACTL              ; Motor ON
            sta PBCTL              ; Command ACTIVE
            
; Get 256 pulses of pilot tone            
L0881       ldy #$00               ; Y=0
L0883       jsr L0852              ; Measure pulse
            cpx #$1D               ; Check width
            bcc L0881              ; If too narrow, try again
            cpx #$32               ; Check width
            bcs L0881              ; If too wide, try again
            iny                    ; Enough pulses
            bne L0883              ; If not, continue
            
; Wait for sync pulse            
L0891       jsr L0852              ; Measure pulse
            cpx #$1D               ; Check width
            bcs L0891              ; If not narrow enough, continue
L0898       rts                    ; If narrow enough, return


;------------------------------------------------------------------------------
; Decode bytes
;------------------------------------------------------------------------------
L0899       lda #$00               ; A=0
            ldy #$08               ; Y=8
L089D       pha                    ; Push A
            jsr L0852              ; Measure pulse
            pla                    ; Pop A
            cpx #$08               ; Pulse too narrow?
            bcc L0898              ; Yes, just get out
            cpx #$36               ; Pulse too wide?
            bcs L0898              ; Yes, just get out
            cpx #$1D               ; Determine 1 or 0
            ror A                  ; And place the bit to the byte
            dey                    ; Decrement bit counter
            bne L089D              ; If not whole byte, keep decoding
            
            sta (DSKFMS),Y         ; Place byte to buffer
            eor ABUFPT             ; Update checksum
            sta ABUFPT             ; 
            
            inc DSKFMS             ; Update buffer pointer
            bne L08BC              ; 
            inc DSKFMS+1           ; 
L08BC       lda DSKFMS             ; Check if all bytes decoded
            cmp DSKUTL             ; 
            bne L0899              ; If not all, keep decoding
            lda DSKFMS+1           ; 
            cmp DSKUTL+1           ; 
            bne L0899              ; If not all, keep decoding
            rts                    ; Otherwise stop decoding

;------------------------------------------------------------------------------
; Set system to stop decoding
;------------------------------------------------------------------------------
L08C9       cli                    ; Re-enable interrupts
            lda #$40               ; Re-enable VBI
            sta NMIEN              ; 
L08CF       lda #$3C               ; Motor OFF
            sta PACTL              ; 
            sta PBCTL              ; Command not active
            rts                    ; Return
;------------------------------------------------------------------------------
; Initialize loader
;------------------------------------------------------------------------------
L08D8       lda #$FF               ; Re-enable ROM
            sta PORTB              ; 
            lda #$01               ; Indicate Boot OK
            sta BOOT               ; 
            lda CASINI             ; DOSINI=CASINI
            sta DOSINI             ; 
            lda CASINI+1           ; 
            sta DOSINI+1           ; 
            jmp L079C              ; And start loading
;------------------------------------------------------------------------------
; Tail
;------------------------------------------------------------------------------
            .byte $76,$65
            .byte $72,$32
            .byte $2E,$20,$F7
;------------------------------------------------------------------------------
; Run vector
;------------------------------------------------------------------------------            
            *=$02E0
            .word $079C
