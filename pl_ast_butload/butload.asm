;=========================================
;BUT LOADER (Atari Super Turbo, Poland)
;=========================================

;This is a placeholder for BUT loader, used
;by TURGEN SYSTEM
;This loader must be 576 bytes long and its
;entry point must be at $786


.INCLUDE "equates.asm"

            *= 1920
            .BYTE 0,5,0,7,0,0
ENTRYPT     JMP INIT 
START       lda NUMSEGBAK    ;Reinitialize number of segments
            sta NUMSEGS

NEXTSEG     lda #<BINHEADER  ;Prepare for segment header block
            sta L00CB
            lda #>BINHEADER
            sta L00CC
            
            jsr READBLOCK    ;Read header block

            lda L00CF        ;Check number of bytes to be read
            cmp #4          
            bne GOERROR
            lda L00D0  
            bne GOERROR

            lda #<FAKEINIT   ;Prepare fake init
            sta 738
            lda #>FAKEINIT
            sta 739
                     
            lda BINHEADER    ;Prepare for segment data block
            sta L00CB
            lda BINHEADER+1
            sta L00CC
             
            jsr READBLOCK    ;Read data block

            lda L00CB        ;Check number of bytes to be read
            cmp BINHEADER+2
            bne GOERROR
            lda L00CC
            cmp BINHEADER+3
            bne GOERROR
            
            jsr DOINIT      ;Perform INIT

            dec NUMSEGS     ;One more successfully read segment
            bne NEXTSEG     ;Read next segment

            jmp (RUNADDR)   ;Run the loaded program
            nop

DOINIT      jmp (738)       
GOERROR     jmp ERRHANDLE
;-----------------------------------------------
;Read block of data. The destination address in
;the main storage is simply given by vector
;at addresses 00CB,00CD. There is no end address
;since AST is using STOP pulses.
;-----------------------------------------------
READBLOCK   sei               ;Disable interrupt
            lda #0            ;Disable:
            sta NMIEN           ;Interrupts
            sta DMACLT          ;DMA
            lda #3            ;Prepare serial port
            sta SKCTL
            lda #52           ;Motor on
            sta PACTL
            sta PBCTL
            jmp L015C         ;Start decoding data 

L0140       ldx #0            ;Wait for edge
L0142       lda SKSTAT
            and #16
            beq L0142
            lda #186
            sta COLBK
L014E       inx               ;Mesure width of pulse
            lda SKSTAT
            and #16
            bne L014E
            lda #0
            sta COLBK
            rts

L015C       ldy #0            ;Zero pilot tone pulses counter(PILOT TONE)
L015E       jsr L0140         ;Go measure width
            cpx #29           ;Less than 29 ticks?
            bcc L015C         ;Yes, do it again
            cpx #50           ;More than 50 ticks?
            bcs L015C         ;Yes, do it again
            iny               ;Pilot tone pulse found. Increase count
            bne L015E         ;If less than 255 pilot tone pulses, go to find others

            lda #0            ;Zero
            sta L00CF         ;Zero loaded bytes counter
            sta L00D0

L0172       jsr L0140         ;Get pulse (WAITING FOR SYNC PULSE)
            cpx #29           ;More than 29 ticks?
            bcs L0172         ;Yes, go and get again

L0179       lda #0            
            ldy #8            ;Bit counter, 8 bits (READING BYTES)
L017D       pha               ;Push accumulator
            jsr L0140         ;Get pulse
            pla               ;Restore accumulator
            cpx #8            ;Less than 8 ticks?
            bcc L01A0         ;Yes, go terminate
            cpx #50           ;More than 50 ticks?
            bcs L01A0         ;Yes, go terminate
            cpx #29           ;Determine 1 or 0
            ror A             ;Shift bit into accumulator
            dey               ;Decrease bit counter
            bne L017D         ;If not all bits in byte, go to decode next bit

            sta (L00CB),Y     ;Put full byte to the buffer (BUFFER UPDATE)
            inc L00CF         ;Increase loaded bytes counter 00CF,00D0
            bne L0198
            inc L00D0
L0198       inc L00CB         ;Update buffer position (LO)
            bne L0179         ;Continue for next byte
            inc L00CC         ;Update buffer position (HI)
            bne L0179         ;Continue for next byte
       
L01A0       cli               ;Enable interrupts
            lda #60           ;Motor off
            sta PACTL
            sta PBCTL
            lda #255          ;Enable NMI
            sta NMIEN
            rts               ;Return
;----------------------------------------------
; Data area
;----------------------------------------------
FAKEINIT    rts
NUMSEGS     .BYTE 0           ;Number of segments to be loaded (mutable)
RUNADDR     .BYTE 0,0         ;Run address (Turgen System fills this)
NUMSEGBAK   .BYTE 0           ;Number of segments to be loaded (constant, Turgen System fills this)
;----------------------------------------------
;Error handler
;----------------------------------------------
ERRHANDLE   lda #255          ;Beep + keypress
            sta CH
            lda #3
            jmp REINIT
;----------------------------------------------
;Initialize loader
;----------------------------------------------
INIT        ldx #<TITLE       ;Display filename
            ldy #>TITLE
            jsr $C642

            lda #0            ;Reset coldstart flag
            sta 580        
            lda #1            ;Indicate disk boot ok
            sta 9

KINIT       lda #255 
            sta CH
            lda #1
REINIT      jsr LFDFC         ;Tape beep
            jmp START         ;Go to the start 

;----------------------------------------------
; Volatile area, binary header, title, filename
;-----------------------------------------------
BINHEADER   .BYTE 0,0,0,0     ;This must be here, so bad header does not destroy the loader

TITLE       .BYTE "BUT:"
FNAME       .BYTE 32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,$9B

;Filler
.REPT (576-(*-1920))
 .BYTE 0
.ENDR