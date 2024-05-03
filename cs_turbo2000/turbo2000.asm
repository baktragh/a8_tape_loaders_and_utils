;===============================================================================
;TURBO 2000 (Czechoslovakia) loader
;===============================================================================


            .INCLUDE "equates.asm"
            ENTRY_ADDR = 1484
            *=ENTRY_ADDR
            

L05D2       jsr DOSINIT       ;Setup DOS vectors   
            ldx #<TITLE       ;Display title
            ldy #>TITLE
            jsr LC642
            lda #1            ;One tape beep
            jsr LFDFC  


L05D7       lda #4            ;Setup address where turbo header will be placed
            sta BUFRHI
            sta BFENHI
            lda #17
            sta BFENLO
            lda #0            ;First byte of the block should be 0 
            sta BUFRLO
            jsr L0631         ;Call Turbo 2000 block decoding subroutine
            bcc L05D7         ;If error occured, try to decode the header again
 
            lda L040B         ;Setup address where main block will be placed
            sta BUFRLO
            clc
            adc L040D
            sta BFENLO
            lda L040C
            sta BUFRHI
            adc L040E
            sta BFENHI

            lda #125          ;Display file name
            sta L0400
            lda #155
            sta L040B
            ldx #0
            ldy #4
            jsr LC642	      
            lda #1            ;One tape beep
            jsr LFDFC

            lda #255          ;First byte should be 255
            jsr L0631         ;Call Turbo 2000 block decoding subroutine
            bcs L0622         ;No error - jump
            jsr LC63E         ;Error - call "BOOT ERROR" ROM routine
            jmp L05D2         ;Back to the beginning

L0622       nop               

            jmp (L040F)       ;Run loaded program

;===============================================================================
;Block decoding subroutine
;Block is placed from: BUFRLO+256*BUFRHI
;                to:   (BFENLO+256*BFENHI)-1
;Register usage
;Input:
;  A - Identification byte - first byte of the block that is not part of the
;      data
;Output:
;  CF - 1 - Block decoding OK
;       0 - Block decoding failed           
;
;Fields used:
; BUFRLO,BUFRHI,BFENLO,BFENHI - Buffer pointer
; LTEMP   - Identification byte
; CHKSUM  - Checksum
; LTEMP+1 - Display mask (0 no display, 255 display)
; ICAX5Z  - Counter of pilot tone pulses
; ICAX6Z  - Byte being decoded
; STATUS  - Prior DATA IN logical value       
;===============================================================================
   
L0631       sta LTEMP         ;Keep the requested first byte value

            lda #52           ;Switch program recorder to turbo mode                                                             
            sta PACTL         ;Motor ON
            sta PBCTL         ;Command ON
            
            lda #128          ;Disable interrupts
            sta POKMSK
            sta IRQEN
            
            clc               ;Clear work fields 
            ldy #0
            sty STATUS
            sty CHKSUM
            sty NMIEN
            sty DMACLT
            php

;-------------------------------------------------------------------------------
; Wait for 256 pilot tone pulses
;-------------------------------------------------------------------------------           
L0650       bne L06C2         ;If not equal, terminate decoding
L0652       jsr L06DB         ;Wait for edge
            bcc L0650         ;If no edge, try again
            
            lda #0            ;Clear           
            sta ICAX5Z        ; Number of pilot tone pulses 
            sta LTEMP+1       ; Display mask (0 No stripes, 255 Stripes)
            
L065D       ldy #180          ;Set pulse width unit counter base value
L065F       jsr L06D6         ;Measure width of the pulse
            bcc L0650         ;If no pulse, start over
            cpy #216          ;Is the pulse too long?
            bcc L0652         ;Yes, start over
            inc ICAX5Z        ;Increment pilot tone pulse counter
            bne L065D         ;If not enoguh pilot tone pulses (255), get next
            dec LTEMP+1       ;More than 255 pilot tone pulses - display stripes

;-------------------------------------------------------------------------------
; Wait for synchronization (very narrow) pulse
;-------------------------------------------------------------------------------
            
L066E       ldy #209          ;Set pulse width unit counter base value
            jsr L06DB         ;Wait for edge
            bcc L0650         ;If no edge, start over
            
            cpy #222          ;Pulse too wide to be a sync pulse?
            bcs L066E         ;Yes, keep waiting
            jsr L06DB         ;Wait for edge
            bcc L06C2         ;If no edge, terminate decoding
            
            ldy #198          ;Set pulse width unit counter base value
            jmp L069D         ;Start decoding the data
            
;-------------------------------------------------------------------------------
; Decode data
;-------------------------------------------------------------------------------            
L0683       plp               ;Is this identification byte?
            bne L068E         ;No, just place the byte to the buffer
            lda LTEMP         ;Yes, please update checksum
            eor ICAX6Z        ;Check identification byte
            bne L06C3         ;Bad - terminate decoding
            beq L069A         ;Good - decode next byte
            
L068E       ldy #0            ;Place byte to the buffer
            lda ICAX6Z
            sta (BUFRLO),Y
            inc BUFRLO        ;Update buffer pointer
            bne L069A
            inc BUFRHI
L069A       ldy #200          ;Set pulse width unit counter base value
                              ;Time compensation
            php
            
L069D       lda #1            ;Prepare bit mask
            sta ICAX6Z         
            
L06A1       jsr L06D6         ;Measure width of the pulse
            bcc L06C2         ;If no pulse, terminate decoding
            cpy #227          ;Determine wide or narrow pulse
            rol ICAX6Z        ;Rotate bit mask
            ldy #198          ;Set pulse width unit counter base value
            bcc L06A1         ;If byte not finished, get next bit
            
            lda CHKSUM        ;Update checksum
            eor ICAX6Z
            sta CHKSUM
            
            lda BUFRLO        ;Check if all bytes decoded
            cmp BFENLO
            lda BUFRHI
            sbc BFENHI
            bcc L0683         ;If not all decoded, place byte to memory   
            lda #0            ;Use CF=0 to indicate bad checksum
            cmp CHKSUM


;-------------------------------------------------------------------------------
; Terminate decoding
;-------------------------------------------------------------------------------            
L06C2       pla               ;Loading complete
L06C3       lda #192          ;Enable interrupts
            sta NMIEN
            sta POKMSK
            sta IRQEN
            
            lda #60           ;Switch program recorder mode to standard           
            sta PACTL         ;Motor OFF
            sta PBCTL         ;Command OFF
            rts               ;Return

;-------------------------------------------------------------------------------
; Detect pulses and edges
;-------------------------------------------------------------------------------            
L06D6       jsr L06DB         ;Wait for edge
            bcc L06FF         ;If no edge, terminate
            
L06DB       ldx #4            ;Delay
L06DD       dex               
            bne L06DD
            
            lda STATUS        ;Get prior status of DATA IN  
            lsr A             ;Shift it
            and LTEMP+1       ;Display stripe (if mask on)
            sta COLBK
            
L06E8       iny               ;Increment pulse width unit counter
            beq L06FE         ;If wraparound, terminate
            lda BRKKEY        ;Check BREAK key
            beq L06FC         ;If pressed, terminate
            lda SKSTAT        ;Get SKSTAT
            and #16           ;Determine DATA IN logical value
            cmp STATUS        ;Compare with prior one
            beq L06E8         ;If the same, then no edge has been found
            sta STATUS        ;Otherwise, there was an edge (0-1 or 1-0)
            sec               ;Indicate edge found
            rts               ;And return
L06FC       dec BRKKEY
L06FE       clc
L06FF       rts


;-----------------------------------------------
;Strings
;-----------------------------------------------
TITLE       .BYTE "TURBO 2000",$9B

;-----------------------------------------------
;Initialization of DOS vectors
;-----------------------------------------------
DOSINIT     ldx #<ENTRY_ADDR
            stx DOSINI
            ldx #>ENTRY_ADDR
            stx DOSINI+1
            ldx #1
            stx BOOT
            dex
            stx COLDST
            rts 