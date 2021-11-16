;===============================================================================
;TURBO 2000 Express Loader
;
; - Binary loader based on Czechoslovak Turbo 2000 system
; - Loading of segmented binary files
; - Pilot tone needed only after INIT segments

; Block format
; (0) ID Byte .... 1001   Turbo 2000 Express Loader
;             1... ....   This is the last block of file
;             .1.. ....   This block holds INIT segment
;
; (1) Buffer range  (BUFLO,BUFHI,BFENLO,BFENHI)
; (5) Data bytes    (given by the buffer range)
; (x) XOR based checksum (all bytes before)
;                  
;===============================================================================


            .INCLUDE "equates.asm"
            *=2048

START       jsr DOSINIT       ;Setup DOS vectors   

GOLOAD      jsr L0631         ;Call Turbo 2000 block decoding subroutine
            bcc HANDLE_ERROR  ;If error occured, handle error
            
            lda ICAX4Z        ;Check ID byte
            and #64           ;Check for INIT indication
            beq NOT_INIT      ;If not, skip
            jsr DOINIT        ;Go perform init jump
            
NOT_INIT    lda ICAX4Z
            and #128          ;Check for EOF
            bne END_LOAD      ;If set, terminate loading
            jmp GOLOAD        ;If not, go and read next block    
            
END_LOAD    jsr RUNIT         ;Run the loaded program
            jmp COLDSV        ;Perform cold start after return
            
RUNIT       jmp (RUNAD)       ;Run loaded program
DOINIT      jmp (INITAD)      ;Perform INIT jump
            
;-------------------------------------------------------------------------------
; Load error
;-------------------------------------------------------------------------------            
HANDLE_ERROR lda #04
             sta COLOR4
             sta COLBK
             jmp HANDLE_ERROR
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
; LTEMP   - State byte
; CHKSUM  - Checksum
; LTEMP+1 - Display mask (0 no display, 255 display)
; ICAX5Z  - Counter of pilot tone pulses
; ICAX6Z  - Byte being decoded
; STATUS  - Prior DATA IN logical value       
;===============================================================================
   
L0631       lda #52           ;Switch program recorder to turbo mode                                                             
            sta PACTL         ;Motor ON
            sta PBCTL         ;Command ON
            
            lda #128          ;Disable interrupts
            sta POKMSK
            sta IRQEN
            
            lda #251          ;Clear state byte
            sta LTEMP
            
            clc               ;Clear work fields 
            ldy #0
            sty STATUS
            sty CHKSUM
            sty NMIEN
            sty DMACLT
            
 
;-------------------------------------------------------------------------------
; Wait for 256 pilot tone pulses
;-------------------------------------------------------------------------------           
L0650       beq L0652         ;If not equal, terminate decoding
            jmp L06C2
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
            bcs NEXT_BYT1
            jmp L06C2         ;If no edge, terminate decoding
            
;-------------------------------------------------------------------------------
; Decode data
;-------------------------------------------------------------------------------
;Get next byte
NEXT_BYT1   ldy #196
NEXT_BYTE   jsr GET8BITS      ;Start decoding the data

;Determine if segment data byte or header            
L0683       ldy LTEMP         ;Check state
            beq L068E         ;If zero, just place byte to the buffer

;-------------------------------------------------------------------------------
; Process block header bytes
;-------------------------------------------------------------------------------
;Initial byte            
            lda ICAX6Z
            cpy #251          ;If one, this is initial byte
            bne NOT_ONE
            sta ICAX4Z        ;Keep the value
            jmp DO_CHSUM

;Setting BUFRLO            
NOT_ONE     cpy #252          ;If two, set buffer start lo
            bne NOT_TWO
            sta BUFRLO
            jmp DO_CHSUM            

;Setting BUFRHI
NOT_TWO     cpy #253         ;If three, set buffer start hi
            bne NOT_THREE
            sta BUFRHI
            jmp DO_CHSUM
            
;Setting BFENLO            
NOT_THREE   cpy #254         ;If four, set buffer end lo
            bne NOT_FOUR
            sta BFENLO
            jmp DO_CHSUM

;Setting BFENHI            
NOT_FOUR    sta BFENHI       ;If five, set buffer end hi
            nop
            nop
            nop 

;Checksum for the header bytes            
DO_CHSUM    lda CHKSUM
            eor ICAX6Z
            sta CHKSUM
;Increment the state variable            
            iny
            sty LTEMP
            ldy #200
            jmp NEXT_BYTE

;-------------------------------------------------------------------------------
; Process segment data bytes
;-------------------------------------------------------------------------------
L068E
;Update checksum
            lda CHKSUM        ;Update checksum
            eor ICAX6Z
            sta CHKSUM

;Verify if all data decoded            
            lda BUFRLO        ;Check if all bytes decoded
            cmp BFENLO
            lda BUFRHI
            sbc BFENHI
            bcs SEGDONE      ;If all decoded, skip to termination

;Place segment byte to a buffer            
            ldy #0            ;Place byte to the buffer
            lda ICAX6Z
            sta (BUFRLO),Y
            inc BUFRLO        ;Update buffer pointer
            bne L069A
            inc BUFRHI
            
L069A       ldy #200
            JMP NEXT_BYTE

;Done with segment
SEGDONE     lda #0            ;Use CF=0 to indicate bad checksum
            cmp CHKSUM
            bcc L06C2         ;If bad checksum, terminate decoding

            lda ICAX4Z        ;What was the first byte
            and #[128+64]     ;Check for special flags
            bne L06C1         ;If set, the block ends
            
            lda #251          ;Reset state to zero
            sta LTEMP
            jmp NEXT_BYTE     ;And continue processing
;-------------------------------------------------------------------------------
; Get 8 bits
;-------------------------------------------------------------------------------
GET8BITS    lda #1            ;Prepare bit mask
            sta ICAX6Z         
            
G8B_L       jsr L06D6         ;Measure width of the pulse
            bcc L06C2         ;If no pulse, terminate decoding
            cpy #227          ;Determine wide or narrow pulse
            rol ICAX6Z        ;Rotate bit mask
            ldy #198          ;Set pulse width unit counter base value
            bcc G8B_L         ;If byte not finished, get next bit
            
            rts
;-------------------------------------------------------------------------------
; Terminate decoding
;------------------------------------------------------------------------------- 
L06C1       sec               ;Successful block reading           
L06C2       lda #192          ;Enable interrupts
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
;Initialization of DOS vectors
;-----------------------------------------------
DOSINIT     ldx #<2048
            stx DOSINI
            ldx #>2048
            stx DOSINI+1
            ldx #0
            sta WARMST
            rts 