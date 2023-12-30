;===============================================================================
;TURBO 2000 Express Loader - Omicron Turbo Version
;
; - Binary loader based on Czechoslovak Turbo 2000 system
; - Loading of segmented binary files
; - Pilot tone needed only after INIT segments

; File format:
;
; The file consists of Turbo 2000 blocks, each block can hold data of
; multiple segments. A new block is started after an INIT segment. 
;
; For each segment, there is the following data: 
; (0)    ID Byte .... 0101  Turbo 2000 Express Loader signature
;                1... ....  This is the last segment
;                .1.. ....  This segment is an INIT segment
; 
; (1..4) Buffer range  (BUFLO,BUFHI,BFENLO,BFENHI)
; (5..x) Data bytes    (given by the buffer range)
; (x+1)  XOR based checksum (all bytes of the segment before)
;
; What bytes are read is given by a "state" of the loader that is
; held in the LTEMP variable.
;
; STATE_ID     - Reading ID byte (initial state)
; STATE_BUFLO  - Reading BUFLO
; STATE_BUFHI  - Reading BUFHI
; STATE_BFENLO - Reading BFENLO
; STATE_BFENHI - Reading BFENHI
; STATE_DATA   - Reading segment data and checksum
;
; Assemble with MADS
;===============================================================================

            OPT H-
            ICL "system_equates.asm"
;--------------------------------------------------------------------------------
; State codes
;--------------------------------------------------------------------------------
            STATE_ID=1
            STATE_BUFLO=2
            STATE_BUFHI=3
            STATE_BFENLO=4
            STATE_BFENHI=5
            STATE_DATA=0            
;--------------------------------------------------------------------------------
; Mainline code
;--------------------------------------------------------------------------------
            ORG 2048
            jsr STARTUP
            jsr SWITCH_SIG

GOLOAD      jsr L0631         ;Call Turbo 2000 block decoding subroutine
            bcc HANDLE_ERROR  ;If error occured, handle error
            
            lda ICAX4Z        ;Check ID byte
            pha               ;Save the ID byte to stack
            and #64           ;Check for INIT indication
            beq NOT_INIT      ;If not, skip
            jsr DOINIT        ;Go perform init jump
            
NOT_INIT    pla               ;Restore the ID byte from stack
            and #128          ;Check for EOF
            bne END_LOAD      ;If set, terminate loading
            jmp GOLOAD        ;If not, go and read next block    
            
END_LOAD    jsr RUNIT         ;Run the loaded program
            jmp COLDSV        ;Perform cold start after return
            
RUNIT       jmp (RUNAD)       ;Run loaded program
DOINIT      jmp (INITAD)      ;Perform INIT jump
            
;-------------------------------------------------------------------------------
; Load error - Display red screen and wait until RESET
;-------------------------------------------------------------------------------            
HANDLE_ERROR lda #$18
             sta COLOR2
             sta COLOR4
             sta COLBK
             sta COLPF2
             sta COLDST       ;Ensure COLD start
ERR_LOOP     jmp ERR_LOOP     ;Endless loop
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
; LTEMP   - State code
; CHKSUM  - Checksum
; LTEMP+1 - Display mask (0 no display, 255 display)
; ICAX5Z  - Counter of pilot tone pulses
; ICAX6Z  - Byte being decoded
; STATUS  - Prior DATA IN logical value   
; ICAX4Z  - ID byte store    
;===============================================================================
   
L0631       lda #52           ;Switch program recorder to turbo mode                                                             
            sta PACTL         ;Motor ON
            sta PBCTL         ;Command ON
            
            lda #128          ;Disable interrupts
            sta POKMSK
            sta IRQEN
            
            lda #STATE_ID     ;Initial state
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
            bne L065D         ;If not enough pilot tone pulses (255), get next
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
NEXT_BYT1   ldy #198          ;Reset pulse timer
NEXT_BYTE   jsr GET8BITS      ;Start decoding the data

;Determine if segment data byte or header            
L0683       ldy LTEMP         ;{3} Check state
            beq L068E         ;{3} If zero, just place byte to the buffer

;-------------------------------------------------------------------------------
; Process block header bytes
; Timing information
;   Spent before       : 6
;   Determine ID/buffer: 8
;   Store ID and jump  : 9
;   Set buffer         : 14
;   Transition         : 13
;   Pulse setup        : 5
;                                                     Compensation
; Total processing ID byte:      6+8+9+13+5 =  41     1*12 + 2*2 => 57
; Total processing buffer setup: 6+8+14+13+5 = 46     1*12       => 58  

;-------------------------------------------------------------------------------
;Check for state            
            lda ICAX6Z        ;{3} Hold the byte we just read
            cpy #STATE_ID     ;{2} Is this the initial byte?
            bne HDR_BUF       ;{3} No, it muse be buffer setup
            
;Process the initial byte            
            sta ICAX4Z        ;{3}  Keep the value
            sta CHKSUM        ;{3}  Begin new checksum
            jsr L06FF         ;[12] Compensation   
            nop               ;[2]  Compensation
            nop               ;[2]  Compensation   
            jmp HDR_TRANS     ;{3}  Perform header state transition
            

;Setting BUFRLO,BUFRHI,BFENLO,BFENHI
;State code is used for indexing, offset is 2            
HDR_BUF     sta BUFRLO-STATE_BUFLO,Y ;{5} Store buffer range value

;Checksum for the header bytes            
            lda CHKSUM              ;{3}
            eor ICAX6Z              ;{3}
            sta CHKSUM              ;{3}
            jsr L06FF               ;[12] Compensation
            
;Transition to the new state
HDR_TRANS   iny                     ;{2} Presume going to higher state
            cpy #[STATE_BFENHI+1]   ;{2} Is the state past last buffer state?
            bne PUT_STATE           ;{3} No,skip
            ldy #STATE_DATA         ;{3} Reset to state data
PUT_STATE   sty LTEMP               ;{3} Store the new state
            
            ldy #200                ;{2} Reset pulse timer
            jmp NEXT_BYTE           ;{3} Go and get next byte

;-------------------------------------------------------------------------------
; Process segment data bytes
; Timing information:
;   Cycles spent before :        6
;   Update checksum     :        9
;   Byffer range check  :        15
;   Placing byte to buf :        24
;   Checking chsum      :        21 
;   Pulse setup         :        5
;                                                     Compensation 
; Total processing one data byte: 6+9+15+24+5 =  59    0   => 59
; Total for processing chksum   : 6+9+15+21+5 =  56    1*2 => 58
;-------------------------------------------------------------------------------
L068E
;Update checksum
            lda CHKSUM        ;{3}
            eor ICAX6Z        ;{3}
            sta CHKSUM        ;{3}

;Verify if all data decoded            
            lda BUFRLO        ;{3} Check if all bytes decoded
            cmp BFENLO        ;{3} 
            lda BUFRHI        ;{3} 
            sbc BFENHI        ;{3}
            bcs SEGDONE       ;{3} If all decoded, skip to termination

;Place segment byte to a buffer            
            ldy #0            ;{2} Place byte to the buffer
            lda ICAX6Z        ;{3}
            sta (BUFRLO),Y    ;{6}
            inc BUFRLO        ;{5} Update buffer pointer
            bne L069A         ;{3} 
            inc BUFRHI        ;{5} 
            
L069A       ldy #200          ;{2} Reset pulse timer
            JMP NEXT_BYTE     ;{3} Go and get next byte

;Done with segment
SEGDONE     nop               ;[2] Compensation nop
            lda #0            ;{2} Use CF=0 to indicate bad checksum
            cmp CHKSUM        ;{3} 
            bcc L06C2         ;{3} If bad checksum, terminate decoding

            lda ICAX4Z        ;{3} What was the first byte
            and #[128+64]     ;{2} Check for special flags
            bne L06C1         ;{3} If set, the block ends
            
            lda #STATE_ID     ;{2} Reset state to ID
            sta LTEMP         ;{3} 
            
            ldy #200          ;{2} 
            jmp NEXT_BYTE     ;{3} And continue processing
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
            lsr               ;Shift it
L_BKG       ora #$00          ;Color
            and LTEMP+1       ;Display stripe (if mask on)
            sta COLBK
            
L06E8       iny               ;Increment pulse width unit counter
            beq L06FE         ;If wraparound, terminate
            lda BRKKEY        ;Check BREAK key
            beq L06FC         ;If pressed, terminate
L_DET       lda SKSTAT        ;Get SKSTAT
L_AND       and #16           ;Determine DATA IN logical value
            cmp STATUS        ;Compare with prior one
            beq L06E8         ;If the same, then no edge has been found
            sta STATUS        ;Otherwise, there was an edge (0-1 or 1-0)
            sec               ;Indicate edge found
            rts               ;And return
L06FC       dec BRKKEY
L06FE       clc
L06FF       rts
;-----------------------------------------------------------------------------
; Switch signal source
;-----------------------------------------------------------------------------
SWITCH_SIG  pha
            lda ICSTAZ         ;Check indicator         
            bne SW_TOSIO       ;If not zero, switch to SIO
            
SW_TOJOY    lda #$00           ;$D300
            sta L_DET+1
            lda #$D3
            sta L_DET+2
            lda #128           ;Bit 7
            sta L_AND+1
            lda #$24           ;Unify luminance to 4, shift colors by 2
            sta L_BKG+1
            bne SW_END
        
SW_TOSIO    lda #$0F
            sta L_DET+1
            lda #$D2
            sta L_DET+2
            lda #16
            sta L_AND+1
            lda #$10           
            sta L_BKG+1        ;Set primary color
            
SW_END      pla                ;Restore everything
            rts                ;Return
;-----------------------------------------------------------------------------
; Start
;-----------------------------------------------------------------------------
STARTUP   lda #<GOLOAD         ;Become DOS
          sta DOSINI
          lda #>GOLOAD
          sta DOSINI+1
          ldx #0               ;Reset cold start flag
          stx COLDST
          inx                  ;Indicate disk boot succeded (1)
          stx BOOT
          rts                      