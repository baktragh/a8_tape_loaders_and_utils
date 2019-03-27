;==============================================================================
; Unerring Master Atari Turbo Tape
; Stage 0 loader
;==============================================================================

    .INCLUDE "equates.asm"

;
; Code equates
;
L00A1       = $00A1
L00A5       = $00A5
L00A9       = $00A9
L00AD       = $00AD
L00C8       = $00C8
L00CB       = $00CB
L00CC       = $00CC
L00CD       = $00CD
L00CE       = $00CE
L00CF       = $00CF
L00D0       = $00D0
L00D1       = $00D1
L0600       = $0600
L0757       = $0757
L07A2       = $07A2
L0DC8       = $0DC8
L2F2C       = $2F2C
LA1AF       = $A1AF
LCC0A       = $CC0A
LCC23       = $CC23
LCC3F       = $CC3F
LCC50       = $CC50
LCC60       = $CC60
LCC96       = $CC96
LCCAC       = $CCAC
LCCBC       = $CCBC
LCD0A       = $CD0A
LCD82       = $CD82
LCE00       = $CE00
LCE38       = $CE38
LCF00       = $CF00
LCF04       = $CF04
LCF05       = $CF05
LCF06       = $CF06
LCF0D       = $CF0D
;
; Start of code
;
            *= $05FA
;
            .byte $00,$00,$00,$00,$00,$00 ; Former boot header

;------------------------------------------------------------------------------
; Initialization - Copy loader to RAM under ROM
;------------------------------------------------------------------------------
            lda #$00               ; Disable DMA
            sta DMACLT             ; 
            sta SDMCTL             ; 
            
            lda #$3C               ; Motor off
            sta PACTL              ;
            
;Copy ROM to RAM under ROM             
            lda #$00               ; NMI Off
            sta NMIEN              ; 
            sta L00CB              ; Set $CB to zero
            lda #$C0               ; Set $CC to $C0 
            sta L00CC              ; 
            
            ldy #$00               ; Y=0
L061A       ldx #$FF               ; X=255
            stx PORTB              ; ROM ON
            lda (L00CB),Y          ; Load $C000,Y
            ldx #$FE               ; ROM OFF
            stx PORTB              ; 
            sta (L00CB),Y          ; Store
            inc L00CB              ; Increment source ptr (LO)
            bne L061A              ; Loop if not whole page
            inc L00CC              ; Increment source ptr (HI)
            bne L0637              ; If not all pages, check I/O area

;Terminate copying loop            
            lda #$FF               ; Re-enable NMI 
            sta NMIEN              ; 
            bne L0643              ; Skip over

;Protect I/O area against copying            
L0637       lda L00CC              ; A5 CC
            cmp #$D0               ; C9 D0
            bne L061A              ; D0 DD
            lda #$D8               ; A9 D8
            sta L00CC              ; 85 CC
            bne L061A              ; D0 D7


; Copy loader portion starting from L0657 to RAM under ROM (addr CC0A)            
L0643       ldx #$00               ; A2 00
L0645       lda L0657,X            ; BD 57 06
            sta LCC0A,X            ; 9D 0A CC
            lda L0757,X            ; BD 57 07
            sta LCD0A,X            ; 9D 0A CD
            inx                    ; E8
            bne L0645              ; D0 F1
            jmp LCCBC              ; Pass control to the loader

;===============================================================================
; Loader - Relocated to RAM under ROM
;===============================================================================

;Prepare for decoding
L0657       sei                    ; Disable interrupts
            lda #$00               ; 
            sta NMIEN              ; 
            sta DMACLT             ; Disable DMA
            lda #$03               ; Set-up serial port
            sta SKCTL              ; 
            lda #$34               ; Motor on
            sta PACTL              ; 
            sta PBCTL              ; Command active
            nop                    ; Wait
            nop                    ; Wait
            rts                    ; Return

;Wait for edge            
            ldx #$00               ; X=0
L0672       lda SKSTAT             ; Check status of serial port
            and #$10               ; Check data-in pin
            beq L0672              ; If zero, then loop
            lda #$00               ; Black background
            sta COLBK              ; 
L067E       inx                    ; Increment X
            lda SKSTAT             ; Check status of serial port
            and #$10               ; Check data-in pin
            bne L067E              ; If not zero, then loop
            lda #$06               ; Gray background
            sta COLBK              ; 
            rts                    ; Return
            
;Wait for pilot tone            
L068C       ldy #$00               ; Y=0
L068E       jsr LCC23              ; Wait for edge
            cpx #$1D               ; Check duration
            bcc L068C              ; Too short, try again
            cpx #$32               ; Check duration
            bcs L068C              ; Too long, try again
            iny                    ; Increment Y
            bne L068E              ; If not 255, then keep waiting
            rts                    ; Return
            
;Wait for synchro bit            
            lda #$00               ; Zero area
            sta L00CF              ; 
            sta L00D0              ; 
            sta L00D1              ; 
L06A5       jsr LCC23              ; Wait for edge
            cpx #$1D               ; Check duration
            bcs L06A5              ; If OK, then continue
L06AC       rts                    ; Return


L06AD       lda #$00               ; A=0
            ldy #$08               ; Y=8 (bit counter)
L06B1       pha                    ; Push A
            jsr LCC23              ; Wait for edge
            pla                    ; Pop A
            cpx #$0A               ; Check duration
            bcc L06AC              ; If too short, return
            cpx #$32               ; If too long, return
            bcs L06AC              ; 
            cpx #$1D               ; Determine wide or narrow
            ror A                  ; Place next decoded bit
            dey                    ; Decrement bit counter
            bne L06B1              ; If not 8 bits, continue
            sta (L00CB),Y          ; Place decoded byte to buffer
            
            eor L00D1              ; Update checksum
            sta L00D1              ;
             
            inc L00CB              ; Update buffer pointer (LO)
            bne L06D0              ; 
            inc L00CC              ; Update buffer pointer (HI)
L06D0       inc L00CF              ; Update byte counter (LO)
            bne L06D6              ; 
            inc L00D0              ; Update byte counter (HI)
            
L06D6       lda L00CF              ; Check if block fully decoded
            cmp L00CD              ; 
            bne L06AD              ; If not, decode nex byte
            lda L00D0              ; 
            cmp L00CE              ; 
            bne L06AD              ; If not, decode another byte
            rts                    ; Otherwise return

; Terminate decoding            
            cli                    ; Re-enable interrupts
            lda #$3C               ; Command not active 
            sta PBCTL              ; 
            lda #$40               ; Re-enable VBI
            sta NMIEN              ; 
            lda #$00               ; Black background
            sta COLBK              ; 
            lda #$3C               ; Motor off
            sta PACTL              ; 
            rts                    ; Return

; Decode whole block            
            jsr LCC0A              ; Prepare for decoding
            jsr LCC3F              ; Wait for edge
            jsr LCC50              ; Wait for pilot tone
            jsr LCC60              ; Wait for synchro bit
            jsr LCC96              ; Decode block
            rts                    ; Return

; Initialization            
            ldx #$01               ; RESET=Cold Start
            stx COLDST             ; 
            ldx #$FE               ; ROM OS Enabled
            stx PORTB              ; 
            ldx #$00               ; Open E:
            lda #$0C               ; 
            sta IOCB0+ICCOM        ; 
            jsr CIOV               ; 
            
            ldx #$00               ; 
            lda #$03               ; 
            sta IOCB0+ICCOM        ; 
            lda #$48               ; 
            sta IOCB0+ICBAL        ; 
            lda #$C4               ; 
            sta IOCB0+ICBAH        ; 
            lda #$0C               ; 
            sta IOCB0+ICAX1        ; 
            jsr CIOV               ; 
;Display prompt            
            ldy #$27               ; Display text
L0738       lda LCD82,Y            ; 
            sta (SAVMSC),Y         ; 
            dey                    ; 
            bpl L0738              ; 10 F8
            
            lda #$00               ; Black background
            sta COLOR2             ; 
            sta L00CB              ; Buffer (LO)=0
            lda #$04               ; Buffer (HI)=4
            sta L00CC              ; 
            
            ldy #$00               ; Clear pages $04 - $9C 
L074D       lda #$00               ; 
            sta (L00CB),Y          ; 
            iny                    ; 
            bne L074D              ; 
            inc L00CC              ; 
            lda L00CC              ; 
            cmp #$9C               ; 
            bne L074D              ; 

; Wait for START            
L075C       lda CONSOL             ; Check CONSOL keys
            cmp #$06               ; Check if START
            bne L075C              ; IF not, keep checking

;Load header            
L0763       lda #$00               ; Buffer $CE00
            sta L00CB              ; 
            lda #$CE               ; 
            sta L00CC              ; 
            
            lda #$0E               ; Length $010E
            sta L00CD              ; 
            lda #$01               ; 
            sta L00CE              ; 
            
            sta LCF0D              ; Set sentinel to 1
            jsr LCCAC              ; Decode block
            lda LCF0D              ; Check sentinel
            bne L0763              ; If sentinel not overwritten to 0, error
            lda LCE38              ; Check byte in the header
            cmp #$35               ; for value of $35 'U' character
            bne L0763              ; If not equal, try again
            
; Display program name            
            ldy #$00               ; A0 00

L0787       lda LCE00,Y            ; Display program name  (255 chars)
            sta (SAVMSC),Y         ; 
            iny                    ; 
            bne L0787              ; 

; Wait for START or OPTION            
L078F       lda CONSOL             ; Check for console keys
            cmp #$06               ; START pressed?
            beq L079C              ; Then quit loop and continue
            cmp #$03               ; OPTION pressed?
            beq L0763              ; Yes, then try loading next header
            bne L078F              ; Loop
            
; Load the main block            
L079C       ldx #$03               ; Set buffer range of the block
L079E       lda LCF00,X            ; 
            sta L00CB,X            ; 
            dex                    ; 
            bpl L079E              ; In a loop
            
            jsr LCCAC              ; Decode main block
            lda LCF06              ; Checksum specified in the header
            cmp L00D1              ; Compare with real checksum
            bne L07CB              ; If the checksum is wrong, go start over
            
; Run the loaded program (as it were boot file)             
            lda LCF04              ; 
            sta DOSINI             ; 
            lda LCF05              ; 
            sta DOSINI+1           ; 
            ldx #$01               ; 
            stx BOOT               ; 
            lda #$94               ; 
            sta COLOR2             ; 
            lda #$00               ; 
            sta COLDST             ; 
            jmp (DOSINI)           ; 
            
L07CB       jmp LCCBC              ; Start over


;------------------------------------------------------------------------------
; Screen data and variables
;------------------------------------------------------------------------------
            .byte $00,$80
            .byte $AC,$AF,$A1
            .byte $A4,$A5
            .byte $B2,$80
            .byte $B5,$AD
            .byte $80,$00,$00
            .byte $48
            .byte $B3
            .byte $B4,$A1
            .byte $B2
            .byte $B4,$C8
            .byte $0D,$2C,$2F
            .byte $21,$24
            .byte $00,$00
            .byte $48
            .byte $AF
            .byte $B0,$B4
            .byte $A9,$AF
            .byte $AE,$C8,$0D
            .byte $33,$2B,$29,$30
            .byte $00,$00,$00
;------------------------------------------------------------------------------
; RUN Vector
;------------------------------------------------------------------------------
            *= $02E0
;
            .word L0600
;
         
