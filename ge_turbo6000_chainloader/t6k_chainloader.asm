;==============================================================================
; Turbo 6000 - ChainLoader
; Special binary loader for Turbo 6000
;==============================================================================

;Special equates

XSTATUS      = 137                  ; Indicates success ($00) or failure ($FF)
XCHKSUM      = 128                  ; Holds checksum
XBFENLO      = 131                  ; Buffer end
XBFENHI      = 132
XLTEMP       = 133                  ; Buffer pointer
            
            .INCLUDE "equates.asm"

; Start of code
             *= 2048
                
;-------------------------------------------------------------------------------
; Initialization
;-------------------------------------------------------------------------------
L0770       cld                      ; Clear the decimal flag
            jmp PREINIT              ; Pre-initialization
            
START       ldx #$FF                 ; Reset stack pointer
            txs                      ; 
            inx                      ; Set X=0
;-------------------------------------------------------------------------------
; Binary load 
;-------------------------------------------------------------------------------
;Decode segment header to the header buffer
DEC_SHEAD   lda #$0C                  ; Expected ID byte is $0C
            sta EXPECTED_ID       
            lda #$14                  ; Brown color
            sta BLOCK_COLOR
            
            lda #<SEG_HEADER_BUFFER  ; Set buffer range
            sta XLTEMP
            lda #>SEG_HEADER_BUFFER
            sta XLTEMP+1
            
            lda #<[SEG_HEADER_BUFFER+4]
            sta XBFENLO
            lda #>[SEG_HEADER_BUFFER+4]
            sta XBFENHI
            
            jsr GODEC_H_OR_M        ; Decode segment header
            lda XSTATUS             ; Success?
            beq DEC_SDATA           ; Yes, proceed to segment data
            jmp LOAD_ERR            ; Otherwise handle error

; Decode segment data
DEC_SDATA   lda #<FAKEINIT          ; Reset INIT vector
            sta INITAD
            lda #>FAKEINIT
            sta INITAD+1

            lda SEG_HEADER_BUFFER   ; Check for termination segment header
            cmp #255
            bne DEC_SDATA_PBUF
            lda SEG_HEADER_BUFFER+1
            cmp #255
            bne DEC_SDATA_PBUF
            lda SEG_HEADER_BUFFER+2
            cmp #0
            bne DEC_SDATA_PBUF
            jmp (RUNAD)
            
DEC_SDATA_PBUF                       
            lda #$0D                ; Expected ID byte is $0D
            sta EXPECTED_ID
            lda #$B4                  ; Brown color
            sta BLOCK_COLOR
            
            lda SEG_HEADER_BUFFER   ; Set buffer range
            sta XLTEMP
            lda SEG_HEADER_BUFFER+1
            sta XLTEMP+1
            lda SEG_HEADER_BUFFER+2
            sta XBFENLO
            lda SEG_HEADER_BUFFER+3
            sta XBFENHI
            
            jsr GODEC_H_OR_M
            lda XSTATUS
            beq DEC_SDATA_OK
            jmp LOAD_ERR

DEC_SDATA_OK 
            jsr GOINIT             ; Execute INIT vector
            jmp DEC_SHEAD          ; And get another segment
            
GOINIT      jmp (INITAD)
          
;================================================================================
; Decode block
;  Pilot tone $02,$02,...
;  Synchro sequence $09,$08,..,$01
;  Bytes of the block (Begins at XLTEMP,XLTEMP+1 end at [XBFENLO,XBFENHI]-1)
;  Checksum byte (eor of all bytes, initialized to zero)
;================================================================================

;-------------------------------------------------------------------------------
; Prepare/Terminate decoding
;------------------------------------------------------------------------------
GODEC_H_OR_M sei                    ; Disable interrupts
             lda #$00               ; Disable NMI
             sta NMIEN              ; 
             jsr DEC_H_OR_M         ; Decode file
             lda #$40               ; Enable VBI
             sta NMIEN              ; 
             cli                    ; Re-enable interrupts
FAKEINIT     rts                    ; Return
;-------------------------------------------------------------------------------
; Block decoding
;------------------------------------------------------------------------------
DEC_H_OR_M  jsr PREP_SPORT         ; Initialize serial port
DECF_MAIN   jsr BEGIN_DEC          ; Begin decoding of a block
            lda ICAX6Z
            cmp EXPECTED_ID
            bne DECF_MAIN
            
            sty XCHKSUM            ; Zero checksum
            dex                    ; Decrement X
            lda BLOCK_COLOR        ; Set brown/green background
            sta COLBK              ; 
L0940       jsr DEC_BYTE           ; Decode byte
            sta (XLTEMP),Y         ; Store byte to buffer
            
L094D       eor XCHKSUM            ; Update checksum
            sta XCHKSUM            ; 
            inc XLTEMP             ; Update buffer pointer
            bne L0957              ; 
            inc XLTEMP+1           ; 
L0957       lda XLTEMP             ; Check buffer pointer
            cmp XBFENLO            ; Low byte
            lda XLTEMP+1           ; High byte
            sbc XBFENHI            ; 
            bcc L0940              ; If not end of file, continue
            jsr DEC_BYTE           ; Decode checksum byte
            cmp XCHKSUM            ; Compare with checksum
            sty XSTATUS            ; Store result to XSTATUS
L0968       beq STOP_SPORT         ; If zero, terminate with success

L096A       ldx #$FF               ; Status = $FF
            stx XSTATUS            ; 
            bne STOP_SPORT         ; Reinitialize
;------------------------------------------------------------------------------
; Begin decoding a block
; Returns Y=0 and last decoded byte
;------------------------------------------------------------------------------
BEGIN_DEC   lda #$70               ; Setup timers
            sta AUDCTL             ; 
            lda #$D2               ; 
            sta AUDF1              ; 
            lda #$01               ; 
            sta AUDF2              ; 
            nop                    ; 

; Wait for pilot tone (series of $02 bytes)            
W_PILOT     jsr DEC_BIT            ; Decode bit
            rol ICAX6Z             ; Rotate byte being decoded
            lda ICAX6Z             ; Check the byte
            cmp #$02               ; If not 2
            bne W_PILOT            ; Then loop
            
;Wait for synchronization sequence ($09,$08,...$01)            
            ldy #$09               ; Set Y=9
L098D       jsr DEC_BYTE           ; Decode byte
            cmp #$02               ; Decoded byte is 2
            beq L098D              ; If yes, then loop
L0994       cpy ICAX6Z             ; If decoded byte is not Y
            bne W_PILOT            ; Then start over
            jsr DEC_BYTE           ; Deocde byte
            dey                    ; Decrement Y
            bne L0994              ; If not zero, continue decoding
            rts                    ; Return
            nop                    ; 

;------------------------------------------------------------------------------
; Decoding bits and bytes
;------------------------------------------------------------------------------

; Decode one byte
DEC_BYTE    lda #$08               ; Set bit counter to 8
            sta NGFLAG             ; Store to NGFLAG
L09A4       jsr DEC_BIT            ; Decode bit
            rol ICAX6Z             ; Rotate byte being decoded
            dec NGFLAG             ; Decrement bit counter
            bne L09A4              ; If not 8 bits, decode next bit
            lda ICAX6Z             ; Get the byte decoded
            rts                    ; Return
            
; Decode BIT            
DEC_BIT     lda #$01               ; Check IRQ status
L09B2       bit IRQST              ; Timer 1 IRQ?
            bne L09B2              ; No, loop
            
W_EDGE      lda PORTA              ; Touch PORTA
            lda #$80               ; Prepare mask
L09BC       bit PACTL              ; Check PACTL for interrupt status bit
            beq L09BC              ; If zero, keep looping
            lda IRQST              ; Check POKEY interrupt status
            pha                    ; Push to stack
            ldx #$00               ; Disable all interrupts
            stx IRQEN              ; 
            dex                    ; Re-enable all interrupts
            stx IRQEN              ; 
            stx STIMER             ; Start counting again
            lda PORTA              ; Touch PORTA
            pla                    ; Restore POKEY interrupt status
            eor #$03               ; Reverse bits
            lsr A                  ; Logical shift right 
            lsr A                  ; Logical shift right (CF has bit value of timer 2)
            rts                    ; Return
;------------------------------------------------------------------------------
; Decoding termination
;------------------------------------------------------------------------------
STOP_SPORT  lda #$3C               ; A9 3C

;------------------------------------------------------------------------------
; Decoding initialization - Serial port and DMA
;------------------------------------------------------------------------------
;Wait and prepare to disable dma
WAIT_SPORT  tay                    ; Y=A
L09E3       dex                    ; Decrement X
            bne L09E3              ; IF not zero, loop
            iny                    ; Increment Y
            bne L09E3              ; If not zero, loop
            sta PACTL              ; MOTOR ON + Enable Proceed interrupt
            lda #$22               ; DMA Enable value
            bne SET_DMA            ; Go to set DMA enable value
            
PREP_SPORT  lda #$36               ; Motor ON + Enable Proceed Interrupt
            sta PACTL        
            jsr WAIT_SPORT         ; Wait for a while and set DMA
            jsr W_EDGE             ; Wait for edge
            tya                    ; A=Y (Set zero)
;Set DMA            
SET_DMA     sta DMACLT             ; Control DMA
            rts                    ; and return
            
;--------------------------------------------------------------------------------
; Buffer for segment header
;--------------------------------------------------------------------------------
EXPECTED_ID       .BYTE 0          ; Expected ID byte
BLOCK_COLOR       .BYTE 0          ; Block color
SEG_HEADER_BUFFER .BYTE 0,0,0,0    ; Segment header
     
VOLATILE = [*-1] 
;--------------------------------------------------------------------------------
;Error - Purple screen
;--------------------------------------------------------------------------------
LOAD_ERR    lda #88
            sta COLOR4
            sta COLOR3
            sta COLBK
            sta COLPF3
            sta COLDST 
L_ERR_L     jmp L_ERR_L
;--------------------------------------------------------------------------------
;Preinitialization
;We better force WARM start, because the standard Turbo 6000 loader doesn't
;bother to restore normal screen
;--------------------------------------------------------------------------------            
PREINIT     ldx #0            ;Reset coldstart flag
            sta COLDST
            inx               ;Indicate disk boot ok (1)
            stx BOOT
            lda #255          ;Turn BASIC off  
            sta 54017
            sta 1016   
            
            lda #<PREINIT_DINI
            sta DOSINI
            lda #>PREINIT_DINI
            sta DOSINI+1
            
            jsr WARMSV        ;Warm start
             
PREINIT_DINI                  ;Set "DOS" Vector 
            lda #<START
            sta DOSVEC
            lda #>START
            sta DOSVEC+1
            clc
            jmp (DOSVEC)
