;===============================================================================
;SuperChain 2 (Super Turbo, Czechoslovakia)
;===============================================================================

;Simple binary loader
;For each segment, there is a pair of Super Turbo blocks.
;
;Segment header - holds pair of addresses. ID byte is 125.
;Segment data   - holds segment data. ID byte is 126.
;
;File is terminated with a termination segment header that
;has addresses of 255,255,255,255.
;
;Loader tolerates first segment to be a RUN segment. If there
;is no RUN segment, address of the first DATA segment is
;used to set the RUNAD vector.

.INCLUDE equates.asm


            *=1800

            jsr BOOTSETUP    ;Initialize the loader
            
RESTART     lda #1
            sta FIRSTFLAG
            
;-------------------------------------------------------------------------------
; Get segment header
;-------------------------------------------------------------------------------
GETHEADER   jsr SETHDBUF
            lda #125         ;ID 125
            jsr GETBLOCK
            bcs PROCHEADER
            jmp HANDLEERROR
            
;-------------------------------------------------------------------------------
; Process segment header
;-------------------------------------------------------------------------------
PROCHEADER  ldx #4                ;Check for EOF
PH_EOFLOOP  lda [HEADBUF-1],X
            cmp #255
            bne PH_NOTEOF
            dex
            bne PH_EOFLOOP
            
            jmp (RUNAD)           ;Run program
            
            
PH_NOTEOF   lda #<FAKEINIT        ;Prepare fake init
            sta INITAD
            lda #>FAKEINIT
            sta INITAD+1
            
            lda FIRSTFLAG         ;Handle 1st segment
            beq LOADSEGDATA
            lda HEADBUF
            sta RUNAD
            lda HEADBUF+1
            sta RUNAD+1
            dec FIRSTFLAG

;-------------------------------------------------------------------------------
; Load and process segment data
;-------------------------------------------------------------------------------
LOADSEGDATA jsr SETDATABUF
            lda #126              ;ID 126
            jsr GETBLOCK
            bcs PROCSEG
            jmp HANDLEERROR
            
PROCSEG     jsr PROCINIT
            jmp GETHEADER

PROCINIT    jmp (INITAD)
            
            
;-------------------------------------------------------------------------------
; Set buffer for segment header
;-------------------------------------------------------------------------------
SETHDBUF    lda #<HEADBUF
            sta BUFRLO
            lda #>HEADBUF
            sta BUFRHI
            lda #<[HEADBUF+HEADLEN]
            sta BFENLO
            lda #>[HEADBUF+HEADLEN]
            sta BFENHI
            rts
;-------------------------------------------------------------------------------
; Set buffer for segment data
;-------------------------------------------------------------------------------
SETDATABUF  lda HEADBUF
            sta BUFRLO
            lda HEADBUF+1
            sta BUFRHI
            
            inc HEADBUF+2     ;Increase LO by one
            bne SDB_10        ;No 255-->0, skip
            inc HEADBUF+3     ;Increase HI by one
            
SDB_10      lda HEADBUF+2     
            sta BFENLO
            lda HEADBUF+3
            sta BFENHI
            
FAKEINIT    rts
;===============================================================================
;Block decoding subroutine
;Block is placed from: BUFRLO+256*BUFRHI
;                to:   (BFENLO+256*BFENHI)-1
;First byte of the block should be in
;acumulator register before subroutine
;is called        
;===============================================================================

;Prepare for decoding
GETBLOCK    sta LTEMP           ;Store ID byte
            jsr GB_PREP         ;Initialize for decoding
            php                 ;Store CPU status
            jsr DETECT_SPEED    ;Detect speed

;Wait for SYNC pulse            
GB_WSYNC    jsr GP_MEASURE      ;Measure width of pulse
GB_WSYNC_L  ldy RECVDN          ;Count from MED
            jsr GP_WEDGE        ;Wait for edge
            bcc GB_WSYNC        ;If Y wrapped, repeat
            cpy XMTDON
            bcs GB_WSYNC_L      ;If Y>HIGH repeat
            jsr GP_WEDGE        ;Wait for edge
            bcc GB_END          ;If Y wrapped, end block decoding
            ldy BUFRFL
            jmp GB_GETBYTE      ;SYNC pulse found, get bytes

;Byte successfuly decoded, update check sum, advance in 
;the buffer
GB_BYTEDONE plp                 ;Store data to memory
            bne GB_STORBYTE
            lda LTEMP
            eor NOCKSM
            bne GB_EXIT
            beq GB_NBUFF
GB_STORBYTE ldy #0
            lda NOCKSM
            sta (BUFRLO),Y
            inc BUFRLO
            bne GB_NBUFF
            inc BUFRHI
GB_NBUFF    ldy BUFRFL
            iny
            iny
            php

; Get single byte            
GB_GETBYTE  lda #1
            sta NOCKSM
GB_GETBIT   jsr GP_MEASURE     ;Measure width of pulse
            bcc GB_END         ;If pulse is too long, end block decoding
            cpy RECVDN         ;Compate to MED
            rol NOCKSM         ;Rotate NOCKSM with carry, width >MED "1", width <MED "0"
            ldy BUFRFL
            bcc GB_GETBIT      ;If bit 7 was 0, repeat
            lda CHKSUM         ;Update checksum
            eor NOCKSM
            sta CHKSUM
;Check if whole block is decoded
            lda BUFRLO         ;Check buffer range
            cmp BFENLO
            lda BUFRHI
            sbc BFENHI
            bcc GB_BYTEDONE    ;Continue with next bytes
            lda #0
            cmp CHKSUM
GB_END      pla


;Terminate decoding
GB_EXIT     jsr GB_TERM
            rts

;-------------------------------------------------------------------------------
;Transfer speed detection
;1. Width of three consequent pilot tone pulses is measured
;2. The width is used to calculate tolerated pulse widths
;3. At least 256 pilot tone pulses must follow
;4. Measurement is repeated 4 times
;-------------------------------------------------------------------------------
DETECT_SPEED  jsr GP_MEASURE           ;Measure width of pulse
              bcc DETECT_SPEED         ;If no CF, repeat
              lda #3
              sta LTEMP+1

DS_NEWROUND   ldy #0
              sty FREQ            

              ldx #3
DS_GETTHREE   jsr GP_MEASURE           ;Measure width of 3 pulses 
              bcc DETECT_SPEED         ;No pulse, start from scratch
              dex                      ;Do we have 3 pulses?
              bne DS_GETTHREE          ;No, get another
              
              tya                      ;A now holds number of measuring loop passes
              lsr A                    ;A=A/2
              pha
              eor #255                 ;A = 255-A
              sta BUFRFL               ;LO (narrowest tolerated pulse)
              pla
              lsr A
              pha
              eor #255
              sta RECVDN               ;MED (0 and 1 determinator)
              pla
              lsr A
              eor #255
              sta XMTDON               ;HI (widest tolerated pulse)
              ldy #0
              jsr GP_WEDGE             ;Wait for edge
              bcc DETECT_SPEED         ;If no
              
DS_PILOTREP   ldy BUFRFL               ;Star from LO
              jsr GP_MEASURE           ;Measure width of pulse
              bcc DETECT_SPEED
              cpy RECVDN               ;Compare with MED
              bcc DETECT_SPEED         ;If Y<MED, measure speed again
              inc FREQ
              bne DS_PILOTREP          ;256 pulses must have appropriate width
              dec LTEMP+1
              bpl DS_NEWROUND          ;Start new round, if less than 4 rounds

              ldy #0
              jsr GP_WEDGE             ;Wait for edge
              bcc DETECT_SPEED         ;No pulse, start from scratch
              rts
;-------------------------------------------------------------------------------
;Measure width of pulse
;-------------------------------------------------------------------------------
GP_MEASURE  jsr GP_WEDGE        ;Call wait for edge
            bcc GP_NOPULSE      ;No pulse, return

;-------------------------------------------------------------------------------
;Wait for edge + colour effect
;-------------------------------------------------------------------------------
GP_WEDGE    lda STATUS
            lsr
            ora #$80
            nop 
            and LTEMP+1
            sta COLBK

GP_MLOOP    iny                 ;Measuring loop
            beq GP_NOPULSE
            lda SKSTAT
            and #16
            cmp STATUS
            beq GP_MLOOP
            sta STATUS
            sec
            rts
GP_NOPULSE  clc
            rts
;-------------------------------------------------------------------------------
; Prepare for block decoding
;-------------------------------------------------------------------------------
GB_PREP     ldy #52
            sty PACTL
            sty PBCTL
            ldy #0
            sty STATUS
            sty CHKSUM
            sty NMIEN
            sty DMACLT
            sty IRQEN
            clc
            rts
;-------------------------------------------------------------------------------
;Terminate block decoding
;-------------------------------------------------------------------------------            
GB_TERM     ldy #192
            sty NMIEN
            sty POKMSK
            sty IRQEN
            ldy #60
            sty PACTL
            sty PBCTL
            rts
;-------------------------------------------------------------------------------
;Data area
;-------------------------------------------------------------------------------
FIRSTFLAG   .BYTE 1
HEADBUF     .BYTE 0,0,0,0
            HEADLEN=4

VOLATILE    = [*-1]            

;-------------------------------------------------------------------------------
; Load error
;-------------------------------------------------------------------------------
HANDLEERROR lda #1            ;Cold start after RESET
            sta COLDST     
            lda #15           ;White screen
            sta COLBK
            sta COLOR4
            sta COLOR2
ERRLOOP     jmp ERRLOOP       ;Wait for RESET     
;-------------------------------------------------------------------------------
;Initialize loader
;-------------------------------------------------------------------------------
BOOTSETUP   ldx #0            ;Reset cold start flag
            stx COLDST
            inx               ;Indicate disk boot is OK (1)
            stx BOOT
            rts
