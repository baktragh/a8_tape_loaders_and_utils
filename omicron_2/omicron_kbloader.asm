;===============================================================================
; Omicron Turbo - Kilobyte Blocks Loader
;=============================================================================== 
        .INCLUDE "system_equates.asm"

        LOADER = 2820
        KBLOCK = 1792

          *=LOADER

          jsr STARTUP             ;Call subroutine that performs initial setup
          jsr SWITCH_SIG          ;Switch signal source


; ==============================================================================
; BINARY LOAD 
; ==============================================================================
LOADSTART lda #1                  ;Expected block 1
          sta BLEXPECT
          sta BLFIRSTFLAG         ;First block
 
          jsr LOADBLOCK           ;At the beginning, read one block
          inc BLEXPECT
          jsr SETDATABUFFER       ;Call subroutine that sets buffer range 
                                  ;according to length of user data in block

          jmp BLHEADER            ;Go to segment header processing


;-------------------------------------------------------------------------------
; Segment data processing
;-------------------------------------------------------------------------------

BLDATA    lda #<FAKEINIT          ;Set INIT vector to RTS
          sta 738
          lda #>FAKEINIT
          sta 739

INSEG     jsr GETBYTE             ;Call subroutine that reads 1 byte
          lda BLSEGHEAD
          sta LTEMP
          lda BLSEGHEAD+1
          sta LTEMP+1
          lda BLBYTE
          ldy #0
          sta (LTEMP),Y           ;Store read byte to memory

CHCKSGE   lda BLSEGHEAD           ;Check for end of segment
          cmp BLSEGHEAD+2
          bne SEGINCR
          lda BLSEGHEAD+1
          cmp BLSEGHEAD+3
          bne SEGINCR

          jsr DOINIT              ;End of segment
          jmp BLHEADER

SEGINCR   inc BLSEGHEAD           ;Update current position in segment
          bne IS2
          inc BLSEGHEAD+1

IS2       jmp INSEG               ;Continue segment data processing

;-------------------------------------------------------------------------------
; Segment header processing
;-------------------------------------------------------------------------------

BLHEADER  jsr GETBYTE             ;Obtain first two header bytes
          lda BLBYTE
          sta BLSEGHEAD
          jsr GETBYTE
          lda BLBYTE
          sta BLSEGHEAD+1

          cmp #255                ;Check for 255 255
          bne BLH1
          lda BLSEGHEAD
          cmp #255
          bne BLH1

          jmp BLHEADER            ;If 255,255 skip these bytes and go back

BLH1      jsr GETBYTE             ;Obtain next two header bytes
          lda BLBYTE
          sta BLSEGHEAD+2
          jsr GETBYTE
          lda BLBYTE
          sta BLSEGHEAD+3

          lda BLFIRSTFLAG         ;Check whether this is first segment
          bne BLHE                ;If not, continue

          lda BLSEGHEAD           ;Address of first segment is stored
          sta 736                 ;to 736.737
          lda BLSEGHEAD+1
          sta 737
          lda #0
          sta BLFIRSTFLAG

BLHE      jmp BLDATA              ;Done with header, process data

;===============================================================================
;Turbo block loading
;===============================================================================

LOADBLOCK 
;--------------------------------------------------------------------------------
; Set-up turbo mode
;--------------------------------------------------------------------------------
          lda #52       ;Group A - COMMAND active
          sta PBCTL
            
          lda #$83      ;Group B - DATA-OUT forced to zero
          sta SKCTL     
            
          lda #$38      ;Group C - Input/Output from joystick port
          sta PACTL     ;Direction control mode
          lda #$60      ;Set direction contol bits
          sta PORTA
          lda #$34      ;PORTA addressing mode
          sta PACTL     ;Motor on
;--------------------------------------------------------------------------------
; Decode block
;--------------------------------------------------------------------------------                      
          jsr SETBUFFER           ;Call subroutine that sets up buffer
          jsr L0634               ;Reset turbo decoder
          jsr L0645               ;Detect speed
          jsr L05FC               ;Enable interrupts

          lda BUFRFL              ;Determine Turbo 2000 or Super Turbo
          cmp #180                
          bcc LBT2K

LBST      lda BLEXPECT            ;Call Super Turbo block decoding subroutine
          jsr L059C
          jmp LBBL

LBT2K     lda BLEXPECT            ;Call Turbo 2000 block decoding subroutine
          jsr L0605

LBBL 
;--------------------------------------------------------------------------------
; Reset turbo mode
;--------------------------------------------------------------------------------                                            
          lda #60      ;Group A - COMMAND Inactive
          sta PBCTL
            
          lda #$03     ;Group B - Do not force DATA-OUT to 0
          sta SKCTL    
            
          lda #$38     ;Group C - Reset joystick port         
          sta PACTL              
          lda #$00               
          sta PORTA              
            
          lda #$3C     ;Motor off
          sta PACTL              
;--------------------------------------------------------------------------------
; Evaluate result
;--------------------------------------------------------------------------------                                            
          bcs LBOK               ;If no erros, continue
          jmp FAILED             ;If error occured, go to handle it
LBOK      rts


;===============================================================================
;Buffer setup
;===============================================================================
SETBUFFER lda #<KBLOCK
          sta BUFRLO
          lda #>KBLOCK
          sta BUFRHI
          lda #<[KBLOCK+1024]
          sta BFENLO
          lda #>[KBLOCK+1024]
          sta BFENHI
          rts

;===============================================================================
;Buffer ranges setup for user data
;===============================================================================
SETDATABUFFER lda #<[KBLOCK+2] ;Start of the buffer
              sta BUFRLO
              lda #>[KBLOCK+2]
              sta BUFRHI

              clc               ;End of buffer calculation
              lda #<[KBLOCK+2]
              adc [KBLOCK+0]
              tax 
              lda [KBLOCK+1]
              and #7
              adc #>[KBLOCK+2]
              sta BFENHI
              stx BFENLO
              rts

;===============================================================================
;One byte reading
;===============================================================================
GETBYTE  lda BUFRLO            ;Past end of block ?
         cmp BFENLO
         bne FROMBUF
         lda BUFRHI
         cmp BFENHI
         bne FROMBUF           ;Not past end, take byte from buffer
         lda [KBLOCK+1]        ;Last block (EOF) ?
         and #128
         beq GBNB

        pla
        pla
RUN     jmp (736)

GBNB    jsr LOADBLOCK          ;Before taking byte from buffer, one block
        inc BLEXPECT           ;must be loaded
        jsr SETDATABUFFER

FROMBUF ldy #0                 ;Reading byte from buffer
        lda (BUFRLO),Y
        sta BLBYTE
        inc BUFRLO
        bne FB2
        inc BUFRHI

FB2

       rts

;===============================================================================
;Main data area
;===============================================================================
BLBYTE      .BYTE 0            ;Byte read from buffer
BLSEGHEAD   .BYTE 0,0,0,0      ;Segment header, first two indicate position in segment
BLEXPECT    .BYTE 0            ;Expected block number
BLFIRSTFLAG .BYTE 1            ;First block read

;===============================================================================
;Emulation of JSR(738)
;===============================================================================
DOINIT      jmp (738)          ;Emulation of JSR(738)
FAKEINIT    rts


;===============================================================================
;Block decoding subroutine
;Block is placed from: BUFRLO+256*BUFRHI
;                to:   (BFENLO+256*BFENHI)-1
;First byte of the block should be in
;acumulator register before subroutine
;is called        
;===============================================================================            
L059C       sta LTEMP
            jsr L0634
            php
L05A2       jsr L0692
L05A5       ldy RECVDN
            jsr L0697
            bcc L05A2
            cpy XMTDON
            bcs L05A5
            jsr L0697
            bcc L05FB
            ldy BUFRFL
            jmp L05D6

L05BA       plp
            bne L05C5
            lda LTEMP
            eor NOCKSM
            bne L05FC
            beq L05D1
L05C5       ldy #0
            lda NOCKSM
            sta (BUFRLO),Y
            inc BUFRLO
            bne L05D1
            inc BUFRHI
L05D1       ldy BUFRFL
            iny
            iny
            php

L05D6       lda #1
            sta NOCKSM
L05DA       jsr L0692
            bcc L05FB
            cpy RECVDN
            rol NOCKSM
            ldy BUFRFL
            bcc L05DA
            lda CHKSUM
            eor NOCKSM
            sta CHKSUM
            lda BUFRLO
            cmp BFENLO
            lda BUFRHI
            sbc BFENHI
            bcc L05BA
            lda #0
            cmp CHKSUM
L05FB       pla


L05FC       lda #64
            sta NMIEN
            sta IRQEN
            rts

L0605       sta LTEMP
            jsr L0634
            php
            lda #176
            sta BUFRFL
            lda #214
            sta RECVDN
            jsr L0692
            bcc L05FB
            ldy #0
            jsr L0692
            bcc L05FB
L061F       ldy #200
            jsr L0697
            bcc L05FB
            cpy #215
            bcs L061F
            jsr L0697
            bcc L05FB
            ldy #176
            jmp L05D6

;Decoder reset
L0634       ldy #0
            sty STATUS
            sty CHKSUM
            sty NMIEN
            sty DMACLT
            sty IRQEN
            clc
            rts

;Speed detection
L0645       jsr L0692
            bcc L0645
            lda #2
            sta LTEMP+1
L064E       ldy #0
            sty FREQ
            ldx #3
L0654       jsr L0692
            bcc L0645
            dex
            bne L0654
            tya
            lsr A
            pha
            eor #255
            sta BUFRFL
            pla
            lsr A
            pha
            eor #255
            sta RECVDN
            pla
            lsr A
            eor #255
            sta XMTDON
            ldy #0
            jsr L0697
            bcc L0645
L0677       ldy BUFRFL
            jsr L0692
            bcc L0645
            cpy RECVDN
            bcc L0645
            inc FREQ
            bne L0677
            dec LTEMP+1
            bpl L064E
            ldy #0
            jsr L0697
            bcc L0645
            rts

L0692       jsr L0697
            bcc L06B4

L0697       lda STATUS    ; 4 cycles   (128/16 or 0)
            lsr A         ; 2 cycles   (64/8 or 0)
L_BKG       ora #$00      ; 2 cycles   (Zapped by signal switch)
            nop           ; 2 cycles   (Filler)
            and LTEMP+1
            sta COLBK

L06A4       iny
            beq L06B4
L_DET       lda SKSTAT
L_AND       and #16
            cmp STATUS
            beq L06A4
            sta STATUS
            sec
            rts
L06B4       clc
            rts


; This code can be overwritten by loaded binary file
VOLATILE    = [*-1] 
;===============================================================================
; Handle errors
;===============================================================================
FAILED      lda #1              ;Cold start after RESET
            sta COLDST     
            sta DMACLT
            sta SDMCTL          ;No image
            lda #88             ;Red/Purple screen
            sta COLBK
            sta COLOR4
ERRLOOP     jmp ERRLOOP         ;Wait for RESET   
;===============================================================================
;Loader initialization
;===============================================================================

STARTUP   lda #<LOADSTART      ;Become DOS
          sta DOSINI
          lda #>LOADSTART
          sta DOSINI+1
          ldx #0               ;Reset cold start flag
          stx COLDST
          inx                  ;Indicate disk boot succeded (1)
          stx BOOT
          rts
          
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
            lda #$24           
            sta L_BKG+1        ;Set primary color            
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
