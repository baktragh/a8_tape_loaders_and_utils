;===============================================================================
;NANOBTAPE (B-TAPE, Czech republic)
;          (TURBO TAPE. Czechoslovakia)
;Minimalistic binary loader for B-TAPE and TURBO TAPE
;Assemble with MADS

;Support is limited to tape modes SS and LS

;If I/O error occurs, the screen goes white.
;Block sequential numbers are checked. If number is lower than
;expected, the screen goes green. If number is higher than
;expected, the screen goes red.

;File name and random number are not checked
;At least 600 pilot tone pulses are required to detect speed

;Caution: Measuring loop must be whole in one memory page,
;labels L0692 - L06B4

;If UNDER_ROM symbol is defined to 1 or 2, turbo blocks are stored to
;memory 'under ROM'. This works with XL/XE models only
;=============================================================================== 
        OPT H-,F-

.IF UNDER_ROM=0
        LOADER = 2820
        KBLOCK = 1792
        SUBROMVARIANT = 0
.ENDIF
.IF UNDER_ROM=1
        LOADER = 1796
        KBLOCK = 49152            
        SUBROMVARIANT = 1
.ENDIF
.IF UNDER_ROM=2
        LOADER = 1796
        KBLOCK = 63488
        SUBROMVARIANT = 1
.ENDIF

        NAMEPRINT=8191
        NAMEBUFFER=8192

        ZP_PRINTLO=128
        ZP_PRINTHI=129
        ZP_PRINTLN=130
        
          ICL "equates.asm"

          ORG LOADER

          jsr STARTUP             ;Call subroutine that performs initial setup

LOADSTART jsr READNAME            ;Call subroutine that loads first block and
                                  ;displays file name
; ==============================================================================
; BINARY LOAD 
; ==============================================================================
BLSTART
.IF SUBROMVARIANT=1
          jsr ROMOFF
.ENDIF
          jsr LOADBLOCK           ;At the beginning, read one block
          inc BLEXPECT            ;Increase expected block sequential number
          jsr SETDATABUFFER       ;Call subroutine that sets buffer range 
                                  ;according to length of user data in block

.IF SUBROMVARIANT=1
          jsr ROMON
.ENDIF

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
          cmp #1
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

LOADBLOCK lda #52                 ;Switch on tape motor
          sta PACTL
          sta PBCTL

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

LBBL      lda #253                ;Switch tape motor off
          sta PACTL
          sta PBCTL

          bcs LBOK                ;If no erros, continue
          jmp FAILED              ;If error occured, go to handle it
LBOK      rts


;===============================================================================
;Buffer setup
;===============================================================================
SETBUFFER lda #<KBLOCK
          sta BUFRLO
          lda #>KBLOCK
          sta BUFRHI
          lda #<(KBLOCK+1024)
          sta BFENLO
          lda #>(KBLOCK+1024)
          sta BFENHI
          rts

;===============================================================================
;Buffer ranges setup for user data
;===============================================================================
SETDATABUFFER lda #<(KBLOCK+16) ;Start of the buffer
              sta BUFRLO
              lda #>(KBLOCK+16)
              sta BUFRHI

              clc               ;End of buffer calculation
              lda #<KBLOCK
              adc KBLOCK+1
              tax 
              lda KBLOCK+2
              and #7
              adc #>KBLOCK
              sta BFENHI
              stx BFENLO
              rts

;===============================================================================
;One byte reading
;===============================================================================
GETBYTE
.IF SUBROMVARIANT=1
         jsr ROMOFF
.ENDIF
         lda BUFRLO            ;Past end of block ?
         cmp BFENLO
         bne FROMBUF
         lda BUFRHI
         cmp BFENHI
         bne FROMBUF           ;Not past end, take byte from buffer
         lda KBLOCK+2          ;Last block (EOF) ?
         and #128
         beq GBNB

.IF SUBROMVARIANT=1            ;If past end and EOF, run loaded binary file
        jsr ROMON
.ENDIF
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
.IF SUBROMVARIANT=1
       jsr ROMON
.ENDIF
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


L05FC      lda #64

.IF SUBROMVARIANT=0
            sta NMIEN
.ENDIF
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
            lsr 
            pha
            eor #255
            sta BUFRFL
            pla
            lsr 
            pha
            eor #255
            sta RECVDN
            pla
            lsr 
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

L0697       lda STATUS
            lsr
            ora #176
            nop 
            and LTEMP+1
            sta COLBK

L06A4       iny
            beq L06B4
            lda SKSTAT
            and #16
            cmp STATUS
            beq L06A4
            sta STATUS
            sec
            rts
L06B4       clc
            rts

;===============================================================================
;Switching ROM on and off
;===============================================================================

.IF SUBROMVARIANT=1
NMISTORE   .BYTE 0
PORTBSTORE .BYTE 0

ROMOFF    sei
          lda NMIEN
          sta NMISTORE
          lda #0
          sta NMIEN
          lda PORTB
          sta PORTBSTORE
          and #254
          sta PORTB
          rts

ROMON     lda PORTBSTORE
          sta PORTB
          lda NMISTORE
          sta NMIEN
          cli
          rts
.ENDIF


; This code can be overwritten by loaded binary file
VOLATILE    = *-1 
;===============================================================================
; Handle errors
;===============================================================================
FAILED    ldx #15                 ;White color

          lda BUFRLO              ;Check buffer pointer. If any bytes decoded
                                  ;Then it is I/O error
          cmp #<KBLOCK
          bne F1
          lda BUFRHI
          cmp #>KBLOCK
          bne F1
          
          lda NOCKSM              ;Incorrect block. Green or Red.
          cmp BLEXPECT            
          beq F1                  
          ldx #176                  
          bcc F1
          ldx #36
F1
.IF SUBROMVARIANT=1
         jsr ROMON
.ENDIF
         lda COLOR4               ;Save original COLOR4
         pha
         lda COLOR2
         pha
         stx COLOR2
         stx COLOR4

         lda #255                 ;Wait for SPACE
         sta CH
F_KB     lda CH
         cmp #33
         bne F_KB

         pla                      ;Restore original color 4
         sta COLOR2
         pla
         sta COLOR4
                
.IF SUBROMVARIANT=1
         jsr ROMOFF
.ENDIF
         jmp LOADBLOCK            ;Next try to load block
;===============================================================================
;Read the first block and display file name
;===============================================================================

;Read file name
READNAME    lda #1
            sta BLEXPECT        ;Expected first block
            sta BLFIRSTFLAG     ;First segment flag set to 1

.IF SUBROMVARIANT=1
           jsr ROMOFF
.ENDIF
           jsr LOADBLOCK        ;Call subroutine that reads block

           ldx #12              ;Copy to RAM that is not under ROM
NMCP       lda KBLOCK+4,x
           sta NAMEBUFFER-1,x
           dex
           bne NMCP

.IF SUBROMVARIANT=1
          jsr ROMON
.ENDIF

;Print file name

          lda #125             ;Prepend CLS
          sta NAMEPRINT
          lda #155             ;Append EOL
          sta NAMEBUFFER+12
          
          lda #<NAMEPRINT
          sta ZP_PRINTLO
          lda #>NAMEPRINT
          sta ZP_PRINTHI
          lda #14
          sta ZP_PRINTLN
          
          jsr PRINT

          jsr SHORT_DELAY
          rts
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

          lda #<TITLE          ;Show program title
          sta ZP_PRINTLO
          lda #>TITLE
          sta ZP_PRINTHI
          lda #<TITLE_L
          sta ZP_PRINTLN
          jsr PRINT

          jsr SHORT_DELAY
          rts
;==============================================================================
; Print routine (using IOCB #0)
;==============================================================================
;CIO channel 0          
          CIO0_OP   =$0342
          CIO0_STAT =$0343
          CIO0_BUFLO=$0344
          CIO0_BUFHI=$0345
          CIO0_LENLO=$0348
          CIO0_LENHI=$0349
          CIO0_AUX1 =$034A
          CIO0_AUX2 =$034B

PRINT     lda #9                  ;Requesting PRINT
          sta CIO0_OP
          lda ZP_PRINTLO
          sta CIO0_BUFLO
          lda ZP_PRINTHI
          sta CIO0_BUFHI
          lda ZP_PRINTLN
          sta CIO0_LENLO
          ldx #0                  ;Channel 0
          stx CIO0_LENHI
          jsr CIOV                ;Call CIO
          rts
;==============================================================================
; Short delay routine
;==============================================================================
SHORT_DELAY
          pha
          lda #0               ;Wait for some time
          sta 20
RDNMWL    lda 20
          cmp #75
          bne RDNMWL
          pla
          rts

;Program title
.IF UNDER_ROM=0
TITLE    .BYTE 125,'NanoBTAPE',155
TITLE_L  = *-TITLE
.ENDIF

.IF UNDER_ROM=1
TITLE    .BYTE 125,'NanoBTAPE[UR]',155
TITLE_L  = *-TITLE
.ENDIF

.IF UNDER_ROM=2
TITLE    .BYTE 125,'NanoBTAPE[U2]',155
TITLE_L  = *-TITLE
.ENDIF
