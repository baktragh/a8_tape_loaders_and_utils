;===============================================================================
;NANOTBL (Turbo 2000 - kilobyte blocks, 
;         Czechoslovakia)


;Minimalistic binary loader for
;Turbo 2000 - kilobyte blocks

;If SUBROMVARIANT symbol is defined, turbo blocks are stored to
;memory 'under ROM'
;===============================================================================

.IF UNDER_ROM=0
  LOADER = 2818
  KBLOCK = 1792
  SUBROMVARIANT=0
.ENDIF
.IF UNDER_ROM=1
  LOADER = 1794
  KBLOCK = 49152
  SUBROMVARIANT=1
.ENDIF
.IF UNDER_ROM=2
  LOADER = 1794
  KBLOCK = 63488
  SUBROMVARIANT=1
.ENDIF

NAMEBUFFER=8192                   ;There will be the file name
                                  ;placed

          .INCLUDE "equates.asm"

          *=LOADER

          jsr STARTUP             ;Call subroutine that performs initial setup

LOADSTART jsr READNAME            ;Call subroutine that loads header and displays
                                  ;file name
;===============================================================================
; BINARY LOAD 
;===============================================================================
BLSTART

.IF SUBROMVARIANT=1
          jsr ROMOFF
.ENDIF
          jsr LOADBLOCK           ;At the beginning, read one block
          jsr SETDATABUFFER       ;Set buffer range for user data part of block

.IF SUBROMVARIANT=1
          jsr ROMON
.ENDIF

          jmp BLHEADER            ;Jump to find segment header

;-------------------------------------------------------------------------------
; Segment data processing
;-------------------------------------------------------------------------------


BLDATA    lda #<FAKEINIT          ;Set INIT vector to RTS
          sta 738
          lda #>FAKEINIT
          sta 739


INSEG     jsr GETBYTE             ;Call subroutine that reads one byte
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

          jmp BLHEADER            ;If 255 255 skip these bytes

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
          sta 736                 ;to 736,737
          lda BLSEGHEAD+1
          sta 737
          lda #0
          sta BLFIRSTFLAG


BLHE      jmp BLDATA              ;Done with header, process data


;===============================================================================
;Turbo block loading
;===============================================================================

LOADBLOCK jsr SETBUFFER           ;Call subroutine that sets up buffer

          lda #255                ;Load kilobyte block
          jsr L0631
          bcs LBOK                ;OK. Continue
          jmp FAILED              ;If error occured, go to handle it
LBOK      rts


;===============================================================================
;Buffer setup
;===============================================================================
SETBUFFER lda #<[KBLOCK+1024]
          sta BFENLO
          lda #>[KBLOCK+1024]
          sta BFENHI

SETBGBUF  lda #<KBLOCK
          sta BUFRLO
          lda #>KBLOCK
          sta BUFRHI
          
          rts
;===============================================================================
;Buffer ranges setup for user data
;===============================================================================
SETDATABUFFER jsr SETBUFFER       ;Set as for full block
              
              ldx RECVDN          ;EOF block ?                                 
              cpx #5
              beq SDBEOF          ;Yes, handle EOF block

              cpx #0              ;Full block ?
              bne SDBPB           ;No, branch to handle partial block
              rts                 ;Yes, we are done

SDBPB         stx LTEMP           ;Partial block
              lda #4
              sec
              sbc LTEMP           ;A holds number of pages
              clc
              adc #>KBLOCK
              tax                 ;X holds BFENHI without correction

              clc                 ;Calculating BFENLO
              lda #<KBLOCK
              adc [KBLOCK+1023]   
              sta BFENLO
              bcc SDBPB1          ;Correction of BFENHI
              inx  
SDBPB1        stx BFENHI
              rts   

SDBEOF        lda #>KBLOCK        ;EOF block
              sta BFENHI          ;Buffer ends at its start
              lda #<KBLOCK
              sta BFENLO
              rts



;===============================================================================
;One byte reading
;===============================================================================

GETBYTE
.IF SUBROMVARIANT=1
          jsr ROMOFF
.ENDIF
          lda BUFRLO              ;Past end of block ?
          cmp BFENLO              
          bne FROMBUF             ;No, get byte from buffer
          lda BUFRHI              
          cmp BFENHI               
          bne FROMBUF             ;No, get byte from buffer

GBPEB     lda RECVDN              ;Past end of block
          cmp #0                  ;Full block ?
          bne RUNIT               ;No, run binary file

GBNB      jsr LOADBLOCK           ;Past end and not EOF or partial block, load block
          jsr SETDATABUFFER           

FROMBUF   ldy #0                  ;Reading byte from buffer
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

RUNIT
.IF SUBROMVARIANT=1               ;End of file - run binary file
          jsr ROMON
.ENDIF
          pla                           
          jmp (736)

;===============================================================================
;Main data area
;===============================================================================
BLSTATE     .BYTE 0
BLBYTE      .BYTE 0
BLSEGHEAD   .BYTE 0,0,0,0
BLFIRSTFLAG .BYTE 1

;===============================================================================
;Emulation of JSR(738)
;===============================================================================
DOINIT    jmp (738)
FAKEINIT  rts

;===============================================================================
;Block decoding subroutine
;Block is placed from: BUFRLO+256*BUFRHI
;                to:   (BFENLO+256*BFENHI)-1
;First byte of the block should be in
;acumulator register before subroutine
;is called        
;===============================================================================     
L0631       sta LTEMP
            lda #52
            sta PACTL
            sta PBCTL
            lda #128
            sta POKMSK
            sta IRQEN
            clc
            ldy #0
            sty STATUS
            sty CHKSUM
            sty NMIEN
            sty DMACLT
            php
L0650       bne L06C2
L0652       jsr L06DB
            bcc L0650
            lda #0
            sta ICAX5Z
            sta LTEMP+1
L065D       ldy #180
L065F       jsr L06D6
            bcc L0650
            cpy #216
            bcc L0652
            inc ICAX5Z
            bne L065D
            dec LTEMP+1
L066E       ldy #209
            jsr L06DB
            bcc L0650
            cpy #222
            bcs L066E
            jsr L06DB
            bcc L06C2
            ldy #198
            jmp L069D
L0683       plp
            bne L068E
            lda LTEMP
            eor ICAX6Z
LXXX        sta RECVDN
            jmp L069A
L068E       ldy #0
            lda ICAX6Z
            sta (BUFRLO),Y
            inc BUFRLO
            bne L069A
            inc BUFRHI
L069A       ldy #200
            php
L069D       lda #1
            sta ICAX6Z
L06A1       jsr L06D6
            bcc L06C2
            cpy #227
            rol ICAX6Z
            ldy #198
            bcc L06A1
            lda CHKSUM
            eor ICAX6Z
            sta CHKSUM
            lda BUFRLO
            cmp BFENLO
            lda BUFRHI
            sbc BFENHI
            bcc L0683
            lda #0
            cmp CHKSUM
L06C2       pla

L06C3       lda #192
            sta POKMSK

.IF SUBROMVARIANT=0
            sta NMIEN
.ENDIF
            sta IRQEN
            lda #60
            sta PACTL
            sta PBCTL
            rts

L06D6       jsr L06DB
            bcc L06FF
L06DB       ldx #4
L06DD       dex
            bne L06DD
            lda STATUS
            lsr A
BARCOLOR    ora #$10         ;Color of the color bar            
            and LTEMP+1
            sta COLBK
L06E8       iny
            beq L06FE
            lda BRKKEY
            beq L06FC
            lda SKSTAT
            and #16
            cmp STATUS
            beq L06E8
            sta STATUS
            sec
            rts
L06FC       dec BRKKEY
L06FE       clc
L06FF       rts

;===============================================================================
;Switching ROM on and off
;===============================================================================

.IF SUBROMVARIANT=1

NMISTORE   .BYTE 0
PORTBSTORE .BYTE 0

ROMOFF    sei
          lda #0
          sta NMIEN
          lda PORTB
          sta PORTBSTORE
          and #254
          sta PORTB
          rts

ROMON     lda PORTBSTORE
          sta PORTB
          lda #192
          sta NMIEN
          cli
          rts
.ENDIF

; This code can be overwritten by loaded binary file
VOLATILE    = [*-1] 
;===============================================================================
; Error handling
;===============================================================================
FAILED    pla                     ;Error occured
          pla

.IF SUBROMVARIANT=1
          jsr ROMON
.ENDIF
          lda COLOR4              ;Backup color 4
          pha
          lda #88                 ;Color indication
          sta COLOR4
          sta COLBK
          lda #255                ;Tape beep + wait for key
          jsr AUTOBEEP
          pla
          sta COLOR4              ;Restore color 4
          
          jmp LOADSTART           ;Try again

;============================================================================
;Tape beep and possible automatic keypress, depends on A
;============================================================================
AUTOBEEP  sta CH
          lda #1
          jsr 65020
          rts
;=============================================================================
;Reading file name
;=============================================================================
READNAME  lda #<NAMEBUFFER
          sta BUFRLO
          lda #>NAMEBUFFER
          sta BUFRHI
          lda #<[NAMEBUFFER+17]
          sta BFENLO
          lda #>[NAMEBUFFER+17]
          sta BFENHI

          lda #1
          sta BLFIRSTFLAG

.IF SUBROMVARIANT=1
          jsr ROMOFF
.ENDIF
          lda #0
          jsr L0631

.IF SUBROMVARIANT=1
          jsr ROMON
.ENDIF

          bcc READNAME            ;Reading header failed, try again

          lda #155                ;Print file name
          sta [NAMEBUFFER+17]
          lda #125
          sta NAMEBUFFER
          ldx #<[NAMEBUFFER]
          ldy #>[NAMEBUFFER]
          jsr LC642

          lda #0                  ;Wait for some time
          sta 20
RDNMWL    lda 20
          cmp #75
          bne RDNMWL
          rts

;================================================================
;Loader initialization
;================================================================
 STARTUP  lda #<LOADSTART         ;Become DOS
          sta DOSINI
          lda #>LOADSTART
          sta DOSINI+1
          ldx #0                  ;Reset cold start flag
          stx COLDST
          inx                     ;Indicate disk boot succeded
          stx BOOT

          ldx #<TITLE             ;Show program title
          ldy #>TITLE
          jsr LC642

          lda #1
          jsr AUTOBEEP
          rts

;Program title
.IF UNDER_ROM=0
TITLE     .BYTE 125,"NanoTBL",155
.ENDIF
.IF UNDER_ROM=1
TITLE     .BYTE 125,"NanoTBL[UR]",155
.ENDIF
.IF UNDER_ROM=2
TITLE     .BYTE 125,"NanoTBL[U2]",155
.ENDIF

