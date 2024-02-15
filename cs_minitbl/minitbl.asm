;===============================================================================
;ROTDEV   (Turbo 2000 - kilobyte blocks,Czechoslovakia)
;Turbo 2000 - kilobyte blocks read only CIO handler 
;T: device that supports only READ operation
; Assemble with MADS
;===============================================================================

KBLOCK = 1792                     ;Address of kilobyte block
NAMEBUFFER=1792                   ;File name buffer

          OPT H-,H-          
          ICL "equates.asm"
          
          
          ORG 2816

          ZP_PRINTLO=128
          ZP_PRINTHI=129
          ZP_PRINTLN=130
;===============================================================================
;Turbo block loading
;===============================================================================

LOADBLOCK jsr SETBUFFER           ;Call subroutine that sets up buffer

          lda #255                ;Load kilobyte block
          jsr L0631
          
          bcc FAILED              ;If error occured, go to handle it

          ldy #0                  ;No error, Y=0
          rts

FAILED    ldy #128                ;Error, Y=128
          rts  

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
              adc KBLOCK+1023   
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

TGETBYTE  lda BUFRLO              ;Past end of block ?
          cmp BFENLO              
          bne FROMBUF             ;No, get byte from buffer
          lda BUFRHI              
          cmp BFENHI               
          bne FROMBUF             ;No, get byte from buffer

GBPEB     lda RECVDN              ;Past end of block
          cmp #0                  ;Full block ?
          bne DOEOF               ;No, run binary file

GBNB      jsr LOADBLOCK           ;Past end and not EOF or partial block, load block

          cpy #0                  ;Block loaded OK ?
          bne FB2                 ;No - Y holds error code and we are bailing out

          jsr SETDATABUFFER           

FROMBUF   ldy #0                  ;Reading byte from buffer
          lda (BUFRLO),Y          ;Buffer is stored in accumulator
          inc BUFRLO
          bne FB2
          inc BUFRHI              

FB2       rts          

DOEOF     ldy #136                ;EOF - Y holds EOR 'error code'
          rts 


;===============================================================================
;Block decoding subroutine
;Block is placed from: BUFRLO+256*BUFRHI
;                to:   (BFENLO+256*BFENHI)-1
;First byte of the block should be in
;acumulator register before subroutine
;is called        

;Mapping 4Z - 67
;        6Z - 68

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
            sta 67
            sta LTEMP+1
L065D       ldy #180
L065F       jsr L06D6
            bcc L0650
            cpy #216
            bcc L0652
            inc 67
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
            eor 68
LXXX        sta RECVDN
            jmp L069A
L068E       ldy #0
            lda 68
            sta (BUFRLO),Y
            inc BUFRLO
            bne L069A
            inc BUFRHI
L069A       ldy #200
            php
L069D       lda #1
            sta 68
L06A1       jsr L06D6
            bcc L06C2
            cpy #227
            rol 68
            ldy #198
            bcc L06A1
            lda CHKSUM
            eor 68
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
            sta NMIEN
            sta POKMSK
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
            lsr  
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

;-------------------------------------------------------------------------------
; BEEP
;-------------------------------------------------------------------------------
BEEP               ldx #240
BEEP_LOOP          lda #8
                   sta CONSOL
                   sta WSYNC
                   sta WSYNC
                   lda #0
                   sta CONSOL
                   sta WSYNC
                   sta WSYNC
                   dex
                   bne BEEP_LOOP

SHORT_DELAY        ldy #84
DELAY_LOOP_E       ldx #255            
DELAY_LOOP_I       stx WSYNC
                   dex
                   bne DELAY_LOOP_I
                   dey 
                   bne DELAY_LOOP_E
                   rts
;===============================================================================
;Opening file
;===============================================================================

OPEN      LDA ICAX1Z             ;Determining type of operation
          AND #12
          CMP #4       
          BNE QUITOPEN           ;No READ operation, quit silently   

          jsr BEEP               ;Beep and wait for key

RDNM      lda #<NAMEBUFFER       ;Set buffer for reading name
          sta BUFRLO
          lda #>NAMEBUFFER
          sta BUFRHI
          lda #<(NAMEBUFFER+17)
          sta BFENLO
          lda #>(NAMEBUFFER+17)
          sta BFENHI
          
          lda #0                 ;Decode turbo block

          jsr L0631
          bcc RDNM               ;Reading header failed, try again

          lda #<(NAMEBUFFER+1)
          sta ZP_PRINTLO
          lda #>(NAMEBUFFER+1)
          sta ZP_PRINTHI
          lda #16
          sta ZP_PRINTLN
          jsr PRINT      
          jsr BEEP              ;Autobeep 

FRCRDB    lda #<(KBLOCK+1024)   ;Open succesfull, force GETBYTE
          sta BUFRLO            ;to read block at the beginning
          sta BFENLO
          lda #>(KBLOCK+1024)
          sta BUFRHI
          sta BFENHI

QUITOPEN  ldy #0                ;Indicate OPEN succesfull
          sty RECVDN
          rts

;================================================================
;Closing file
;================================================================
CLOSE     ldy #1
          rts  

FAKEFUN   ldy #0
          rts

;================================================================
;HATABS entry
;================================================================
HTBENT  .WORD OPEN-1              ;OPEN routine
        .WORD CLOSE-1             ;CLOSE routine
        .WORD TGETBYTE-1          ;GETBYTE routine
        .WORD FAKEFUN-1           ;PUTBYTE routine - not implemented
        .WORD $FDCB               ;GETSTATUS routine
        .WORD $FCE4               ;SPECIAL routine
        jmp CLOSE                 ;Jump to the initialization

;===============================================================================
;BINARY LOAD from T: device
;XL/XE only
;===============================================================================

;Minimalistic implementation of BINARY LOAD
          
BLOAD_ST  jsr STARTUP
RESTART   jsr FOPEN               ;Call subroutine that opens T: file
          lda $353                ;Check for error
          bpl BLHEADER            ;No error, continue
          jmp HANDLEERROR         ;If error occured, go to handle it               

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
          
          lda #0                  ;255 255 header found
          sta BLNOBINFLAG         ;Indicate it
          jmp BLHEADER            

BLH1      lda BLNOBINFLAG         ;Check whether 255 255 flag was found
          cmp #1
          bne BLH2                ;If not found ,handle error
          jmp HANDLEERROR
          
            
BLH2      jsr GETBYTE             ;Obtain next two header bytes
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
;Subroutine that gets byte using CIO
;===============================================================================
GETBYTE ldx #16                   ;16 represents offset for channel 1
        lda #7                    ;requesting cio read operation

        sta $0342,X               ;Requesting reading to BLBYTE
        lda #<BLBYTE
        sta $0344,X
        lda #>BLBYTE
        sta $0345,X

        lda #1                    ;Requesting 1 byte
        sta $0348,X
        lda #0
        sta $0349,X
        jsr CIOV                  ;Call CIO

        lda $353                  ;Check for error
        bmi GBERR                 ;Error occured - handle it
        rts

GBERR   tax                       ;Error code to X
        pla                       ;Pull A from stack - dirty return from
        txa                       ;subroutine and put error code back to A

        cmp #136                  ;Is this EOF ?
        beq GBDONE                ;Yes, loading complete
        jmp HANDLEERROR           ;No - handle error
GBDONE  jsr FCLOSE                ;OK - close file
        jmp (736)                 ;RUN loaded binary file
 
;===============================================================================
;Emulation of JSR(738)
;===============================================================================
DOINIT    jmp (738)
FAKEINIT  rts

;===============================================================================
;Main data area
;===============================================================================
BLBYTE      .BYTE 0              ;Byte currently processed
BLSEGHEAD   .BYTE 0,0,0,0        ;Segment header and position pointer
BLFIRSTFLAG .BYTE 1              ;First segment indication
BLNOBINFLAG .BYTE 1

BLTITLE     .BYTE 'T:',155       ;Program title and also file name
BLTITLE_L    EQU *-BLTITLE

;===============================================================================
;Subroutine that closes file
;===============================================================================

FCLOSE    ldx #16         ;16 represent offset for channel 1
          lda #12         ;Requesting CIO CLOSE operation with code 12
          sta $0342,X     
          jsr CIOV        ;Call CIO
          rts             

;===============================================================================
;Subroutine that opens file
;===============================================================================
FOPEN     ldx #16                ;16 represents offset for channel 1
          lda #3                 ;Requesting CIO OPEN operation with code 3
          sta $0342,X     
          lda #4                 ;Auxiliary value 4 - open for reading
          sta $034A,X  
   
          lda #<BLTITLE          ;Setting address of buffer
          sta $0344,X
          lda #>BLTITLE
          sta $0345,X
          jsr CIOV               ;Call CIO

          lda #1
          sta BLFIRSTFLAG        ;First segment of binary file to be loaded
          sta BLNOBINFLAG        ;Not binary file flag - no 255 255 header reached yet
          rts 

VOLATILE    = *-1                 
;===============================================================================
;Error handling
;===============================================================================
HANDLEERROR lda COLOR4         ;Backup color 4
            pha
            
            lda #15            ;White screen
            sta COLBK
            sta COLOR4

            lda #255           ;Wait for any key
            sta CH            
            
ERRORKEY    lda CH
            cmp #33
            bne ERRORKEY
            
            pla                ;Restore color 4
            sta COLOR4
            ldx #255
            txs
            jsr FCLOSE
            jmp BLOAD_ST

;===============================================================================
; Loader startup
;===============================================================================
STARTUP   ldx #0                  ;Reset cold start flag
          stx COLDST
          inx                     ;Indicate disk boot succeded
          stx BOOT
          
          lda #<REINIT            ;Reset - Try to reinitialize
          sta DOSINI
          lda #>REINIT
          sta DOSINI+1
          
          lda #<BLTITLE            ;Show title
          sta ZP_PRINTLO          
          lda #>BLTITLE
          sta ZP_PRINTHI
          lda #<(BLTITLE_L-1)
          sta ZP_PRINTLN
          jsr PRINT
          rts

REINIT    jsr ROTDEVI
          jmp BLOAD_ST

;===============================================================================
; Print - we must print without CIO, because we are in CIO already.
;===============================================================================
PRINT      ldy #0
PRTLOOP    cpy ZP_PRINTLN        ;Continue loop?
           beq PR_DONE           ;No, done printing

           lda (ZP_PRINTLO),y    ;Get next character

           and #(255-128)        ;Mask inverse bit
           cmp #32               ;Compare with 32
           bcc LT32              ;If A<32, then add 64
           cmp #96               ;Compare with 96
           bcs PRTSTA            ;If A>=96, then leave alone              
LE32LT96   sec                   ;Otherwise subtract 32
           sbc #32
           bne PRTSTA            ;And always skip
           beq PRTSTA
LT32       ora #64               ;Add 64
           
PRTSTA     sta (SAVMSC),y        ;Write to the display memory
           iny
           jmp PRTLOOP
PR_DONE    rts 
;===============================================================================
;CIO handler initialization.
;==============================================================================
ROTDEVI ldy  #0                   ;Handler table initialization

NF      lda 794,Y                 ;Empty field found ?
        beq PF                    ;Yes - jump to PF
        iny                       ;Move to next field
        iny
        iny
        cpy  #33                  ;At end of table
        bcc NF                    ;No - continue

PF      lda  #'T'                 ;T: device
        sta 794,Y
        lda  #<HTBENT             ;Links to handler routines
        sta 795,Y
        lda  #>HTBENT
        sta 796,Y
        rts         
