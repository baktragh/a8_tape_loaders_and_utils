;=======================================================================
; ChainCopy
;
; This copier loads type 04 Turbo 2000 file
; (binary turbo) and creates a chain of Turbo
; 2000 block pairs for each segment of the
; binary file. The ChainLoader 2 binary loader is prepended
; before the block pairs.
; 
; Other file types are copied as they are
;
; This copier resides solely under ROM, so the
; input file can be reasonably long.
;
; Copier written by Michael Kalouš (BAKTRA Software) as an
; auxiliary utility for TURGEN SYSTEM.
;
; http://turgen.sourceforge.net/
;
; This software is a public domain
;
; MAINTENANCE LOG:
; 2014-04-24 Initial version 1.0
; 2014-04-28 Version 1.1. Fixed bug in the Chainloader 2 that prevented
;            loading of certain binary files. 
; 2016-10-19 Version 1.2. Support copying of other file types         ]
; 2018-01-20 Version 1.3. Fixed crashing on real hardware. Updated with
;            new version of ChainLoader and new file format.                
;
;=======================================================================
           
.INCLUDE equates.asm 
            
;
; Start of code
;
            *= 58368        ;Page 228
            
            MAXLEN=52479    ;Maximum input file length
            MAINBUF=768     ;Buffer
            
            DTB_PTR_LO=140  ;Position in the binary file
            DTB_PTR_HI=141
            DTB_PTR=DTB_PTR_LO
            
;=======================================================================
; MAINLINE CODE
;=======================================================================
;-----------------------------------------------------------------------
;Initialization
;-----------------------------------------------------------------------
START              ldx #255             ;Clear STACK
                   txs  
                   lda #<BREAK_HANDLER  ;Setup BREAK key vector
                   sta 566
                   lda #>BREAK_HANDLER
                   sta 567

                   jsr WAIT_FOR_VBLANK
                               
                   lda #<DLIST          ;Setup display list
                   sta DLISTL
                   lda #>DLIST
                   sta DLISTH
                    
                   jsr WAIT_FOR_VBLANK                                       
                   lda #34              ;Enable DMA
                   sta DMACLT
                   
;-----------------------------------------------------------------------
;Ready to read
;-----------------------------------------------------------------------
READ_INIT          lda #180
                   sta BG_COLOR
                   jsr RESTORE_COLORS   ;Set colors
                   jsr CLEAR_FNAME_AREA
                   jsr DISP_READ_MSG    ;Display READ ready msg
                   jsr DISP_ABORT_KEYS  ;Display abort keys
                   jsr WAIT_FOR_START
                   jsr BEEP

READ_HEADER        jsr CLEAR_ABORT_K_AREA
                   jsr SET_HEADER_RD_BUF   ;Set buffer for header
                   lda #0               ;Expect header
                   jsr READ_T2K         ;Read T2K block
                   bcs READ_HDR_OK      ;Header appears to be OK
                   jsr RESTORE_COLORS   ;Restore colors
                   jsr DISP_IO_ERROR_MSG  ;Display I/O error msg
                   jsr WAIT_FOR_SELECT  ;Wait for SELECT key
                   jmp READ_INIT        ;And try again
                   
;-----------------------------------------------------------------------
;Process header
;-----------------------------------------------------------------------                   
READ_HDR_OK        jsr RESTORE_COLORS    ;Restore colors
                   jsr CLEAR_MSG_AREA    ;Clear message area
                   jsr DISP_FILENAME

                   lda HDR_TYPE          ;Get type
                   sta FILE_TYPE         ;Keep the file type
                   jsr DISP_FILE_TYPE    ;And display it
                   
                   
TYPE_OK            lda HDR_LENGTH+1      ;Check length hi byte
                   cmp #>[MAXLEN]         
                   beq LEN_LOTEST        ;Equal, then test lo byte
                   bcs LEN_BAD           ;Greater, error
                   jmp LEN_OK            ;Surely OK
                   
LEN_LOTEST         lda HDR_LENGTH        ;Test length lo byte
                   cmp #<[MAXLEN]
                   beq LEN_OK            ;If equal, then OK
                   bcs LEN_BAD           ;If greater, error
                   jmp LEN_OK            ;We are OK
                   
LEN_BAD            jsr DISP_TOO_LONG_MSG ;Bad length message
                   jsr WAIT_FOR_SELECT   ;Wait for SELECT key
                   jmp READ_INIT         ;And try again

;-----------------------------------------------------------------------
; Keep file name displayed for a while
;-----------------------------------------------------------------------
LEN_OK             jsr SHORT_DELAY
         
                   
                   
;-----------------------------------------------------------------------
; Load main part of the file
;-----------------------------------------------------------------------                   
LOAD_MAIN_PART     lda #<MAINBUF          ;Set buffer
                   sta BUFRLO
                   lda #>MAINBUF
                   sta BUFRHI
                   clc 
                   lda BUFRLO
                   adc HDR_LENGTH
                   sta BFENLO
                   sta DTB_SENTINEL_LO    ;Keep for further proc
                   lda BUFRHI
                   adc HDR_LENGTH+1
                   sta BFENHI
                   sta DTB_SENTINEL_HI    ;Keep for further proc

                   lda #255               ;ID byte is 255
                   jsr READ_T2K           ;Read block
                   bcs READY_SAVE         ;If OK, continue
                   
                   jsr RESTORE_COLORS     ;Restore colors
                   jsr DISP_IO_ERROR_MSG  ;Display I/O error msg
                   jsr WAIT_FOR_SELECT    ;Wait for SELECT key
                   jmp READ_INIT          ;And try again 
                   
;-----------------------------------------------------------------------
; Prepare to save
;-----------------------------------------------------------------------
READY_SAVE         lda #20
                   sta BG_COLOR
                   jsr RESTORE_COLORS     ;Restore colors
                   jsr DISP_ABORT_KEYS    ;Display abortion keys
                   jsr DISP_SAVE_MSG      ;Display ready to save
                   jsr WAIT_FOR_START     ;Wait for START key

                   lda FILE_TYPE          ;Check file type
                   cmp #04                ;Binary file?
                   beq SAVE_CHAIN_H       ;Yes, save as chainloading   

                   
;=======================================================================
; Normal copy
;=======================================================================                   
NORMAL_COPY        jsr SET_HEADER_WR_BUF  ;Set buffer for header
                   lda #0                 ;Identification byte
                   jsr WRITE_BLOCK        ;Write block                     
                   jsr RESTORE_COLORS     ;Restore colors
                   
                   lda #<[MAINBUF-1]      ;Set buffer for main block
                   sta BUFRLO
                   lda #>[MAINBUF-1]
                   sta BUFRHI          
                   
                   clc 
                   lda #<[MAINBUF-1]
                   adc HDR_LENGTH
                   sta BFENLO
                   lda #>[MAINBUF-1]
                   adc HDR_LENGTH+1
                   sta BFENHI    
                   
                   lda #255               ;Identification byte
                   jsr WRITE_BLOCK        ;Write block
                   jsr RESTORE_COLORS     ;Restore colors
                   
                   jmp READY_SAVE         ;And start over     
                   
                   
;=======================================================================
; Save as Chainloading
;=======================================================================                   
;-----------------------------------------------------------------------
; Save Chainloader header
;-----------------------------------------------------------------------                   
SAVE_CHAIN_H       lda #3                 ;Of type 3
                   sta HDR_TYPE
                   
                   lda CHAINLOADER_DATA+8  ;Load address
                   sta HDR_LOAD
                   lda CHAINLOADER_DATA+9
                   sta HDR_LOAD+1
                   
                   lda CHAINLOADER_DATA+6  ;Run address
                   sta HDR_RUN
                   lda CHAINLOADER_DATA+7
                   sta HDR_RUN+1
                   
                   lda #<[CHAINLOADER_L-12] ;Length
                   sta HDR_LENGTH
                   lda #>[CHAINLOADER_L-12]
                   sta HDR_LENGTH+1
                   
                   jsr SET_HEADER_WR_BUF
                   lda #0
                   jsr WRITE_BLOCK
                   jsr RESTORE_COLORS
                   
;-----------------------------------------------------------------------
; Save Chainloader data
;-----------------------------------------------------------------------                                      
SAVE_CHAIN_D       lda #<[CHAINLOADER_DATA+12-1]
                   sta BUFRLO
                   lda #>[CHAINLOADER_DATA+12-1]
                   sta BUFRHI
                   lda #<[CHAINLOADER_DATA+12+CHAINLOADER_L-12-1]
                   sta BFENLO
                   lda #>[CHAINLOADER_DATA+12+CHAINLOADER_L-12-1]
                   sta BFENHI
                   lda #255
                   jsr WRITE_BLOCK
                   jsr RESTORE_COLORS

;-----------------------------------------------------------------------
; Save segments of the binary file
;-----------------------------------------------------------------------
;Initialize pointer
                   lda #<MAINBUF
                   sta DTB_PTR_LO
                   lda #>MAINBUF
                   sta DTB_PTR_HI

;Check if whole binary file has been processed                   
DTB_LOOP           lda DTB_PTR_HI            ;Check high pointer
                   cmp DTB_SENTINEL_HI       ;Compare with sentinel
                   beq CHECK_END_LO          ;Equal, then check LO
                   bcc GET_SEG_HDR           ;Smaller, continue
DTB_GOEND          jmp DTB_END               ;Greater, we are done    
                   
CHECK_END_LO       lda DTB_PTR_LO            ;Check low pointer
                   cmp DTB_SENTINEL_LO       ;Compare with sentinel
                   beq DTB_GOEND             ;Equal, can continue
                   bcs DTB_GOEND             ;Greater, we are done
                   
;Try to get segment header                   

GET_SEG_HDR        ldy #0

GET_SEG_HDR_L1     lda (DTB_PTR),y            ;Copy segment header
                   sta [DTB_SEG_HEAD+0],y
                   iny
                   cpy #4
                   bne GET_SEG_HDR_L1
                  
                   lda DTB_SEG_HEAD+0         ;Check for 255 255
                   cmp #255
                   bne GET_SEG_HDR_OK         ;Not 255, header OK
                   lda DTB_SEG_HEAD+1
                   cmp #255                   
                   bne GET_SEG_HDR_OK         ;Not 255, header OK
                   
                   lda #2
                   sta DTB_PTR_INC
                   jsr DTB_ADVANCE_PTR
                   
                   jmp GET_SEG_HDR            ;And try again
                   
;Save the segment header
GET_SEG_HDR_OK     jsr SET_SEGHEAD_BUF        ;Set buffer for seg head
                   lda #125                   ;ID byte 125
                   jsr WRITE_BLOCK            ;Write block
                   jsr RESTORE_COLORS         ;Restore colors
                   
;Prepare to save the segment data
                   lda #3                     ;Point right before seg
                   sta DTB_PTR_INC
                   jsr DTB_ADVANCE_PTR
                   
                   lda DTB_PTR_LO             ;Buffer start
                   sta BUFRLO
                   lda DTB_PTR_HI
                   sta BUFRHI
   
                   lda #1                     ;Point to segment data
                   sta DTB_PTR_INC
                   jsr DTB_ADVANCE_PTR
                   
;Point to the last byte of the segment
                   sec                        ;Calculate length
                   lda DTB_SEG_HEAD+2
                   sbc DTB_SEG_HEAD+0
                   sta DTB_SEG_LEN_LO
                   lda DTB_SEG_HEAD+3
                   sbc DTB_SEG_HEAD+1
                   sta DTB_SEG_LEN_HI
                   clc                        ;Corrective + 1
                   lda DTB_SEG_LEN_LO
                   adc #1
                   sta DTB_SEG_LEN_LO
                   lda DTB_SEG_LEN_HI
                   adc #0
                   sta DTB_SEG_LEN_HI
                   
                   clc                        ;Calculate buffer end
                   lda BUFRLO
                   adc DTB_SEG_LEN_LO
                   sta BFENLO
                   sta DTB_PTR_LO
                   lda BUFRHI
                   adc DTB_SEG_LEN_HI
                   sta BFENHI
                   sta DTB_PTR_HI
                   
                   lda #126                  ;ID byte 126
                   jsr WRITE_BLOCK
                   jsr RESTORE_COLORS
                   
                   lda #1                    ;Advance pointer
                   sta DTB_PTR_INC
                   jsr DTB_ADVANCE_PTR
                   
                   jmp DTB_LOOP              ;Try next segment
                   
; Write termination segment header                   
DTB_END            lda #255
                   sta DTB_SEG_HEAD+0
                   sta DTB_SEG_HEAD+1
                   sta DTB_SEG_HEAD+2
                   sta DTB_SEG_HEAD+3
                   
                   jsr SET_SEGHEAD_BUF        ;Set buffer for seg head
                   lda #125                   ;ID byte 125
                   jsr WRITE_BLOCK            ;Write block
                   jsr RESTORE_COLORS         ;Restore colors
                   
                   jmp READY_SAVE             ;Done

;=======================================================================
; BUFFER SUBROUTINES
;=======================================================================
;-----------------------------------------------------------------------
; Set buffer for reading of the header
;-----------------------------------------------------------------------                   
SET_HEADER_RD_BUF  lda #<[HDR_START]
                   sta BUFRLO
                   lda #>[HDR_START]
                   sta BUFRHI
                   lda #<[HDR_START+HDR_L]
                   sta BFENLO
                   lda #>[HDR_START+HDR_L]
                   sta BFENHI
                   rts
                   
;-----------------------------------------------------------------------
; Set buffer for writing of the header
;-----------------------------------------------------------------------                   
SET_HEADER_WR_BUF  lda #<[HDR_START-1]      ;Buffer for header
                   sta BUFRLO
                   lda #>[HDR_START-1]
                   sta BUFRHI
                   lda #<[HDR_START+HDR_L-1]
                   sta BFENLO
                   lda #>[HDR_START+HDR_L-1]
                   sta BFENHI       
                   rts            
;-----------------------------------------------------------------------
; Set buffer for segment header
;-----------------------------------------------------------------------                   
SET_SEGHEAD_BUF    lda #<[DTB_SEG_HEAD-1]
                   sta BUFRLO
                   lda #>[DTB_SEG_HEAD-1]
                   sta BUFRHI
                   lda #<[DTB_SEG_HEAD+3]
                   sta BFENLO
                   lda #>[DTB_SEG_HEAD+3]
                   sta BFENHI
                   rts                   
                   
;=======================================================================
; MESSAGING SUBROUTINES
;=======================================================================
;-----------------------------------------------------------------------
; Display file name
;-----------------------------------------------------------------------
DISP_FILENAME      jsr CLEAR_FNAME_AREA
                   ldx #10
DF_LOOP            lda [HDR_NAME-1],x        ;Pick ATASCII
                   and #$7F                  ;No inverse video
                   
                   cmp #32                   ;Compare with 32
                   bmi DF_PLUS64             ;Smaller.. +64
                   cmp #96                   ;Compare with 96
                   bmi DF_MINUS32            ;Smaller.. -32
                   jmp DF_PUTC               ;Otherwise equality
                   
DF_MINUS32         sec                       ;Subtract 32
                   sbc #32
                   jmp DF_PUTC 
                   
DF_PLUS64          clc                       ;Add 64
                   adc #64
                   
DF_PUTC            sta [FNAMEL-1],x
                   dex 
                   bne DF_LOOP
                   rts
;-----------------------------------------------------------------------
; Display read message subroutine
;-----------------------------------------------------------------------
DISP_READ_MSG      jsr CLEAR_MSG_AREA
                   ldx #DRM_MESSAGE_L         ;Pick length
DRM_LOOP           lda [DRM_MESSAGE-1],x      ;Get character
                   sta [LINE1-1],x                ;Place character
                   dex                        ;Iteration
                   bne DRM_LOOP               ;Loop if not done
                   rts

DRM_MESSAGE   .SBYTE "Insert source tape with Turbo 2000 file."
              .SBYTE "Then press ",+$80,"START",+$00,"."
;                     0123456789012345678901234567890123456789                    
              DRM_MESSAGE_L=[*-DRM_MESSAGE]
                   
;-----------------------------------------------------------------------
; Display ready to save message
;-----------------------------------------------------------------------
DISP_SAVE_MSG      jsr CLEAR_MSG_AREA
                   ldx #DSM_MESSAGE_L         ;Pick length
DSM_LOOP           lda [DSM_MESSAGE-1],x      ;Get character
                   sta [LINE1-1],x                ;Place character
                   dex                        ;Iteration
                   bne DSM_LOOP               ;Loop if not done
                   rts

DSM_MESSAGE        .SBYTE "Insert the target tape, press PLAY and  "
                   .SBYTE "RECORD. Then press ",+$80,"START",+$00,"."
;                          0123456789012345678901234567890123456789                    
                   DSM_MESSAGE_L=[*-DSM_MESSAGE]
;-----------------------------------------------------------------------
; Display I/O error message
;-----------------------------------------------------------------------                   
DISP_IO_ERROR_MSG  jsr CLEAR_MSG_AREA
                   ldx #DIEM_MESSAGE_L
DIEM_LOOP          lda [DIEM_MESSAGE-1],x
                   sta [LINE2-1],x
                   dex 
                   bne DIEM_LOOP
                   rts
                   
DIEM_MESSAGE      .SBYTE "I/O error - Press ",+$80,"SELECT"
                   DIEM_MESSAGE_L=[*-DIEM_MESSAGE]
                   
;-----------------------------------------------------------------------
; Display file too long message
;-----------------------------------------------------------------------                   
DISP_TOO_LONG_MSG  jsr CLEAR_MSG_AREA
                   ldx #DTLM_MESSAGE_L
DTLM_LOOP          lda [DTLM_MESSAGE-1],x
                   sta [LINE2-1],x
                   dex 
                   bne DTLM_LOOP
                   rts
                   
DTLM_MESSAGE      .SBYTE "File is too long - Press ",+$80,"SELECT"
;                         0123456789012345678901234567890123456789
                   DTLM_MESSAGE_L=[*-DTLM_MESSAGE]                   
                   
;-----------------------------------------------------------------------
; Display file type indicator
;-----------------------------------------------------------------------
DISP_FILE_TYPE     ldx #45             ;Presume monolithic
                   lda HDR_TYPE        ;Check type
                   cmp #4              ;Is that 04 - Binary?
                   bne DFT_MONO        ;No, keep assumption                      
                   ldx #34             ;Yes, it is binary
DFT_MONO           stx FNAMEL+39       ;And display it
                   rts                            
;-----------------------------------------------------------------------
; Clear message area subroutine
;-----------------------------------------------------------------------
CLEAR_MSG_AREA     ldx #120
                   lda #0
CMA_LOOP           sta [LINE1-1],x
                   dex
                   bne CMA_LOOP
                   rts
;-----------------------------------------------------------------------
; Clear file name area subroutine
;-----------------------------------------------------------------------
CLEAR_FNAME_AREA   ldx #40
                   lda #0
CFA_LOOP           sta [FNAMEL-1],x
                   dex
                   bne CFA_LOOP
                   rts                   
                   
;-----------------------------------------------------------------------
; Clear abort keys area
;-----------------------------------------------------------------------
CLEAR_ABORT_K_AREA ldx #40
                   lda #0
CAKA_LOOP          sta [ABORTKL-1],x
                   dex
                   bne CAKA_LOOP
                   rts                   
;-----------------------------------------------------------------------
; Clear abort keys area
;-----------------------------------------------------------------------
DISP_ABORT_KEYS    jsr CLEAR_ABORT_K_AREA
                   ldx #DAK_MESSAGE_L
DAK_LOOP           lda [DAK_MESSAGE-1],x
                   sta [ABORTKL-1],x
                   dex 
                   bne DAK_LOOP
                   rts
                   
DAK_MESSAGE       .SBYTE +$80,"BREAK",+$00," Restart copier ",
                  .SBYTE +$80,"OPTION",+$00," Exit"
;                         0123456789012345678901234567890123456789
                   DAK_MESSAGE_L=[*-DAK_MESSAGE]                   

                   
;=======================================================================
; KEYBOARD SUBROUTINES
;=======================================================================                   
;-----------------------------------------------------------------------
; Wait for START subroutine, allow BREAK to ABORT and OPTION
; to COLD start
;-----------------------------------------------------------------------
WAIT_FOR_START     lda #8
                   sta CONSOL
WFS_LOOP           lda BRKKEY             ;Chance to abort
                   beq WFS_BREAK          ;Was taken
                   
                   lda CONSOL             ;What keys?  
                   cmp #6                 ;Is that START?
                   beq WFS_DONE           ;Yes, we are done
                   
                   cmp #3                 ;Is that OPTION?
                   bne WFS_LOOP           ;No, keep waiting
                   jmp FORCE_COLDSTART    ;Yes, force COLDST
                   
WFS_DONE           rts
                   
WFS_BREAK          dec BRKKEY             ;Reset BREAK status
                   jmp START              ;Start from scratch
                   
;-----------------------------------------------------------------------
; Wait for SELECT subroutine
;-----------------------------------------------------------------------
WAIT_FOR_SELECT    lda #8
                   sta CONSOL
WFSEL_LOOP         lda CONSOL           
                   cmp #5
                   bne WFSEL_LOOP
                   rts
                   
;=======================================================================
; OTHER SUBROUTINES
;=======================================================================                   
;-----------------------------------------------------------------------
; Beep subroutine
;-----------------------------------------------------------------------                   
BEEP               ldx #200
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
                   rts
                   
;-----------------------------------------------------------------------
; Wait for VBLANK
;-----------------------------------------------------------------------
WAIT_FOR_VBLANK    php
                   lda #0
WFV_1              cmp VCOUNT
                   bne WFV_1
                   plp
                   rts                                                   
;-----------------------------------------------------------------------
;Restore colors subroutine
;-----------------------------------------------------------------------
RESTORE_COLORS     lda #0          ;Black border
                   sta COLBK
                   lda BG_COLOR    ;Background - Green/black
                   sta COLPF2
                   lda #14         ;Light text
                   sta COLPF1
                   sta COLPF0
                   rts

;-----------------------------------------------------------------------
;BREAK key interrupt handler
;-----------------------------------------------------------------------                   
BREAK_HANDLER      lda #0
                   sta BRKKEY
                   pla
                   rti
;-----------------------------------------------------------------------
;Cold start
;-----------------------------------------------------------------------                   
FORCE_COLDSTART    ldx #FC_ROUTINE_L
FC_LOOP            lda [FC_ROUTINE-1],X
                   sta [128-1],X
                   dex
                   bne FC_LOOP
                   jmp 128

FC_ROUTINE         lda #255        ;Enable ROM
                   sta PORTB 
                   jmp COLDSV      ;Reboot
                   FC_ROUTINE_L=[*-FC_ROUTINE]
                   
;-----------------------------------------------------------------------
; Short delay
;-----------------------------------------------------------------------                   
SHORT_DELAY        ldy #32
DELAY_LOOP_E       ldx #255            
DELAY_LOOP_I       stx WSYNC
                   dex
                   bne DELAY_LOOP_I
                   dey 
                   bne DELAY_LOOP_E
                   rts
;-----------------------------------------------------------------------
; Binary file pointer advance
;-----------------------------------------------------------------------                                      
DTB_ADVANCE_PTR    clc                        
                   lda DTB_PTR_LO
                   adc DTB_PTR_INC
                   sta DTB_PTR_LO
                   lda DTB_PTR_HI
                   adc #0
                   sta DTB_PTR_HI
                   rts 
;=======================================================================
;Block decoding subroutine
;Block is placed from: BUFRLO+256*BUFRHI
;                to:   (BFENLO+256*BFENHI)-1
;First byte of the block should be in
;acumulator register before subroutine
;is called        
;=======================================================================
    
READ_T2K    sta LTEMP
;            lda BRKKEY
;            sta BRK_BACKUP 
            lda #52
            sta PACTL
            sta PBCTL
            lda #128
;           sta POKMSK
;           sta IRQEN
            clc
            ldy #0
            sty STATUS
            sty CHKSUM
;           sty NMIEN
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
            bne L06C3
            beq L069A
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
L06C3       jsr WAIT_FOR_VBLANK  
            lda #34          ; Was 192
;           sta NMIEN
;           sta POKMSK
;           sta IRQEN
            sta DMACLT       ; Inserted newly
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


;=======================================================================
; Write block of data
; BUFRLO,BUFRHI  - Right before first byte
; BFENLO,BFENHI  - Last byte
; A              - Identification byte
;=======================================================================
WRITE_BLOCK    pha                    ;Keep A in the stack
               lda #52                ;Prepare tape drive for writing
               sta PACTL
               jsr SHORT_DELAY        ;Let motor get to full speed

               jsr WR_RESET_ALL       ;Reset coder
               pla                    ;Restore A
               sta ICAX6Z             ;Keep identification byte
               sta CHKSUM             ;Use as base for check sum
            
               lda #192
               sta AUDCTL
               
;              ldx #32                ;Generate pilot tone  
               ldx #12                ;Generate pilot tone                 
               lda ICAX6Z
               beq WR_PILOTLEN        ;If ID zero, skip
;              ldx #12
               ldx #12
WR_PILOTLEN    stx STATUS             ;Keep status
WR_PILOTL1     dey
               bne WR_PILOTL1         ;Wait
               lda #3                 
               sta SKCTL              ;Generate half of pulse
               ldy #119
WR_PILOTL2     dey
               bne WR_PILOTL2
               lda #11
               sta SKCTL              ;Generate half of pulse
               ldy #118
               dex
               bne WR_PILOTL1         ;Continue with part of pilot tone  
               dey
               dey
               dec STATUS              
               bne WR_PILOTL1         ;Continue with part of pilot tone

            
               ldy #32                ;Generate SYNC pulse
WR_SYNC1       dey
               bne WR_SYNC1
               lda #3
               sta SKCTL
               ldy #39
WR_SYNC2       dey
               bne WR_SYNC2
               lda #11
               sta SKCTL
            
               ldy #43                ;Generate bytes
               sec
               jmp WR_GBYTE
            
WR_PICKBYTE    lda BFENLO
               cmp BUFRLO
               lda BFENHI
               sbc BUFRHI
               bcc WR_GBYTE_CSM
               lda (BUFRLO,X)
               sta ICAX6Z
               eor CHKSUM
               sta CHKSUM
            
WR_GBYTE       jmp WR_GBYTE_NBIT      ;Go and generate byte
WR_GBYTE_CSM   lda CHKSUM             
               sta ICAX6Z
               dex
               sec
               bcs WR_GBYTE
WR_GBYTE_W1    dey
               bne WR_GBYTE_W1
               bcc WR_GBYTE_HI
               ldy #48
WR_GBYTE_W2    dey
               bne WR_GBYTE_W2
WR_GBYTE_HI    lda #3
               sta SKCTL
               ldy #46
               bcc WR_GBYTE_W3
               ldy #94
WR_GBYTE_W3    dey
               bne WR_GBYTE_W3
               lda #11
               sta SKCTL
               clc
               ldy #44
            
WR_GBYTE_NBIT  rol ICAX6Z             ;Still bits to go
               bne WR_GBYTE_W1        ;Yes, write bit
               inc BUFRLO             ;No, advance in the buffer
               beq WR_ADVBUF             
               bne WR_CHANI1
WR_CHANI1      bne WR_CHAIN2
WR_ADVBUF      inc BUFRHI
WR_CHAIN2      ldy #32
               txa
               beq WR_PICKBYTE        ;Get other byte from buffer
            
WR_GBYTE_W4    dey                    ;Keep waiting 
               bne WR_GBYTE_W4             
            
               lda #3                 ;Write safety pulse
               sta SKCTL
               lda #60                ;Stop motor
               sta PACTL
               lda #0
               sta AUDCTL
               jmp WR_TERM            ;Terminate
               
WR_TERM        lda #255
;              sta PORTB
;              lda #64
;              sta NMIEN
;              sta IRQEN
               jsr WAIT_FOR_VBLANK   
               lda #34                ;New
               sta DMACLT             ;New
               rts
               
               
WR_RESET_ALL   ldy #0
               sty STATUS
               sty CHKSUM
;              sty NMIEN
               jsr WAIT_FOR_VBLANK
               sty DMACLT
               lda #254
;              sty IRQEN
;              sta PORTB

               clc
               rts               

;=======================================================================
; DISPLAY DATA
;=======================================================================
;-----------------------------------------------------------------------
; Display list
;-----------------------------------------------------------------------
DLIST      .BYTE 112,112,112,2+64,<TITLINE,>TITLINE
           .BYTE 112
           .BYTE 2+64,<FNAMEL,>FNAMEL
           .BYTE 112
           .BYTE 2+64,<LINE1,>LINE1,2,2
           .BYTE 112,112,112,112,112,112,112,112,112,112,112,112
           .BYTE 112,112,112
           .BYTE 2+64,<ABORTKL,>ABORTKL
           .BYTE 65,<DLIST,>DLIST
;-----------------------------------------------------------------------
; Screen lines
;-----------------------------------------------------------------------
;                          0123456789012345678901234567890123456789   
TITLINE    .SBYTE    +$80,"CHAINCOPY 1.3                           "

LINE1      .REPT 40
           .BYTE 0
           .ENDR
LINE2      .REPT 40
           .BYTE 0
           .ENDR
LINE3      .REPT 40
           .BYTE 0
           .ENDR
FNAMEL     .REPT 40
           .BYTE 0
           .ENDR         
ABORTKL    .REPT 40
           .BYTE 0
           .ENDR                    
;=======================================================================
; DATA AREAS
;=======================================================================           
;-----------------------------------------------------------------------
; Header buffer
;-----------------------------------------------------------------------           
HDR_FILLER0 .BYTE 0           
HDR_TYPE    .BYTE 0
HDR_NAME    .BYTE 0,0,0,0,0,0,0,0,0,0
HDR_LOAD    .WORD 0
HDR_LENGTH  .WORD 0
HDR_RUN     .WORD 0
            HDR_START = HDR_TYPE
            HDR_L = [*-HDR_START]
HDR_FILLER1 .BYTE 0           
;-----------------------------------------------------------------------
; Binary file tracking
;-----------------------------------------------------------------------
DTB_SEG_HEAD     .BYTE 0,0,0,0       ;Segment header   
DTB_SEG_LEN_LO   .BYTE 0             ;Aux buffer pointer
DTB_SEG_LEN_HI   .BYTE 0
DTB_PTR_INC      .BYTE 0             ;Binary file pointer increment

DTB_SENTINEL_LO .BYTE 0              ;Point right past binary
DTB_SENTINEL_HI .BYTE 0

;-----------------------------------------------------------------------
; Various flags
;-----------------------------------------------------------------------
FILE_TYPE       .BYTE 0              ;Stored file type
BG_COLOR        .BYTE 0              ;Background color

;-----------------------------------------------------------------------           
; ChainLoader data. Whole binary file with a RUN segment
; at the beginning.
;-----------------------------------------------------------------------           
CHAINLOADER_DATA            
            .INCBIN chainloader2.xex
CHAINLOADER_END
            CHAINLOADER_L=[CHAINLOADER_END-CHAINLOADER_DATA]
