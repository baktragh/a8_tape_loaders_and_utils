;=======================================================================
; Turbo 2000 Self-extractor skeleton   
; Assemble with the MADS assembler
;
; This skeleton is used to create "Turbo 2000 self-extractors". 
; The purpose of the self-extractor is to record a Turbo 2000 file
; to a cassette.
;
; This can be useful, when the only cassette recorder at hand is one of the
; atari data recorders. Vendors of the original software can
; include this self-extractor in the package to let their customers
; to create their own self-installed tape recording.  
; 
; From the user's perspective, the recording works as follows:
; 1. Load the self-extractor from a universal cartridge, floppy disk, or
;    emulated floppy disk, or cassette.
; 2. Insert a blank tape in the data recorder (enhanced with the 
;    Czechoslovak Turbo 2000 or compatible upgrade)
; 3. Press PLAY+RECORD
; 4. Press START to commence recording.
;
; Limitations:
; - Restricted to 2270 bps (base Turbo 2000 speed)
; - Around 43 KB is left for the data. Larger files can be split into
;   multiple parts and recorded sequentially.
; - The self-extractrs begin from address $0C00.
;   The self-extractors should be loaded by miniature binary loaders, not
;   by full-size disk operating systems.   
 
; Formats supported:
; The self-extractor writes raw Turbo 2000 blocks, so the formats
; practically usable are the following:
; Turbo 2000 base format 
; Turbo 2000 - kilobyte blocks
; Turbo 2000 - ChainLoading, BlockLoading, ExpressLoading                  
; 
;=======================================================================
           
                 ICL "equates.asm" 
                 OPT H+,F-

;=======================================================================
; Private constants
;=======================================================================
                ZP_TAB_PTR_LO = 128
                ZP_TAB_PTR_HI = 129
                VBI_VCOUNT    = 124
                START_ADDR    = 3072
;=======================================================================
; INITITALIZATION CODE - Switches off the display, so that
; loading data into the screen memory does no harm. Also ensure
; that RESET results in cold start.
;=======================================================================
                   ORG  START_ADDR      
                   ldy #0
                   sty SDMCTL
                   iny
                   sty COLDST
                   jsr IN_WBV
                   jsr IN_WBV

IN_WBV             lda #VBI_VCOUNT
IN_WBV_L           cmp VCOUNT
                   bne IN_WBV_L
                   rts     
                   
                   ORG INITAD
                   .WORD START_ADDR
            
;=======================================================================
; MAINLINE CODE
;=======================================================================
                   ORG START_ADDR
;-----------------------------------------------------------------------
;Initialization
;-----------------------------------------------------------------------
                   jsr WAIT_FOR_VBLANK

                   sei                                 
                   lda #<DLIST          ;Setup display list
                   sta SDLSTL
                   lda #>DLIST
                   sta SDLSTH
                   cli

                   jsr WAIT_FOR_VBLANK ;Enable the screen
                   lda #34
                   sta SDMCTL
                   lda #0
                   sta COLOR2
                   lda #$1A
                   sta COLOR0
                   sta COLOR1                                       
;-----------------------------------------------------------------------
; Prepare to save
;-----------------------------------------------------------------------
READY_SAVE         lda #<DATA_TABLE        ;Reset the table and counter
                   sta ZP_TAB_PTR_LO
                   lda #>DATA_TABLE
                   sta ZP_TAB_PTR_HI

                   jsr WAIT_FOR_START     ;Wait for START key
                   jsr BEEP

;-----------------------------------------------------------------------      
SAVE_LOOP          ldy #0                 ;Get buffer range
                   lda (ZP_TAB_PTR_LO),Y
                   sta BUFRLO
                   iny
                   lda (ZP_TAB_PTR_LO),Y
                   sta BUFRHI
                   iny 
                   lda (ZP_TAB_PTR_LO),Y
                   sta BFENLO
                   iny
                   lda (ZP_TAB_PTR_LO),Y
                   sta BFENHI

                   lda BUFRLO
                   and BUFRHI
                   and BFENLO
                   and BFENHI
                   cmp #$FF
                   beq READY_SAVE
    
SAVE_DOBLOCK       ldy #0                 ;Get ID byte
                   lda (BUFRLO),Y
                   sta ID_BYTE
                   
                   jsr WRITE_BLOCK

                   clc                          ;Increment table pointer
                   lda #4
                   adc ZP_TAB_PTR_LO
                   sta ZP_TAB_PTR_LO
                   bcc SAVE_CONT
                   inc ZP_TAB_PTR_HI  
SAVE_CONT
                   jsr SHORT_DELAY              ;Delay between blocks
                   jmp SAVE_LOOP                ;Continue saving.
                   
;=======================================================================
; KEYBOARD SUBROUTINES
;=======================================================================                   
;-----------------------------------------------------------------------
; Wait for START 
;-----------------------------------------------------------------------
WAIT_FOR_START     lda #8
                   sta CONSOL
WFS_LOOP           lda CONSOL             ;What keys?  
                   cmp #6                 ;Is that START?
                   beq WFS_DONE           ;Yes, we are done
                   bne WFS_LOOP
WFS_DONE           rts
                   
                   
;=======================================================================
; OTHER SUBROUTINES
;=======================================================================                   
;-----------------------------------------------------------------------
; Beep
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
                   lda #VBI_VCOUNT
WFV_1              cmp VCOUNT
                   bne WFV_1
                   plp
                   rts                                                   
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
WR_GBYTE_CSM   jmp WR_SAFPULSE        ;Just end the writing
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
            
WR_SAFPULSE    lda #3                 ;Write safety pulse
               sta SKCTL
               lda #60                ;Stop motor
               sta PACTL
               lda #0
               sta AUDCTL
               jmp WR_TERM            ;Terminate
               
WR_TERM        lda #255
               sta PORTB
               lda #64
               sta NMIEN
               sta IRQEN
               jsr WAIT_FOR_VBLANK   
               lda #34                ;New
               sta DMACLT             ;New
               rts
               
WR_RESET_ALL   ldy #0
               sty STATUS
               sty CHKSUM
               sty NMIEN
               jsr WAIT_FOR_VBLANK
               sty DMACLT
               lda #254
               sty IRQEN
               sta PORTB
               clc
               rts               

;=======================================================================
; DISPLAY DATA
;=======================================================================
;-----------------------------------------------------------------------
; Display list
;-----------------------------------------------------------------------
DLIST      .BYTE 112,112,112
           .BYTE 7+64,<LINE_NAME,>LINE_NAME
           .BYTE 112,112
           .BYTE 2+64,<LINE_TITLE,>LINE_TITLE
           .BYTE $30
           .BYTE 2+64,<LINE_INSTR,>LINE_INSTR,2
           .BYTE 65,<DLIST,>DLIST
;-----------------------------------------------------------------------
; Screen lines
;-----------------------------------------------------------------------
;                          0123456789012345678901234567890123456789   
LINE_NAME   .BYTE         "tttttttttttttttttttt"

LINE_TITLE  .BYTE         "Turbo 2000 Self-Extractor            ppp"

LINE_INSTR  .BYTE         "Insert blank tape. Press PLAY+RECORD.   "
            .BYTE         "Then press START to begin recording.    "                     
;=======================================================================
; DATA AREAS
;=======================================================================
ID_BYTE     .BYTE 0       
            DATA_TABLE=*  
