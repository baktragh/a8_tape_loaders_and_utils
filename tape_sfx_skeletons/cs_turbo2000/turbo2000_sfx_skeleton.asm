;=======================================================================
; Turbo 2000 Self-extractor skeleton   
; Assemble with the MADS assembler
;
; This skeleton is used to create "Turbo 2000 self-extractors". 
; The purpose of the self-extractor is to record a Turbo 2000 file
; to a cassette.
;
; This can be useful, when the only cassette recorder at hand is one of the
; Atari data recorders. Vendors of the original software can
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
; - Around 45 KB is left for the data. Larger files can be split into
;   multiple parts and recorded sequentially.
; - The self-extractors begin at address $0C00.
;   The self-extractors should be loaded by miniature binary loaders, not
;   by full-size disk operating systems.   
 
; Formats supported:
; The self-extractor writes raw Turbo 2000 blocks, so the formats
; practically usable are the following:
; Turbo 2000 base format 
; Turbo 2000 - kilobyte blocks
; Turbo 2000 - ChainLoading, BlockLoading, ExpressLoading
; Omicron Turbo - BlockLoading, ExpressLoading, Kilobyte Blocks, Twokilobyte
; blocks.      
; 
; How to use this skeleton.
; This skeleton holds the following segments
; 1. Initialization code
; 2. INIT vector pointing to the initialization code
; 3. Mainline code.
;
; The user of the skeleton is supposed to append the following segments:
; - A data segment that begins at DATA_TABLE address. This segment holds
;   the turbo block table followed by turbo block data.
; - A RUN vector pointing to the START_ADDR address    
;
; The user of the skeleton is supposed to zap the following areas:
; 1. LINE_NAME with the name of the program (internal code)
; 2. The first 36 characters of the LINTE_TITLE (internal code)
; 3. The last three characters of the LINE_TITLE with partition number (i.c.)  
; 4. Configuration flags
;    - Indicate if the self-extractor is a part of a composite one
;    - Inidcate if there is a 5-second gap before the recording     
; 
; The turbo block table is a table of 5-byte items, each item represents
; a buffer range for the turbo 2000 block write routine, followed by a
; block flag byte.
; The last item in the table is a termination mark - $FF $FF $FF $FF.
; After the table, the data of the turbo blocks follows. Note that
; the turbo blocks must include the checksum. The turbo 2000 block write
; routine calculates the checksum, but doesn't record it.
;=======================================================================
           
                 ICL "equates.asm" 
                 OPT H+,F-
;=======================================================================
; Global constants
;=======================================================================
                 ICL "../commons/gconstants.asm"          
;=======================================================================
; Private constants
;=======================================================================
                START_ADDR      = 2800
                
                ZP_TAB_PTR_LO   = 128
                ZP_TAB_PTR_HI   = 129
                
                ZP_BLOCKFLAG    = 130
                ZP_ID_BYTE      = 131
                ZP_SILENCE      = 132

;Block flag constants
                
                CS_PILOT_HEADER = 0x80;
                CS_PILOT_DATA   = 0x40;
                CS_PILOT_LOOSE  = 0x20;
    
                CS_GAP_NONE = 0x08;
                CS_GAP_ELONGATED = 0x04;
;=======================================================================
; INITITALIZATION CODE 
;=======================================================================
                ICL "../commons/preinit.asm"            
;=======================================================================
; MAINLINE CODE
;=======================================================================
                   ORG START_ADDR
                   jmp ENTRY_ADDR
;-----------------------------------------------------------------------
; Screen lines
;-----------------------------------------------------------------------
                ICL "../commons/screen.asm"
;-----------------------------------------------------------------------
; Configuration
;-----------------------------------------------------------------------
                ICL "../commons/gconfig.asm"
;------------------------------------------------------------------------
;Initialization
;------------------------------------------------------------------------
ENTRY_ADDR         
                ICL "../commons/screeni.asm"                                       
;-----------------------------------------------------------------------
; Prepare to save
;-----------------------------------------------------------------------
READY_SAVE         lda #<DATA_TABLE        ;Reset the table and counter
                   sta ZP_TAB_PTR_LO
                   lda #>DATA_TABLE
                   sta ZP_TAB_PTR_HI

                   bit CFG_FLAGS
                   bmi SKIP_START         ;If $80 (composite)
                   jsr WAIT_FOR_START     ;Wait for START key
SKIP_START         jsr BEEP
;-----------------------------------------------------------------------
;From now on, disable interrupts and DMA, keep motor ON until the contents
;is fully recorded.
;-----------------------------------------------------------------------
                   jsr RECENV_INIT
                   lda #52
                   sta PACTL

                   ldy CFG_SAFETY_DELAY   ;Presum just safety delay
                   bit CFG_FLAGS          ;Check if long separator requested
                   bvc NORM_SEP           ;No, stick with safety delay
                   ldy CFG_SEP_DURATION   ;Use delay for long separator
NORM_SEP           jsr DELAY_CUSTOM_Y     ;Make the delay
;-----------------------------------------------------------------------      
SAVE_LOOP          ldx #4
                   ldy #0                 ;Get buffer range
@                  lda (ZP_TAB_PTR_LO),Y
                   sta BUFRLO,Y
                   iny
                   dex
                   bne @-
                   
                   lda (ZP_TAB_PTR_LO),Y
                   sta ZP_BLOCKFLAG 
                   iny 
                   lda (ZP_TAB_PTR_LO),Y
                   sta ZP_SILENCE
                   iny

                   lda BUFRLO
                   and BUFRHI
                   and BFENLO
                   and BFENHI
                   cmp #$FF
                   beq SAVE_TERM
                   
SAVE_DOSILENCE                   
                   ldy ZP_SILENCE               ;Silence before the block
                   beq SAVE_DOWRITE             ;Yes, skip
                   jsr DELAY_TENTHS             ;Otherwise, do silence
SAVE_DOWRITE			
                   ldy #0                       ;Get ID byte
                   lda (BUFRLO),Y
                   sta ZP_ID_BYTE
                   jsr WRITE_BLOCK

SAVE_TONEXT
                   clc                          ;Increment table pointer
                   lda #6                       ;By 6 bytes
                   adc ZP_TAB_PTR_LO
                   sta ZP_TAB_PTR_LO
                   bcc SAVE_CONT
                   inc ZP_TAB_PTR_HI  

SAVE_CONT          
SAVE_NEXTBLOCK     jmp SAVE_LOOP                ;Continue saving.
;-----------------------------------------------------------------------
SAVE_TERM          jsr RECENV_TERM              ;Back with DMA and INTRs
                   lda #60                      ;Motor off
                   sta PACTL

                   bit CFG_FLAGS                ;Check for composite($80)
                   bmi SAVE_QUIT                ;If composite, quit
                   lda CFG_FLAGS                ;Check for alarm
                   and #CFG_F_ALARM                
                   beq SAVE_AGAIN               ;No alarm, just skip

SAVE_ALARM         jsr BEEP                     ;Three beeps for alarm
                   jsr BEEP
                   jsr BEEP
SAVE_AGAIN         jmp READY_SAVE               ;Otherwise start over    
SAVE_QUIT          rts          
;=======================================================================
; Common Auxiliary Subroutines
;=======================================================================
                   ICL "../commons/routines.asm"                   
;=======================================================================
; Write block of data
; BUFRLO,BUFRHI  - Right before first byte
; BFENLO,BFENHI  - Last byte
; A              - Identification byte
;=======================================================================
WRITE_BLOCK    pha                    ;Keep A in the stack

               pla                    ;Restore A
               sta ICAX6Z             ;Keep identification byte
               sta CHKSUM             ;Use as base for check sum
            
               lda #192
               sta AUDCTL

WR_PILOT_LEN_DET
               ldx #10                ;Presume "loose" pilot tone.
               lda ZP_BLOCKFLAG       ;Check what pilot we have?
               and #CS_PILOT_HEADER   ;Is that header pilot?
               beq @+                 ;No skip
               ldx #30                ;Yes, header, 30*256
               bne WR_PILOT           ;Then continue
@              lda ZP_BLOCKFLAG       ;Check again
               and #CS_PILOT_DATA     ;Is that pilot tone for data block
               beq WR_PILOT           ;No, skip
               ldx #12                ;Yes, have it 12*256 pulses

WR_PILOT       
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
;            
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
            
WR_SAFPULSE    ldy #5                 ;Ensure safety pulse is long enough.
               jsr DELAY_CUSTOM_Y
               lda #3                 ;End the safety pulse
               sta SKCTL
               lda #0
               sta AUDCTL
               rts                    ;Terminate writing
;-------------------------------------------------------------------------------
; Terminate recording environment
;-------------------------------------------------------------------------------               
RECENV_TERM    lda #64
               sta NMIEN
               sta IRQEN
               jsr WAIT_FOR_VBLANK   
               lda #34                
               sta DMACLT             
               rts

;-------------------------------------------------------------------------------
; Initiate recording environment;
;-------------------------------------------------------------------------------               
RECENV_INIT    ldy #0
               sty STATUS
               sty CHKSUM
               sty NMIEN
               sty DMACLT
               jsr WAIT_FOR_VBLANK
               sty IRQEN
               clc
               rts               
;=======================================================================
; DISPLAY list
;=======================================================================
               ICL "../commons/dlist.asm"
;=======================================================================
; DATA AREAS
;=======================================================================
;=======================================================================
; Segment data table
;=======================================================================
            DATA_TABLE=*  
            SFX_CAPACITY = 49152-DATA_TABLE-6-6-1
            START = START_ADDR
