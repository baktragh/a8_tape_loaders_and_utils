;===============================================================================
;TURGEN - LB-Express (LBE)
;Binary loader for standard (FSK) tape records using special file
;format for increased loading efficiency.
;
;Michael Kalouš of the BAKTRA Software, 2022.
;===============================================================================
;
;The author has placed this work in the Public Domain, thereby relinquishing all
;copyrights. Everyone is free to use, modify, republish, sell or give away this
;work without prior consent from anybody.
;
;
;Storing as tape boot file
;-------------------------
;This loader uses the "trailing EOF record trick" - the last 128 bytes of the
;loader are in the EOF block. These 128 bytes are moved from the cassette
;buffer to the intended memory location. The trick allows this loader to be
;one record shorter. The loader is compatible with XL/XE and 
;pre-XL/XE Atari computers.
;
;Assemble with the MADS cross-assembler
;
;Building the loader
;-------------------
; Symbol     Values
; BUILD      0 B_BOOT   Build boot file
;            1 B_XEX    Build binary load file
;
; LDR_CFG    0 C_PLAIN  Plain loader
; LDR_CFG    1 C_PMG    Show progress with PMG          
;
;Physical block format
;---------------------
; Blocks are 512+2+1+1 bytes long
; Block structure:
; 0,1: 0x55 0x55 - Standard for SIO tape records
;   2: 1... .... - EOF flag
;      .1....... - INIT flag
;      ..11 1111 - Block sequential number
; 512 bytes of user data
; Checksum (Standard SIO checksum)
;
;Logical file format
; 0 Segment block(s)
;   2 bytes first address
;   2 bytes last address
;   Or 0xFF 0xFF 0xFE 0xFE EOF indicator
;   Bytes of the segment

;Maintenance log
;---------------
;2022-03-04 Initial version
;2022-04-11 Add support for showing loading progress using PMG
;2022-07-21 Support for ATRACT mode suppression 
;2023-08-30 Move loader configuration to the loader itself
;===============================================================================
            
;===============================================================================
; Includes and EQUs
;===============================================================================
          ICL "equates.asm"
.IF BUILD=0
          OPT H-         
          LDR_START=[2048-23]
.ELSE
          OPT H+,F+
          LDR_START=2048
.ENDIF

;CIO channel 0          
          CIO0_OP   =$0342
          CIO0_STAT =$0343
          CIO0_BUFLO=$0344
          CIO0_BUFHI=$0345
          CIO0_LENLO=$0348
          CIO0_LENHI=$0349
          CIO0_AUX1 =$034A
          CIO0_AUX2 =$034B

;Buffer Setup      
          SG_BUFRLO = 129
          SG_BUFRHI = 130
          SG_BFENLO = 131
          SG_BFENHI = 132  
          
          BUF_LEN   = [512+2+1+1]
          BLK_DOFS  = 3

;File format              
          CFG_V_LUMA_MASK = $0F
          CFG_V_F_CRSIN = $80
          CFG_V_F_NOATRACT = $40
          CFG_V_TITLE_LEN = 30+2
          
;Miscellaneous          
          LDR_END = BLOCK_BUFFER+BUF_LEN-1
          
;Build configurations
          B_BOOT  = 0
          B_XEX   = 1
          C_PLAIN = 0
          C_PMG   = 1        
          
.IF LDR_CFG = C_PLAIN
          BOOT_BLKS = 3
          BOOT_SIZE = 384
.ENDIF
.IF LDR_CFG = C_PMG
          BOOT_BLKS = 4
          BOOT_SIZE = 512
.ENDIF                                

;===============================================================================
; MAINLINE CODE
;===============================================================================
          ORG LDR_START
;-------------------------------------------------------------------------------
; Boot header
;-------------------------------------------------------------------------------
.IF BUILD=B_BOOT
BOOTHEAD  .BYTE 0                 ;Boot flag 0
          .BYTE BOOT_BLKS         ;3 blocks + 1 EOF block
          .WORD [LDR_START]       ;Load address
          .WORD DO_RTS            ;Initialization address - just RTS
;-------------------------------------------------------------------------------
; Boot continuation code
;-------------------------------------------------------------------------------          
          lda  #60                ;Motor off
          sta  PACTL
          ldx  #255               ;Clear pushdown store
          txs
;-------------------------------------------------------------------------------
; Move last portion of the loader code from cassette buffer
; Move 128 bytes of the EOF block from cassette
; buffer to the intended place.
; X = 255 from above, loop down to 128.
;-------------------------------------------------------------------------------
RELO_P2_L lda  1024-128,X
          sta  LDR_START+BOOT_SIZE-128,X
          dex  
          bmi  RELO_P2_L
.ENDIF         
;-------------------------------------------------------------------------------
; Loader mainline code
;-------------------------------------------------------------------------------
.IF LDR_CFG=C_PMG          
          jsr SET_PROGRESS        ;Set progress indicators
.ENDIF          
          jsr SCREEN              ;Setup screen

          lda #52                 ;Motor on
          sta PACTL

BLTOP     jsr GET_BLOCK           ;Get first block
BL_START  
;===============================================================================
; Read a segment 
;===============================================================================
GS_START  lda #255                 ;Set fake INIT vector to $FFFF (fake value)
          sta INITAD
          sta INITAD+1
;-------------------------------------------------------------------------------
; Get segment header
;-------------------------------------------------------------------------------
GS_GETH   jsr GET_BYTE             ;Get four bytes of the header
          sta SG_BUFRLO     
          jsr GET_BYTE
          sta SG_BUFRHI
          jsr GET_BYTE
          sta SG_BFENLO
          jsr GET_BYTE
          sta SG_BFENHI                 
          
          lda SG_BUFRLO            ;Check for EOF
          cmp #$FF
          bne GS_GETDATA
          lda SG_BUFRHI
          cmp #$FF
          bne GS_GETDATA
          lda SG_BFENLO
          cmp #$FE
          bne GS_GETDATA
          lda SG_BFENHI
          cmp #$FE
          bne GS_GETDATA
          jmp RUN_PROGRAM

;-------------------------------------------------------------------------------
; Get segment data
;-------------------------------------------------------------------------------          
GS_GETDATA  jsr GET_BYTE         ;Get next byte
            ldy #0
            sta (SG_BUFRLO),Y    ;Place to its respective location
            
            lda SG_BUFRHI        ;Check for end of segment
            cmp SG_BFENHI
            bne GS_ADVANCE
            lda SG_BUFRLO
            cmp SG_BFENLO
            bne GS_ADVANCE
            jmp GS_INIT

GS_ADVANCE            
            clc                  ;Advance in the segment
            lda SG_BUFRLO
            adc #1
            sta SG_BUFRLO
            lda SG_BUFRHI
            adc #0
            sta SG_BUFRHI
            jmp GS_GETDATA
;-------------------------------------------------------------------------------
; Perform jump through INITAD if needed
;-------------------------------------------------------------------------------          
GS_INIT   lda INITAD             ;Check if there was real INIT segment
          and INITAD+1
          cmp #255
          beq POSTINI            ;Skip INIT if $FFFF
          
REALINI   jsr DOINIT             ;Execute INIT segment code
          
POSTINI   jmp GS_START           ;Get another segment


DOINIT    jmp (INITAD)

;===============================================================================
; Read a byte, place to A
;===============================================================================
GET_BYTE  lda BUFRHI             ;Are we about to read a byte from next block?
          cmp BFENHI
          bne GB_DOGET           ;No, just get next
          lda BUFRLO
          cmp BFENLO
          bne GB_DOGET           ;No, just get next
          
          jsr GET_BLOCK          ;Yes, then read the next block
.IF LDR_CFG=C_PMG          
          jsr UPD_PROGRESS       ;And then update progress
.ENDIF          
          
GB_DOGET  ldy #0
          lda (BUFRLO),Y
          tay                    ;Backup the yielded value
          
          clc                    ;Advance in the buffer
          lda BUFRLO
          adc #1
          sta BUFRLO
          lda BUFRHI
          adc #0
          sta BUFRHI    
          
          tya                    ;Restore the yielded value
          
          rts        
;===============================================================================
; Read a raw block
;===============================================================================
GET_BLOCK pha
          
          lda W_FLAGS             ;Check flags
          and #CFG_V_F_NOATRACT    ;Suppress ATRACT?
          beq GB_SIOSET           ;No, skip
          lda #0                  ;Yes, reset ATRACT
          sta ATRACT
          
GB_SIOSET lda #$60               ;Cassette
          sta DDEVIC
          lda #0                 
          sta DUNIT              ;Zero unit
          sta DUNUSE             ;Zero unused byte
          sta DAUX1              ;Zero AUX1 byte

          lda #$52
          sta DCOMND             ;No command

          lda #64                ;Indicate READ
          sta DSTATS
           
          lda #<BLOCK_BUFFER     ;Set buffer address
          sta DBUFLO
          lda #>BLOCK_BUFFER
          sta DBUFHI
          lda #35
          sta DTIMLO
          
          lda #<[BUF_LEN-1]      ;Set buffer length
          sta DBYTLO
          lda #>[BUF_LEN-1]
          sta DBYTHI
          
          lda #$80               ;Short gaps between records
          sta DAUX2
          
          jsr SIOV               ;Call SIO
          bpl GB_OK
          
          jmp ERRHNDL
          
GB_OK     lda BLOCK_BUFFER+2     ;Get flag byte
          and #128+64            ;Test for EOF or INIT
          beq GB_NOSPECIAL       ;None of these, continue
          
          lda #60                ;Motor OFF
          sta PACTL
          
GB_NOSPECIAL                     ;Set the standard buffer range
          lda #<[BLOCK_BUFFER+BLK_DOFS]
          sta BUFRLO
          lda #>[BLOCK_BUFFER+BLK_DOFS]
          sta BUFRHI
          lda #<[BLOCK_BUFFER+BLK_DOFS+512]
          sta BFENLO
          lda #>[BLOCK_BUFFER+BLK_DOFS+512]
          sta BFENHI
          
          pla
          rts
;===============================================================================
; Run program
;===============================================================================
RUN_PROGRAM
          jmp (RUNAD)

          
;===============================================================================
; Progress handling
; The progress indication works as follows:
; M1 and M2 indicate first and last position
; M0 indicates the actual progress by moving from M1 to M2
; The positions are set by an authoring tool
; Movement is controlled by movement counter. In general, the movement counter
; indicates how far M0 moves after each block.
;===============================================================================
.IF LDR_CFG = C_PMG
;-------------------------------------------------------------------------------
; Update progress after each block
;-------------------------------------------------------------------------------                                  
UPD_PROGRESS
          pha                                 ;Save A

          lda P_CNTR                          ;Check the step counter
          beq UP_MOVE                         ;If zero, move M0
          
          dec P_CNTR                          ;Decrement the counter                             
          jmp UP_EXIT                         ;Done with progress

UP_MOVE   inc P_X                             ;Increment position
          lda P_X                             ;Get to A
          sta HPOSM0                          ;Update position
          
          lda P_STEP                          ;Reset counter
          sta P_CNTR                          
          
          lda BLOCK_BUFFER+2                  ;Check for EOF
          and #128
          beq UP_EXIT                         ;No EOF, just skip
          
UP_EOF    lda #0                              ;Remove the PMG
          sta GRAFM
          sta HPOSM0
          sta HPOSM1
          sta HPOSM2
          sta PCOLR0
          sta PCOLR1
          sta PCOLR2
          sta PRIOR

UP_EXIT   pla                                 ;Restore A
          rts


;-------------------------------------------------------------------------------
; Initialize progress
;-------------------------------------------------------------------------------
SET_PROGRESS          
          pha
;          
;Set positions of the Ms
          lda CFG_P_INI                        ;Get first position
          sta P_X                              ;Save it
          sta HPOSM0                           ;Set it for M0 and M1
          sta HPOSM1
          lda CFG_P_FIN                        ;Get last position
          sta HPOSM2                           ;Set it for M2
          lda CFG_P_STEP                       ;Get step code
          sta P_CNTR                           ;Save it
          sta P_STEP
;          
;Set graphics pattern for the missiles
          lda #(1+4+8+16+32)
          sta GRAFM
;Set color shadows for the missiles
          lda #12
          sta PCOLR0
          sta PCOLR1
          sta PCOLR2    
;Switch the PMG on
          lda #1
          sta PRIOR
;
          pla          
          rts
;===============================================================================
;Progress data area
;===============================================================================
P_X       .BYTE 0
P_CNTR    .BYTE 0
P_STEP    .BYTE 0

.ENDIF

;===============================================================================
; Error handling
; Note: No need to preserve stack consistency, error handling 
;       always results in warm start.
;===============================================================================
ERRHNDL  lda #$24                ;I/O error - red background

ERRSIG   sta COLOR4              ;Signalize error by changing background
         sta COLOR2
         sta COLBK
         sta COLPF2
         
         lda #60                 ;Switch off the motor
         sta PACTL  
         
         lda #255                ;Wait for any key
         sta CH
WFORKEYL cmp CH
         beq WFORKEYL
         
ERRREST  jmp WARMSV              ;Perform warm reset 

;===============================================================================
; Loader Screen and startup sequence
;===============================================================================
SCREEN
;Other elements of LaF
          lda CFG_BG  ;Set background
          sta COLOR2
          
          lda CFG_LUMA ;Set luminance
          and #CFG_V_LUMA_MASK
          sta COLOR1
          
          lda CFG_SOUNDR ;Set SOUNDR
          sta SOUNDR
          
          lda CFG_LUMA  ;Copy LUMA+flags byte
          sta W_FLAGS
          
          and #CFG_V_F_CRSIN         ;Inhibit cursor when needed
          sta CRSINH
         

;Program title
          lda #9                  ;Requesting PRINT
          sta CIO0_OP
          lda #<CFG_CLRSCR
          sta CIO0_BUFLO
          lda #>CFG_CLRSCR
          sta CIO0_BUFHI
          lda #CFG_V_TITLE_LEN
          sta CIO0_LENLO
          ldx #0                  ;Channel 0
          stx CIO0_LENHI
          jsr CIOV                ;Call CIO
          
          rts
;-------------------------------------------------------------------------------
; Loader startup  - establish the loader as DOS
;-------------------------------------------------------------------------------
STARTUP   ldx #0                  ;Reset cold start flag
          stx COLDST
          inx                     ;Indicate disk boot succeeded
          stx BOOT
          
          lda #<DINI              ;Setup DOSINI
          sta DOSINI
          lda #>DINI
          sta DOSINI+1
                                  ;Falls trough DINI to also setup DOSVEC
          
DINI      lda #<BLTOP             ;DOSINI sets DOSVEC
          sta DOSVEC
          lda #>BLTOP
          sta DOSVEC+1
DO_RTS    rts

;===============================================================================
;General data area
;===============================================================================
W_FLAGS   .BYTE 0

;===============================================================================
; Configuration area, to be zapped
;===============================================================================
CFG_EYE       .BYTE 'C'         ;Eye-catcher
CFG_P_INI     .BYTE 0           ;Progress indicator, initial position
CFG_P_FIN     .BYTE 0           ;Progress indicator, final position
CFG_P_STEP    .BYTE 0           ;Progress indicator, step code
CFG_SOUNDR    .BYTE 0           ;SOUNDR
CFG_BG        .BYTE 0           ;Background
CFG_LUMA      .BYTE 0           ;Luminance 
                                ; .... 1111 Foreground luminance
                                ; 1... .... Cursor off flag
                                ; .1.. .... Atract suppresion flag 
CFG_RESERVE   .BYTE 0           ;Reserve for future use
CFG_CLRSCR    .BYTE 125         ;Clear screen character
CFG_TITLE     .BYTE '                              ' ;30
CFG_EOL       .BYTE $9B         ;End of line

;===============================================================================
; BUFFER
;===============================================================================
          .BYTE 'B'               ;Eye-catcher before buffer
BLOCK_BUFFER

;===============================================================================
; RUN segment
;===============================================================================
.IF BUILD=B_XEX
          RUN BLTOP
          
.ENDIF       
