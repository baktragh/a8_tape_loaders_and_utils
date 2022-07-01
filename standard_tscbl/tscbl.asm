;===============================================================================
;TURGEN SYSTEM - TSCBL
;Binary loader for standard (FSK) tape records

;The author has placed this work in the Public Domain, thereby relinquishing all
;copyrights. Everyone is free to use, modify, republish, sell or give away this
;work without prior consent from anybody.
;
;This loader uses the "trailing EOF record trick" - the last 128 bytes of the
;loader are in the EOF block. These 128 bytes are moved from the cassette
;buffer to the intended memory location. The trick allows this loader to be
;only 4 records long
;
;The loader is compatible with XL/XE and pre-XL/XE Atari computers
;
;Using the LDRTYPE symbol, this loader can be assembled 
;to either boot (LDRTYPE=0) or binary (LDRTYPE=1) file. 
;
;Assemble with the ATASM cross-assembler
;
;2019-07-18 Initial version 1.0
;2019-08-12 Shortening based  and updates based on suggestions of
;AtariAge forum members dmsc,xxl,phaeron. Version 1.1
;2022-06-30 Attract suppresion option
;===============================================================================

          .INCLUDE "equates.asm"
.IF LDRTYPE=0          
          LDR_START=[2048-23]
.ELSE
          LDR_START=2048
.ENDIF
          
          
          CIO1_OP   =$0342+16
          CIO1_STAT =$0343+16
          CIO1_BUFLO=$0344+16
          CIO1_BUFHI=$0345+16
          CIO1_LENLO=$0348+16
          CIO1_LENHI=$0349+16
          CIO1_AUX1 =$034A+16
          CIO1_AUX2 =$034B+16
          
          CIO0_OP   =$0342
          CIO0_STAT =$0343
          CIO0_BUFLO=$0344
          CIO0_BUFHI=$0345
          CIO0_LENLO=$0348
          CIO0_LENHI=$0349
          CIO0_AUX1 =$034A
          CIO0_AUX2 =$034B

          *=[LDR_START]
;-------------------------------------------------------------------------------
; Boot header
;-------------------------------------------------------------------------------
.IF LDRTYPE=0
BOOTHEAD  .BYTE 0                 ;Boot flag 0
          .BYTE 3                 ;3 blocks + 1 EOF blocks
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
RELO_P2_L lda  [1024-128],X
          sta  [LDR_START+384-128],X
          dex  
          bmi  RELO_P2_L
.ENDIF         
;-------------------------------------------------------------------------------
; Loader mainline code
;-------------------------------------------------------------------------------
BLTOP     jsr  SCREEN             ;Establish this loader as "DOS", setup screen

          ldx #0                  ;Reset flag indicating 255 255 header found
          stx BL_HDR_FOUND                                  
          
          lda CONSOL              ;Check for the SELECT key
          cmp #5
          bne BLFCLOS             ;If not pressed, continue
          
REBOOT    stx NMIEN               ;Kill NMIs
          jmp COLDSV              ;Perform cold start
          
BLFCLOS   jsr FCLOSE              ;Close channel #1 
          jsr FOPEN               ;Open C: file
          
LOADER_CORE_BEGIN
;===============================================================================
; Read a segment 
;===============================================================================
GETSEG    lda #255                 ;Set fake INIT vector to $FFFF (fake value)
          sta INITAD
          sta INITAD+1
          
          lda CONFIG_ATRACT        ;Attract allowance
          bne GS_STRTA             ;Yes, skip
          sta ATRACT               ;Reset ATRACT
;-------------------------------------------------------------------------------
; Get segment header
;-------------------------------------------------------------------------------
GS_STRTA  ldy #<BL_SEG_HEAD     ;Read the first two bytes of segment header
          lda #>BL_SEG_HEAD
          jsr GETBLK_2
           
          lda BL_SEG_HEAD       ;Check for 255 255
          and BL_SEG_HEAD+1
          cmp #255
          bne GS_ENDA             ;If 255 255 not found, continue
          
          sta BL_HDR_FOUND        ;Indicate header was found
          beq GS_STRTA            ;And then start over
          
GS_ENDA   ;Get rest of the segment header
          ldy #<[BL_SEG_HEAD+2]
          lda #>[BL_SEG_HEAD+2]
          jsr GETBLK_2
          
;-------------------------------------------------------------------------------
; Header (255 255) check 
;-------------------------------------------------------------------------------
          lda BL_HDR_FOUND       ;Check if 255 255 header was found
          bne GS_CALCLN          ;It was, we can continue
          jmp ERRNOBIN           ;If not, this is not a binary load file
;-------------------------------------------------------------------------------
; Calculate length of the segment           
;-------------------------------------------------------------------------------          
GS_CALCLN sec                    ;Subtract start address from end address
          lda BL_SEG_HEAD+2        
          sbc BL_SEG_HEAD+0
          sta CIO1_LENLO
          lda BL_SEG_HEAD+3
          sbc BL_SEG_HEAD+1
          sta CIO1_LENHI
                                 ;Increase the difference by 1 to get length
          inc CIO1_LENLO
          bne GS_GETD
          inc CIO1_LENHI
;-------------------------------------------------------------------------------          
; Read segment data to its location in the memory          
;-------------------------------------------------------------------------------          
GS_GETD   ldy BL_SEG_HEAD
          lda BL_SEG_HEAD+1
          jsr GETBLK
;-------------------------------------------------------------------------------
; Perform jump through INITAD if needed
;-------------------------------------------------------------------------------          
          lda INITAD             ;Check if there was real INIT segment
          and INITAD+1
          cmp #255
          beq POSTINI            ;Skip INIT if $FFFF
          
REALINI   lda #60                ;Switch off the motor
          sta PACTL 
          jsr DOINIT             ;Execute INIT segment code
          lda #52                ;Switch on the motor
          sta PACTL

POSTINI   jmp GETSEG             ;Get another segment

;===============================================================================
;Handle errors in GETBLK.
;===============================================================================
GBERR     cpy #136                  ;Is this EOF ?
          bne ERRHNDL               ;No - handle error
          ldx #255                  ;Yes, this is EOF
          txs                       ;Clear stack
          jsr FCLOSE                ;Close file
          
          jmp (RUNAD)               ;Run the program
;===============================================================================
;Calls GETBLK with a length of 2 bytes.
;===============================================================================
GETBLK_2  ldx #2
          stx CIO1_LENLO
          ldx #0
          stx CIO1_LENHI
          ; Fall through
;===============================================================================
;Subroutine that gets a blocks using CIO. Buffer length of the block must be set
;by the caller, A and Y holds buffer address HI/LO.
;===============================================================================
GETBLK    sta CIO1_BUFHI
          sty CIO1_BUFLO
          lda #7                    ;Requesting CIO READ operation
          jsr CIO_OP1               ;Call CIO on channel 1
                                    ;Check for error
          bmi GBERR                 ;Error occured - handle it
          rts
;===============================================================================
;Emulation of JSR(738)
;===============================================================================
DOINIT    jmp (INITAD)
;===============================================================================
;Main data area
;===============================================================================
CDEV         .BYTE "C:",155       ;File name
BL_SEG_HEAD  .BYTE 0,0,0,0        ;Segment header
BL_HDR_FOUND .BYTE 0              ;Binary file header flag
;===============================================================================
;Subroutine that closes file
;===============================================================================
FCLOSE    lda #12         ;Requesting CIO CLOSE operation with code 12

CIO_OP1   ldx #16         ;Calls CIO one channel 1 and operation in A
          sta CIO1_OP    
          jmp CIOV        ;Call CIO and return

LOADER_CORE_END

;======================================================================
; Subroutine that opens file
;======================================================================
FOPEN     lda #4                  ;Auxiliary value 4 - open for reading
          sta CIO1_AUX1
          
          lda #12                ;And also simulate key press
          sta CH                 
                       
          lda #128               ;Auxiliary value 128 - short IRGs
          sta CIO1_AUX2
          lda #<CDEV             ;Buffer- DEVICE:FILENAME ("C:")
          sta CIO1_BUFLO
          lda #>CDEV
          sta CIO1_BUFHI
          
          lda #3                 ;Requesting CIO OPEN operation with code 3
          jsr CIO_OP1            ;Call CIO in channel 1

          bmi ERRHNDL            ;If error occured, handle it
          rts
;===============================================================================
; Error handling
; Note: No need to preserve stack consistency, error handling 
;       always results in warm start
;===============================================================================
ERRNOBIN lda #$0E                ;Not a binary file - white background
         bne ERRSIG

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
SCREEN    lda CONFIG_BG           ;Set background
          sta COLOR2
          lda CONFIG_FG           ;Set foreground
          sta COLOR1
          lda CONFIG_SNDR         ;Set SOUNDR
          sta SOUNDR
          lda CONFIG_CRSR         ;Set cursor visibility
          sta CRSINH
          
          lda #9                  ;Requesting PRINT
          sta CIO0_OP
          lda #<CONFIG_TITLE
          sta CIO0_BUFLO
          lda #>CONFIG_TITLE
          sta CIO0_BUFHI
          lda #[1+34+1]
          sta CIO0_LENLO
          ldx #0                  ;Channel 0
          stx CIO0_LENHI
          jsr CIOV                ;Call CIO
;-------------------------------------------------------------------------------
; Loader startup  - establish ourselves as DOS
;-------------------------------------------------------------------------------
STARTUP   ldx #0                  ;Reset cold start flag
          stx COLDST
          inx                     ;Indicate disk boot succeded
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
; Program title and configuration bytes
; Defaults: 148,202,1,0,1
;===============================================================================
CONFIG_EYE      .BYTE "@CBL"          ;Eye-catcher
CONFIG_TITLE    .BYTE 125             ;Clear screen
CONFIG_NAME     .BYTE "TSCBL                             " ;34 chars
CONFIG_NAME_EOL .BYTE 155             ;End of line
CONFIG_BG       .BYTE 148             ;Background color             
CONFIG_FG       .BYTE 202             ;Foreground color                  
CONFIG_SNDR     .BYTE 1               ;SOUNDR value                  
CONFIG_CRSR     .BYTE 0               ;Cursor on-off
CONFIG_ATRACT   .BYTE 1               ;Atract mode allowance 
;===============================================================================
; RUN segment
;===============================================================================
.IF LDRTYPE=1
          *=RUNAD
          .BYTE <BLTOP,>BLTOP
.ENDIF