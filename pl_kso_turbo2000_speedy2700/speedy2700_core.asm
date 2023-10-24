;===============================================================================
; KSO Turbo 2000 - Speedy 2700 binary loader
; - Unprotected, simplified version
; - Improved signal source detection
;
; Assemble with the MADS cross-assembler
; A portion of the source code is self-modifying, program cannot reside in ROM 
;
; File format:

;  1. Start block with pilot tone (pilot tone pulses)
;  2a. Segment header 1 - Word. First address of the segment 
;      If the addres is $FFFF then this is EOF. Block ends, file ends.
;  2b. Segment header 2 - Word. Last address of the segment 
;  3. Segment data - bytes of the segment data
;  4. Segment checksum - Sum of all bytes of the segment header and segment data
;                        modulo 256
;  5. If the segment changes INITAD, then block ends. Goto 1
;  6. Goto 2a
;
;  Assembler output
;  LDRTYPE = 1 Binary load file
;  LDRTYPE = 2 Cassette boot file
;===============================================================================
            ICL 'equates.asm'

;-------------------------------------------------------------------------------
; Private EQUs
;-------------------------------------------------------------------------------
            ZP_PTR_LO   = $D0
            ZP_PTR_HI   = $D1 
;===============================================================================
; Mainline code
;===============================================================================
            OPT H-
.IF LDRTYPE=2
;-------------------------------------------------------------------------------
; Cassette boot file prologue
;-------------------------------------------------------------------------------
            ORG ($0700-(BOOT_END-BOOTHEAD))   ;Boot header before the loader
; 
            LDR_SIZE = (LDR_END-BOOTHEAD)     ;Count number of blocks
            BLK_NUM = (LDR_SIZE / 128)
            BLK_BYTES = (BLK_NUM * 128)
            .IF BLK_BYTES<LDR_SIZE
            BLK_FNUM = (BLK_NUM + 1)
            .ELSE
            BLK_FNUM = (BLK_NUM)
            .ENDIF
            
BOOTHEAD    .BYTE $00                         ;Boot flag
            .BYTE BLK_FNUM                    ;Number of blocks
            .BYTE <BOOTHEAD,>BOOTHEAD         ;Load address
            .BYTE <JUSTRTS,>JUSTRTS           ;Init address (nothing)
            
            lda #<ENTRY_POINT                 ;Set CASINI
            sta CASINI
            lda #>ENTRY_POINT
            sta CASINI+1
            lda #2                            ;Set boot flag
            sta BOOT
            lda #0
            sta COLDST                        ;Warm reset - restart loader
            
            JMP ENTRY_POINT                   ;Jump to entry
BOOT_END
.ELSE
;-------------------------------------------------------------------------------
; Binary load file prologue
;-------------------------------------------------------------------------------

            ORG $0700
.ENDIF

ENTRY_POINT    ldx #$FF                      ;Reset stack
               txs
               jsr LDR_SETUP
.IF TITLE=1
               jsr TITLE_SCREEN
.ENDIF
;-------------------------------------------------------------------------------
; Loader
;-------------------------------------------------------------------------------               
BEGIN_LOADING  jsr DEC_INIT
               jmp BL_BEGIN

;-------------------------------------------------------------------------------
; Wait for pilot pulse
; This routine can be zapped to use PORTA and different bit mask when the
; signal goes from the joystick port. If there is no edge for some time,
; the source of the signal is switched.
;-------------------------------------------------------------------------------
DPILOT_BEGIN   ldy #$00                       ;Reset pilot tone counter
               jsr SWITCH_SIGNAL_SOURCE       ;Switch signal source
               
DPILOT_0       ldx #$00                       ;Reset mini counter
DPILOT_1       inx                            ;Increment mini counter                             
               beq DPILOT_BEGIN               ;Timout, start over
               
L_MASK_A       lda #$10                       ;Get mask for 1                     
L_BIT_A        bit SKSTAT                     ;Compare with signal
               bne DPILOT_1                   ;If 1, then loop
               
               ldx #$00                       ;Reset mini counter
               stx COLBK                      ;Blank background
DPILOT_MID     inx                            ;Increment mini counter
               bmi DPILOT_BEGIN               ;Timeout, start over
L_BIT_B        bit SKSTAT                     ;Compare with signal
               beq DPILOT_MID                 ;If 0, then loop
               lda #$08                       ;Background = gray
               sta COLBK
               cpx #$36                       ;Check duration of the pulse
               bcc DPILOT_BEGIN               ;Too long, start over
               iny                            ;Inc pilot tone counter
               bne DPILOT_0                   ;Less than 256, keep in pilot
JUSTRTS        rts
;-------------------------------------------------------------------------------
; Decode 1 byte and store to the STATUS register
; This routine can be zapped to use PORTA and different bit mask when the
; signal goes from the joystick port.
;-------------------------------------------------------------------------------            
DBYT_BEGIN  ldy #$08                 ; Bit counter = 8
DBYT_PULS   ldx RANDOM               ; For background color
L_MASK_C    lda #$10                 ; Bit mask
L_BIT_C     bit SKSTAT               ; Check input signal for HI->LO
            bne L_BIT_C              ; If HI, keep checking
            stx COLBK                ; Set background color
            
            ldx #$00                 ; Reset timing counter
            
DBYT_PUMID  inx                      ; Tick
L_BIT_D     bit SKSTAT               ; Check input signal for LO->HI
            beq DBYT_PUMID
            
            lda #$08                 ; Background color
            sta COLBK
            cpx #$36                 ; Check counter
            bcs DBYT_PILOT           ; Check if pilot signal, if so, mark it
            cpx #$20                 ; Determine one or zero
            rol STATUS
            dey                      ; Decrement bit counter
            bne DBYT_PULS            ; If not whole byte, next bit
            rts                      ; Return
            
DBYT_PILOT  ldx #$45                 ; Mark pilot signal color
            bne L_MASK_C             ; Continue decoding

;-------------------------------------------------------------------------------
; Decoding binary file data
;-------------------------------------------------------------------------------            
BL_BEGIN    jsr DPILOT_BEGIN         ;Wait for pilot pulse

;-------------------------------------------------------------------------------
; Read segment header to FMSZPG+3,4 and FMSZPG+5,6
;-------------------------------------------------------------------------------
BL_SH_1     lda #$46
            sta BL_STORB+1
            
            lda #$00                     ;Prepare to read 4 bytes
            sta FMSZPG+4
            lda #$04
            sta BUFRFL
            
BL_SH_2     jsr DBYT_BEGIN               ;Decode 1 byte
            lda STATUS
            
BL_STORB    sta ZCHAIN                   ;Store to desired location. This is
                                         ;zapped
            clc                          ;Update checksum
            adc CHKSUM
            sta CHKSUM
            
            lda FMSZPG+3                 ;Check for $FF $FF - EOF
            and FMSZPG+4
            cmp #$FF
            bcs BL_RUNIT                 ;EOF occured, go run the program
            
            inc BL_STORB+1               ;Increment desired location                     
            dec BUFRFL                   ;Decrement 'bytes to read' counter
            bne BL_SH_2                  ;If more bytes to read, continue
            
            inc FMSZPG+5                 ;Increment by one
            bne BL_SD_1
            inc FMSZPG+6
;-------------------------------------------------------------------------------
; Read segment data and place to
; FMSZPG+3,4 to (FMSZPG+3,4)-1
;-------------------------------------------------------------------------------            
BL_SD_1     jsr DBYT_BEGIN               ;Decode segment byte
            ldy #$00
            lda STATUS
            sta (FMSZPG+3),Y             ;Store to the desried location
            clc
            adc CHKSUM                   ;Update checksum
            sta CHKSUM
            
            inc FMSZPG+3                 ;Increment desired location
            bne BL_SD_2
            inc FMSZPG+4
            
BL_SD_2     lda FMSZPG+3                 ;Check if segment fully read
            cmp FMSZPG+5
            bne BL_SD_1
            lda FMSZPG+4
            cmp FMSZPG+6
            bne BL_SD_1                  ;If not, continue with segment bytes
            
            
            jsr DBYT_BEGIN               ;Get another byte - checksum
            lda STATUS
            cmp CHKSUM                   ;Verify checksum
            bne BL_NOPILOT               ;If no match, then load error
            
            lda #$00                     ;Reset checksum
            sta CHKSUM
            
            lda INITAD                   ;Check for INITAD change
            ora INITAD+1
            beq BL_SH_1                  ;If no change, continue with next seg.
            
            jsr DEC_TERM                 ;Terminate decoding
            jsr BL_DOINIT                ;Execute INIT segment
            
            lda #$00                     ;Reset the INITAD
            sta INITAD
            sta INITAD+1
            jsr BEGIN_LOADING            ;Restart decoding and wait for pilot
            
            beq BL_SH_1                  ;If pilot, go decode next segment
BL_NOPILOT  jmp LD_ERROR                 ;No pilot - load error
BL_DOINIT   jmp (INITAD)

;-------------------------------------------------------------------------------
; Run the loaded program
;-------------------------------------------------------------------------------
BL_RUNIT    lda #$25
            sta PUPBT3
            jsr DEC_TERM                ;Terminate decoding
            jmp (RUNAD)                 ;Run the loaded program
            
;-------------------------------------------------------------------------------
; Prepare for decoding
; - Disable interrupts and DMA
; - Set joystick port 2 for input - for input from JS port
; - Enable COMMAND signal - for input from DATA IN
; - Motor ON
;-------------------------------------------------------------------------------            
DEC_INIT    sei
            lda #$00
            sta NMIEN
            sta DMACLT
            sta CHKSUM
            lda #$38
            sta PACTL
            lda #$60
            sta PORTA
            lda #$34
            sta PACTL
            sta PBCTL
            rts
;-------------------------------------------------------------------------------
; Terminate for decoding
; - Disable COMMAND signal
; - Disable input from joystick port
; - Motor OFF
; - Re-enable interrupts
;-------------------------------------------------------------------------------            
DEC_TERM    lda #$38
            sta PACTL
            lda #$00
            sta PORTA
            lda #$3C
            sta PACTL
            sta PBCTL
            lda #$C0
            sta NMIEN
            cli
            rts
;-------------------------------------------------------------------------------
; Loader setup
;-------------------------------------------------------------------------------            
LDR_SETUP   ldx #0
            stx INITAD                    ;Clear INITAD
            stx INITAD+1
            
            stx ZP_PTR_LO                 ;Zero memory from $0A00 to $BBFF 
            lda #$0A
            sta ZP_PTR_HI
            txa
            tay
L_MEMCLR    sta (ZP_PTR_LO),Y
            iny
            bne L_MEMCLR
            inc ZP_PTR_HI
            ldx ZP_PTR_HI
            cpx #$BC
            bne L_MEMCLR
            rts

;-------------------------------------------------------------------------------
; Switch signal source SIO->JS or JS->SIO
;-------------------------------------------------------------------------------
SWITCH_SIGNAL_SOURCE
           pha
           lda L_MASK_A+1              ;Check current bit mask
           cmp #128                    ;Is that of joystick port?
           beq SW_TO_SIO               ;Yes, then switch to SIO
            
SW_TO_JS   lda #<PORTA                 ;Push PORTA low byte
           pha
           lda #>PORTA                 ;Push PORTA high byte
           pha
           lda #128                    ;Push Bit Mask
           pha
           jmp SW_ZAP
            
SW_TO_SIO  lda #<SKSTAT
           pha
           lda #>SKSTAT                ;Push SKSTAT high byte
           pha
           lda #$10                    ;Push Bit Mask
           pha

SW_ZAP     pla                         ;Get bit mask
           sta L_MASK_A+1              ;Zap instructions
           sta L_MASK_C+1
           pla                         ;Get PORTA/SKSTAT high byte
           sta L_BIT_A+2               ;Zap instructions
           sta L_BIT_B+2
           sta L_BIT_C+2
           sta L_BIT_D+2 
           pla                         ;Get PORTA/SKSTAT low byte
           sta L_BIT_A+1               ;Zap instructions
           sta L_BIT_B+1
           sta L_BIT_C+1
           sta L_BIT_D+1 
;
           pla 
           rts                       

;-------------------------------------------------------------------------------
;Load Error handling
;------------------------------------------------------------------------------
LD_ERROR    lda #255                
            sta PORTB               ;Re-enable OS-ROM
            jsr DEC_TERM            ;Terminate decodning
            
            lda #$24                ;I/O error - red background and border
            sta COLOR4              
            sta COLOR2
            sta COLBK
            sta COLPF2
            
            lda #255                ;Reset keys
            sta CH                  
            
WFORKEYL    cmp CH                  ;Wait for any key
            beq WFORKEYL
         
ERRREST     jmp WARMSV              ;Perform warm reset

.IF TITLE=1
;-------------------------------------------------------------------------------
; Title screen
;-------------------------------------------------------------------------------
          CIO0_OP   =$0342
          CIO0_STAT =$0343
          CIO0_BUFLO=$0344
          CIO0_BUFHI=$0345
          CIO0_LENLO=$0348
          CIO0_LENHI=$0349
          CIO0_AUX1 =$034A
          CIO0_AUX2 =$034B

TITLE_SCREEN   
          lda #$3C
          sta PACTL
 
          lda #9                  ;Requesting PRINT
          sta CRSINH
          sta CIO0_OP
          lda #<TITLE_CHARS
          sta CIO0_BUFLO
          lda #>TITLE_CHARS
          sta CIO0_BUFHI
          lda #<TITLE_CHARS_LEN
          sta CIO0_LENLO
          ldx #0                  ;Channel 0
          stx CIO0_LENHI
          jsr CIOV                ;Call CIO

          lda #0
          sta RTCLOK+2
DLOOP     lda RTCLOK+2
          cmp #200
          bcc DLOOP
          rts

TITLE_CHARS .BYTE 125
            .BYTE 29,29,29,29,29
            .BYTE 31,31,31,31,31,31,31,31,31,31,31,31
            .BYTE 'SPEEDY 2700'
            .BYTE 29,29
            .BYTE 30,30,30,30,30,30,30,30,30,30,30,30,30,30,30
            .BYTE 'File''s Turbo Loader'
            .BYTE 29
            .BYTE 30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30
            .BYTE '-------------------'
            .BYTE 29
            .BYTE 30,30,30,30,30,30,30,30,30,30
            .BYTE 30,30,30,30,30,30,30,30,30,30,30
            .BYTE '(C)*AJEK  WARSZAWA 1990' 
            .BYTE 155
            
          TITLE_CHARS_LEN = *-TITLE_CHARS
.ENDIF
;-------------------------------------------------------------------------------
LDR_END     
