;===============================================================================
; KSO Turbo 2000 - Speedy 2700 binary loader
; Unprotected, simplified version, signal source is automatically detected
; Assemble with the MADS cross-assembler
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
;Code equates
;-------------------------------------------------------------------------------
L00D0       = $00D0
L00D1       = $00D1
L0706       = $0706
L0708       = $0708
L0709       = $0709
L0715       = $0715
L0716       = $0716
L072C       = $072C
L072E       = $072E
L072F       = $072F
L0739       = $0739
L073A       = $073A
L0768       = $0768

             

;===============================================================================
; Mainline code
;===============================================================================
            OPT H-
.IF LDRTYPE=2
;-------------------------------------------------------------------------------
; Cassette boot file prologue
;-------------------------------------------------------------------------------
            ORG ($0700-(BOOT_END-BOOTHEAD))               ;Boot header before the loader
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
            
            lda #<L0813                       ;Set CASINI
            sta CASINI
            lda #>L0813
            sta CASINI+1
            lda #2                            ;Set boot flag
            sta BOOT
            lda #0
            sta COLDST                        ;Warm reset - restart loader
            
            JMP L0813                         ;Jump to entry
BOOT_END
.ELSE
;-------------------------------------------------------------------------------
; Binary load file prologue
;-------------------------------------------------------------------------------

            ORG $0700
.ENDIF

L0700      jsr L07DC

;-------------------------------------------------------------------------------
; Wait for pilot pulse
; This routine can be zapped to use PORTA and different bit mask when the
; signal goes from the joystick port.
;-------------------------------------------------------------------------------
L0703       ldy #$00
L0705       lda #$10
L0707       bit SKSTAT
            bne L0707
            ldx #$00
            stx COLBK
L0711       inx
            bmi L0703
            bit SKSTAT
            beq L0711
            lda #$08
            sta COLBK
            cpx #$36
            bcc L0703
            iny
            bne L0705
JUSTRTS     rts
;-------------------------------------------------------------------------------
; Decode 1 byte and store to the STATUS register
; This routine can be zapped to use PORTA and different bit mask when the
; signal goes from the joystick port.
;-------------------------------------------------------------------------------            
L0726       ldy #$08                 ; Bit counter = 8
L0728       ldx RANDOM               ; For background color
L072B       lda #$10                 ; Bit mask
L072D       bit SKSTAT               ; Check input signal for HI->LO
            bne L072D                ; If HI, keep checking
            stx COLBK                ; Set background color
            
            ldx #$00                 ; Reset timing counter
            
L0737       inx                      ; Tick
            bit SKSTAT               ; Check input signal for LO->HI
            beq L0737
            
            lda #$08                 ; Background color
            sta COLBK
            cpx #$36                 ; Check counter
            bcs L074E                ; Check if pilot signal, if so, mark it
            cpx #$20                 ; Determine one or zero
            rol STATUS
            dey                      ; Decrement bit counter
            bne L0728                ; If not whole byte, next bit
            rts                      ; Return
            
L074E       ldx #$45                 ; Mark pilot signal color
            bne L072B                ; Continue decoding

;-------------------------------------------------------------------------------
; Decoding binary file data
;-------------------------------------------------------------------------------            
L0752       jsr L0703                    ;Wait for pilot pulse

;-------------------------------------------------------------------------------
; Read segment header to FMSZPG+3,4 and FMSZPG+5,6
;-------------------------------------------------------------------------------
L0755       lda #$46
            sta L0768
            
            lda #$00                     ;Prepare to read 4 bytes
            sta FMSZPG+4
            lda #$04
            sta BUFRFL
            
L0762       jsr L0726                    ;Decode 1 byte
            lda STATUS
            
            sta ZCHAIN                   ;Store to desired location. This is
                                         ;zapped
            
            clc                          ;Update checksum
            adc CHKSUM
            sta CHKSUM
            
            lda FMSZPG+3                 ;Check for $FF $FF - EOF
            and FMSZPG+4
            cmp #$FF
            bcs L07D1                    ;EOF occured, go run the program
            
            inc L0768                    ;Increment desired location                     
            dec BUFRFL                   ;Decrement 'bytes to read' counter
            bne L0762                    ;If more bytes to read, continue
            
            inc FMSZPG+5                 ;Increment by one
            bne L0783
            inc FMSZPG+6
;-------------------------------------------------------------------------------
; Read segment data and place to
; FMSZPG+3,4 to (FMSZPG+3,4)-1
;-------------------------------------------------------------------------------            
L0783       jsr L0726                    ;Decode segment byte
            ldy #$00
            lda STATUS
            sta (FMSZPG+3),Y             ;Store to the desried location
            clc
            adc CHKSUM                   ;Update checksum
            sta CHKSUM
            
            inc FMSZPG+3                 ;Increment desired location
            bne L0797
            inc FMSZPG+4
            
L0797       lda FMSZPG+3                 ;Check if segment fully read
            cmp FMSZPG+5
            bne L0783
            lda FMSZPG+4
            cmp FMSZPG+6
            bne L0783                    ;If not, continue with segment bytes
            
            
            jsr L0726                    ;Get another byte - checksum
            lda STATUS
            cmp CHKSUM                   ;Verify checksum
            bne L07CB                    ;If no match, then load error
            
            lda #$00                     ;Reset checksum
            sta CHKSUM
            
            lda INITAD                   ;Check for INITAD change
            ora INITAD+1
            beq L0755                    ;If no change, continue with next seg.
            
            jsr L07FA                    ;Terminate decoding
            jsr L07CE                    ;Execute INIT segment
            
            lda #$00                     ;Reset the INITAD
            sta INITAD
            sta INITAD+1
            jsr L0700                    ;Restart decoding and wait for pilot
            
            beq L0755                    ;If pilot, go decode next segment
L07CB       jmp L0861                    ;No pilot - load error
L07CE       jmp (INITAD)

;-------------------------------------------------------------------------------
; Run the loaded program
;-------------------------------------------------------------------------------
L07D1       lda #$25
            sta PUPBT3
            jsr L07FA                  ;Terminate decoding
            jmp (RUNAD)                ;Run the loaded program
            
;-------------------------------------------------------------------------------
; Prepare for decoding
; - Disable interrupts and DMA
; - Set joystick port 2 for input - for input from JS port
; - Enable COMMAND signal - for input from DATA IN
; - Motor ON
;-------------------------------------------------------------------------------            
L07DC       sei
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
L07FA       lda #$38
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
; Entry point
;-------------------------------------------------------------------------------            
L0813       ldx #$FF                      ;Reset stack
            txs
            inx
            stx INITAD                    ;Clear INITAD
            stx INITAD+1
            
            stx L00D0                     ;Zero memory from $000A to $BBFF 
            lda #$0A
            sta L00D1
            txa
            tay
L0825       sta (L00D0),Y
            iny
            bne L0825
            inc L00D1
            ldx L00D1
            cpx #$BC
            bne L0825
            
            jsr L07DC                     ;Prepare for decoding
            lda PORTA                     ;Check signal at joystick port
            bpl L085E                     ;If no signal, then skip
            
            lda #$0F                      ;Modify the decoding routine so
            sta L0708                     ;it reads from the joystick port
            sta L0715
            sta L072E
            sta L0739
            lda #$D2
            sta L0709
            sta L0716
            sta L072F
            sta L073A
            lda #$10
            sta L0706
            sta L072C
            
L085E       jmp L0752                    ;Go to data decoding

;-------------------------------------------------------------------------------
;Load Error handling
;------------------------------------------------------------------------------
L0861       lda #255                
            sta PORTB               ;Re-enable OS-ROM
            jsr L07FA               ;Terminate decodning
            
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
;-------------------------------------------------------------------------------
LDR_END     
