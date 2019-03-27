;===============================================================================
; RAMBIT Turbo System Loader for segmented binary files
; Modernized version by BAKTRA Software
;
; Changes:
; - Removed custom display list
; - Loading process is indicated by dark and bright green stripes
; - Program name is displayed (max. 16 characters)
; - Loader moved to $0800 to resemble DOS
; - Interrupts are enabled when executing INIT segments
; 
; Notes:
; - This loader loads data with disabled DMA because of the timing issues.
;   The original loader uses Graphics 2 display which does not require
;   that many cycles. This loader keeps Graphics 0 to be compatible with
;   programs that do not set any graphics mode. Unfortunately, Graphics 0
;   requires too many cycles and prevents binary files from loading.
;
; - This loader uses "Data in EOF block" trick to save 1 block
;
; Assembly:
; - Assemble with ATASM 
; - LDRTYPE 0 indicates boot file. LDRTYPE 1 indicates binary file
; - COLORSYSTEM 0 indicates two color system, color changed for each byte.
; - COLORSYSTEM 1 indicates shades of single color changed for each bit.
;===============================================================================

.INCLUDE "equates_segmented.asm"

LDR_START_X = $0800                ; Loader start - binary file
LDR_START_B = $0800-$6             ; Loader start - boot file


.IF LDRTYPE=0
            *= LDR_START_B
            .BYTE  $00,
            .BYTE  $02,
            .WORD LDR_START_B
            .WORD RELO_P2
.ELSE
            *= LDR_START_X
.ENDIF            

.IF LDRTYPE=0
;===============================================================================
; Move last portion of the loader code from cassette buffer
;===============================================================================
RELO_P2   ldx  #128                   ;Move 128 bytes of the EOF block
RELO_P2_L lda  [1024-1],X             ;from cassette buffer
          sta  [LDR_START_B+256-1],X  ;to the intended place
          dex  
          bne  RELO_P2_L
.ENDIF         

;==============================================================================
; Display program name
;==============================================================================
DISP_NAME   ldy #16                ; Display title
NAMELOOP    lda [PROGNAME-1],y
            sta (SAVMSC),y
            dey
            bne NAMELOOP
            
            sty RTCLOK+2
WAITLOOP    lda RTCLOK+2
            cmp #100
            bcc WAITLOOP

;==============================================================================
; Initialization
;==============================================================================
START       ldy #$00               ; Zero Y Register
            sei                    ; Disable Interrupts (IRQs)
            sty NMIEN              ; Disable all NMIs
            sty INITAD             ; Clear INITAD LO
            sty INITAD+1           ; Clear INITAD HI
            sty DMACLT             ; Disable screen display (DMA)

            lda #$34               ; Cassette motor ON
            sta PACTL              ; 
            
            lda VIMIRQ             ; Preserve IRQ handler
            pha                    ; by pushing to the stack
            lda VIMIRQ+1           ; 
            pha                    ; 
            
            lda #<IRQ_H1           ; Set new IRQ vector (one shot) 
            sta VIMIRQ             ; 
            lda #>IRQ_H1           ;
            sta VIMIRQ+1           ;
            
            sty IRQEN              ; Disable all interrputs
            lda #$35               ; Set PORTB   00110 101
            sta PBCTL              ; Motor on,Direction control,A Intr. enable         
            
            lda PORTB              ; Read PORT B
            ldx #$12               ; Set frequency of channel 1
            stx AUDF1              ; 
            ldy #$04               ; Y=$04. Start reading segment header
            lda #$5A               ; Prepare block identification byte in A ($5A)
            sta COLBK              ; Rainbow
            cli                    ; Enable interrputs
            
;==============================================================================
; Block decoding (main routine)
;
; Significant fields:
; - Buffer pointer (where the current byte goes) - BUFRFL,RECVDN
; - Buffer end pointer (sentinel)                - XMTDON,CHKSNT
; - EOF/INIT indicator                           - NOCKSM
; - Current byte                                 - FTYPE
; 
; Y - Indicator of what is being done
;     $04 - Start reading segment header
;     $00 - Read segment data      
;     
;
; How the block is decoded
;
; 0. Start. Y=4.
; 1. If Y=4 (Start reading segment header)
;
;    Read byte, use the byte as new buffer pointer LO
;    Y=3
;    Read byte, if not $FF, use the byte as new buffer pointer HI
;               if $FF, then execute INIT and start over (0)
;                (if INIT nonzero), or just start over (INIT zero)
;    Y=2
;    Read byte, use as new buffer end pointer LO
;    Y=1
;    Read byte, use as new buffer end pointer HI
;    Y=0
;    If buffer end pointer is $00 00, then RUN the program (EOF)
;    
; 2. If Y=0
;    Read byte, place to the buffer, update buffer pointers and repeat
;    this until the buffer is full. If the buffer is full, Y=4. 
;    
;
; Internal structure of the blocks
;
; $FF,$FF,$FF....    - Pilot tone
; $5A                - Identification byte
; $00,$00            - Filler
; $LO,$HI,$LO,$HI    - Buffer range 
;
; Special buffer ranges
; $??,$??,$00,$00    - End of file marker
; $??,$FF,$??,$??    - Execute INIT
;
;==============================================================================            
            
SYNCHRO     cmp FTYPE              ; Wait for identification byte ($5A)
            bne SYNCHRO            ; If not present, start over
            
            ldx #$18               ; Set X to 24 (prepare to read three bytes)
                                   ; This includes the id byte
            
BYTEREAD    cpx #$00               ; Done with reading three bytes?
            bne BYTEREAD           ; No, continue loop
            
            lda FTYPE              ; Get the most recently read byte
            ldx #$08               ; Set X to 8 (prepare to read one byte)
            cpy #$00               ; Check if Y=0
            bne HDR_READ           ; If not, we are reading header and not user data 
            
;-----------------------------------------------------------------------------                        
;Process bytes that belongs to user data part
;-----------------------------------------------------------------------------                        
            sta (BUFRFL),Y         ; Store the most recently read byte to buffer
.IF COLORSYSTEM=0            
            and #1                 ; Odd?
            bne BYTECOLOR1         ; Yes, use primary color
            clc
            lda #0                 ; No, use secondary color
            bcc BYTECOLOR2         ; Skip over
BYTECOLOR1  lda #176               ; Primary color
BYTECOLOR2  sta COLBK              ; Change background
.ENDIF

            lda RECVDN             ; Load buffer pointer HI
            cmp CHKSNT             ; Compare buffer pointer HI against buffer end pointer HI 
            bcc INC_BUF            ; Buffer not full, continue
            bne NEXT_SEG           ; Buffer full, next segment
            lda XMTDON             ; Load buffer end pointer LO
            cmp BUFRFL             ; Compare with buffer pointer LO
            beq NEXT_SEG           ; If equal,  next segment
INC_BUF     inc BUFRFL             ; Increment buffer pointer LO
            bne BYTEREAD           ; No wraparound, continue decoding loop
            inc RECVDN             ; Increment buffer pointer HI
            bne BYTEREAD           ; And continue decoding loop
            
;-----------------------------------------------------------------------------
; Prepare for next segment. 
;-----------------------------------------------------------------------------
NEXT_SEG    lda #$38               ; Instruction at $0799 set to "STA BUFRFL"
            sta STOR_BUFPTR+1      ; 
            
            lda INITAD+1           ; Check if INITAD+1 is nonzero
            bne HAS_INIT           ; If nonzero, skip
            lda INITAD             ; Get INITAD
HAS_INIT    sta NOCKSM             ; Store to EOF/INIT indicator
            bne TERM_BLK           ; If INITAD is nonzero, then RUN/INIT
            ldy #$04               ; Y=4 - Start reading segment header
            bne BYTEREAD           ; Continue decoding loop

;-----------------------------------------------------------------------------
; Determine beginning of next segment
;-----------------------------------------------------------------------------            
HDR_READ    cpy #$03               ; Y=3?
            bne STOR_BUFPTR        ; If not, skip
            cmp #$FF               ; Check for 255
            beq NEXT_SEG           ; Yes, prepare for next segment

;-----------------------------------------------------------------------------
; Prepare for new buffer pointers
;-----------------------------------------------------------------------------
STOR_BUFPTR sta BUFRFL             ; Store new buffer pointer LO
            inc [STOR_BUFPTR+1]    ; Change instruction to change BUFRFL+1
            dey                    ; Decrement Y
            bne BYTEREAD           ; If not zero, continue decoding loop

;-----------------------------------------------------------------------------
; Check for end of file (buffer end pointer is $0000
;-----------------------------------------------------------------------------            
            cpy CHKSNT             ; Check If Y=CHKSNT
            bne BYTEREAD           ; No, continue decoding loop
            cpy XMTDON             ; IF Y=XMTDON
            bne BYTEREAD           ; No, continue decoding loop
            sty NOCKSM             ; Store Y to NOCKSM to indicate RUN program

;-----------------------------------------------------------------------------
; Terminate decoding and RUN or execute INIT segment
;-----------------------------------------------------------------------------            
TERM_BLK    sei                    ; Disable interrputs
            jsr SIOINV             ; Serial input-output initialization
            pla                    ; Restore original IRQ vector
            sta VIMIRQ+1           ; 
            pla                    ; 
            sta VIMIRQ             ; 8D 16 02
            
            sty COLDST             ; Warm start after reset
            sty AUDF1              ; Reset channel 1 frequency
            ldx POKMSK             ; Read interrput mask
            stx IRQEN              ; Re-enable selected interrputs
            ldx #$40               ; Re-enable VBI
            stx NMIEN              ; 8E 0E D4
            
            ldx NOCKSM             ; Check if to run the program 
            beq CHK_RUNAD          ; If zero, go to run the loaded program
            cli                    ; Enable IRQs
            jsr DO_INIT            ; Otherwise execute INIT segment
            jmp START              ; And start over
            
CHK_RUNAD   cli                    ; Enable IRQs
            lda RUNAD+1            ; Check RUNAD+1
            bne RUNIT              ; If nonzero, go to run the program
            lda RUNAD              ; Check RUNAD
            bne RUNIT              ; If nonzero, run program
            
            jmp WARMSV             ; No run adddress, WARM START

RUNIT       sty COLDST             ; Warm start after reset
            lda #1                 ; Successful disk boot
            sta BOOT
            jmp (RUNAD)            ; Run program

DO_INIT     jmp (INITAD)           ; Execute INIT segment JSR(INITAD) 




;==============================================================================
; Filler
;==============================================================================            
.IF ($0900-*)>0
            .REPT ($0900-*)
            .BYTE $00
            .ENDR
.ENDIF            
;==============================================================================
; IRQ handlers (must be on same page!)
;==============================================================================            

IRQ_H1      pha                    ; 48
            tya                    ; 98
            pha                    ; 48
            sta STIMER             ; 8D 09 D2
            lda #<IRQ_H2           ; A9 2A
            sta VIMIRQ             ; 8D 16 02
            lda PORTB              ; AD 01 D3
            lda #$01               ; A9 01
            ldy #$00               ; A0 00
            beq IRQ_CE             ; F0 1D
            
IRQ_H2      pha                    ; 48
            tya                    ; 98
            pha                    ; 48
            ldy #$00               ; A0 00
            lda #$01               ; A9 01
            bit IRQST              ; 2C 0E D2
            beq IRQ_CE             ; F0 11
            sta STIMER             ; 8D 09 D2
            cpy BPTR               ; C4 3D
            rol FTYPE              ; 26 3E
            sta BPTR               ; 85 3D
.IF COLORSYSTEM=1            
            txa
            ora #176
            sta COLBK
.ENDIF            
            dex                    ; CA
            lda PORTB              ; AD 01 D3
IRQ_TERM    pla                    ; 68
            tay                    ; A8
            pla                    ; 68
            rti                    ; 40
IRQ_CE      sty IRQEN              ; 8C 0E D2
            sty BPTR               ; 84 3D
            sta IRQEN              ; 8D 0E D2
            beq IRQ_TERM           ; F0 F2
            
;==============================================================================
; Data area
;==============================================================================                        
PROGNAME   .SBYTE "RAMBIT BL......."

;==============================================================================
; RUN Segment
;==============================================================================                        
.IF LDRTYPE=1
            *= $02E0
            .WORD LDR_START_X
.ENDIF
