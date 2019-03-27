;===============================================================================
; RAMBIT Turbo System Loader for monolithic binary files
; Modernized version by BAKTRA Software
;
; Changes:
; - Program name is displayed (max. 16 characters)
; - Loading process is indicated by dark and bright green stripes displayed
;   on screen
; - DMA is disabled
; - To fit to three cassette blocks, "Data in EOF block is used"
;
; Notes:
; This loader is a skeleton and cannot be used as it is
; An external program must update the following:
; - Buffer range (RB_BUFLO...)
; - Instruction that set run address (stored in RAMLO)
; - PROGNAME field (optional)
;
; Assembly
; - Assemble with XASM 
; - LDRTYPE 0 indicates boot file. LDRTYPE 1 indicates binary file
; - COLORSYSTEM 0 indicates two color system, color changed for each byte.
; - COLORSYSTEM 1 indicates shades of single color changed for each bit.
;===============================================================================

 ICL "equates_mono.asm"
;
; Code equates
;
L008B       EQU $008B

;
; Start of code
;

             ORG $0080

 IFT LDRTYPE==0
             OPT H-
 ELS
             OPT H+
             ORG $0080
 EIF

 IFT LDRTYPE==0          
;Boot header
             DTA $01              ;Boot flag
             DTA $02              ;2 blocks           
             DTA $80,$00          ;Load address
             DTA $86,$91          ;Initialization address 
            
;-------------------------------------------------------------------------------
; Move last portion of the loader code from cassette buffer
;-------------------------------------------------------------------------------
RELO_P2     ldx  #128               ;Move 128 bytes of the EOF block
RELO_P2_L   lda  [1024-1],X         ;from cassette buffer
            sta  [$80+256-1],X      ;to the intended place
            dex  
            bne  RELO_P2_L            
 EIF

;-------------------------------------------------------------------------------
; Real program start
;------------------------------------------------------------------------------- 
START       ldx #$E0             ; Set RAMLO to $22E0
            stx RAMLO            ; 
            lda #$22             ; 
            sta RAMLO+1          ; 
            bne MAIN             ; And then jump to the main routine
            
RB_BUFLO    DTA $00              ;Buffer pointer LO    ($0A00)
RB_BUFHI    DTA $0A              ;Buffer pointer HI
RB_BFENLO   DTA $82              ;Buffer end pointer LO ($2382)
RB_BFENHI   DTA $23              ;Buffer end pointer HI

;===============================================================================
; Interrupt handlers
;===============================================================================

;First one-shot handler
IHANDLER1   pha                    ; Preserve A
            sty LOMEM+1            ; Zero checksum
            sta STIMER             ; Set value for timer (countdown to zero)
            lda #<IHANDLER2        ; Set new IRQ vector
            sta VIMIRQ             ; 
            lda PORTB              ; Read port B
            
            lda #$01               ; Jump to end of interrupt handler 
            bne IHANDLEREND        ; 

;Normal handler            
IHANDLER2   pha                    ; Preserve A
            lda #$01               ; Check IRQ status (timer 1)
            bit IRQST              ; 
            beq IHANDLEREND        ; If zero, jump to end of interrupt handler
            sta STIMER             ; Set value for timer (countdown to zero)
            cpy L008B              ; Compare $8B with 0
            rol LOMEM              ; Insert another bit (0 or 1)
            sta L008B              ; Store to $8B
 IFT COLORSYSTEM==1            
            txa
            ora #176
            sta COLBK
 EIF                        
            dex                    ; Decrement X
            lda PORTB              ; Read PORTB
            pla                    ; Restore A
            rti                    ; Return from interrupt

;Common end            
IHANDLEREND sty IRQEN              ; Disable interrupts
            sty L008B              ; Clear $8B
            sta IRQEN              ; IRQ enabled is timer 1 
            pla                    ; Restore A
            rti                    ; Return from interrupt
;===============================================================================            
; Main routine
;===============================================================================            
MAIN        ldy #16                ; Display title
NAMELOOP    lda [PROGNAME-1],y
            sta (SAVMSC),y
            dey
            bne NAMELOOP
            
            sty RTCLOK+2
WAITLOOP    lda RTCLOK+2
            cmp #100
            bcc WAITLOOP
            
            sei                    ; Disable hardware IRQ
            ldy #$00                
            ldx #$01                
            lda VIMIRQ             ; Push Immediate IRQ vector to the stack
            pha                     
            lda VIMIRQ+1            
            pha                     
            
            lda #<IHANDLER1        ; Set new Immediate IRQ vector ($0094)
            sta VIMIRQ             ; 
            sty IRQEN              ; Disable all IRQs
            sty NMIEN              ; Disable all NMIs (DLI,VBI,RESET)
            sty DMACLT             ; Disable screen display (DMA)
            sty VIMIRQ+1           ; 
            
            ldx #$34               ;Cassette Motor ON
            stx PACTL
            inx                    ;Set X to $35
            stx PBCTL              ;Direction control,A Intr. enable         
            
            lda PORTB              ; Read PORTB
            ldx #$12               ; Set frequency of channel 1
            stx AUDF1              ; 
            cli                    ; Enable IRQs
            
            lda #$5A               ; Check LOMEM for $5A
            sta COLBK              ; Color
            
SYNCHRO     cmp LOMEM              ; 
            bne SYNCHRO            ; If not equal, loop
            
            ldx #$08               ; Loop (X is bit counter)
BYTEREAD    cpx #$00               ; 
            bne BYTEREAD           ; Until bit counter is zero
            
            lda LOMEM              ; Check current byte
            ldx #$08               ; Set bit counter to 8
            sta (RB_BUFLO),Y       ; Store current byte to buffer
            eor LOMEM+1            ; Eor with checksum
            sta LOMEM+1            ; Store updated checksum
 IFT COLORSYSTEM==0
            and #1                 ; Odd?
            bne BYTECOLOR1         ; Yes, use primary color
            clc
            lda #0                 ; No, use secondary color
            bcc BYTECOLOR2         ; Skip over
BYTECOLOR1  lda #176               ; Primary color
BYTECOLOR2  sta COLBK              ; Change background
 EIF            
            inc RB_BUFLO           ; Increment buffer pointer at $90,$91
            bne CHKHIBUF           ; 
            inc RB_BUFHI           ; 
            
CHKHIBUF    lda RB_BUFHI           ; Check high buffer pointer
            cmp RB_BFENHI          ; Check for end of file (ptr at $92,93) 
            bne BYTEREAD           ; 
            lda RB_BUFLO           ; 
            cmp RB_BFENLO          ; 
            bne BYTEREAD           ; If not EOF, continue
            
WTERM       cpx #$00               ; Loop 
            bne WTERM              ; Until X=0
            
            sei                    ; Disable IRQs
            pla                    ; Restore original IRQ vector
            sta VIMIRQ+1           ; 
            pla                    ; 
            sta VIMIRQ             ; 
            sty AUDF1              ; No frequency
            sty IRQEN              ; Disable interrupts
            lda PORTB              ; Read PORTB
            lda POKMSK             ; Read original pokey mask
            sta IRQEN              ; Re-enable previously enabled interrupts
            
            lda #$40               ; Re-Enable VBI
            sta NMIEN              ; 
            lda #$3C               ; Set PORTA and PORTB 
            sta PACTL              ; Motor off
            sta PBCTL              ; 
            cli                    ; Enable IRQs
            lda LOMEM+1            ; Read checksum
            beq FILL1              ; If zero, run program
            jmp COLDSV             ; Otherwise error and cold start
            
FILL1       ldx #[$FF-$80]         ; Clear storage from $80 to $FF
FILL_LOOP1  sta [$80-$1],x         ; 
            dex                    ; 
CLREND      bne FILL_LOOP1         ; Loop until X=0
            
FILL_LOOP2  sta [$100],x           ; Clear storage from $100 to CLREND
            inx                    ; Increment X
            cpx #[CLREND-$100]     ; Done ?
            bne FILL_LOOP2         ; No, loop

RUNIT       sty COLDST             ; Reset cold start flag
            lda #1                 ; Indicate successful disk boot
            sta BOOT
            jmp (RAMLO)            ; Run program

;===============================================================================            
; Data area
;===============================================================================                        
PROGNAME   DTA d'RAMBIT..........'

 IFT LDRTYPE=1
            *= $02E0
            DTA a(START)
 EIF
         
