;*******************************************************************************
;COPY T2/H (C) 2021 BAKTRA Software - Wrapper and loader
;Assemble with MADS
;
; 1. Copy OS ROM to RAM
; 2. Load the copier
; 3. Run the copier
;*******************************************************************************
            ICL 'equates.asm'
            
            ORG $2000
            OPT H+
;-------------------------------------------------------------------------------
; Private equates
;-------------------------------------------------------------------------------
            W_PTR EQU  $81 
            W_SPTR EQU $81
            W_TPTR EQU $83   
            W_EPTR EQU $85        

;===============================================================================
; Mainline code
;=============================================================================== 

;-------------------------------------------------------------------------------
; Copy ROM to RAM
;-------------------------------------------------------------------------------
BGN  LDA #0
     STA SDMCTL  ;Disable DMA
     STA NMIEN   ;Disable NMI
     STA IRQEN   ;Disable IRQ
     STA DMACLT  ;Disable DMA immediately
     SEI         ;Disable CPU interrupts
     
     LDA #$C0       ;Set working pointer to $C000 
     STA W_PTR+1  
     LDY #0
     STY W_PTR
;     
L3   LDA (W_PTR),Y  ;Take one ROM byte
     TAX            ;Transfer to X
     LDA #$FE       ;Get OS ROM MASK
     AND $D301      ;Prepare new PORTB 
     STA $D301      ;Now we have OS ROM off
     TXA            ;Transfer back to A
     STA (W_PTR),Y  ;Store the copied byte
     
     LDA #$01       ;OS ROM ON
     ORA $D301
     STA $D301   
     INY            ;Increment pointer
     CPY #0
     BNE L3         ;Repeat until 256 bytes transferred
     
NOK  INC W_PTR+1    ;Increment pointer HI
     CLC            ;Skip the I/O area
     LDA W_PTR+1      
     CMP #$D0      
     BCC T1      
     CMP #$D8    
     BCC NOK
     
T1   CMP #$00        ;Is everything copied??   
     BNE L3          ;No, continue
     CLC 
     LDA #$40        ;Turn on NMI
     STA NMIEN
     LDA #$F7        ;Enable IRQs
     STA IRQEN      
     LDA #$22
     STA SDMCTL      ;Enable ANTIC DMA
     CLI             ;Enable CPU interrupts
     
;-------------------------------------------------------------------------------
; Load the body of the copier, partially to low RAM, partially to the
; 'international character set area'
;-------------------------------------------------------------------------------
;Switch the ROM off
     LDA #$FE       ;Get OS ROM MASK
     AND $D301      ;Prepare new PORTB 
     STA $D301      ;Now we have OS ROM off

;Point to the beginning of the first segment (skip $FF,$FF)
     lda #<(PAYLOAD_START+2)
     sta W_SPTR
     lda #>(PAYLOAD_START+2)
     sta W_SPTR+1
     ldy #0
     
;Load segment header     
SGHL lda (W_SPTR),Y     ;Get fist address LO
     sta  W_TPTR
     jsr INC_SPTR       
     lda (W_SPTR),Y     ;Get first address HI
     sta  W_TPTR+1
     jsr INC_SPTR
     
     lda (W_SPTR),Y     ;Get last address LO
     sta W_EPTR
     jsr INC_SPTR
     lda (W_SPTR),Y     ;Get last address HI
     sta W_EPTR+1
     jsr INC_SPTR       ;Point to the segment data
     
;Load segment data
SGDL
     lda (W_SPTR),Y      ;Get source byte
     sta (W_TPTR),Y      ;Place to target
     
     lda W_TPTR+1        ;Check if all bytes loaded
     cmp W_EPTR+1
     bne SGDL2
     lda W_TPTR
     cmp W_EPTR
     bne SGDL2
     jmp NXSG            ;All loaded, go get next segment
     
SGDL2 jsr INC_SPTR       ;Contine loading segment
      jsr INC_TPTR
      jmp SGDL     
     
;Continue to the next segment, if any present
NXSG
     jsr INC_SPTR       ;Increment source pointer
     lda W_SPTR+1       ;Check if the payload ends
     cmp #>PAYLOAD_END
     bne SGHL
     lda W_SPTR
     cmp #<PAYLOAD_END
     bne SGHL
;-------------------------------------------------------------------------------     
; Payload loaded
;-------------------------------------------------------------------------------
     jmp $0400
;-------------------------------------------------------------------------------
; Pointer increments
;-------------------------------------------------------------------------------     
INC_SPTR
     inc W_SPTR
     bne INC_SPTR9
     inc W_SPTR+1
INC_SPTR9
     rts
     
INC_TPTR
     inc W_TPTR
     bne INC_TPTR9
     inc W_TPTR+1
INC_TPTR9
     rts

;-------------------------------------------------------------------------------
; Payload binary load file
;-------------------------------------------------------------------------------
            dta c'@COP'                
PAYLOAD_START
            INS 'build\body11.bin'
PAYLOAD_END   EQU *
;-------------------------------------------------------------------------------
; Run segment
;-------------------------------------------------------------------------------

     ORG RUNAD
     dta a(BGN)            
            