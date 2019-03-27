;===============================================
;Turbo ROM Loader - diagnostic version
;Wrapper and relocator portion. ROM to RAM copy
;===============================================

;
; Equates
;
            BUFRLO=50
            BUFRHI=51
            BFENLO=52
            BFENHI=53
            
            SOURCE=$CB            
            DEST=SOURCE+2
            OSROM=$C000           ;address of OS ROM start
            OSRAM=$4000           ;address of ROM
            NMIEN=$D40E           ;NMI enable register
            PORTB=$D301           ;memory mgt control

;
; Start of code
;
            *= 8192
LOADER            
            .INCBIN turbo_rom_diagnostic_loader.bin
LOADER_END


LENPTR      .BYTE <[LOADER_END-LOADER],>[LOADER_END-LOADER]

; Move ROM to RAM
MOVROMRAM   LDA     #<OSROM
            STA     SOURCE
            STA     DEST            
            LDA     #>OSROM
            STA     SOURCE+1
            LDA     #>OSRAM
            STA     DEST+1
            LDY     #0
                                     
Pass1       LDA     (SOURCE),Y      
            STA     (DEST),Y
            INY
            BNE     Pass1 
            INC     DEST+1
            INC     SOURCE+1
            BEQ     Swap            
            LDA     SOURCE+1
            CMP     #$D0
            BNE     Pass1           
            LDA     #$D8
            STA     SOURCE+1
            BNE     Pass1           
Swap        PHP                     
            SEI                     
            LDA     NMIEN
            PHA                     
            LDA     #0
            STA     NMIEN           
            LDA     PORTB
            AND     #$FE            
            STA     PORTB           
            
            LDA     #>OSROM
            STA     DEST+1          
            LDA     #>OSRAM
            STA     SOURCE+1
                                    
Pass2       LDA     (SOURCE),Y      
            STA     53274 
            STA     (DEST),Y
            INY
            BNE     Pass2
            INC     SOURCE+1        
            INC     DEST+1
            BEQ     Enable          
            LDA     DEST+1 
            CMP     #$D0
            BNE     Pass2           
            LDA     #$D8
            STA     DEST+1
            BNE     Pass2           
Enable      PLA
            STA     NMIEN           
            
; Disable OS ROM
            sei
            lda     #[0+2+128+32+16]
            sta     PORTB
            cli       
            
     
; Relocator routine
MOVDAT      LDA #<LOADER
            STA BUFRLO
            LDA #>LOADER
            STA BUFRHI
            
            LDA #<$CC00
            STA BFENLO
            LDA #>$CC00
            STA BFENHI
            
            LDY #0
            LDX LENPTR+1
            BEQ MVPART
MVPAGE
            LDA (BUFRLO),Y
            STA (BFENLO),Y
            INY
            BNE MVPAGE
            INC BUFRLO+1
            INC BFENLO+1
            DEX
            BNE MVPAGE
MVPART
            LDX LENPTR
            BEQ MVEXIT
MVLAST
            LDA (BUFRLO),Y
            STA (BFENLO),Y
            INY
            DEX
            BNE MVLAST
MVEXIT

;
; Start loader
;
            JMP $CC00
;
; Run segment
;
            *=736
            .BYTE <MOVROMRAM,>MOVROMRAM
