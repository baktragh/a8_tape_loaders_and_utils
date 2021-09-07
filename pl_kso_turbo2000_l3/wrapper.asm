;======================================================
;KSO Turbo 2000 L3 loader wrapper
;======================================================

;
; Equates
;
            BUFRLO=50
            BUFRHI=51
            BFENLO=52
            BFENHI=53
            COLDST=$0244
            BOOT  =$0009
            CASINI      = $0002
            WARMSV      = $E474
            
            LOADERSTART=2048
             

;
; Start of code
;
            *= 8192
LOADER            
            .incbin l3_loader.bin
LOADER_END


LENPTR      .BYTE <[LOADER_END-LOADER],>[LOADER_END-LOADER]
            
            
; Relocator routine
MOVDAT
            LDA #<LOADER
            STA BUFRLO
            LDA #>LOADER
            STA BUFRHI
            
            LDA #<LOADERSTART
            STA BFENLO
            LDA #>LOADERSTART
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
            lda #0                        ;Warm start
            sta COLDST                    
            lda #02                       ;Cassette boot successfull
            sta BOOT
            lda #<LOADERSTART             ;CASINI to loader entry
            sta CASINI
            lda #>LOADERSTART
            sta CASINI+1
;            
            JMP WARMSV                    ;Warm start
;
; Run segment
;
            *=736
            .BYTE <MOVDAT,>MOVDAT
          
