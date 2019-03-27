;===============================================
;Omicron Turbo - Wrapper for Blockloader
;===============================================

; Local equates
            WRAPPER_START=8192
            BUFRLO=50
            BUFRHI=51
            BFENLO=52
            BFENHI=53
            LOADERENTRY=2048
            LOADERSTART=2048

; Start of code
            *= WRAPPER_START
            
           
; Relocator routine - Entry point
MOVDAT      LDA #<LOADER
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
            
MVEXIT      JMP LOADERENTRY

; Data of the loader            
LOADER            
            .INCBIN "omicron_blockloader.xbin"
LOADER_END
LENPTR      .BYTE <[LOADER_END-LOADER],>[LOADER_END-LOADER]            

; Extension - Make this wrapper 860 bytes long
            .REPT [840-[*-WRAPPER_START]]
            .BYTE 0
            .ENDR
; Extension - 20 characters for file name
ENAME       .REPT 20
            .BYTE 0
            .ENDR
