;===============================================================================
;Omicron Turbo - Express binary loader wrapper
;===============================================================================

;-------------------------------------------------------------------------------
; Equates
;-------------------------------------------------------------------------------
            BUFRLO=50
            BUFRHI=51
            BFENLO=52
            BFENHI=53
            LOADERENTRY=$0800
            LOADERSTART=$0800
            COLDST=$0244
            BOOT=$0009
            CASINI=$0002
            WARMSV=$E474

;-------------------------------------------------------------------------------
; Start of code
;-------------------------------------------------------------------------------
            OPT H-
            ORG 8192
            
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
            JMP LOADERENTRY               ;Jump to the loader

;-------------------------------------------------------------------------------
; Loader BLOB
;-------------------------------------------------------------------------------            
LOADER            
            INS 'omicron_express.xbin'
LOADER_END
LENPTR      .BYTE <[LOADER_END-LOADER],>[LOADER_END-LOADER]            
            
;-------------------------------------------------------------------------------
; Filler up to 860 bytes
;-------------------------------------------------------------------------------
            .REPT (840-(*-MOVDAT))
            .BYTE 0
            .ENDR
; Extension - 20 characters for file name
ENAME       .REPT 20
            .BYTE 0
            .ENDR             