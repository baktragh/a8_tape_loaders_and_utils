;===============================================
;Turbo ROM Binary Loader
;Simple wrapper without relocation
;===============================================
            *=2048 
LOADER            
            .INCBIN turbo_rom_binload.bin
LOADER_END
            *=736
            .BYTE <2048,>2048
           
