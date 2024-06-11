;Backup T/D
;ATR identification sector, pristine indicator sector

                    OPT h-f+
                    ORG 0

ATR_ID_SECT_TITLE   dta c'TURGEN BACKUP T/D 1.1.0'
ATR_ID_SECT_TITLE_L equ *-ATR_ID_SECT_TITLE
                   .REPT 128-ATR_ID_SECT_TITLE_L
                    .byte 0
                   .ENDR

ATR_PRISTINE_SECT  .REPT 128
                   .byte $55
                   .ENDR

