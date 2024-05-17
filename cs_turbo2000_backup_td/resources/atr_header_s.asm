;Backup T/D
;ATR header - 8MB disk, 

                    OPT h-f+
                    ORG 0

ATR_HEADER_EYE      .word $0296
ATR_HEADER_SIZE     .word $2D00
ATR_HEADER_SECSIZE  .word $0080
ATR_HEADER_SIZE_HI  .word $0000
ATR_HEADER_FLAGS    .byte $00
ATR_HEADER_1STBAD   .word $0000
ATR_HEADER_SPARE5   .byte $00,$00,$00,$00,$00                   

