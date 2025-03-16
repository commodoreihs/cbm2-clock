* = $0400

KEYDOWN = $06FD ; last key pressed (used for debounce)
CHROUT  = $FFD2
SCNKEY  = $FF9F

START JSR $E24D ; enable graphics mode
      LDA #$01
      SEI       ; don't clear this until we set our irq handler
      STA $00   ; if an interrupt fires once the execution bank is
      STA $01   ; changed and before we set up our handler, we die
      JMP $2400
      BRK

LSCNKY LDX #$0F
       SEI
       STX $00
       STX $01
       JSR SCNKEY
       CMP KEYDOWN
       BNE NEWKEY
       LDA #$FF
       LDX #$01
       STX $00
       STX $01
       CLI
       RTS
NEWKEY STA KEYDOWN
       LDX #$01
       STX $00
       STX $01
       CLI
       RTS 

LCHROT LDX #$0F
       SEI 
       STX $00
       STX $01
       LDX #$16   ; set the row height explicitly
       STX $CA
       JSR CHROUT
       LDX #$01
       STX $00
       STX $01
       CLI
       RTS
