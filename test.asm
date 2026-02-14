    LDR R0, =0x1234
    BL func
    MOV R1, #42
    B end

func:
    MOV R2, #100
    MOV PC, LR

end:
    MOV R3, #7
