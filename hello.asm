    MOV R1, #0x10000000
    MOV R2, #72
    STR R2, [R1]
    MOV R2, #101
    STR R2, [R1]
    MOV R2, #108
    STR R2, [R1]
    MOV R2, #108
    STR R2, [R1]
    MOV R2, #111
    STR R2, [R1]
    MOV R2, #10
    STR R2, [R1]
loop:
    B loop
