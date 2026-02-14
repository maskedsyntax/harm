MOV R1, #0
MOV R2, #10
loop:
ADD R1, R1, #1
CMP R1, R2
BLT loop
