import re

# ins will have all the instructions 
# each member is a separate instruction
ins = []
while True:
    line = input()
    if line:
        ins.append(line)
    else:
        break

#print(ins)
#for i in ins:
#    for j in i.split(" "):
#        if(j=="ADD"):
#            print("YES")
#        else:
#            print("NO")

"""
Instruction: BIC r0, r1, r2, LSL #1 => BIC is Add with complement
Initial value::
    r1 = 0b1111
    r2 = 0b0101

    What will be the content of register r0?

"""

# ========================================================================>
"""
Functions for logical operation
"""

def comp(x):
    x_ = ""
    for i in range(len(x)):
        if x[i] == '1':
            x_ += '0'
        elif x[i] == '0':
            x_ += '1'
    return x_

def logicalOr(x, y):
    # x = 01010011
    # y = 11011001
    x_ = ""
    for i in range(len(x)):
        if x[i]=='0' and y[i] == '0':
            x_ += '0'
        else:
            x_ += '1'
    return x_

def logicalAnd(x, y):
    x_ = ""
    for i in range(len(x)):
        if x[i]=='1' and y[i] == '1':
            x_ += '1'
        else:
            x_ += '0'
    return x_

def logicalXor(x, y):
    x_ = ""
    for i in range(len(x)):
        if (x[i]=='0' and y[i] == '0') or (x[i]=='1' and y[i]=='1'):
            x_ += '0'
        else:
            x_ += '1'
    return x_


# ========================================================================>





def BIC(op, in1, in2):
    not_in2 = comp(in2)
    res = logicalAnd(in1, not_in2)
    print(res)

r1 = input("Enter the value for r1: ")
r2 = input("Enter the value for r2: ") 
r0 = ""

#for i in ins:
#    j = i.split(" ")
#    if j[0]=="BIC":
#        BIC(r0, r1, r2)
#    else:
#        print("Option not available")

for i in ins:
    inst_list = re.split(" |, |\(|\)", i)
    if inst_list[0] == "BIC":
        BIC(r0, r1, r2)
    else:
        print("Option not available")
