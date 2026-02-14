import re
from tkinter import font
from tkinter import *
"""
findall, search, split, sub, finditer
"""

#string = "ADD r0, (r1), r2"

#print(re.split(" |, |\(|\)", string))

x = "0101"
y = "1101"

x_ = ""
for i in range(len(x)):
    if (x[i]=='0' and y[i] == '0') or (x[i]=='1' and y[i]=='1'):
        x_ += '0'
    else:
        x_ += '1'
print(x_)

print(not('1'))
root = Tk()
fonts_list = list(font.families())
for i in fonts_list:
    print(i)
print("================================================")
print(font.names())
