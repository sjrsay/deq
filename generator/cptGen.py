
import sys

tabs = { "transitions": 1, "transition": 2, 
        "assignments": 3, "assignment": 4, "assign": 5,
        "guards": 3, "guard": 4, 
        "locations": 1, "location": 2, 
        "alphabet": 1, "constants": 1, 
        "inputs": 2, "outputs": 2, "symbol": 3, "globals": 1, "variable": 2}

def cptGen(n):
    qsI = ["qI"+str(i) for i in range(1,n+2)] # qIi for i = 1,...,n+1
    qsO = ["qO"+str(i) for i in range(1,n+2)] # qOi for i = 1,...,n+1
    qsF = ["qIF", "qOF"] # just to remember their names
    tsI = ["It"+str(i) for i in range(0,n+1)] # Iti for i = 0,1,...,n
    
    res = '<?xml version="1.0" encoding="UTF-8"?>\n<register-automaton>\n'
    res += alphabet(n,tsI)
    res += tabbed('<constants/>\n')
    res += globs(n)
    res += locs(qsI,qsO,qsF)
    res += trans(n,qsI,qsO,qsF,tsI)
    res += '</register-automaton>'
    return res
    
def trans(n,qsI,qsO,qsF,tsI):
    res = tabbed('<transitions>\n')
    # fail loop(s)
    res += tabbed('<transition from="')+qsF[1]+'" to ="'+qsF[0]+'" symbol="OFail"/>\n'
    for t in tsI:
        res += tabbed('<transition from="')+qsF[0]+'" to ="'+qsF[1]+'" symbol="'+t+'"/>\n'
    # skip from O to I
    for i in range(len(qsI)):
        res += tabbed('<transition from="')+qsO[i]+'" to ="'+qsI[i]+'" symbol="OSkip"/>\n'
    # input assignments (populate) on label It0
    for i in range(n):
        res += tabbed('<transition from="')+qsI[i]+'" to ="'+qsO[i+1]+'" symbol="'+tsI[0]+'" params="p">\n'
        res += tabbed('<assignments>\n')
        res += tabbed('<assign to="')+reg(i+1)+'">p</assign>\n'
        res += tabbed('</assignments>\n')
        if i>0:
            res += tabbed('<guards>\n')
            gd = tabbed('<guard>')+andGuard(i)+'</guard>\n'
            res += gd+tabbed('</guards>\n')
        res += tabbed('</transition>\n')
    # read transitions (populate) on label It0
    for i in range(n):
        for j in range(i):
            res += tabbed('<transition from="')+qsI[i]+'" to ="'+qsO[i]+'" symbol="'+tsI[0]+'" params="p">\n'
            res += tabbed('<guards>\n')
            res += tabbed('<guard>p==')+reg(j+1)+'</guard>\n'
            res += tabbed('</guards>\n')
            res += tabbed('</transition>\n')
    # final transitions, on labels Iti, i>0
    for i in range(1,n+1):
        res += tabbed('<transition from="')+qsI[n]+'" to ="'+qsO[n]+'" symbol="'+tsI[i]+'" params="p">\n'
        res += tabbed('<assignments>\n')
        res += tabbed('<assign to="')+reg(i)+'">p</assign>\n'
        res += tabbed('</assignments>\n')
        res += tabbed('<guards>\n')
        gd = tabbed('<guard>')+andGuard(n)+'</guard>\n'
        res += gd+tabbed('</guards>\n')
        res += tabbed('</transition>\n')
        for j in range(1,n+1):
            res += tabbed('<transition from="')+qsI[n]+'" to ="'+qsO[n]+'" symbol="'+tsI[i]+'" params="p">\n'
            res += tabbed('<guards>\n')
            res += tabbed('<guard>p=')+reg(j)+'</guard>\n'
            res += tabbed('</guards>\n')+tabbed('</transition>\n')
    # all ti transitions, i>0, from qIj to qOF, for j in [1,n]
    for i in range(n):
        for j in range(1,n+1):
            res += tabbed('<transition from="')+qsI[i]+'" to ="'+qsF[1]+'" symbol="'+tsI[j]+'"/>\n'
#         if i>0: 
#             res += tabbed('<transition from="')+qsI[i]+'" to ="'+qsF[1]+'" symbol="'+tsI[0]+'" params="p">\n'
#             res += tabbed('<guards>\n')
#             res += tabbed('<guard>')+orGuard(i)+'</guard>\n'
#             res += tabbed('</guards>\n')+tabbed('</transition>\n')
    # t0 transitions from qI(n+1) to qOF 
    res += tabbed('<transition from="')+qsI[n]+'" to ="'+qsF[1]+'" symbol="'+tsI[0]+'"/>\n'
    # ti transitions 
    return res+tabbed('</transitions>\n')

def alphabet(n,tsI):
    res = tabbed('<alphabet>\n')+tabbed('<inputs>\n')
    for t in tsI:
        res += tabbed('<symbol name="')+t+'"><param type="int" name="p"/></symbol>\n'
    res += tabbed('</inputs>\n')+tabbed('<outputs>\n')
    res += tabbed('<symbol name="OSkip"/>\n')
    res += tabbed('<symbol name="OFail"/>\n')
    res += tabbed('</outputs>\n')+tabbed('</alphabet>\n')
    return res
    
def globs(n):
    res = tabbed('<globals>\n')
    for i in range(1,n+1):
        res += tabbed('<variable type="int" name="r')+str(i)+'">0</variable>\n'
    return res+tabbed('</globals>\n')

def locs(qsI,qsO,qsF):
    res = tabbed('<locations>\n')
    res += tabbed('<location name="')+qsI[0]+'" initial="true"/>\n'
    for i in range(1,len(qsI)):
        res += tabbed('<location name="')+qsI[i]+'"/>\n'
    for i in range(len(qsO)):
        res += tabbed('<location name="')+qsO[i]+'"/>\n'
    for i in range(len(qsF)):
        res += tabbed('<location name="')+qsF[i]+'"/>\n'
    return res+tabbed('</locations>\n')

def orGuard(n):
    res = ""
    for j in range(1,n+1):
        res += "p=="+reg(j)
        if j<n: res += " || "
    return res

def andGuard(n):
    res = ""
    for j in range(1,n+1):
        res += "p!="+reg(j)
        if j<n: res += " &amp;&amp; "
    return res

def tabbed(s):
    i = s.find(">")
    j = s.find(" ")
    if i < 0 or (0 < j < i):
        i = j
    j = s.find("/")
    if i < 0 or (1 < j < i):
        i = j
    if s[1]=="/":
        tag = s[2:i]
    else: tag = s[1:i] 
    return tab(tabs[tag])+s

def tab(i):
    return 2*i*' '

def reg(i):
    return "r"+str(i)

print(cptGen(int(sys.argv[1])))
