#!/usr/bin/python3
import random
import string

maxDepth = 5
topLevelStmts = 5
topLevelVars = 5

exprs = [ "+","-","*","%","|","&","!","&&","||","<",">","^",">=","<=","==","<>","term" ]
stmts = [ "while", "repeat", "if", "return", "assign" ]
terms = [ "const", "var" ]

sDepth = 0
eDepth = 0

error = False

alphanum = string.ascii_letters + string.digits
ints = []
strings = []

def genIntExpr():
    global eDepth
    eDepth += 1
    try:
        if eDepth == maxDepth:
            return genIntTerm()

        e = random.choice(exprs)
        match e:
            case "term":
                return genIntTerm()
            case "!":
                return "!" + genIntExpr()
            case _:
                return genIntExpr() + e + genIntExpr()
        return ""
    finally:
        eDepth -= 1

def genStringExpr():
    global eDepth
    eDepth += 1
    try:
        if eDepth == maxDepth:
            return genStringTerm()

        e = random.choice(exprs)
        match e:
            case _:
                return genStringTerm()
        return ""
    finally:
        eDepth -= 1

def genIntStmt():
    global sDepth
    sDepth += 1
    try:
        if sDepth == maxDepth:
            return "return " + genIntExpr() + ";"

        e = random.choice(stmts)
        match e:
            case "return":
                return "return " + genIntExpr() + ";"
            case "if":
                return "if (" + genIntExpr() + ") {" + genIntStmt() + "}"
            case "while":
                return "while (" + genIntExpr() + ") {" + genIntStmt() + "}"
            case "repeat":
                return "repeat (" + genIntExpr() + ") {" + genIntStmt() + "}"
            case "assign":
                return genIntVar() + ":=" + genIntExpr() + ";"
        return ""
    finally:
        sDepth -= 1

def genStringStmt():
    global sDepth
    sDepth += 1
    try:
        if sDepth == maxDepth:
            return "return " + genIntExpr() + ";"

        e = random.choice(stmts)
        match e:
            case _:
                return genStringVar() + ":=" + genStringExpr() + ";"
        return ""
    finally:
        sDepth -= 1


def genIntTerm():
    e = random.choice(exprs)
    match random.choice(terms):
        case "const":
            return genInt()
        case "var":
            return genIntVar()

def genInt():
    return str(random.randint(1,0xFFFF))

def genStringTerm():
    e = random.choice(exprs)
    match random.choice(terms):
        case "const":
            return genString()
        case "var":
            return genStringVar()

def genString():
    return '"' + (''.join(random.choices(alphanum, k=random.randint(1,20))))+'"'

def genFreshVar():
    global error

    if not error and random.randint(0,100) == 1:
        s = genStringVar();
        i = genIntVar();
        if len(s):
            error = True
            return s
        if len(i):
            error=True
            return i

    return random.choice(string.ascii_letters) + (''.join(random.choices(alphanum, k=random.randint(1,20))))

def genIntVar():
    global error

    if not error and random.randint(0,100) == 1:
        error = True
        return genStringVar()

    if not error and random.randint(0,100) == 1:
        error = True
        return genFreshVar()

    global ints
    if not ints:
        return genInt()
    return random.choice(ints)

def genStringVar():
    global error

    if not error and random.randint(0,100) == 1:
        error = True
        return genIntVar()

    if not error and random.randint(0,100) == 1:
        error = True
        return genFreshVar()

    global strings
    if not strings:
        return genString()
    return random.choice(strings)

def genIntGlobal():
    global ints
    fresh = genFreshVar()
    ret = "var " + fresh + " int := " + genIntExpr() + ";"
    ints.append(fresh)
    return ret

def genStringGlobal():
    global strings
    fresh = genFreshVar()
    ret = "var " + fresh + " string := " + genStringExpr() + ";"
    strings.append(fresh)
    return ret

for i in range(topLevelVars):
    if random.choice([True, False]):
        print(genIntGlobal())
    else:
        print(genStringGlobal())

while not error:
    for i in range(topLevelStmts):
        if random.choice([True, False]):
            print(genIntStmt())
        else:
            print(genStringStmt())

print("return 0;")