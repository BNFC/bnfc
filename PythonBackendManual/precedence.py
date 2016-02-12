import pyparsing as pp

Exp0 = pp.Forward()
Exp1 = pp.Forward()

Integer = pp.Word(pp.nums)

# Exp0 << ((Exp0 + pp.Suppress("+") + Exp1) ^ Exp1 )
# Exp1 << ((Exp0 + pp.Suppress("*") + Exp2) ^ Exp2)
# Exp2 << (Integer ^ pp.Suppress("(") + Exp0 +pp.Suppress(")")) 

def operation(lhs, op, rhs):
    return (lhs + pp.Suppress(op) + rhs)

# this is not good because it will never end
Exp0 << (Exp1 ^  (operation(Exp0, "+", Exp1)))
Exp1 << (Integer ^ pp.Suppress("(") + Exp0 +pp.Suppress(")"))

print(Exp0.parseString("1+2+3+4+3"))