import calendar
import pyparsing as pp 

# builtin tokens in RE BNFC grammar.
def _tok(all):
    return pp.Regex("[%s]"%all)

def _suppr(v):
    return pp.Suppress(v)

digit  = _tok("0-9")
upper  = _tok("A-Z")
lower  = _tok("a-z")
letter = _tok("a-zA-Z")
eps    = pp.Empty()
char   = _tok("\x00-\x0F")

def _usertok(v):
    return pp.Combine(v)

LIdent = _usertok(lower + pp.ZeroOrMore(letter|digit|"_"))
UIdent = _usertok(upper + pp.ZeroOrMore(letter|digit|"_"))
Wild   = _usertok("_"   + pp.ZeroOrMore(char))

# builtin categories in BNFC grammar.

def _checkChar(s,v,k):
    if len(s) > 3:
        raise pp.ParseFatalException('Too long char literal')

Integer = pp.Word(pp.nums)
Double = pp.Regex("[0-9]*\.[0-9]+")
String = pp.QuotedString('"', escChar='\\')
Char = pp.QuotedString("'", escChar='\\').addParseAction(_checkChar)
Ident = pp.Word( pp.alphas, pp.alphanums + "_$" )

V = UIdent
A = Wild

Var = V ^ A

Atm = LIdent
EAtm = "'" +Ident + "'"
Atom = Atm ^ EAtm

ListTerm = pp.Forward()

List = pp.Forward()

Empty = _suppr("[") + _suppr("]")
Enum = _suppr("[") + ListTerm + _suppr("]")
Cons  = _suppr("[") + ListTerm + _suppr("|") + List + _suppr("]")
ConsV = _suppr("[") + ListTerm + _suppr("|") + Var + _suppr("]")

List = Empty ^ Enum ^ Cons ^ ConsV

TAtom = Atom
VarT = Var
Complex = Atom + _suppr("(") + pp.Group(ListTerm) + _suppr(")")
TList = pp.Group(List)

Term = TAtom ^ VarT ^ Complex ^ TList  

APred = Atom("APred")
CPred = Atom("Atom") +_suppr("(") + ListTerm("ListTerm") + _suppr(")")("CPred")
Predicate = APred("APred") ^ CPred("CPred")
ListPredicate = pp.delimitedList(Predicate, ",")

Fact = Predicate ("Fact")
Rule = pp.Group(Predicate + _suppr(":-") + Predicate)("Rule")
Directive = pp.Group(_suppr(":-") + Predicate)("Directive")

Clause = pp.Group(Fact ^ Rule ^ Directive)

ListClause = pp.delimitedList(Clause, ".")
ListTerm << pp.delimitedList(Term, ",")

Db = ListClause
Database = Db


 
simpsons = """
child(bart,homer).

child(homer,abe).

child(maggie,homer).

grandchild(X,Y) :-
    child(X,Z),
    child(Z,Y).
    """
simple = """
append([],Ys,Ys).append([X|Xs],Ys,[X|Zs]) :-append(Xs,Ys,Zs)."""

parsed = Db.parseString(simpsons)
#print(parsed)
parsed = Db.parseString(simple)
#print(parsed)

Exp = pp.Forward()
Exp1 = pp.Forward()
Exp2 = pp.Forward()
# 
Exp <<  Exp1 ^ pp.Group(Exp + ("+") + Exp1)  
Exp1 <<  Exp2 \
         ^ pp.Group(Exp1 + ("*") + Exp2) 
Exp2 << Integer \
        ^ ("(")+ Exp + (")")


ppa = Exp.parseString("1")
print(ppa)
