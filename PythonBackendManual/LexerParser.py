import calendar
import pyparsing as pp

TRIPLE_QUOTES = pp.Literal("\"\"\"")
# First problem: I cannot understand beforehand if somebody is trying to have a 
# multiline quoted string.
# Without any special actions.
# Therefore I cannot use pyparsing's QuotedString but I need to make sure that 
# the lexer matches correctly the contents of a multiline string
# (difference set in bnfc, char-'\"''\"''\"')

### String tokens ###
single = pp.QuotedString(quoteChar='"""', multiline=True) 
triple = pp.QuotedString(quoteChar='"', multiline=False)


def keyword(str):
    return pp.Keyword(str)

def token(str):
    return pp.Token(str)

def ident(name):
    return Word( alphas, alphanums + "_" )(name)

# maybe define tokens if you use them in several points...
# Must follow the automated translation of BNFC, therefore DO repeat
# since each function becomes a parser below
# todo : Indentation to be implemented.
flashlog = pp.ZeroOrMore(toplevel)
toplevel = event ^ struct

# implementation is : Word( alphas, alphanums + "_$" )
event = keyword("event") + ident('ident') + token(":") + member
struct = keyword("struct") + ident('ident') + token(":") + member
#todo noMember

field = typename + ident('ident') + 

noarray = 
yesarray = token("[") + pp.nums + token("]")

quotedStr = (single ^ triple)

u8 = keyword("u8")
s8 = keyword("s8")
u16= keyword("u16")
s16= keyword("s16")
u32= keyword("u32")
s32= keyword("s32")
structType = keyword("struct") + ident('ident') ;

typename = (U8 ^ S8 ^ U16 ^ S16 ^ U32 ^ S32 ^ structType)

parsed = (quotedStr('pippo')^quotedStr('aaa')).parseString("""\"O beh\" \"O allora\" """)
print (parsed)

