# Abstract Syntax classes. 
# Categories must be first.
class Array:
    pass

class Declaration:
    pass

class Documentation:
    pass

class TopLevel:
    pass

class Member:
    pass

class QuotedString():
    pass

class TypeName:
    pass
# Then come labels.
class Descr(Documentation):
    pass

class Doc(Documentation):
    pass

class Docs(Member):
    pass

class Event(TopLevel):
    pass

class Field(Member):
    pass

class Flashlog(Declaration):
    pass
 
class Help(Documentation):
    pass
# is it really needed? I could just have a list of items
class ListMember:
    pass
# is it really needed? I could just have a list of items
class ListTopLevel:
    pass

class NoArray(Array):
    pass

class NoMember(Member):
    pass

class NoTopLevel(TopLevel):
    pass


class S8(TypeName):
    pass

class S16(TypeName):
    pass

class S32(TypeName):
    pass

class Single(QuotedString) :
    pass

class Struct(TopLevel):
    pass

class StructType(TypeName):
    pass

class Triple(QuotedString):
    pass

class U8(TypeName):
    pass

class U16(TypeName):
    pass

class U32(TypeName):
    pass

class YesArray(Array):
    pass