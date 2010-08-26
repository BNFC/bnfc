#ifndef SKELETON_HEADER
#define SKELETON_HEADER
/* You might want to change the above name. */

#include "Absyn.h"


void visitExp(Exp p);


void visitIdent(Ident i);
void visitInteger(Integer i);
void visitDouble(Double d);
void visitChar(Char c);
void visitString(String s);

#endif

