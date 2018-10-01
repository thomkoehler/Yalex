
#include "lexer/Ast.h"

using namespace lexer;

SimpleChar::SimpleChar(char character) : _character(character)
{
}

bool SimpleChar::operator()(char c) const
{
   return _character == c;
}
