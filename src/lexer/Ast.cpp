
#include "lexer/Ast.h"

using namespace lexer;

SimpleChar::SimpleChar(char character) : _character(character)
{
}

std::function<bool(char)> SimpleChar::getTransitionPred() const
{
   return [this](const char c) { return c == _character; };
}

