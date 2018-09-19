
#include "lexer/LexerException.h"

using namespace lexer;

LexerException::LexerException(const char *msg) : std::runtime_error(msg)
{

}
