
#pragma once

#include<stdexcept>

namespace lexer
{

   class LexerException : public std::runtime_error
   {
   public:
      LexerException(const char *msg);
   };

}