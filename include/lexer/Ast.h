
#pragma once

#include <functional>

namespace lexer
{
   class Ast
   {
   public:
      virtual ~Ast() {};
      virtual std::function<bool(char)> getTransitionPred() const = 0;
   };

   class SimpleChar : public Ast
   {
   private:
      char _character;

   public:
      SimpleChar(char character);
      virtual std::function<bool(char)> getTransitionPred() const;
   };
}