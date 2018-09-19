
#pragma once

#include <functional>

namespace lexer
{
   class Predicate
   {
   public:
      virtual ~Predicate() {};
      virtual bool operator()(char c) const = 0;
   };

   class SimpleChar : public Predicate
   {
   private:
      char _character;

   public:
      SimpleChar(char character);
      virtual bool operator()(char c) const;
   };
}