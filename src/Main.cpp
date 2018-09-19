
#include<iostream>
#include<exception>

#include "lexer/StateMachine.h"
#include "lexer/Ast.h"

using namespace lexer;

int main(int argc, char *argv[])
{
   try
   {
      StateMachine stateMachine;

      stateMachine.addPredicate(std::make_shared<Predicate>(SimpleChar('a')));
      stateMachine.addPredicate(std::make_shared<Predicate>(SimpleChar('b')));
      stateMachine.addPredicate(std::make_shared<Predicate>(SimpleChar('c')));

      auto res = stateMachine.run("abc");

      if (res.has_value())
      {
         std::cout << res.value() << std::endl;
      }
   }
   catch (const std::exception &ex)
   {
      std::cerr << ex.what() << std::endl;
      return 1;
   }

   return 0;
}
