#pragma once

#include<vector>

namespace lexer
{

   class StateMachine
   {
   private:
      int _initialState;
      std::vector<int> _acceptingStates;


   private:
      std::vector<int> calcNextStates(const std::vector<int> &currStates, char input);
   };

}