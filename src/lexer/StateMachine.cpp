
#include <stdexcept>
#include <algorithm>
#include <iterator>

#include "lexer/StateMachine.h"
#include "lexer/LexerException.h"
#include "lexer/Ast.h"
#include "common/NotImplementedException.h"

using namespace lexer;

StateMachine::StateMachine() : _initialState(0)
{
}

void StateMachine::addSimpleChar(SimpleChar &simpleChar)
{
}

std::optional<size_t> StateMachine::run(const std::string input) const
{
   if (_acceptingStates.empty())
   {
      return std::optional<size_t>();
   }

   size_t counter = 0;
   std::vector<size_t> currentStates = { _initialState };

   for (char c: input)
   {
      currentStates = calcNextStates(currentStates, c);
      if (currentStates.empty())
      {
         return std::optional<size_t>();
      }

      std::sort(currentStates.begin(), currentStates.end());

      std::vector<size_t> diff;
      set_symmetric_difference
      (
         currentStates.begin(), 
         currentStates.end(), 
         _acceptingStates.begin(), 
         _acceptingStates.end(), 
         std::back_inserter(diff)
      );

      ++counter;
      if (!diff.empty())
      {
         return std::make_optional(counter);
      }
   }

   return std::optional<size_t>();
}

std::vector<size_t> StateMachine::calcNextStates(const std::vector<size_t> &states, char input) const
{
   std::vector<size_t> allStates;

   for (auto state : states)
   {
      auto states = calcNextStates(state, input);
      std::copy(states.begin(), states.end(), std::back_inserter(states));
   }

   std::sort(allStates.begin(), allStates.end());
   std::unique(allStates.begin(), allStates.end());
   return allStates;
}

std::vector<size_t> StateMachine::calcNextStates(size_t state, char input) const
{
   if (state >= _transitionTable.size())
   {
      throw LexerException("State machine is corrupt.");
   }

   auto transitionEntry = _transitionTable[state];
   std::vector<size_t> nextStates;
   for (const TransitionEntry &entry: *transitionEntry)
   {
      if (std::get<1>(entry)(input))
      {
         nextStates.push_back(std::get<0>(entry));
      }
   }

   return nextStates;
}