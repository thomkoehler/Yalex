#pragma once

#include <vector>
#include <functional>
#include <tuple>
#include <memory>
#include <string>
#include <optional>

namespace lexer
{
   class SimpleChar;

   typedef std::function<bool(char)> TransitionPred;

   class StateMachine
   {
   private:
      typedef std::tuple<size_t, TransitionPred> TransitionEntry;
      typedef std::vector<std::shared_ptr<std::vector<TransitionEntry>>> TransitionTable;

      size_t _initialState;
      std::vector<int> _acceptingStates;
      TransitionTable _transitionTable;

   public:
      StateMachine();
      void addSimpleChar(SimpleChar &simpleChar);
      std::optional<size_t> run(const std::string input) const;

   private:
      std::vector<size_t> calcNextStates(size_t state, char input) const;
      std::vector<size_t> calcNextStates(const std::vector<size_t> &states, char input) const;
   };

}