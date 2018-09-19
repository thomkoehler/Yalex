#pragma once

#include <vector>
#include <functional>
#include <tuple>
#include <memory>
#include <string>
#include <optional>

#include "lexer/Ast.h"

namespace lexer
{
   class StateMachine
   {
   private:
      typedef std::tuple<size_t, std::shared_ptr<Predicate>> TransitionEntry;
      typedef std::vector<std::shared_ptr<std::vector<TransitionEntry>>> TransitionTable;

      size_t _initialState;
      std::vector<int> _acceptingStates;
      TransitionTable _transitionTable;

   public:
      StateMachine();
      void addPredicate(std::shared_ptr<Predicate> pred);
      std::optional<size_t> run(const std::string input) const;

   private:
      std::vector<size_t> calcNextStates(size_t state, char input) const;
      std::vector<size_t> calcNextStates(const std::vector<size_t> &states, char input) const;
      void removeAcceptingState(size_t state);
   };

}