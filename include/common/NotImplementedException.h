
#pragma once

#include<stdexcept>

namespace common
{
   class NotImplementedException : public std::runtime_error
   {
   public:
      NotImplementedException();
   };
}