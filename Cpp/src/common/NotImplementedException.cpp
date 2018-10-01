
#include "common/NotImplementedException.h"

using namespace common;

NotImplementedException::NotImplementedException() : std::runtime_error("Not implemented exception")
{

};