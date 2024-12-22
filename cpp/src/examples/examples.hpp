#pragma once

#include "diracoq.hpp"

namespace examples {

    struct EqExample {
        std::string name;
        std::string preproc_code;
        std::string termA;
        std::string termB;
    };

    extern std::vector<EqExample> eq_examples;

}   // namespace examples