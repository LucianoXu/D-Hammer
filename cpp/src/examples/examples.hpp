#pragma once

#include "diracoq.hpp"

namespace examples {

    struct EqExample {
        std::string name;
        std::string preproc_code;
        std::string termA;
        std::string termB;
        bool expected_res = true;
    };

    extern std::vector<EqExample> eq_examples;

    extern std::vector<EqExample> labelled_eq_examples;

}   // namespace examples