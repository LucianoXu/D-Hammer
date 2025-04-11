#pragma once

#include "dhammer.hpp"

namespace examples {

    struct EqExample {
        std::string name;
        std::string preproc_code;
        std::string termA;
        std::string termB;
        bool expected_res = true;
    };

    /**
     * @brief Run the examples and return the timing results.
     *
     * @param examples
     * @return std::vector<std::pair<std::string, float>>
     */
    std::vector<std::pair<std::string, float>> timing_examples(const std::vector<EqExample>& examples);


    extern std::vector<EqExample> QCQI_examples;

    extern std::vector<EqExample> CoqQ_examples;

    extern std::vector<EqExample> Circuit_examples;

    extern std::vector<EqExample> Jens2024_examples;

    extern std::vector<EqExample> others_examples;

    extern std::vector<EqExample> labelled_eq_examples;


}   // namespace examples