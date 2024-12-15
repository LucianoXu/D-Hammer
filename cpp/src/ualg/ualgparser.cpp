#include "ualg.hpp"

namespace ualg {

    Signature<int> compile_string_sig(const std::vector<std::string>& symbols) {
        std::map<std::string, int> name2head;

        for (const auto& name : symbols) {
            name2head[name] = name2head.size();
        }

        return {name2head};
    }

}