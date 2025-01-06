#pragma once

#include "symbols.hpp"

namespace diracoq {

    /**
     * @brief transform the term into Coq code.
     * 
     * @param kernel
     * @param term 
     * @return std::string 
     */
    std::string term_to_coq(Kernel& kernel, ualg::TermPtr<int> term);

    std::string pos_to_string(const ualg::TermPos& pos);

    std::string pos_replace_record_to_string(Kernel& kernel, const PosReplaceRecord& record);
}; // namespace diracoq