#pragma once

#include "reduction.hpp"

namespace diracoq {

    /**
     * @brief transform the term into Coq code.
     * 
     * @param kernel
     * @param term 
     * @return std::string 
     */
    std::string term_to_coq(Kernel& kernel, const ualg::Term<int>* term);

    std::string pos_to_string(const ualg::NormalTermPos& pos);

    std::string pos_replace_record_to_string(Kernel& kernel, const PosReplaceRecord& record);

    std::string normalize_to_coq(Kernel& kernel, const ualg::Term<int>* init_term, const ualg::Term<int>* final_term, const std::vector<PosReplaceRecord>& trace);

}; // namespace diracoq