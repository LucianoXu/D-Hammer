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

    std::string CInstruct_to_string(const ualg::CProofInstruct& instruct);

    std::string normalize_to_coq(Kernel& kernel, const ualg::Term<int>* init_term, const ualg::Term<int>* final_term, const std::vector<PosReplaceRecord>& trace, const ualg::CProofInstruct& instruct);

    std::string checkeq_to_coq(Kernel& kernel, 
    const ualg::Term<int>* termA, const ualg::Term<int>* termB, 
    const std::vector<PosReplaceRecord>& traceA, const std::vector<PosReplaceRecord>& traceB,
    const ualg::CProofInstruct& instructA, const ualg::CProofInstruct& instructB, 
    const ualg::Term<int>* normalized_term);

}; // namespace diracoq