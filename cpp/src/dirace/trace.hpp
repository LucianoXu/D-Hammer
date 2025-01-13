#pragma once

#include "symbols.hpp"

namespace dirace {

    std::string pos_to_string(const ualg::TermPos& pos);

    extern std::map<PosRewritingRule, std::string> rule_name;

    std::string record_to_string(Kernel& kernel, const PosReplaceRecord& record);
}; // namespace dirace