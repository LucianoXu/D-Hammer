#include "ualg.hpp"

namespace ualg {
    /////////////////////////////////////////////////////////////////
    // Implementation

    std::string CProofInstruct::to_string() const {
        if (this->ls.size()==0) {
            return "E";
        }
        std::string str = "[";
        for (const auto& [i, instruct] : ls) {
            str += std::to_string(i) + ":" + instruct.to_string() + " ";
        }
        str.pop_back();
        str += "]";
        return str;
    }


    CProofInstruct CProofInstruct::compose(const CProofInstruct& other) const {
        if (this->ls.size()==0) {
            return other;
        }
        if (other.ls.size()==0) {
            return *this;
        }

        if (this->ls.size() != other.ls.size()) {
            throw std::runtime_error("The two instructions are not compatible.");
        }

        CProofInstruct::PermutationSeq seq{this->ls.size()};
        for (int i = 0; i < this->ls.size(); ++i) {
            seq[i] = {
                this->ls[other.ls[i].first].first, 
                this->ls[other.ls[i].first].second.compose(other.ls[i].second)
            };
        }

        return CProofInstruct{seq};   
    }


    CProofInstruct CProofInstruct::inverse() const{
        if (this->ls.size()==0) {
            return *this;
        }
        CProofInstruct::PermutationSeq seq{this->ls.size()};
        for (int i = 0; i < this->ls.size(); ++i) {
            seq[this->ls[i].first] = {i, this->ls[i].second.inverse()};
        }

        return CProofInstruct{seq};
    }
}