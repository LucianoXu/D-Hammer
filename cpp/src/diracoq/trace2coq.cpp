#include "diracoq.hpp"

namespace diracoq {
    using namespace std;
    using namespace ualg;

    std::string term_to_coq(Kernel& kernel, const Term<int>* term) {
        ListArgs<int> args;
        // TYPE
        if (match_normal_head(term, TYPE, args)) {
            return "Type";
        }
        // ARROW
        if (match_normal_head(term, ARROW, args)) {
            if (args.size() != 2) {
                goto UNEXPECTED_ERROR;
            }
            return "(" + term_to_coq(kernel, args[0]) + "->" + term_to_coq(kernel, args[1]) + ")";
        }
        // FUN
        if (match_normal_head(term, FUN, args)) {
            if (args.size() != 3) {
                goto UNEXPECTED_ERROR;
            }
            return "(fun(" + kernel.term_to_string(args[0]) + " : " + term_to_coq(kernel, args[1]) + ") => " + term_to_coq(kernel, args[2]) + ")";
        }
        // APPLY
        if (match_normal_head(term, APPLY, args)) {
            if (args.size() != 2) {
                goto UNEXPECTED_ERROR;
            }
            return "(" + term_to_coq(kernel, args[0]) + " " + term_to_coq(kernel, args[1]) + ")";
        }
        // 0
        if (match_normal_head(term, ZERO, args)) {
            return "0";
        }
        // 1
        if (match_normal_head(term, ONE, args)) {
            return "1";
        }
        // ADDS
        if (match_normal_head(term, ADDS, args)) {
            if (args.empty()) {
                goto UNEXPECTED_ERROR;
            }
            string res = "[+ ";
            for (const auto& arg : args) {
                res += term_to_coq(kernel, arg) + "; ";
            }
            res.pop_back();
            res.pop_back();
            res += "]";
            return res;
        }
        // MULS
        if (match_normal_head(term, MULS, args)) {
            if (args.empty()) {
                goto UNEXPECTED_ERROR;
            }
            string res = "[* ";
            for (const auto& arg : args) {
                res += term_to_coq(kernel, arg) + "; ";
            }
            res.pop_back();
            res.pop_back();
            res += "]";
            return res;
        }
        // CONJ
        if (match_normal_head(term, CONJ, args)) {
            if (args.size() != 1) {
                goto UNEXPECTED_ERROR;
            }
            return "(S_conj " + term_to_coq(kernel, args[0]) + ")";
        }
        if (term->is_atomic()) {
            return kernel.term_to_string(term);
        }
        throw std::runtime_error("Unimplemented term_to_coq for term " + kernel.term_to_string(term) + ".");
        
UNEXPECTED_ERROR:
        throw std::runtime_error("Unexpected error in 'term_to_coq' on term " + kernel.term_to_string(term) + ".");
    }


    std::string pos_to_string(const NormalTermPos& pos) {
        if (pos.size() == 0) {
            return "P_all";
        }
        return "(P_ac " + to_string(pos[0]) + " " + pos_to_string(NormalTermPos(pos.begin() + 1, pos.end())) + ")";
    }


    std::string pos_replace_record_to_string(Kernel& kernel, const PosReplaceRecord& record) {
        string res = "";
        if (record.rule == R_BETA) {
            return "(" + pos_to_string(record.pos) + ", " + "R_BETA" + ")";
        }
        if (record.rule == R_DELTA) {
            return "(" + pos_to_string(record.pos) + ", " + "R_DELTA" + ")";
        }
        if (record.rule == R_ETA) {
            return "(" + pos_to_string(record.pos) + ", " + "R_ETA" + ")";
        }
        if (record.rule == R_FLATTEN) {
            return "(" + pos_to_string(record.pos) + ", " + "R_FLATTEN" + ")";
        }
        if (record.rule == R_ADDSID) {
            return "(" + pos_to_string(record.pos) + ", " + "R_ADDSID" + ")";
        }
        if (record.rule == R_MULSID) {
            return "(" + pos_to_string(record.pos) + ", " + "R_MULSID" + ")";
        }
        if (record.rule == R_ADDS0) {
            return "(" + pos_to_string(record.pos) + ", " + "R_ADDS0" + ")";
        }
        if (record.rule == R_MULS0) {
            return "(" + pos_to_string(record.pos) + ", " + "R_MULS0" + ")";
        }
        if (record.rule == R_MULS1) {
            return "(" + pos_to_string(record.pos) + ", " + "R_MULS1" + ")";
        }
        if (record.rule == R_MULS2) {
            return "(" + pos_to_string(record.pos) + ", " + "R_MULS2" + ")";
        }
        if (record.rule == R_CONJ0) {
            return "(" + pos_to_string(record.pos) + ", " + "R_CONJ0" + ")";
        }
        if (record.rule == R_CONJ1) {
            return "(" + pos_to_string(record.pos) + ", " + "R_CONJ1" + ")";
        }
        if (record.rule == R_CONJ2) {
            return "(" + pos_to_string(record.pos) + ", " + "R_CONJ2" + ")";
        }
        if (record.rule == R_CONJ3) {
            return "(" + pos_to_string(record.pos) + ", " + "R_CONJ3" + ")";
        }
        if (record.rule == R_CONJ4) {
            return "(" + pos_to_string(record.pos) + ", " + "R_CONJ4" + ")";
        }
        
        throw runtime_error("Unknown rule.");
    }

    std::string CInstruct_to_string(const CProofInstruct& instruct) {
        if (instruct.E()) {
            return "T_E";
        }
        string ls_a, ls_b;
        for (const auto& [i, instruct] : instruct.ls) {
            ls_a += to_string(i) + ";";
            ls_b += CInstruct_to_string(instruct) + ";";
        }
        ls_a.pop_back();
        ls_b.pop_back();
        return "[T " + ls_a + " | [:: " + ls_b + "]]";
    }


    std::string normalize_to_coq(Kernel& kernel, const ualg::Term<int>* init_term, const ualg::Term<int>* final_term, const std::vector<PosReplaceRecord>& trace, const CProofInstruct& instruct) {
        string res = "";

        res += "Goal " + term_to_coq(kernel, init_term) + " =s " + term_to_coq(kernel, final_term) + ".\n";
        res += "Proof.\n";
        for (int i = 0; i < trace.size(); ++i) {
            string step_name = "pr" + to_string(i);
            res += "\tpose " + step_name + " := [::" + pos_replace_record_to_string(kernel, trace[i]) + "].\n";
            res += "\t rewrite (R_apply_seq_correct " + step_name + ") /=.\n";
        }

        res += "\tpose p := " + CInstruct_to_string(instruct) + ".\n";
        res += "\trewrite (perm_apply_correct p) //=.\n";
        res += "Qed.\n";

        return res;
    }


    std::string checkeq_to_coq(Kernel& kernel, 
    const ualg::Term<int>* termA, const ualg::Term<int>* termB, 
    const std::vector<PosReplaceRecord>& traceA, const std::vector<PosReplaceRecord>& traceB,
    const ualg::CProofInstruct& instructA, const ualg::CProofInstruct& instructB, 
    const ualg::Term<int>* normalized_term) {
        string res = "";

        res += "Goal " + term_to_coq(kernel, termA) + " =s " + term_to_coq(kernel, termB) + ".\n";
        res += "Proof.\n";

        // Term A
        res += "(* Term A *)\n";
        res += "have HA: " + term_to_coq(kernel, termA) + " =s " + term_to_coq(kernel, normalized_term) + ".\n";
        for (int i = 0; i < traceA.size(); ++i) {
            string step_name = "prA" + to_string(i);
            res += "\tpose " + step_name + " := [::" + pos_replace_record_to_string(kernel, traceA[i]) + "].\n";
            res += "\t rewrite (R_apply_seq_correct " + step_name + ") /=.\n";
        }
        res += "\tpose pA := " + CInstruct_to_string(instructA) + ".\n";
        res += "\trewrite (perm_apply_correct pA) //=.\n";

        // Term B
        res += "(* Term B *)\n";
        res += "have HB: " + term_to_coq(kernel, termB) + " =s " + term_to_coq(kernel, normalized_term) + ".\n";
        for (int i = 0; i < traceB.size(); ++i) {
            string step_name = "prB" + to_string(i);
            res += "\tpose " + step_name + " := [::" + pos_replace_record_to_string(kernel, traceB[i]) + "].\n";
            res += "\t rewrite (R_apply_seq_correct " + step_name + ") /=.\n";
        }
        res += "\tpose pB := " + CInstruct_to_string(instructB) + ".\n";
        res += "\trewrite (perm_apply_correct pB) //=.\n";

        res += "\nby rewrite HA HB.\n";
        res += "Qed.\n";
        return res;
    }

}; // namespace diracoq