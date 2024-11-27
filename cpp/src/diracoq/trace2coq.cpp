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
        if (record.rule == BETA) {
            throw runtime_error("NOT IMPLEMENTED");
        }
        if (record.rule == DELTA) {
            throw runtime_error("NOT IMPLEMENTED");
        }
        if (record.rule == ETA) {
            throw runtime_error("NOT IMPLEMENTED");
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

    std::string normalize_to_coq(Kernel& kernel, const ualg::Term<int>* init_term, const ualg::Term<int>* final_term, const std::vector<PosReplaceRecord>& trace) {
        string res = "";

        res += "Goal " + term_to_coq(kernel, init_term) + " =s " + term_to_coq(kernel, final_term) + ".\n";
        res += "Proof.\n";
        res += "pose pr := \n\t[::";
        for (const auto& record : trace) {
            res += "\t" + pos_replace_record_to_string(kernel, record) + ";\n";
        }
        res.pop_back();
        res.pop_back();
        res += "].\n";
        res += "by rewrite (R_apply_seq_correct pr).\n";
        res += "Qed.\n";

        return res;
    }

}; // namespace diracoq