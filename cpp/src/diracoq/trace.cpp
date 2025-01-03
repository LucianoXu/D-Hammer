#include "diracoq.hpp"

namespace diracoq {
    using namespace std;
    using namespace ualg;

    std::string term_to_coq(Kernel& kernel, const Term<int>* term) {
        auto head = term->get_head();
        auto& args = term->get_args();
        // TYPE
        if (head == TYPE) {
            return "Type";
        }
        // ARROW
        if (head == ARROW) {
            if (args.size() != 2) {
                goto UNEXPECTED_ERROR;
            }
            return "(" + term_to_coq(kernel, args[0]) + "->" + term_to_coq(kernel, args[1]) + ")";
        }
        // FUN
        if (head == FUN) {
            if (args.size() != 3) {
                goto UNEXPECTED_ERROR;
            }
            return "(fun(" + kernel.term_to_string(args[0]) + " : " + term_to_coq(kernel, args[1]) + ") => " + term_to_coq(kernel, args[2]) + ")";
        }
        // APPLY
        if (head == APPLY) {
            if (args.size() != 2) {
                goto UNEXPECTED_ERROR;
            }
            return "(" + term_to_coq(kernel, args[0]) + " " + term_to_coq(kernel, args[1]) + ")";
        }
        // 0
        if (head == ZERO) {
            return "0";
        }
        // 1
        if (head == ONE) {
            return "1";
        }
        // ADDS
        if (head == ADDS) {
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
        if (head == MULS) {
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
        if (head == CONJ) {
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


    std::string pos_to_string(const TermPos& pos) {
        if (pos.size() == 0) {
            return "P_all";
        }
        return "(P_ac " + to_string(pos[0]) + " " + pos_to_string(TermPos(pos.begin() + 1, pos.end())) + ")";
    }


    std::string pos_replace_record_to_string(Kernel& kernel, const PosReplaceRecord& record) {
        string res = "";
        if (record.rule == R_BETA_ARROW) {
            return "(" + pos_to_string(record.pos) + ", " + "R_BETA_ARROW" + ")";
        }
        if (record.rule == R_BETA_INDEX) {
            return "(" + pos_to_string(record.pos) + ", " + "R_BETA_INDEX" + ")";
        }
        if (record.rule == R_DELTA) {
            return "(" + pos_to_string(record.pos) + ", " + "R_DELTA" + ")";
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
        if (record.rule == R_CONJ5) {
            return "(" + pos_to_string(record.pos) + ", " + "R_CONJ5" + ")";
        }
        if (record.rule == R_CONJ6) {
            return "(" + pos_to_string(record.pos) + ", " + "R_CONJ6" + ")";
        }
        if (record.rule == R_DOT0) {
            return "(" + pos_to_string(record.pos) + ", " + "R_DOT0" + ")";
        }
        if (record.rule == R_DOT1) {
            return "(" + pos_to_string(record.pos) + ", " + "R_DOT1" + ")";
        }
        if (record.rule == R_DOT2) {
            return "(" + pos_to_string(record.pos) + ", " + "R_DOT2" + ")";
        }
        if (record.rule == R_DOT3) {
            return "(" + pos_to_string(record.pos) + ", " + "R_DOT3" + ")";
        }
        if (record.rule == R_DOT4) {
            return "(" + pos_to_string(record.pos) + ", " + "R_DOT4" + ")";
        }
        if (record.rule == R_DOT5) {
            return "(" + pos_to_string(record.pos) + ", " + "R_DOT5" + ")";
        }
        if (record.rule == R_DOT6) {
            return "(" + pos_to_string(record.pos) + ", " + "R_DOT6" + ")";
        }
        if (record.rule == R_DOT7) {
            return "(" + pos_to_string(record.pos) + ", " + "R_DOT7" + ")";
        }
        if (record.rule == R_DOT8) {
            return "(" + pos_to_string(record.pos) + ", " + "R_DOT8" + ")";
        }
        if (record.rule == R_DOT9) {
            return "(" + pos_to_string(record.pos) + ", " + "R_DOT9" + ")";
        }
        if (record.rule == R_DOT10) {
            return "(" + pos_to_string(record.pos) + ", " + "R_DOT10" + ")";
        }
        if (record.rule == R_DOT11) {
            return "(" + pos_to_string(record.pos) + ", " + "R_DOT11" + ")";
        }
        if (record.rule == R_DOT12) {
            return "(" + pos_to_string(record.pos) + ", " + "R_DOT12" + ")";
        }
        if (record.rule == R_DELTA0) {
            return "(" + pos_to_string(record.pos) + ", " + "R_DELTA0" + ")";
        }
        if (record.rule == R_DELTA1) {
            return "(" + pos_to_string(record.pos) + ", " + "R_DELTA1" + ")";
        }
        if (record.rule == R_SCR0) {
            return "(" + pos_to_string(record.pos) + ", " + "R_SCR0" + ")";
        }
        if (record.rule == R_SCR1) {
            return "(" + pos_to_string(record.pos) + ", " + "R_SCR1" + ")";
        }
        if (record.rule == R_SCR2) {
            return "(" + pos_to_string(record.pos) + ", " + "R_SCR2" + ")";
        }
        if (record.rule == R_SCRK0) {
            return "(" + pos_to_string(record.pos) + ", " + "R_SCRK0" + ")";
        }
        if (record.rule == R_SCRK1) {
            return "(" + pos_to_string(record.pos) + ", " + "R_SCRK1" + ")";
        }
        if (record.rule == R_SCRB0) {
            return "(" + pos_to_string(record.pos) + ", " + "R_SCRB0" + ")";
        }
        if (record.rule == R_SCRB1) {
            return "(" + pos_to_string(record.pos) + ", " + "R_SCRB1" + ")";
        }
        if (record.rule == R_SCRO0) {
            return "(" + pos_to_string(record.pos) + ", " + "R_SCRO0" + ")";
        }
        if (record.rule == R_SCRO1) {
            return "(" + pos_to_string(record.pos) + ", " + "R_SCRO1" + ")";
        }
        if (record.rule == R_ADDID) {
            return "(" + pos_to_string(record.pos) + ", " + "R_ADDID" + ")";
        }
        if (record.rule == R_ADD0) {
            return "(" + pos_to_string(record.pos) + ", " + "R_ADD0" + ")";
        }
        if (record.rule == R_ADD1) {
            return "(" + pos_to_string(record.pos) + ", " + "R_ADD1" + ")";
        }
        if (record.rule == R_ADD2) {
            return "(" + pos_to_string(record.pos) + ", " + "R_ADD2" + ")";
        }
        if (record.rule == R_ADD3) {
            return "(" + pos_to_string(record.pos) + ", " + "R_ADD3" + ")";
        }
        if (record.rule == R_ADDK0) {
            return "(" + pos_to_string(record.pos) + ", " + "R_ADDK0" + ")";
        }
        if (record.rule == R_ADDB0) {
            return "(" + pos_to_string(record.pos) + ", " + "R_ADDB0" + ")";
        }
        if (record.rule == R_ADDO0) {
            return "(" + pos_to_string(record.pos) + ", " + "R_ADDO0" + ")";
        }
        if (record.rule == R_ADJ0) {
            return "(" + pos_to_string(record.pos) + ", " + "R_ADJ0" + ")";
        }
        if (record.rule == R_ADJ1) {
            return "(" + pos_to_string(record.pos) + ", " + "R_ADJ1" + ")";
        }
        if (record.rule == R_ADJ2) {
            return "(" + pos_to_string(record.pos) + ", " + "R_ADJ2" + ")";
        }
        if (record.rule == R_ADJ3) {
            return "(" + pos_to_string(record.pos) + ", " + "R_ADJ3" + ")";
        }
        if (record.rule == R_ADJK0) {
            return "(" + pos_to_string(record.pos) + ", " + "R_ADJK0" + ")";
        }
        if (record.rule == R_ADJK1) {
            return "(" + pos_to_string(record.pos) + ", " + "R_ADJK1" + ")";
        }
        if (record.rule == R_ADJK2) {
            return "(" + pos_to_string(record.pos) + ", " + "R_ADJK2" + ")";
        }
        if (record.rule == R_ADJB0) {
            return "(" + pos_to_string(record.pos) + ", " + "R_ADJB0" + ")";
        }
        if (record.rule == R_ADJB1) {
            return "(" + pos_to_string(record.pos) + ", " + "R_ADJB1" + ")";
        }
        if (record.rule == R_ADJB2) {
            return "(" + pos_to_string(record.pos) + ", " + "R_ADJB2" + ")";
        }
        if (record.rule == R_ADJO0) {
            return "(" + pos_to_string(record.pos) + ", " + "R_ADJO0" + ")";
        }
        if (record.rule == R_ADJO1) {
            return "(" + pos_to_string(record.pos) + ", " + "R_ADJO1" + ")";
        }
        if (record.rule == R_ADJO2) {
            return "(" + pos_to_string(record.pos) + ", " + "R_ADJO2" + ")";
        }
        if (record.rule == R_ADJO3) {
            return "(" + pos_to_string(record.pos) + ", " + "R_ADJO3" + ")";
        }
        if (record.rule == R_TSR0) {
            return "(" + pos_to_string(record.pos) + ", " + "R_TSR0" + ")";
        }
        if (record.rule == R_TSR1) {
            return "(" + pos_to_string(record.pos) + ", " + "R_TSR1" + ")";
        }
        if (record.rule == R_TSR2) {
            return "(" + pos_to_string(record.pos) + ", " + "R_TSR2" + ")";
        }
        if (record.rule == R_TSR3) {
            return "(" + pos_to_string(record.pos) + ", " + "R_TSR3" + ")";
        }
        if (record.rule == R_TSRK0) {
            return "(" + pos_to_string(record.pos) + ", " + "R_TSRK0" + ")";
        }
        if (record.rule == R_TSRK1) {
            return "(" + pos_to_string(record.pos) + ", " + "R_TSRK1" + ")";
        }
        if (record.rule == R_TSRK2) {
            return "(" + pos_to_string(record.pos) + ", " + "R_TSRK2" + ")";
        }
        if (record.rule == R_TSRB0) {
            return "(" + pos_to_string(record.pos) + ", " + "R_TSRB0" + ")";
        }
        if (record.rule == R_TSRB1) {
            return "(" + pos_to_string(record.pos) + ", " + "R_TSRB1" + ")";
        }
        if (record.rule == R_TSRB2) {
            return "(" + pos_to_string(record.pos) + ", " + "R_TSRB2" + ")";
        }
        if (record.rule == R_TSRO0) {
            return "(" + pos_to_string(record.pos) + ", " + "R_TSRO0" + ")";
        }
        if (record.rule == R_TSRO1) {
            return "(" + pos_to_string(record.pos) + ", " + "R_TSRO1" + ")";
        }
        if (record.rule == R_TSRO2) {
            return "(" + pos_to_string(record.pos) + ", " + "R_TSRO2" + ")";
        }
        if (record.rule == R_TSRO3) {
            return "(" + pos_to_string(record.pos) + ", " + "R_TSRO3" + ")";
        }
        if (record.rule == R_MULK0) {
            return "(" + pos_to_string(record.pos) + ", " + "R_MULK0" + ")";
        }
        if (record.rule == R_MULK1) {
            return "(" + pos_to_string(record.pos) + ", " + "R_MULK1" + ")";
        }
        if (record.rule == R_MULK2) {
            return "(" + pos_to_string(record.pos) + ", " + "R_MULK2" + ")";
        }
        if (record.rule == R_MULK3) {
            return "(" + pos_to_string(record.pos) + ", " + "R_MULK3" + ")";
        }
        if (record.rule == R_MULK4) {
            return "(" + pos_to_string(record.pos) + ", " + "R_MULK4" + ")";
        }
        if (record.rule == R_MULK5) {
            return "(" + pos_to_string(record.pos) + ", " + "R_MULK5" + ")";
        }
        if (record.rule == R_MULK6) {
            return "(" + pos_to_string(record.pos) + ", " + "R_MULK6" + ")";
        }
        if (record.rule == R_MULK7) {
            return "(" + pos_to_string(record.pos) + ", " + "R_MULK7" + ")";
        }
        if (record.rule == R_MULK8) {
            return "(" + pos_to_string(record.pos) + ", " + "R_MULK8" + ")";
        }
        if (record.rule == R_MULK9) {
            return "(" + pos_to_string(record.pos) + ", " + "R_MULK9" + ")";
        }
        if (record.rule == R_MULK10) {
            return "(" + pos_to_string(record.pos) + ", " + "R_MULK10" + ")";
        }
        if (record.rule == R_MULK11) {
            return "(" + pos_to_string(record.pos) + ", " + "R_MULK11" + ")";
        }
        if (record.rule == R_MULB0) {
            return "(" + pos_to_string(record.pos) + ", " + "R_MULB0" + ")";
        }
        if (record.rule == R_MULB1) {
            return "(" + pos_to_string(record.pos) + ", " + "R_MULB1" + ")";
        }
        if (record.rule == R_MULB2) {
            return "(" + pos_to_string(record.pos) + ", " + "R_MULB2" + ")";
        }
        if (record.rule == R_MULB3) {
            return "(" + pos_to_string(record.pos) + ", " + "R_MULB3" + ")";
        }
        if (record.rule == R_MULB4) {
            return "(" + pos_to_string(record.pos) + ", " + "R_MULB4" + ")";
        }
        if (record.rule == R_MULB5) {
            return "(" + pos_to_string(record.pos) + ", " + "R_MULB5" + ")";
        }
        if (record.rule == R_MULB6) {
            return "(" + pos_to_string(record.pos) + ", " + "R_MULB6" + ")";
        }
        if (record.rule == R_MULB7) {
            return "(" + pos_to_string(record.pos) + ", " + "R_MULB7" + ")";
        }
        if (record.rule == R_MULB8) {
            return "(" + pos_to_string(record.pos) + ", " + "R_MULB8" + ")";
        }
        if (record.rule == R_MULB9) {
            return "(" + pos_to_string(record.pos) + ", " + "R_MULB9" + ")";
        }
        if (record.rule == R_MULB10) {
            return "(" + pos_to_string(record.pos) + ", " + "R_MULB10" + ")";
        }
        if (record.rule == R_MULB11) {
            return "(" + pos_to_string(record.pos) + ", " + "R_MULB11" + ")";
        }
        if (record.rule == R_OUTER0) {
            return "(" + pos_to_string(record.pos) + ", " + "R_OUTER0" + ")";
        }
        if (record.rule == R_OUTER1) {
            return "(" + pos_to_string(record.pos) + ", " + "R_OUTER1" + ")";
        }
        if (record.rule == R_OUTER2) {
            return "(" + pos_to_string(record.pos) + ", " + "R_OUTER2" + ")";
        }
        if (record.rule == R_OUTER3) {
            return "(" + pos_to_string(record.pos) + ", " + "R_OUTER3" + ")";
        }
        if (record.rule == R_OUTER4) {
            return "(" + pos_to_string(record.pos) + ", " + "R_OUTER4" + ")";
        }
        if (record.rule == R_OUTER5) {
            return "(" + pos_to_string(record.pos) + ", " + "R_OUTER5" + ")";
        }
        if (record.rule == R_MULO0) {
            return "(" + pos_to_string(record.pos) + ", " + "R_MULO0" + ")";
        }
        if (record.rule == R_MULO1) {
            return "(" + pos_to_string(record.pos) + ", " + "R_MULO1" + ")";
        }
        if (record.rule == R_MULO2) {
            return "(" + pos_to_string(record.pos) + ", " + "R_MULO2" + ")";
        }
        if (record.rule == R_MULO3) {
            return "(" + pos_to_string(record.pos) + ", " + "R_MULO3" + ")";
        }
        if (record.rule == R_MULO4) {
            return "(" + pos_to_string(record.pos) + ", " + "R_MULO4" + ")";
        }
        if (record.rule == R_MULO5) {
            return "(" + pos_to_string(record.pos) + ", " + "R_MULO5" + ")";
        }
        if (record.rule == R_MULO6) {
            return "(" + pos_to_string(record.pos) + ", " + "R_MULO6" + ")";
        }
        if (record.rule == R_MULO7) {
            return "(" + pos_to_string(record.pos) + ", " + "R_MULO7" + ")";
        }
        if (record.rule == R_MULO8) {
            return "(" + pos_to_string(record.pos) + ", " + "R_MULO8" + ")";
        }
        if (record.rule == R_MULO9) {
            return "(" + pos_to_string(record.pos) + ", " + "R_MULO9" + ")";
        }
        if (record.rule == R_MULO10) {
            return "(" + pos_to_string(record.pos) + ", " + "R_MULO10" + ")";
        }
        if (record.rule == R_MULO11) {
            return "(" + pos_to_string(record.pos) + ", " + "R_MULO11" + ")";
        }
        if (record.rule == R_MULO12) {
            return "(" + pos_to_string(record.pos) + ", " + "R_MULO12" + ")";
        }
        if (record.rule == R_SET0) {
            return "(" + pos_to_string(record.pos) + ", " + "R_SET0" + ")";
        }
        if (record.rule == R_SUM_CONST0) {
            return "(" + pos_to_string(record.pos) + ", " + "R_SUM_CONST0" + ")";
        }
        if (record.rule == R_SUM_CONST1) {
            return "(" + pos_to_string(record.pos) + ", " + "R_SUM_CONST1" + ")";
        }
        if (record.rule == R_SUM_CONST2) {
            return "(" + pos_to_string(record.pos) + ", " + "R_SUM_CONST2" + ")";
        }
        if (record.rule == R_SUM_CONST3) {
            return "(" + pos_to_string(record.pos) + ", " + "R_SUM_CONST3" + ")";
        }
        if (record.rule == R_SUM_CONST4) {
            return "(" + pos_to_string(record.pos) + ", " + "R_SUM_CONST4" + ")";
        }
        if (record.rule == R_SUM_ELIM0) {
            return "(" + pos_to_string(record.pos) + ", " + "R_SUM_ELIM0" + ")";
        }
        if (record.rule == R_SUM_ELIM1) {
            return "(" + pos_to_string(record.pos) + ", " + "R_SUM_ELIM1" + ")";
        }
        if (record.rule == R_SUM_ELIM2) {
            return "(" + pos_to_string(record.pos) + ", " + "R_SUM_ELIM2" + ")";
        }
        if (record.rule == R_SUM_ELIM3) {
            return "(" + pos_to_string(record.pos) + ", " + "R_SUM_ELIM3" + ")";
        }
        if (record.rule == R_SUM_ELIM4) {
            return "(" + pos_to_string(record.pos) + ", " + "R_SUM_ELIM4" + ")";
        }
        if (record.rule == R_SUM_ELIM5) {
            return "(" + pos_to_string(record.pos) + ", " + "R_SUM_ELIM5" + ")";
        }
        if (record.rule == R_SUM_ELIM6) {
            return "(" + pos_to_string(record.pos) + ", " + "R_SUM_ELIM6" + ")";
        }
        if (record.rule == R_SUM_ELIM7) {
            return "(" + pos_to_string(record.pos) + ", " + "R_SUM_ELIM7" + ")";
        }
        if (record.rule == R_SUM_PUSH0) {
            return "(" + pos_to_string(record.pos) + ", " + "R_SUM_PUSH0" + ")";
        }
        if (record.rule == R_SUM_PUSH1) {
            return "(" + pos_to_string(record.pos) + ", " + "R_SUM_PUSH1" + ")";
        }
        if (record.rule == R_SUM_PUSH2) {
            return "(" + pos_to_string(record.pos) + ", " + "R_SUM_PUSH2" + ")";
        }
        if (record.rule == R_SUM_PUSH3) {
            return "(" + pos_to_string(record.pos) + ", " + "R_SUM_PUSH3" + ")";
        }
        if (record.rule == R_SUM_PUSH4) {
            return "(" + pos_to_string(record.pos) + ", " + "R_SUM_PUSH4" + ")";
        }
        if (record.rule == R_SUM_PUSH5) {
            return "(" + pos_to_string(record.pos) + ", " + "R_SUM_PUSH5" + ")";
        }
        if (record.rule == R_SUM_PUSH6) {
            return "(" + pos_to_string(record.pos) + ", " + "R_SUM_PUSH6" + ")";
        }
        if (record.rule == R_SUM_PUSH7) {
            return "(" + pos_to_string(record.pos) + ", " + "R_SUM_PUSH7" + ")";
        }
        if (record.rule == R_SUM_PUSH8) {
            return "(" + pos_to_string(record.pos) + ", " + "R_SUM_PUSH8" + ")";
        }
        if (record.rule == R_SUM_PUSH9) {
            return "(" + pos_to_string(record.pos) + ", " + "R_SUM_PUSH9" + ")";
        }
        if (record.rule == R_SUM_PUSH10) {
            return "(" + pos_to_string(record.pos) + ", " + "R_SUM_PUSH10" + ")";
        }
        if (record.rule == R_SUM_PUSH11) {
            return "(" + pos_to_string(record.pos) + ", " + "R_SUM_PUSH11" + ")";
        }
        if (record.rule == R_SUM_PUSH12) {
            return "(" + pos_to_string(record.pos) + ", " + "R_SUM_PUSH12" + ")";
        }
        if (record.rule == R_SUM_PUSH13) {
            return "(" + pos_to_string(record.pos) + ", " + "R_SUM_PUSH13" + ")";
        }
        if (record.rule == R_SUM_PUSH14) {
            return "(" + pos_to_string(record.pos) + ", " + "R_SUM_PUSH14" + ")";
        }
        if (record.rule == R_SUM_PUSH15) {
            return "(" + pos_to_string(record.pos) + ", " + "R_SUM_PUSH15" + ")";
        }
        if (record.rule == R_SUM_PUSH16) {
            return "(" + pos_to_string(record.pos) + ", " + "R_SUM_PUSH16" + ")";
        }
        if (record.rule == R_SUM_ADDS0) {
            return "(" + pos_to_string(record.pos) + ", " + "R_SUM_ADDS0" + ")";
        }
        if (record.rule == R_SUM_ADD0) {
            return "(" + pos_to_string(record.pos) + ", " + "R_SUM_ADD0" + ")";
        }
        if (record.rule == R_SUM_INDEX0) {
            return "(" + pos_to_string(record.pos) + ", " + "R_SUM_INDEX0" + ")";
        }
        if (record.rule == R_SUM_INDEX1) {
            return "(" + pos_to_string(record.pos) + ", " + "R_SUM_INDEX1" + ")";
        }

        return "Some Rule";
        
        // throw runtime_error("Unknown rule.");
    }

}; // namespace diracoq