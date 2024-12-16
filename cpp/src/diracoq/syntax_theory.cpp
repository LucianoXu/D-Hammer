#include "diracoq.hpp"

namespace diracoq {
    using namespace ualg;
    using namespace std;

    inline int search_bound(const vector<int>& bound_var_stack, int var) {
        for (int i = bound_var_stack.size() - 1; i >= 0; --i) {
            if (bound_var_stack[i] == var) {
                return bound_var_stack.size() - 1 - i;
            }
        }
        return -1;
    }

    // bound_var_stack: the outermost variable is at the front of the vector
    const Term<int>* to_deBruijn(Signature<int>& sig, TermBank<int>& bank, const Term<int>* term, vector<int>& bound_var_stack) {

        auto head = term->get_head();

        if (term->is_atomic()) {
            auto search_res = search_bound(bound_var_stack, head);
            if (search_res == -1) {
                return term;
            }
            return bank.get_term(search_res);
        }

        auto& args = term->get_args();
        
        if (head == IDX || head == FORALL) {
            bound_var_stack.push_back(args[0]->get_head());
            auto res = bank.get_term(head, {to_deBruijn(sig, bank, args[1], bound_var_stack)});
            bound_var_stack.pop_back();
            return res;
        }

        if (head == FUN) {
            auto T = to_deBruijn(sig, bank, args[1], bound_var_stack);
            bound_var_stack.push_back(args[0]->get_head());
            auto body = to_deBruijn(sig, bank, args[2], bound_var_stack);
            bound_var_stack.pop_back();
            return bank.get_term(head, {T, body});
        }

        ListArgs<int> new_args;
        for (const auto& arg : args) {
            new_args.push_back(to_deBruijn(sig, bank, arg, bound_var_stack));
        }
        return bank.get_term(head, std::move(new_args));
    }

    const Term<int>* to_deBruijn(Signature<int>& sig, TermBank<int>& bank, const Term<int>* term) {
        vector<int> bound_var_stack;
        return to_deBruijn(sig, bank, term, bound_var_stack);
    }
}