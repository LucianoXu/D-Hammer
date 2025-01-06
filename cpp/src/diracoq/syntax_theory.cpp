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
    TermPtr<int> to_deBruijn(Signature<int>& sig, TermPtr<int> term, vector<int>& bound_var_stack) {

        auto head = term->get_head();

        if (term->is_atomic()) {
            auto search_res = search_bound(bound_var_stack, head);
            if (search_res == -1) {
                return term;
            }
            return create_term(search_res);
        }

        auto& args = term->get_args();
        
        if (head == IDX || head == FORALL) {
            bound_var_stack.push_back(args[0]->get_head());
            auto res = create_term(head, {to_deBruijn(sig, args[1], bound_var_stack)});
            bound_var_stack.pop_back();
            return res;
        }

        if (head == FUN) {
            auto T = to_deBruijn(sig, args[1], bound_var_stack);
            bound_var_stack.push_back(args[0]->get_head());
            auto body = to_deBruijn(sig, args[2], bound_var_stack);
            bound_var_stack.pop_back();
            return create_term(head, {T, body});
        }

        ListArgs<int> new_args;
        for (const auto& arg : args) {
            new_args.push_back(to_deBruijn(sig, arg, bound_var_stack));
        }
        return create_term(head, std::move(new_args));
    }

    TermPtr<int> to_deBruijn(Signature<int>& sig, TermPtr<int> term) {
        vector<int> bound_var_stack;
        return to_deBruijn(sig, term, bound_var_stack);
    }
}