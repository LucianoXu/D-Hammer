#include "diracoq.hpp"

namespace diracoq {

    using namespace std;
    using namespace ualg;

    std::optional<Declaration> Kernel::find_in_env(int symbol) {
        for (const auto& [sym, def] : env) {
            if (sym == symbol) {
                return def;
            }
        }
        return std::nullopt;
    }

    std::optional<Declaration> Kernel::find_dec(int symbol) {
        for (int i = ctx.size() - 1; i >= 0; i--) {
            if (ctx[i].first == symbol) {
                return ctx[i].second;
            }
        }
        for (const auto& [sym, def] : env) {
            if (sym == symbol) {
                return def;
            }
        }
        return std::nullopt;
    }

    std::string Kernel::dec_to_string(const std::string& name, const Declaration& dec) const {
        if (dec.is_def()) {
            return name + " := " + term_to_string(*dec.def) + " : " + term_to_string(dec.type);
        }
        else {
            return name + " : " + term_to_string(dec.type);
        }
    }


    const Term<int>* Kernel::parse(const std::string& code) {
        return ualg::parse(sig, bank, code);
    }

    const Term<int>* Kernel::parse(const astparser::AST& ast) {
        return ualg::parse(sig, bank, ast);
    }

    string Kernel::term_to_string(const Term<int>* term) const {
        return sig.term_to_string(term);
    }

    string Kernel::env_to_string() const {
        string res = "";
        for (const auto& [sym, def] : env) {
            res += dec_to_string(sig.get_name(sym), def) + "\n";
        }
        return res;
    }

    bool Kernel::is_index(const Term<int>* term) {
        auto type = calc_type(term);

        if (type->get_head() == INDEX) {
            return true;
        }

        return false;
    }

    bool Kernel::is_type(const Term<int>* term) {
        auto type = calc_type(term);

        if (type->get_head() == TYPE) {
            return true;
        }

        return false;
    }




    const Term<int>* Kernel::calc_type(const Term<int>* term) {
        auto head = term->get_head();
        auto args = term->get_args();

        // (COMPO rules)
        if (head == COMPO) {
            arg_number_check(args, 2);

            auto typeA = calc_type(args[0]);
            auto typeB = calc_type(args[1]);

            if (typeA->get_head() == SType) {
                // S @ S : S
                if (typeB->get_head() == SType) {
                    return bank.get_term(SType);
                }
                // S @ K(A) : K(A)
                if (typeB->get_head() == KType) {
                    return typeB;
                }
                // S @ B(A) : B(A)
                if (typeB->get_head() == BType) {
                    return typeB;
                }
                // S @ O(A, B) : O(A, B)
                if (typeB->get_head() == OType) {
                    return typeB;
                }
            }
            
            if (typeA->get_head() == KType) {
                // K(A) @ S : K(A)
                if (typeB->get_head() == SType) {
                    return typeA;
                }
                // K(A) @ K(B) : K(Prod(A B))
                if (typeB->get_head() == KType) {
                    return bank.get_term(KType, 
                        {bank.get_term(Prod, {typeA->get_args()[0], typeB->get_args()[0]})});
                }
                // K(A) @ B(B) : O(A, B)
                if (typeB->get_head() == BType) {
                    return bank.get_term(OType, {typeA->get_args()[0], typeB->get_args()[0]});
                }
                // K(A) @ O(B, C) --- NOT VALID
            }
            if (typeA->get_head() == BType) {
                // B(A) @ S : B(A)
                if (typeB->get_head() == SType) {
                    return typeA;
                }
                // B(A) @ K(A) : S
                if (typeB->get_head() == KType) {
                    if (is_eq(sig, bank, typeA->get_args()[0], typeB->get_args()[0])) {
                        return bank.get_term(SType);
                    }
                    else {
                        throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the argument " + sig.term_to_string(typeA->get_args()[0]) + " of the first term is not equal to the argument " + sig.term_to_string(typeB->get_args()[0]) + " of the second term.");
                    }
                }
                // B(A) @ B(B) : B(Prod(A, B))
                if (typeB->get_head() == BType) {
                    return bank.get_term(BType, 
                        {bank.get_term(Prod, {typeA->get_args()[0], typeB->get_args()[0]})});
                }
                // B(A) @ O(A, B) : B(B)
                if (typeB->get_head() == OType) {
                    if (is_eq(sig, bank, typeA->get_args()[0], typeB->get_args()[0])) {
                        return bank.get_term(BType, {typeB->get_args()[1]});
                    }
                    else {
                        throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the argument " + sig.term_to_string(typeA->get_args()[0]) + " of the first term is not equal to the argument " + sig.term_to_string(typeB->get_args()[0]) + " of the second term.");
                    }
                }   
            }
            if (typeA->get_head() == OType) {
                // O(A, B) @ S : O(A, B)
                if (typeB->get_head() == SType) {
                    return typeA;
                }
                // O(A, B) @ K(B) : K(A)
                if (typeB->get_head() == KType) {
                    if (is_eq(sig, bank, typeA->get_args()[1], typeB->get_args()[0])) {
                        return bank.get_term(KType, {typeA->get_args()[0]});
                    }
                    else {
                        throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the argument " + sig.term_to_string(typeA->get_args()[1]) + " of the first term is not equal to the argument " + sig.term_to_string(typeB->get_args()[0]) + " of the second term.");
                    }
                }
                // O(A, B) @ B(C) --- NOT VALID
                // O(A, B) @ O(B, C) : O(A, C)
                if (typeB->get_head() == OType) {
                    if (is_eq(sig, bank, typeA->get_args()[1], typeB->get_args()[0])) {
                        return bank.get_term(OType, {typeA->get_args()[0], typeB->get_args()[1]});
                    }
                    else {
                        throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the argument " + sig.term_to_string(typeA->get_args()[1]) + " of the first term is not equal to the argument " + sig.term_to_string(typeB->get_args()[0]) + " of the second term.");
                    }
                }
            }
            // (T1 -> T2) @ T1 : T2
            if (typeA->get_head() == ARROW) {
                if (!is_eq(sig, bank, typeA->get_args()[0], typeB)) {
                    throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the argument " + sig.term_to_string(typeA->get_args()[0]) + " of the first term is not equal to the second term.");
                }
                return typeA->get_args()[1];
            }
            // (Forall x, T) @ (t : Index) : T{x/t}
            if (typeA->get_head() == FORALL) {
                auto& args_typeA = typeA->get_args();
                if (!is_index(args[1])) {
                    throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the second term is not equal to the index.");
                }
                return subst(sig, bank, args_typeA[1], args_typeA[0]->get_head(), args[1]);
            }

            throw std::runtime_error("Typing error: invalid composition of " + sig.term_to_string(typeA) + " and " + sig.term_to_string(typeB) + ".");
        }

        // (Index-Prod)
        if (head == Prod) {
            arg_number_check(args, 2);

            if (!is_index(args[0])) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the first argument " + sig.term_to_string(args[0]) + " is not an index.");
            }

            if (!is_index(args[1])) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the second argument " + sig.term_to_string(args[1]) + " is not an index.");
            }

            return bank.get_term(INDEX);
        }

        // (Type-Arrow)
        if (head == ARROW) {
            arg_number_check(args, 2);

            if (!is_type(args[0])) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the type of the argument " + sig.term_to_string(args[0]) + " is not a well-typed type.");
            }
            if (!is_type(args[1])) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the type of the body " + sig.term_to_string(args[1]) + " is not a well-typed type.");
            }

            return bank.get_term(TYPE);
        }

        // (Type-Index)
        if (head == FORALL) {
            arg_number_check(args, 2);

            if (!args[0]->is_atomic()) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the first argument " + sig.term_to_string(args[0]) + " is not a variable.");
            }

            context_push(args[0]->get_head(), bank.get_term(INDEX));

            try {
                if (!is_type(args[1])) {
                    throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the type of the body " + sig.term_to_string(args[1]) + " is not a well-typed type.");
                }

                context_pop();
                return bank.get_term(TYPE, {});
            }
            catch (const std::runtime_error& e) {
                context_pop();
                throw e;
            }
        }

        // (Type-Basis) 
        if (head == BASIS) {
            arg_number_check(args, 1);

            if (!is_index(args[0])) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the argument " + sig.term_to_string(args[0]) + " is not an index.");
            }
            return bank.get_term(TYPE);
        }
        // (Type-Ket)
        if (head == KType) {
            arg_number_check(args, 1);
            
            if (!is_index(args[0])) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the argument " + sig.term_to_string(args[0]) + " is not an index.");
            }
            return bank.get_term(TYPE);
        }
        // (Type-Bra)
        if (head == BType) {
            arg_number_check(args, 1);

            if (!is_index(args[0])) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the argument " + sig.term_to_string(args[0]) + " is not an index.");
            }
            return bank.get_term(TYPE);
        }
        // (Type-Opt)
        if (head == OType) {
            arg_number_check(args, 2);
            
            if (!(is_index(args[0]) && is_index(args[1]))) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the arguments " + sig.term_to_string(args[0]) + " and " + sig.term_to_string(args[1]) + " are not indices.");
            }
            return bank.get_term(TYPE);
        }
        // (Type-Scalar)
        if (head == SType) {
            arg_number_check(args, 0);

            return bank.get_term(TYPE);
        }
        // (Type-Set)
        if (head == Set) {
            arg_number_check(args, 1);

            if (!is_index(args[0])) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the argument " + sig.term_to_string(args[0]) + " is not an index.");
            }

            return bank.get_term(TYPE);
        }

        // (Lam)
        if (head == FUN) {
            arg_number_check(args, 3);

            if (!args[0]->is_atomic()) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the first argument " + sig.term_to_string(args[0]) + " is not a variable.");
            }

            if (!is_type(args[1])) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the type of the argument " + sig.term_to_string(args[0]) + " is not a well-typed type.");
            }

            context_push(args[0]->get_head(), args[1]);

            try {
                
                // calculate the type of the body
                auto type_body = calc_type(args[2]);

                if (!is_type(type_body)) {
                    throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the type of the body " + sig.term_to_string(args[2]) + " is not a well-typed type.");
                }

                context_pop();
                return bank.get_term(ARROW, {args[1], type_body});
            }
            catch (const std::runtime_error& e) {
                // pop the definition
                context_pop();
                throw e;
            }
        }

        // (Index)
        if (head == IDX) {

            arg_number_check(args, 2);

            // try to add to the context
            context_push(args[0]->get_head(), bank.get_term(INDEX));

            try {
                // calculate the type of the body
                auto type_body = calc_type(args[1]);

                if (!is_type(type_body)) {
                    throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the type of the body " + sig.term_to_string(args[1]) + " is not a well-typed type.");
                }

                context_pop();
                return bank.get_term(FORALL, {args[0], type_body});
            }
            catch (const std::runtime_error& e) {
                // pop the definition
                context_pop();
                throw e;
            }
        }

        // (App-Arrow), (App-Index)
        if (head == APPLY) {
            arg_number_check(args, 2);


            // calculate the type of f, which should be Arrow(T, U)
            auto type_f = calc_type(args[0]);

            auto head_f = type_f->get_head();
            auto& args_f = type_f->get_args();
            if (head_f == ARROW) {
        
                // check whether the type of u matches
                if (!type_check(args[1], args_f[0])) {
                    throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the type of the argument " + sig.term_to_string(args[1]) + " does not match the type of the function argument " + sig.term_to_string(args_f[0]) + ".");
                }

                return args_f[1];
            }

            else if (head_f == FORALL) {
                // check whether the type of u is Index
                if (!is_index(args[1])) {
                    throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the type of the argument " + sig.term_to_string(args[1]) + " is not an index.");
                }

                // return the instantiated type
                return subst(sig, bank, args_f[1], args_f[0]->get_head(), args[1]);
            }

            throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the type of the function " + sig.term_to_string(args[0]) + " is not an arrow type or forall type.");
        }

        // (Pair-Base)
        if (head == PAIR) {
            arg_number_check(args, 2);

            auto type_a = calc_type(args[0]);
            auto type_b = calc_type(args[1]);

            auto type_a_head = type_a->get_head();
            auto type_b_head = type_b->get_head();

            auto& args_a = type_a->get_args();
            auto& args_b = type_b->get_args();

            if (type_a_head != BASIS || type_b_head != BASIS) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the types of the arguments " + sig.term_to_string(args[0]) + " and " + sig.term_to_string(args[1]) + " are not of type Basis.");
            }

            return bank.get_term(BASIS, {bank.get_term(Prod, {args_a[0], args_b[0]})});
        }

        // (Sca-0)
        if (head == ZERO) {
            arg_number_check(args, 0);

            return bank.get_term(SType);
        }
        // (Sca-1)
        if (head == ONE) {
            arg_number_check(args, 0);
            
            return bank.get_term(SType);
        }
        // (Sca-Delta)
        if (head == DELTA) {
            arg_number_check(args, 2);

            // Calculate the type of the first argument
            auto type_a = calc_type(args[0]);

            // Check whether it is Base
            if (type_a->get_head() != BASIS) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the first argument " + sig.term_to_string(args[0]) + " is not of type Basis.");
            }

            // Check whether b is of type type_a
            if (!(type_check(args[1], type_a))) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the second argument " + sig.term_to_string(args[1]) + " is not of type " + sig.term_to_string(type_a) + ".");
            }
            
            return bank.get_term(SType);
        }
        // (Sca-Add)
        if (head == ADDS) {
            auto SType_term = bank.get_term(SType);
            if (args.size() == 0) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because it has no arguments.");
            }
            for (const auto& arg : args) {
                if (!type_check(arg, SType_term)) {
                    throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the argument " + sig.term_to_string(arg) + " is not of type SType.");
                }
            }
            return SType_term;
        }
        // (Sca-Mul)
        if (head == MULS) {
            auto SType_term = bank.get_term(SType);
            if (args.size() == 0) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because it has no arguments.");
            }
            for (const auto& arg : args) {
                if (!type_check(arg, SType_term)) {
                    throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the argument " + sig.term_to_string(arg) + " is not of type SType.");
                }
            }
            return SType_term;
        }
        // (Sca-Conj)
        if (head == CONJ) {
            auto SType_term = bank.get_term(SType);
            arg_number_check(args, 1);
            
            if (!type_check(args[0], SType_term)) {
                throw runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the argument " + sig.term_to_string(args[0]) + " is not of type SType.");
            }
            return SType_term;
        }
        // (Sca-Dot)
        if (head == DOT) {
            arg_number_check(args, 2);
            
            auto type_B = calc_type(args[0]);
            auto& args_B = type_B->get_args();
            if (type_B->get_head() != BType) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the first argument " + sig.term_to_string(args[0]) + " is not of type BType.");
            }

            auto type_K = calc_type(args[1]);
            auto& args_K = type_K->get_args();
            if (type_K->get_head() != KType) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the second argument " + sig.term_to_string(args[1]) + " is not of type KType.");
            }

            // check whether the index of type_K is the same as the index of type_B
            if (!is_eq(sig, bank, args_B[0], args_K[0])) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the index of the first argument " + sig.term_to_string(args[0]) + " is not the same as the index of the second argument " + sig.term_to_string(args[1]) + ".");
            }

            return bank.get_term(SType);
        }

        // (Ket-Adj), (Bra-Adj), (Opt-Adj)
        if (head == ADJ) {
            arg_number_check(args, 1);

            auto type_X = calc_type(args[0]);
            auto type_X_head = type_X->get_head();
            auto& args_X = type_X->get_args();
            if (type_X_head == BType) {
                return bank.get_term(KType, {args_X[0]});
            }
            else if (type_X_head == KType) {
                return bank.get_term(BType, {args_X[0]});
            }
            else if (type_X_head == OType) {
                return bank.get_term(OType, {args_X[1], args_X[0]});
            }

            throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the argument " + sig.term_to_string(args[0]) + " is not of type BType, KType or OType.");            
        }

        // (Ket-Scr), (Bra-Scr), (Opt-Scr)
        if (head == SCR) {
            arg_number_check(args, 2);

            auto type_a = calc_type(args[0]);
            if (type_a->get_head() != SType) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the first argument " + sig.term_to_string(args[0]) + " is not of type SType.");
            }

            auto type_X = calc_type(args[1]);
            auto type_X_head = type_X->get_head();
            if (type_X_head == KType || type_X_head == BType || type_X_head == OType) {
                return type_X;
            }

            throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the second argument " + sig.term_to_string(args[1]) + " is not of type KType, BType or OType.");
        }

        // (Ket-Add), (Bra-Add), (Opt-Add)
        if (head == ADD) {
            if (args.size() == 0) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because it has no arguments.");
            }

            auto type_X = calc_type(args[0]);
            auto type_X_head = type_X->get_head();
            ListArgs<int> args_X;
            if (type_X_head != KType && type_X_head != BType && type_X_head != OType) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the first argument " + sig.term_to_string(args[0]) + " is not of type BType, KType or OType.");
            }

            for (int i = 1; i < args.size(); i++) {
                if (!is_eq(sig, bank, calc_type(args[i]), type_X)) {
                    throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the argument " + sig.term_to_string(args[i]) + " is not of the same type as the first argument " + sig.term_to_string(args[0]) + ".");
                }
            }

            return type_X;
        }

        // (Ket-Tsr), (Bra-Tsr), (Opt-Tsr)
        if (head == TSR) {
            arg_number_check(args, 2);

            auto type_X1 = calc_type(args[0]);
            auto type_X1_head = type_X1->get_head();
            auto& args_X1 = type_X1->get_args();
            auto type_X2 = calc_type(args[1]);
            auto type_X2_head = type_X2->get_head();
            auto& args_X2 = type_X2->get_args();
            // (Ket-Tsr)
            if (type_X1_head == KType && type_X2_head == KType) {
                return bank.get_term(KType, {bank.get_term(Prod, {args_X1[0], args_X2[0]})});
            }
            // (Bra-Tsr)
            else if (type_X1_head == BType && type_X2_head == BType) {
                return bank.get_term(BType, {bank.get_term(Prod, {args_X1[0], args_X2[0]})});
            }
            // (Opt-Tsr)
            else if (type_X1_head == OType && type_X2_head == OType) {
                return bank.get_term(OType, 
                    {
                        bank.get_term(Prod, {args_X1[0], args_X2[0]}), 
                        bank.get_term(Prod, {args_X1[1], args_X2[1]})
                    }
                );
            }
            
            throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the arguments " + sig.term_to_string(args[0]) + " and " + sig.term_to_string(args[1]) + " are not of type KType, BType or OType.");
        }

        // (Ket-0)
        if (head == ZEROK) {
            arg_number_check(args, 1);
            
            if (!is_index(args[0])) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the argument " + sig.term_to_string(args[0]) + " is not an index.");
            }

            return bank.get_term(KType, {args[0]});
        }

        // (Ket-Base)
        if (head == KET) {
            arg_number_check(args, 1);

            auto type_t = calc_type(args[0]);
            auto& args_t = type_t->get_args();
            if (type_t->get_head() != BASIS) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the argument " + sig.term_to_string(args[0]) + " is not of type Base.");
            }

            return bank.get_term(KType, {args_t[0]});
        }

        // (Ket-MulK)
        if (head == MULK) {
            arg_number_check(args, 2);

            auto type_O = calc_type(args[0]);
            auto& args_O = type_O->get_args();
            if (type_O->get_head() != OType) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the first argument " + sig.term_to_string(args[0]) + " is not of type OType.");
            }

            auto type_K = calc_type(args[1]);
            auto& args_K = type_K->get_args();
            if (type_K->get_head() != KType) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the second argument " + sig.term_to_string(args[1]) + " is not of type KType.");
            }

            // check whether the index of type_K is the same as the second index of type_O
            if (!is_eq(sig, bank, args_O[1], args_K[0])) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the second index of the first argument " + sig.term_to_string(args[0]) + " is not the same as the index of the second argument " + sig.term_to_string(args[1]) + ".");
            }

            return bank.get_term(KType, {args_O[0]});
        }

        // (Bra-0)
        if (head == ZEROB) {
            arg_number_check(args, 1);

            if (!is_index(args[0])) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the argument " + sig.term_to_string(args[0]) + " is not an index.");
            }

            return bank.get_term(BType, {args[0]});
        }

        // (Bra-Base)
        if (head == BRA) {
            arg_number_check(args, 1);

            auto type_t = calc_type(args[0]);
            auto& args_t = type_t->get_args();
            if (type_t->get_head() != BASIS) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the argument " + sig.term_to_string(args[0]) + " is not of type Base.");
            }

            return bank.get_term(BType, {args_t[0]});
        }

        // (Bra-MulB)
        if  (head == MULB) {
            arg_number_check(args, 2);

            auto type_B = calc_type(args[0]);
            auto& args_B = type_B->get_args();
            if (type_B->get_head() != BType) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the first argument " + sig.term_to_string(args[0]) + " is not of type BType.");
            }

            auto type_O = calc_type(args[1]);
            auto& args_O = type_O->get_args();
            if (type_O->get_head() != OType) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the second argument " + sig.term_to_string(args[1]) + " is not of type OType.");
            }

            // check whether the index of type_B is the same as the first index of type_O
            if (!is_eq(sig, bank, args_B[0], args_O[0])) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the index of the first argument " + sig.term_to_string(args[0]) + " is not the same as the first index of the second argument " + sig.term_to_string(args[1]) + ".");
            }

            return bank.get_term(BType, {args_O[1]});
        }

        // (Opt-0)
        if (head == ZEROO) {
            arg_number_check(args, 2);

            if (!(is_index(args[0]) && is_index(args[1]))) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the arguments " + sig.term_to_string(args[0]) + " and " + sig.term_to_string(args[1]) + " are not indices.");
            }

            return bank.get_term(OType, {args[0], args[1]});
        }

        // (Opt-1)
        if (head == ONEO) {
            arg_number_check(args, 1);
            
            if (!is_index(args[0])) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the argument " + sig.term_to_string(args[0]) + " is not an index.");
            }

            return bank.get_term(OType, {args[0], args[0]});
        }

        // (Opt-Outer)
        if (head == OUTER) {
            arg_number_check(args, 2);

            auto type_K = calc_type(args[0]);
            auto& args_K = type_K->get_args();
            if (type_K->get_head() != KType) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the first argument " + sig.term_to_string(args[0]) + " is not of type KType.");
            }

            auto type_B = calc_type(args[1]);
            auto& args_B = type_B->get_args();
            if (type_B->get_head() != BType) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the second argument " + sig.term_to_string(args[1]) + " is not of type BType.");
            }

            return bank.get_term(OType, {args_K[0], args_B[0]});
        }

        // (Opt-MulO)
        if (head == MULO) {
            arg_number_check(args, 2);

            auto type_O1 = calc_type(args[0]);
            auto& args_O1 = type_O1->get_args();
            if (type_O1->get_head() != OType) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the first argument " + sig.term_to_string(args[0]) + " is not of type OType.");
            }

            auto type_O2 = calc_type(args[1]);
            auto& args_O2 = type_O2->get_args();
            if (type_O2->get_head() != OType) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the second argument " + sig.term_to_string(args[1]) + " is not of type OType.");
            }

            // check whether the second index of type_O1 is the same as the first index of type_O2

            if (!is_eq(sig, bank, args_O1[1], args_O2[0])) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the second index of the first argument " + sig.term_to_string(args[0]) + " is not the same as the first index of the second argument " + sig.term_to_string(args[1]) + ".");
            }

            return bank.get_term(OType, {args_O1[0], args_O2[1]});
        }

        // (Set-U)
        if (head == USET) {
            arg_number_check(args, 1);

            if (!is_index(args[0])) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the argument " + sig.term_to_string(args[0]) + " is not an index.");
            }

            return bank.get_term(Set, {args[0]});
        }

        // (Set-Prod)
        if (head == CATPROD) {
            arg_number_check(args, 2);

            auto type_a = calc_type(args[0]);
            auto type_b = calc_type(args[1]);

            auto& args_a = type_a->get_args();
            auto& args_b = type_b->get_args();
            if (type_a->get_head() != Set || type_b->get_head() != Set) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the arguments " + sig.term_to_string(args[0]) + " and " + sig.term_to_string(args[1]) + " are not of type Set.");
            }
            
            return bank.get_term(
                Set, {
                    bank.get_term(Prod, {args_a[0], args_b[0]})
                }
            );
        }

        // (Sum-Scalar), (Sum-Ket), (Sum-Bra), (Sum-Opt)
        if (head == SUM) {
            arg_number_check(args, 2);

            
            auto type_s = calc_type(args[0]);
            auto& args_s = type_s->get_args();
            if (type_s->get_head() != Set) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the first argument " + sig.term_to_string(args[0]) + " is not of type Set.");
            }

            auto type_f = calc_type(args[1]);
            auto& args_f = type_f->get_args();
            if (type_f->get_head() != ARROW) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the second argument " + sig.term_to_string(args[1]) + " is not of type Arrow.");
            }

            auto& args_f_basis = args_f[0]->get_args();
            if (args_f[0]->get_head() != BASIS) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the first argument of the second argument " + sig.term_to_string(args[1]) + " is not of type Basis.");
            }

            if (!is_eq(sig, bank, args_s[0], args_f_basis[0])) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the index of the first argument " + sig.term_to_string(args[0]) + " is not the same as the index of the first argument of the second argument " + sig.term_to_string(args[1]) + ".");
            }

            if (args_f[1]->get_head() == SType || args_f[1]->get_head() == KType || args_f[1]->get_head() == BType || args_f[1]->get_head() == OType) {
                return args_f[1];
            }

            else {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the second argument " + sig.term_to_string(args[1]) + " is not of type SType, KType, BType or OType.");
            }
        }

        if (term->is_atomic()) {

            auto dec_find = find_dec(term->get_head());
            if (dec_find != std::nullopt) {
                return dec_find->type;
            }

            throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not assumed or defined.");
        }

        throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' cannot be typed.");
    }

    void Kernel::assum(int symbol, const Term<int>* type) {
        auto TYPE_term = bank.get_term(TYPE, {});
        auto INDEX_term = bank.get_term(INDEX, {});

        if (is_reserved(symbol)) {
            throw std::runtime_error("The symbol '" + sig.term_to_string(bank.get_term(symbol, {})) + "' is reserved.");
        }
        if (find_in_env(symbol) != std::nullopt) {
            throw std::runtime_error("The symbol '" + sig.term_to_string(bank.get_term(symbol, {})) + "' is already in the environment.");
        }

        // W-Assum-Index
        if (type == INDEX_term) {
            env.push_back({symbol, {std::nullopt, type}});
        }

        // W-Assum-Type
        else if (type == TYPE_term) {
            env.push_back({symbol, {std::nullopt, type}});
        }

        // W-Assum-Term
        else {
            if (!is_type(type)) {
                throw std::runtime_error("The type of the symbol '" + sig.term_to_string(bank.get_term(symbol, {})) + "' is not a well-typed type.");
            }
            env.push_back({symbol, {std::nullopt, type}});
        }
    }

    void Kernel::def(int symbol, const Term<int>* term, std::optional<const ualg::Term<int>*> type) {
        if (is_reserved(symbol)) {
            throw std::runtime_error("The symbol '" + sig.term_to_string(bank.get_term(symbol)) + "' is reserved.");
        }

        if (ctx.size() != 0) {
            throw std::runtime_error("The context is not empty.");
        }

        if (find_in_env(symbol) != std::nullopt) {
            throw std::runtime_error("The symbol '" + sig.term_to_string(bank.get_term(symbol)) + "' is already in the environment."); 
        }

        auto deducted_type = calc_type(term);
        if (!is_type(deducted_type)) {
            throw std::runtime_error("The term '" + sig.term_to_string(term) + "' is not well-typed.");
        }

        if (type.has_value()) {
            if (!type_check(term, type.value())) {
                throw std::runtime_error("The term '" + sig.term_to_string(term) + "' is not well-typed with the type '" + sig.term_to_string(type.value()) + "'.");
            }
        }
        else {
            type = deducted_type;
        }
        env.push_back({symbol, {term, type.value()}});
    }

    void Kernel::env_pop() {
        if (env.size() == 0) {
            throw std::runtime_error("The environment is empty.");
        }
        env.pop_back();
    }


    void Kernel::context_push(int symbol, const ualg::Term<int>* type) {
        if (!((type->get_head() == INDEX && type->get_args().size()) == 0 || is_type(type))) {

            throw std::runtime_error("The term '" + sig.term_to_string(type) + "' is not a valid type for bound index.");
        }

        ctx.push_back({symbol, {std::nullopt, type}});
    }

    void Kernel::context_pop() {
        if (ctx.size() == 0) {
            throw std::runtime_error("The context is empty.");
        }
        ctx.pop_back();
    }


} // namespace diracoq