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

    bool Kernel::is_type(const Term<int>* term) {
        auto type = calc_type(term);

        if (type->get_head() == TYPE || type->get_head() == BASE) {
            return true;
        }

        return false;
    }


    const Term<int>* Kernel::calc_type(const Term<int>* term) {
        ListArgs<int> args;

        // (Arrow)
        if (match_normal_head(term, ARROW, args)) {
            arg_number_check(args, 2);

            if (!is_type(args[0])) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the type of the argument " + sig.term_to_string(args[0]) + " is not a well-typed type.");
            }
            if (!is_type(args[1])) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the type of the body " + sig.term_to_string(args[1]) + " is not a well-typed type.");
            }

            return bank.get_normal_term(TYPE, {});
        }

        // (Lam)
        if (match_normal_head(term, FUN, args)) {
            arg_number_check(args, 3);

            // try to add the definition
            assum(args[0]->get_head(), args[1]);
            
            // calculate the type of the body
            auto type_body = calc_type(args[2]);
            env.pop_back();

            return bank.get_normal_term(ARROW, {args[1], type_body});
        }

        // (App)
        if (match_normal_head(term, APPLY, args)) {
            arg_number_check(args, 2);

            // calculate the type of f, which should be Arrow(T, U)
            auto type_f = calc_type(args[0]);

            ListArgs<int> args_f;
            if (match_normal_head(type_f, ARROW, args_f)) {
        
                // check whether the type of u matches
                if (!type_check(args[1], args_f[0])) {
                    throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the type of the argument " + sig.term_to_string(args[1]) + " does not match the type of the function argument " + sig.term_to_string(args_f[0]) + ".");
                }

                return args_f[1];
            }

            throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the type of the function " + sig.term_to_string(args[0]) + " is not an arrow type.");
        }
        // (Type-Base)
        if (match_normal_head(term, BASE, args)) {
            arg_number_check(args, 0);

            return bank.get_normal_term(TYPE, {});
        }
        // (Type-Ket)
        if (match_normal_head(term, KType, args)) {
            arg_number_check(args, 1);
            
            if (!type_check(args[0], bank.get_normal_term(BASE, {}))) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the argument " + sig.term_to_string(args[0]) + " is not of type Base.");
            }
            return bank.get_normal_term(TYPE, {});
        }
        // (Type-Bra)
        if (match_normal_head(term, BType, args)) {
            arg_number_check(args, 1);

            if (!type_check(args[0], bank.get_normal_term(BASE, {}))) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the argument " + sig.term_to_string(args[0]) + " is not of type Base.");
            }
            return bank.get_normal_term(TYPE, {});
        }
        // (Type-Opt)
        if (match_normal_head(term, OType, args)) {
            arg_number_check(args, 2);
            
            if (!(type_check(args[0], bank.get_normal_term(BASE, {})) && type_check(args[1], bank.get_normal_term(BASE, {})))) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the arguments " + sig.term_to_string(args[0]) + " and " + sig.term_to_string(args[1]) + " are not of type Base.");
            }
            return bank.get_normal_term(TYPE, {});
        }
        // (Type-Scalar)
        if (match_normal_head(term, SType, args)) {
            arg_number_check(args, 0);

            return bank.get_normal_term(TYPE, {});
        }
        // (Sca-0)
        if (match_normal_head(term, ZERO, args)) {
            arg_number_check(args, 0);

            return bank.get_normal_term(SType, {});
        }
        // (Sca-1)
        if (match_normal_head(term, ONE, args)) {
            arg_number_check(args, 0);
            
            return bank.get_normal_term(SType, {});
        }
        // (Sca-Delta)
        if (match_normal_head(term, DELTA, args)) {
            arg_number_check(args, 2);

            // Calculate the type of the first argument
            auto type_a = calc_type(args[0]);
            
            // Check whether it is Base
            if (!(type_check(type_a, bank.get_normal_term(BASE, {})))) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the type of first argument " + sig.term_to_string(type_a) + " is not of type Base.");
            }

            // Check whether b is of type type_a
            if (!(type_check(args[1], type_a))) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the second argument " + sig.term_to_string(args[1]) + " is not of type " + sig.term_to_string(type_a) + ".");
            }
            
            return bank.get_normal_term(SType, {});
        }
        // (Sca-Add)
        if (match_normal_head(term, ADDS, args)) {
            auto SType_term = bank.get_normal_term(SType, {});
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
        if (match_normal_head(term, MULS, args)) {
            auto SType_term = bank.get_normal_term(SType, {});
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
        if (match_normal_head(term, CONJ, args)) {
            auto SType_term = bank.get_normal_term(SType, {});
            arg_number_check(args, 1);
            
            if (!type_check(args[0], SType_term)) {
                throw runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the argument " + sig.term_to_string(args[0]) + " is not of type SType.");
            }
            return SType_term;
        }
        // (Sca-Dot)
        if (match_normal_head(term, DOT, args)) {
            arg_number_check(args, 2);
            
            auto type_B = calc_type(args[0]);
            ListArgs<int> args_B;
            if (!match_normal_head(type_B, BType, args_B)) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the first argument " + sig.term_to_string(args[0]) + " is not of type BType.");
            }

            auto type_K = calc_type(args[1]);
            ListArgs<int> args_K;
            if (!match_normal_head(type_K, KType, args_K)) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the second argument " + sig.term_to_string(args[1]) + " is not of type KType.");
            }

            // check whether the index of type_K is the same as the index of type_B
            if (args_B[0] != args_K[0]) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the index of the first argument " + sig.term_to_string(args[0]) + " is not the same as the index of the second argument " + sig.term_to_string(args[1]) + ".");
            }

            return bank.get_normal_term(SType, {});
        }

        // (Base-Prod)
        if (match_normal_head(term, Prod, args)) {
            arg_number_check(args, 2);

            if (!(type_check(args[0], bank.get_normal_term(BASE, {}))) || !(type_check(args[1], bank.get_normal_term(BASE, {}))) ) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the arguments " + sig.term_to_string(args[0]) + " and " + sig.term_to_string(args[1]) + " are not of type Base.");
            }

            return bank.get_normal_term(TYPE, {});
        }

        // (Pair-Base)
        if (match_normal_head(term, PAIR, args)) {
            arg_number_check(args, 2);

            auto type_a = calc_type(args[0]);
            auto type_b = calc_type(args[1]);

            if (!(type_check(type_a, bank.get_normal_term(BASE, {}))) || !(type_check(type_b, bank.get_normal_term(BASE, {}))) ) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the arguments " + sig.term_to_string(args[0]) + " and " + sig.term_to_string(args[1]) + " are not of type Base.");
            }

            return bank.get_normal_term(Prod, {type_a, type_b});
        }

        // (Ket-Adj), (Bra-Adj), (Opt-Adj)
        if (match_normal_head(term, ADJ, args)) {
            arg_number_check(args, 1);

            auto type_X = calc_type(args[0]);
            ListArgs<int> args_X;
            if (match_normal_head(type_X, BType, args_X)) {
                return bank.get_normal_term(KType, {args_X[0]});
            }
            else if (match_normal_head(type_X, KType, args_X)) {
                return bank.get_normal_term(BType, {args_X[0]});
            }
            else if (match_normal_head(type_X, OType, args_X)) {
                return bank.get_normal_term(OType, {args_X[1], args_X[0]});
            }

            throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the argument " + sig.term_to_string(args[0]) + " is not of type BType, KType or OType.");            
        }

        // (Ket-Scr), (Bra-Scr), (Opt-Scr)
        if (match_normal_head(term, SCR, args)) {
            arg_number_check(args, 2);

            ListArgs<int> args_a;
            auto type_a = calc_type(args[0]);
            if (!match_normal_head(type_a, SType, args_a)) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the first argument " + sig.term_to_string(args[0]) + " is not of type SType.");
            }

            ListArgs<int> args_X;
            auto type_X = calc_type(args[1]);
            if (match_normal_head(type_X, KType, args_X) || match_normal_head(type_X, BType, args_X) || match_normal_head(type_X, OType, args_X)) {
                return type_X;
            }

            throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the second argument " + sig.term_to_string(args[1]) + " is not of type KType, BType or OType.");
        }

        // (Ket-Add), (Bra-Add), (Opt-Add)
        if (match_normal_head(term, ADD, args)) {
            if (args.size() == 0) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because it has no arguments.");
            }

            auto type_X = calc_type(args[0]);
            ListArgs<int> args_X;
            if (!match_normal_head(type_X, KType, args_X) && !match_normal_head(type_X, BType, args_X) && !match_normal_head(type_X, OType, args_X)) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the first argument " + sig.term_to_string(args[0]) + " is not of type BType, KType or OType.");
            }

            for (int i = 1; i < args.size(); i++) {
                if (calc_type(args[i]) != type_X) {
                    throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the argument " + sig.term_to_string(args[i]) + " is not of the same type as the first argument " + sig.term_to_string(args[0]) + ".");
                }
            }

            return type_X;
        }

        // (Ket-Tsr), (Bra-Tsr), (Opt-Tsr)
        if (match_normal_head(term, TSR, args)) {
            arg_number_check(args, 2);

            auto type_X1 = calc_type(args[0]);
            ListArgs<int> args_X1;
            auto type_X2 = calc_type(args[1]);
            ListArgs<int> args_X2;
            // (Ket-Tsr)
            if (match_normal_head(type_X1, KType, args_X1) && match_normal_head(type_X2, KType, args_X2)) {
                return bank.get_normal_term(KType, {bank.get_normal_term(Prod, {args_X1[0], args_X2[0]})});
            }
            // (Bra-Tsr)
            else if (match_normal_head(type_X1, BType, args_X1) && match_normal_head(type_X2, BType, args_X2)) {
                return bank.get_normal_term(BType, {bank.get_normal_term(Prod, {args_X1[0], args_X2[0]})});
            }
            // (Opt-Tsr)
            else if (match_normal_head(type_X1, OType, args_X1) && match_normal_head(type_X2, OType, args_X2)) {
                return bank.get_normal_term(OType, 
                    {
                        bank.get_normal_term(Prod, {args_X1[0], args_X2[0]}), 
                        bank.get_normal_term(Prod, {args_X1[1], args_X2[1]})
                    }
                );
            }
            
            throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the arguments " + sig.term_to_string(args[0]) + " and " + sig.term_to_string(args[1]) + " are not of type KType, BType or OType.");
        }

        // (Ket-0)
        if (match_normal_head(term, ZEROK, args)) {
            arg_number_check(args, 1);

            if (!type_check(args[0], bank.get_normal_term(BASE, {}))) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the argument " + sig.term_to_string(args[0]) + " is not of type Base.");
            }

            return bank.get_normal_term(KType, {args[0]});
        }

        // (Ket-Base)
        if (match_normal_head(term, KET, args)) {
            arg_number_check(args, 1);

            auto type_t = calc_type(args[0]);

            if (!type_check(type_t, bank.get_normal_term(BASE, {}))) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the argument " + sig.term_to_string(args[0]) + " is not of type Base.");
            }

            return bank.get_normal_term(KType, {type_t});
        }

        // (Ket-MulK)
        if (match_normal_head(term, MULK, args)) {
            arg_number_check(args, 2);

            auto type_O = calc_type(args[0]);
            ListArgs<int> args_O;
            if (!match_normal_head(type_O, OType, args_O)) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the first argument " + sig.term_to_string(args[0]) + " is not of type OType.");
            }

            auto type_K = calc_type(args[1]);
            ListArgs<int> args_K;
            if (!match_normal_head(type_K, KType, args_K)) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the second argument " + sig.term_to_string(args[1]) + " is not of type KType.");
            }

            // check whether the index of type_K is the same as the second index of type_O
            if (args_O[1] != args_K[0]) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the second index of the first argument " + sig.term_to_string(args[0]) + " is not the same as the index of the second argument " + sig.term_to_string(args[1]) + ".");
            }

            return bank.get_normal_term(KType, {args_O[0]});
        }

        // (Bra-0)
        if (match_normal_head(term, ZEROB, args)) {
            arg_number_check(args, 1);

            if (!type_check(args[0], bank.get_normal_term(BASE, {}))) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the argument " + sig.term_to_string(args[0]) + " is not of type Base.");
            }

            return bank.get_normal_term(BType, {args[0]});
        }

        // (Bra-Base)
        if (match_normal_head(term, BRA, args)) {
            arg_number_check(args, 1);

            auto type_t = calc_type(args[0]);

            if (!type_check(type_t, bank.get_normal_term(BASE, {}))) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the argument " + sig.term_to_string(args[0]) + " is not of type Base.");
            }

            return bank.get_normal_term(BType, {type_t});
        }

        // (Bra-MulB)
        if  (match_normal_head(term, MULB, args)) {
            arg_number_check(args, 2);

            auto type_B = calc_type(args[0]);
            ListArgs<int> args_B;
            if (!match_normal_head(type_B, BType, args_B)) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the first argument " + sig.term_to_string(args[0]) + " is not of type BType.");
            }

            auto type_O = calc_type(args[1]);
            ListArgs<int> args_O;
            if (!match_normal_head(type_O, OType, args_O)) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the second argument " + sig.term_to_string(args[1]) + " is not of type OType.");
            }

            // check whether the index of type_B is the same as the first index of type_O
            if (args_B[0] != args_O[0]) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the index of the first argument " + sig.term_to_string(args[0]) + " is not the same as the first index of the second argument " + sig.term_to_string(args[1]) + ".");
            }

            return bank.get_normal_term(BType, {args_O[1]});
        }

        // (Opt-0)
        if (match_normal_head(term, ZEROO, args)) {
            arg_number_check(args, 2);

            if (!(type_check(args[0], bank.get_normal_term(BASE, {})) && type_check(args[1], bank.get_normal_term(BASE, {}))) ) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the arguments " + sig.term_to_string(args[0]) + " and " + sig.term_to_string(args[1]) + " are not of type Base.");
            }

            return bank.get_normal_term(OType, {args[0], args[1]});
        }

        // (Opt-1)
        if (match_normal_head(term, ONEO, args)) {
            arg_number_check(args, 1);

            if (!type_check(args[0], bank.get_normal_term(BASE, {}))) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the argument " + sig.term_to_string(args[0]) + " is not of type Base.");
            }

            return bank.get_normal_term(OType, {args[0], args[0]});
        }

        // (Opt-Outer)
        if (match_normal_head(term, OUTER, args)) {
            arg_number_check(args, 2);

            auto type_K = calc_type(args[0]);
            ListArgs<int> args_K;
            if (!match_normal_head(type_K, KType, args_K)) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the first argument " + sig.term_to_string(args[0]) + " is not of type KType.");
            }

            auto type_B = calc_type(args[1]);
            ListArgs<int> args_B;
            if (!match_normal_head(type_B, BType, args_B)) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the second argument " + sig.term_to_string(args[1]) + " is not of type BType.");
            }

            return bank.get_normal_term(OType, {args_K[0], args_B[0]});
        }

        // (Opt-MulO)
        if (match_normal_head(term, MULO, args)) {
            arg_number_check(args, 2);

            auto type_O1 = calc_type(args[0]);
            ListArgs<int> args_O1;
            if (!match_normal_head(type_O1, OType, args_O1)) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the first argument " + sig.term_to_string(args[0]) + " is not of type OType.");
            }

            auto type_O2 = calc_type(args[1]);
            ListArgs<int> args_O2;
            if (!match_normal_head(type_O2, OType, args_O2)) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the second argument " + sig.term_to_string(args[1]) + " is not of type OType.");
            }

            // check whether the second index of type_O1 is the same as the first index of type_O2

            if (args_O1[1] != args_O2[0]) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the second index of the first argument " + sig.term_to_string(args[0]) + " is not the same as the first index of the second argument " + sig.term_to_string(args[1]) + ".");
            }

            return bank.get_normal_term(OType, {args_O1[0], args_O2[1]});
        }

        if (term->is_atomic()) {

            // (Const)
            auto env_find = find_in_env(term->get_head());
            if (env_find != std::nullopt) {
                return env_find->type;
            }

            throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not assumed or defined.");
        }

        throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' cannot be typed.");
    }

    bool Kernel::is_subtype(const ualg::Term<int>* typeA, const ualg::Term<int>* typeB) {
        if (typeA == typeB) {
            return true;
        }        
        // (Base-Subtype)
        if (typeB->get_head() == TYPE && typeA->get_head() == BASE) {
            return true;
        }

        return false;
    }

    void Kernel::assum(int symbol, const Term<int>* type) {
        auto TYPE_term = bank.get_normal_term(TYPE, {});

        if (is_reserved(symbol)) {
            throw std::runtime_error("The symbol '" + sig.term_to_string(bank.get_normal_term(symbol, {})) + "' is reserved.");
        }
        if (find_in_env(symbol) != std::nullopt) {
            throw std::runtime_error("The symbol '" + sig.term_to_string(bank.get_normal_term(symbol, {})) + "' is already in the environment.");
        }

        // W-Assum-Type
        if (type == TYPE_term) {
            env.push_back({symbol, {std::nullopt, type}});
        }

        // W-Assum-Term
        else {
            if (!type_check(type, bank.get_normal_term(TYPE, {}))) {
                throw std::runtime_error("The type of the symbol '" + sig.term_to_string(bank.get_normal_term(symbol, {})) + "' is not a well-typed type.");
            }
            env.push_back({symbol, {std::nullopt, type}});
        }
    }

    void Kernel::def(int symbol, const Term<int>* term, std::optional<const ualg::Term<int>*> type) {
        if (is_reserved(symbol)) {
            throw std::runtime_error("The symbol '" + sig.term_to_string(bank.get_normal_term(symbol, {})) + "' is reserved.");
        }
        if (find_in_env(symbol) != std::nullopt) {
            throw std::runtime_error("The symbol '" + sig.term_to_string(bank.get_normal_term(symbol, {})) + "' is already in the environment."); 
        }
        if (type.has_value()) {
            if (!type_check(term, type.value())) {
                throw std::runtime_error("The term '" + sig.term_to_string(term) + "' is not well-typed with the type '" + sig.term_to_string(type.value()) + "'.");
            }
        }
        else {
            type = calc_type(term);
        }
        env.push_back({symbol, {term, type.value()}});
    }

    void Kernel::env_pop() {
        if (env.size() == 0) {
            throw std::runtime_error("The context is empty.");
        }
        env.pop_back();
    }

} // namespace diracoq