#include "term.hpp"
#include "termbank.hpp"

namespace ualg {

    /**
     * @brief The rewriting rule type.
     * 
     * Every rewriting rule is a function that takes a term bank, a term, 
     * and returns an optional term.
     * 
     * @tparam T The type of the term.
     * @param bank The term bank.
     * @param term The term to be rewritten.
     * 
     * @return If the rule can be applied to the term, returns the rewritten term. 
     *         Otherwise, returns `nullopt`.
     */
    template <class T>
    using RewritingRule = std::optional<const Term<T>*> (*)(TermBank<T>& bank, const Term<T>* term);

    // The parameter base class
    struct ParameterBase {
        virtual ~ParameterBase() = default;
    };

    template<typename T>
    struct Parameter : ParameterBase {
        T value;
        Parameter(const T& v) : value(v) {}
    };

    template <class T>
    struct RewritingRecord {
        RewritingRule<T> rule;
        const Term<T>* term;
        std::unique_ptr<ParameterBase> parameter;
        
        template <class ParamT>
        RewritingRecord(RewritingRule<T> rule, const Term<T>* term, const ParamT& param) 
            : rule(rule), term(term), parameter(std::make_unique<Parameter<ParamT>>(param)) {}

        // Get the parameter value of specific type
        template<typename ParamT>
        ParamT& get_parameter() const {
            return static_cast<Parameter<ParamT>*>(parameter.get())->value;
        }    
    };

    template <class T>
    using RecordPrinter = std::string (*)(const Signature<T>&, const RewritingRecord<T>&);

    /**
     * @brief The rewriting trace type.
     * 
     * @tparam T 
     */
    template <class T>
    using RewritingTrace = std::vector<RewritingRecord<T>>;


    /**
     * @brief The function to convert the rewriting trace to a string.
     * 
     * @tparam T 
     * @param sig
     * @param trace 
     * @param printer 
     * @return std::string 
     */
    template <class T>
    std::string trace_to_string(const Signature<T>& sig, const RewritingTrace<T>& trace, RecordPrinter<T> printer) {
        std::string res = "";
        for (const auto& r : trace) {
            res += printer(sig, r);
        }
        return res;
    }
        

/**
 * @brief The macro for defining a rewriting rule.
 * 
 * This macro is used to define a rewriting rule by specifying the rule's name,
 * the term bank, and the term to be rewritten.
 * 
 * @tparam T The type of the term.
 * @param name The name of the rewriting rule.
 * @param bank The term bank.
 * @param term The term to be rewritten.
 * 
 * @note Usage: 
 * REWRITE_DEF(name, bank, term) { ... }
 */
#define REWRITE_DEF(T, name, bank, term) std::optional<const ualg::Term<T>*> name(ualg::TermBank<T>& bank, const ualg::Term<T>* term)

#define REWRITE_COMPILED_DEF(name, bank, term) std::optional<const ualg::Term<int>*> name(ualg::TermBank<int>& bank, const ualg::Term<int>* term)


    //////////////////////////////////////////////////////////////////////////
    // Some helper functions for structurize the rewriting rules
    template <class T>
    inline bool match_normal_head(const Term<T>* term, const T& head, ListArgs<T>& args) {
        if (term->get_head() == head) {
            // cast into the normal term
            const NormalTerm<T>* normal_term = static_cast<const NormalTerm<T>*>(term);
            args = normal_term->get_args();
            return true;
        }
        return false;
    }

    template <class T>
    inline bool match_c_head(const Term<T>* term, const T& head, TermCountMapping<T>& args) {
        if (term->get_head() == head) {
            // cast into the c term
            const CTerm<T>* c_term = static_cast<const CTerm<T>*>(term);
            args = c_term->get_args();
            return true;
        }
        return false;
    }

    template <class T>
    inline bool match_ac_head(const Term<T>* term, const T& head, TermCountMapping<T>& args) {
        if (term->get_head() == head) {
            // cast into the ac term
            const ACTerm<T>* ac_term = static_cast<const ACTerm<T>*>(term);
            args = ac_term->get_args();
            return true;
        }
        return false;
    }


    /**
     * @brief Find a mapping from the term to the rewritten term using the given rewriting rules.
     * 
     * 
     * @tparam T The type of the term.
     * @param bank (TermBank): The term bank.
     * @param term (Term): The term to be matched and rewritten.
     * @param rules (std::vector<RewritingRule>): The rewriting rules.
     * @param trace (RewritingTrace): The trace of the rewriting. If `nullptr`, the trace will not be recorded.
     * 
     * @return Optional<TermMapping>: If the term can be rewritten, it returns the mapping from the term to the rewritten term. Otherwise, it returns nullopt.
     */
    template <class T>
    std::optional<TermMapping<T>> get_mapping(TermBank<T>& bank, const Term<T>* term, const std::vector<RewritingRule<T>>& rules, RewritingTrace<T>* trace = nullptr);

    /**
     * @brief Rewrite the term one step, using the given rewriting rules.
     * 
     * Here one step means that the procedure of finding the mapping, and conducting the replacement is done once.
     * 
     * @tparam T The type of the term.
     * @param bank (TermBank): The term bank.
     * @param term (Term): the term to be rewritten.
     * @param rules (std::vector<RewritingRule>): The rewriting rules.
     * @param trace (RewritingTrace): The trace of the rewriting. If `nullptr`, the trace will not be recorded.
     * 
     * @return Optional<const Term*>: If the term can be rewritten, it returns the rewritten term. Otherwise, it returns nullopt.
     */
    template <class T>
    std::optional<const Term<T>*> rewrite_all(TermBank<T>& bank, const Term<T>* term, const std::vector< RewritingRule<T>>& rules, RewritingTrace<T>* trace = nullptr);


    /**
     * @brief Rewrite the term repeatedly using the given rewriting rules, until no more rules can apply.
     * 
     * @tparam T The type of the term.
     * @param bank (TermBank): The term bank.
     * @param term (Term): The term to be rewritten.
     * @param rules (std::vector<RewritingRule>): The rewriting rules.
     * @param trace (RewritingTrace): The trace of the rewriting. If `nullptr`, the trace will not be recorded.
     * 
     * @return const Term* : the repeated rewritten result.
     */
    template <class T>
    const Term<T>* rewrite_repeated(TermBank<T>& bank, const Term<T>* term, const std::vector<RewritingRule<T>>& rules, RewritingTrace<T>* trace = nullptr);


    //////////////////////////////////////////////////////////////////////////
    // Implementation


    template <class T>
    std::optional<TermMapping<T>> get_mapping(TermBank<T>& bank, const Term<T>* term, const std::vector<RewritingRule<T>>& rules, RewritingTrace<T>* trace) {

        // Check whether the rule can be applied to this term
        for (const auto& rule : rules) {
            auto apply_res = rule(bank, term);
            if (apply_res.has_value()) {
                // return the discovered mapping
                TermMapping<T> mapping = {{term, apply_res.value()}};

                // record the trace
                if (trace != nullptr) {
                    // for normal rewriting rules, we don't need to record the parameter
                    trace->push_back({rule, term, nullptr});
                }

                return std::move(mapping);
            }
        }
        
        // Check whether the rule can be applied to the subterms
        if (typeid(*term) == typeid(NormalTerm<T>)) {
            const NormalTerm<T>* normal_term = static_cast<const NormalTerm<T>*>(term);

            for (const auto& arg : normal_term->get_args()) {
                auto mapping = get_mapping(bank, arg, rules, trace);
                if (mapping.has_value()) {
                    return mapping;
                }
            }
        }

        else if (typeid(*term) == typeid(CTerm<T>)) {
            const CTerm<T>* c_term = static_cast<const CTerm<T>*>(term);

            for (const auto& [arg, _] : c_term->get_args()) {
                auto mapping = get_mapping(bank, arg, rules, trace);
                if (mapping.has_value()) {
                    return mapping;
                }
            }
        }

        else if (typeid(*term) == typeid(ACTerm<T>)) {
            const ACTerm<T>* ac_term = static_cast<const ACTerm<T>*>(term);

            for (const auto& [arg, _] : ac_term->get_args()) {
                auto mapping = get_mapping(bank, arg, rules, trace);
                if (mapping.has_value()) {
                    return mapping;
                }
            }
        }

        else {
            throw std::runtime_error("Unknown term type.");
        }

        return std::nullopt;
    }


    template <class T>
    std::optional<const Term<T>*> rewrite_all(TermBank<T>& bank, const Term<T>* term, const std::vector<RewritingRule<T>>& rules, RewritingTrace<T>* trace) {

        auto mapping = get_mapping(bank, term, rules, trace);

        if (!mapping.has_value()) {
            return std::nullopt;
        }

        return bank.replace_term(term, mapping.value());
    }

    template <class T>
    const Term<T>* rewrite_repeated(TermBank<T>& bank, const Term<T>* term, const std::vector<RewritingRule<T>>& rules, RewritingTrace<T>* trace) {
        auto current_term = term;
        while (true) {
            auto rewritten_term = rewrite_all(bank, current_term, rules, trace);
            if (!rewritten_term.has_value()) {
                return current_term;
            }
            current_term = rewritten_term.value();
        }
    }


} // namespace ualg