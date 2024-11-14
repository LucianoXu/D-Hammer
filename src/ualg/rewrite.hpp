#include "term.hpp"
#include "termbank.hpp"

namespace ualg {

    /**
     * @brief The rewriting rule type.
     * 
     * Every rewriting rule is a function that takes a term bank, a term, 
     * and returns an optional term.
     * 
     * @param bank The term bank.
     * @param term The term to be rewritten.
     * 
     * @return If the rule can be applied to the term, returns the rewritten term. 
     *         Otherwise, returns `nullopt`.
     */
    using RewritingRule = std::optional<const Term*> (*)(TermBank& bank, const Term* term);

/**
 * @brief The macro for defining a rewriting rule.
 * 
 * This macro is used to define a rewriting rule by specifying the rule's name,
 * the term bank, and the term to be rewritten.
 * 
 * @param name The name of the rewriting rule.
 * @param bank The term bank.
 * @param term The term to be rewritten.
 * 
 * @note Usage: 
 * REWRITE_DEF(name, bank, term) { ... }
 */
#define REWRITE_DEF(name, bank, term) std::optional<const ualg::Term*> name(ualg::TermBank& bank, const ualg::Term* term)

    /**
     * @brief Find a mapping from the term to the rewritten term using the given rewriting rules.
     * 
     * 
     * @param bank (TermBank): The term bank.
     * @param term (Term): The term to be matched and rewritten.
     * @param rules (std::vector<RewritingRule>): The rewriting rules.
     * 
     * @return Optional<TermMapping>: If the term can be rewritten, it returns the mapping from the term to the rewritten term. Otherwise, it returns nullopt.
     */
    std::optional<TermMapping> get_mapping(TermBank& bank, const Term* term, const std::vector<RewritingRule>& rules);

    /**
     * @brief Rewrite the term one step, using the given rewriting rules.
     * 
     * Here one step means that the procedure of finding the mapping, and conducting the replacement is done once.
     * 
     * @param bank (TermBank): The term bank.
     * @param term (Term): the term to be rewritten.
     * @param rules (std::vector<RewritingRule>): The rewriting rules.
     * 
     * @return Optional<const Term*>: If the term can be rewritten, it returns the rewritten term. Otherwise, it returns nullopt.
     */
    std::optional<const Term*> rewrite_all(TermBank& bank, const Term* term, const std::vector< RewritingRule>& rules);


    /**
     * @brief Rewrite the term repeatedly using the given rewriting rules, until no more rules can apply.
     * 
     * @param bank (TermBank): The term bank.
     * @param term (Term): The term to be rewritten.
     * @param rules (std::vector<RewritingRule>): The rewriting rules.
     * @return const Term* : the repeated rewritten result.
     */
    const Term* rewrite_repeated(TermBank& bank, const Term* term, const std::vector<RewritingRule>& rules);

} // namespace ualg