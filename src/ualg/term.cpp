
#include "ualg.hpp"

namespace ualg {

    ///////////////
    // Term

    std::size_t Term::hash_value() const {
        return hvalue;
    }

    const std::string& Term::get_head() const {
        return head;
    }

    bool Term::operator != (const Term& other) const {
        return !(*this == other);
    }

    ////////////////
    // Normal Term

    NormalTerm::NormalTerm(const std::string& head) {
        this->head = head;
        hvalue = calc_hash_normal(head, args);
    }

    NormalTerm::NormalTerm(const std::string& head, const std::vector<const Term*>& args) {
        this->head = head;
        this->args = args;
        hvalue = calc_hash_normal(head, args);
    }

    NormalTerm::NormalTerm(const std::string& head, std::vector<const Term*>&& args) {
        this->head = head;
        this->args = std::move(args);
        hvalue = calc_hash_normal(head, this->args);
    }

    const std::vector<const Term*>& NormalTerm::get_args() const {
        return args;
    }

    bool NormalTerm::operator == (const Term& other) const {
        if (this == &other) {
            return true;
        }
        if (typeid(other) != typeid(NormalTerm)) {
            return false;
        }
        const NormalTerm& other_term = static_cast<const NormalTerm&>(other);
        if (head != other_term.head) {
            return false;
        }
        if (args.size() != other_term.args.size()) {
            return false;
        }
        for (unsigned int i = 0; i < args.size(); ++i) {
            if (*args[i] != *other_term.args[i]) {
                return false;
            }
        }
        return true;
    }

    bool NormalTerm::is_atomic() const {
        return args.size() == 0;
    }

    std::size_t NormalTerm::get_term_size() const {
        std::size_t size = 1;

        // only count the unique subterms
        for (int i = 0; i < args.size(); ++i) {
            for (int j = i + 1; j < args.size(); ++j) {
                if (args[i] == args[j]) {
                    goto SIZE_SKIP;
                }
            }
            size += args[i]->get_term_size();
SIZE_SKIP:
        }
        return size;
    }

}   // namespace ualg