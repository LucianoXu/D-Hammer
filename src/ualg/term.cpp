
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

    std::size_t Term::get_term_size() const {
        return get_all_nodes(this).size();
    }

    bool Term::is_atomic() const {
        return get_term_size() == 1;
    }

    // get_all_nodes

    void _get_all_nodes(const Term* term, std::set<const Term*>& nodes) {
        if (nodes.find(term) != nodes.end()) {
            return;
        }
        nodes.insert(term);
        if (typeid(*term) == typeid(NormalTerm)) {
            const NormalTerm* normal_term = static_cast<const NormalTerm*>(term);
            for (const auto& arg : normal_term->get_args()) {
                _get_all_nodes(arg, nodes);
            }
        }
        else if (typeid(*term) == typeid(ACTerm)) {
            const ACTerm* ac_term = static_cast<const ACTerm*>(term);
            for (const auto& arg : ac_term->get_args()) {
                _get_all_nodes(arg.first, nodes);
            }
        }
        else {
            throw std::runtime_error("Unknown term type.");
        }
    }

    std::set<const Term*> get_all_nodes(const Term* term) {
        std::set<const Term*> nodes;
        _get_all_nodes(term, nodes);
        return nodes;
    }

    ////////////////
    // Normal Term

    NormalTerm::NormalTerm(const std::string& head) {
        this->head = head;
        hvalue = calc_hash_normal(head, args);
    }

    NormalTerm::NormalTerm(const std::string& head, const std::vector<const Term*>& normal_args) {
        this->head = head;
        args = normal_args;
        hvalue = calc_hash_normal(head, args);
    }

    NormalTerm::NormalTerm(const std::string& head, std::vector<const Term*>&& normal_args) {
        this->head = head;
        args = std::move(normal_args);
        hvalue = calc_hash_normal(head, args);
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
        if (args != other_term.args) {
            return false;
        }
        return true;
    }

    std::string NormalTerm::to_string() const {
        std::string str = head;
        if (args.size() > 0) {
            str += "(";
            for (const auto& arg : args) {
                str += arg->to_string() + ", ";
            }
            str.pop_back();
            str.pop_back();
            str += ")";
        }
        return str;
    }


    ////////////////////////////////////////////////////
    // C Terms

    CTerm::CTerm(const std::string& head) {
        this->head = head;
        hvalue = calc_hash_c(head, args);
    }

    CTerm::CTerm(const std::string& head, const TermCountMappping& c_args) {
        this->head = head;
        args = std::move(c_args);
        hvalue = calc_hash_c(head, args);
    }


    const TermCountMappping& CTerm::get_args() const {
        return args;
    }

    bool CTerm::operator == (const Term& other) const {
        if (this == &other) {
            return true;
        }
        if (typeid(other) != typeid(CTerm)) {
            return false;
        }
        const CTerm& other_term = static_cast<const CTerm&>(other);
        if (head != other_term.head) {
            return false;
        }
        if (args != other_term.args) {
            return false;
        }
        return true;
    }

    std::string CTerm::to_string() const {
        std::string str = head;
        if (args.size() > 0) {
            str += "(";
            for (const auto& arg : args) {
                str += arg.first->to_string() + ":" + std::to_string(arg.second) + ", ";
            }
            str.pop_back();
            str.pop_back();
            str += ")";
        }
        return str;
    }

    ////////////////
    // AC Terms

    ACTerm::ACTerm(const std::string& head) {
        this->head = head;
        hvalue = calc_hash_ac(head, args);
    }

    ACTerm::ACTerm(const std::string& head, const TermCountMappping& ac_args) {
        this->head = head;
        args = TermCountMappping{};

        // Check and flatten the arguments
        for (const auto& arg : ac_args) {
            // if should be flattened
            if (typeid(*arg.first) == typeid(ACTerm) && arg.first->get_head() == head) {
                // cast the term to AC term
                auto sub_term = static_cast<const ACTerm*>(arg.first);
                for (const auto& sub_arg : sub_term->get_args()) {
                    update_TermCountMapping(args, sub_arg.first, arg.second * sub_arg.second);
                }
            }
            else {
                update_TermCountMapping(args, arg.first, arg.second);
            }
        }
        hvalue = calc_hash_ac(head, args);
    }

    const TermCountMappping& ACTerm::get_args() const {
        return args;
    }

    bool ACTerm::operator == (const Term& other) const {
        if (this == &other) {
            return true;
        }
        if (typeid(other) != typeid(ACTerm)) {
            return false;
        }
        const ACTerm& other_term = static_cast<const ACTerm&>(other);
        if (head != other_term.head) {
            return false;
        }
        if (args != other_term.args) {
            return false;
        }
        return true;
    }

    std::string ACTerm::to_string() const {
        std::string str = head;
        if (args.size() > 0) {
            str += "(";
            for (const auto& arg : args) {
                str += arg.first->to_string() + ":" + std::to_string(arg.second) + ", ";
            }
            str.pop_back();
            str.pop_back();
            str += ")";
        }
        return str;
    }

}   // namespace ualg