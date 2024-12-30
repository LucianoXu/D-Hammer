#pragma once

#include <string>
#include <set>
#include <boost/container_hash/hash_fwd.hpp>
#include <boost/functional/hash.hpp>

namespace ualg {

    inline std::string data_to_string(const std::string& str) {
        return str;
    }

    inline std::size_t hash_value(const std::string& str) {
        boost::hash<std::string> string_hash;
        return string_hash(str);
    }

    inline std::string data_to_string(const int& i) {
        return std::to_string(i);
    }

    inline std::size_t hash_value(const int& i) {
        return i;
    }



    using TermPos = std::vector<unsigned int>;

    template <class T>
    class Term;

    template <class T>
    using ListArgs = std::vector<const Term<T>*>;



    template <class T>
    inline std::size_t calc_hash(const T& head, const ListArgs<T>& args) {
        std::size_t seed = 0;
        boost::hash_combine(seed, hash_value(head));
        for (const auto& arg : args) {
            boost::hash_combine(seed, arg);
        }
        return seed;
    }


    /**
     * @brief The abstract class for terms.
     * 
     * @tparam T The type of the head(data) of the term. The function std::string data_to_string(const T&) and std::size_t hash_value(const T&) should be defined.
     */
    template <class T>
    class Term {
    protected:
        T head;
        std::size_t hvalue;
        ListArgs<T> args;

    public: 
        Term(const T& head);
        Term(const T& head, const ListArgs<T>& args);
        Term(const T& head, ListArgs<T>&& args);

        std::size_t hash_value() const;

        const T& get_head() const;
        const ListArgs<T>& get_args() const;

        bool operator == (const Term& other) const;
        bool operator < (const Term<T>& other) const;
        bool operator != (const Term& other) const;

        std::set<const Term<T>*> get_all_nodes() const;
        std::size_t get_term_size() const;

        bool is_atomic() const;

        std::string to_string(std::shared_ptr<const std::map<T, std::string>> head_naming=nullptr) const;

        const Term<T>* get_subterm(const TermPos& pos) const;

        virtual ~Term() = default;
    
    };



    template <class T>
    inline std::size_t hash_value(const Term<T>& term) {
        return term.hash_value();
    }

    /////////////////////////////////////////////////////////////////
    // Implementations


    ///////////////
    // Term

    template <class T>
    Term<T>::Term(const T& head) {
        this->head = head;
        this->hvalue = calc_hash(head, this->args);
    }

    template <class T>
    Term<T>::Term(const T& head, const ListArgs<T>& args) {
        this->head = head;
        this->args = args;
        this->hvalue = calc_hash(head, this->args);
    }

    template <class T>
    Term<T>::Term(const T& head, ListArgs<T>&& args) {
        this->head = head;
        this->args = std::move(args);
        this->hvalue = calc_hash(head, this->args);
    }


    template <class T>
    std::size_t Term<T>::hash_value() const {
        return this->hvalue;
    }

    template <class T>
    const T& Term<T>::get_head() const {
        return this->head;
    }

    template <class T>
    const ListArgs<T>& Term<T>::get_args() const {
        return this->args;
    }


    template <class T>
    bool Term<T>::operator == (const Term<T>& other) const {
        if (this == &other) {
            return true;
        }
        if (this->head != other.head) {
            return false;
        }
        if (this->args != other.args) {
            return false;
        }
        return true;
    }

    template <class T>
    bool Term<T>::operator != (const Term<T>& other) const {
        return !(*this == other);
    }

    template <class T>
    bool Term<T>::operator < (const Term<T>& other) const {
        if (this->hvalue < other.hash_value()) {
            return true;
        }

        if (this->head != other.head) {
            return this->head < other.head;
        }
        return this->args < other.args;
    }


    template <class T>
    std::size_t Term<T>::get_term_size() const {
        return get_all_nodes().size();
    }

    template <class T>
    bool Term<T>::is_atomic() const {
        return get_term_size() == 1;
    }


    template <class T>
    std::string Term<T>::to_string(std::shared_ptr<const std::map<T, std::string>> head_naming) const {
        std::string str;
        if (head_naming != nullptr) {
            str = head_naming->at(this->head);
        }
        else {
            str = data_to_string(this->head);
        }
        if (args.size() > 0) {
            str += "[" + args[0]->to_string(head_naming);
            for (int i = 1; i < args.size(); i++) {
                str += ", " + args[i]->to_string(head_naming);
            }
            str += "]";
        }
        return str;
    }

    template <class T>
    const Term<T>* Term<T>::get_subterm(const TermPos& pos) const {
        if (pos.size() == 0) {
            return this;
        }

        if (pos[0] >= args.size()) {
            throw std::runtime_error("Position out of range.");
        }

        return args[pos[0]];
    }

    template <class T>
    void _get_all_nodes(const Term<T>* term, std::set<const Term<T>*>& nodes) {
        if (nodes.find(term) != nodes.end()) {
            return;
        }
        nodes.insert(term);
        
        for (const auto& arg : term->get_args()) {
            _get_all_nodes(arg, nodes);
        }
    }

    template <class T>
    std::set<const Term<T>*> Term<T>::get_all_nodes() const {
        std::set<const Term<T>*> nodes;
        _get_all_nodes(this, nodes);
        return nodes;
    }

}   // namespace ualg