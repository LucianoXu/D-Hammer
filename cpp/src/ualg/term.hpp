#pragma once

#include <string>
#include <set>

namespace ualg {

    inline std::string data_to_string(const std::string& str) {
        return str;
    }

    inline std::string data_to_string(const int& i) {
        return std::to_string(i);
    }


    using TermPos = std::vector<unsigned int>;

    template <class T>
    class Term;

    template <class T>
    using TermPtr = std::shared_ptr<const Term<T>>;

    template <class T>
    using ListArgs = std::vector<TermPtr<T>>;

    enum COMPARE_TYPE {
        EQUAL,
        LESS,
        GREATER
    };

    /**
     * @brief The abstract class for terms.
     * 
     * @tparam T The type of the head(data) of the term. The function std::string data_to_string(const T&) and std::size_t hash_value(const T&) should be defined.
     */
    template <class T>
    class Term : public std::enable_shared_from_this<Term<T>> {
    protected:
        T head;
        ListArgs<T> args;

    public: 
        Term(const T& head);
        Term(const T& head, const ListArgs<T>& args);
        Term(const T& head, ListArgs<T>&& args);

        const T& get_head() const;
        const ListArgs<T>& get_args() const;

        COMPARE_TYPE compare(const Term<T>& other) const;

        bool operator == (const Term& other) const;
        bool operator < (const Term<T>& other) const;
        bool operator != (const Term& other) const;

        std::size_t get_term_size() const;

        bool is_atomic() const;

        std::string to_string(const std::map<T, std::string>* head_naming=nullptr) const;

        TermPtr<T> get_subterm(const TermPos& pos) const;

        TermPtr<T> replace_term(TermPtr<T> pattern, TermPtr<T> replacement) const;

        TermPtr<T> replace_at(const TermPos& pos, TermPtr<T> new_subterm) const;

        virtual ~Term() = default;
    
    };


    /////////////////////////////////////////////////////////////////
    // Implementations


    ///////////////
    // Term

    template <class T>
    Term<T>::Term(const T& head) {
        this->head = head;
    }

    template <class T>
    Term<T>::Term(const T& head, const ListArgs<T>& args) {
        this->head = head;
        this->args = args;
    }

    template <class T>
    Term<T>::Term(const T& head, ListArgs<T>&& args) {
        this->head = head;
        this->args = std::move(args);
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
    COMPARE_TYPE Term<T>::compare(const Term<T>& other) const {
        if (this->head != other.head) {
            return this->head < other.head ? LESS : GREATER;
        }
        if (this->args.size() != other.args.size()) {
            return this->args.size() < other.args.size() ? LESS : GREATER;
        }
        for (int i = 0; i < this->args.size(); i++) {
            auto comp = this->args[i]->compare(*other.args[i]);
            if (comp != EQUAL) {
                return comp;
            }
        }
        return EQUAL;
    }
    

    template <class T>
    bool Term<T>::operator == (const Term<T>& other) const {
        return compare(other) == EQUAL;
    }

    template <class T>
    bool Term<T>::operator != (const Term<T>& other) const {
        return !(*this == other);
    }

    template <class T>
    bool Term<T>::operator < (const Term<T>& other) const {
        return compare(other) == LESS;
    }


    template <class T>
    std::size_t Term<T>::get_term_size() const {
        std::size_t size = 1;
        for (const auto& arg : args) {
            size += arg->get_term_size();
        }
        return size;
    }

    template <class T>
    bool Term<T>::is_atomic() const {
        return args.size() == 0;
    }


    template <class T>
    std::string Term<T>::to_string(const std::map<T, std::string>* head_naming) const {
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
    TermPtr<T> Term<T>::get_subterm(const TermPos& pos) const {
        if (pos.size() == 0) {
            return this->shared_from_this();
        }

        if (pos[0] >= args.size()) {
            throw std::runtime_error("Position out of range.");
        }

        return args[pos[0]];
    }

    template <class T>
    TermPtr<T> Term<T>::replace_term(TermPtr<T> pattern, TermPtr<T> replacement) const {
        if (*this == *pattern) {
            return replacement;
        }

        ListArgs<T> new_args;
        for (const auto& arg : args) {
            new_args.push_back(arg->replace_term(pattern, replacement));
        }
        return std::make_shared<const Term<T>>(this->head, std::move(new_args));
    }


    template <class T>
    TermPtr<T> Term<T>::replace_at(const TermPos& pos, TermPtr<T> new_subterm) const {

        if (pos.size() == 0) {
            return new_subterm;
        }

        auto args = this->get_args();

        ListArgs<T> new_args;
        for (unsigned int i = 0; i < args.size(); i++) {
            if (i == pos[0]) {
                auto new_term = args[i]->replace_at(
                    TermPos(pos.begin() + 1, pos.end()),
                    new_subterm);

                new_args.push_back(new_term);
            }
            else {
                new_args.push_back(args[i]);
            }
        }
        return std::make_shared<const Term<T>>(this->head, std::move(new_args));
    }


}   // namespace ualg