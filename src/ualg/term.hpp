#pragma once

#include <string>
#include <set>
#include <boost/unordered_map.hpp>
#include <boost/unordered_set.hpp>
#include <boost/container_hash/hash_fwd.hpp>

namespace ualg {

    enum SymbolType {
        Normal,
        C,
        AC
    };

    using Signature = boost::unordered_map<std::string, SymbolType>;

    class Term {
    protected:
        std::string head;
        std::size_t hvalue;

    public: 
        std::size_t hash_value() const;

        const std::string& get_head() const;

        virtual bool operator == (const Term& other) const = 0;

        bool operator != (const Term& other) const;

        virtual bool is_atomic() const = 0;

        virtual std::size_t get_term_size() const = 0;

        virtual ~Term() = default;
    
    };

    inline bool hash_value(const Term& term) {
        return term.hash_value();
    }
    //////////////////////////////////////////////////////////////////////////
    // Normal Terms

    inline std::size_t calc_hash_normal(const std::string& head, const std::vector<const Term*>& args) {
        std::size_t seed = 0;
        boost::hash_combine(seed, head);
        for (const auto& arg : args) {
            boost::hash_combine(seed, arg);
        }
        return seed;
    }
    class NormalTerm : public Term {
    private:
        std::vector<const Term*> args;

    public:
        NormalTerm(const std::string& head);
        NormalTerm(const std::string& head, const std::vector<const Term*>& args);
        NormalTerm(const std::string& head, std::vector<const Term*>&& args);

        const std::vector<const Term*>& get_args() const;

        bool operator == (const Term& other) const;

        bool is_atomic() const;

        std::size_t get_term_size() const;

    };

    inline bool hash_value(const NormalTerm& term) {
        return term.hash_value();
    }
}   // namespace ualg