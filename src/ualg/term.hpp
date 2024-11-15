#pragma once

#include <string>
#include <set>
#include <boost/container_hash/hash_fwd.hpp>

namespace ualg {

    enum SymbolType {
        Normal,
        C,
        AC
    };

    using Signature = std::map<std::string, SymbolType>;

    class Term {
    protected:
        std::string head;
        std::size_t hvalue;

    public: 
        std::size_t hash_value() const;

        const std::string& get_head() const;

        virtual bool operator == (const Term& other) const = 0;

        bool operator != (const Term& other) const;

        std::size_t get_term_size() const;

        bool is_atomic() const;

        virtual std::string to_string() const = 0;

        virtual ~Term() = default;
    
    };

    inline bool hash_value(const Term& term) {
        return term.hash_value();
    }

    /**
     * @brief Get all the unique nodes in the term.
     * 
     * @param term 
     * @return std::set<const Term*> 
     */
    std::set<const Term*> get_all_nodes(const Term* term);

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
        NormalTerm(const std::string& head, const std::vector<const Term*>& normal_args);
        NormalTerm(const std::string& head, std::vector<const Term*>&& normal_args);

        const std::vector<const Term*>& get_args() const;

        bool operator == (const Term& other) const;

        std::string to_string() const;

    };

    inline bool hash_value(const NormalTerm& term) {
        return term.hash_value();
    }

    //////////////////////////////////////////////////////////////////////////
    // TermCountMapping and corresponding functions

    using TermCountMappping = std::map<const Term*, unsigned int>;

    inline void update_TermCountMapping(TermCountMappping& mapping, const Term* term, unsigned int count) {
        if (mapping.find(term) != mapping.end()) {
            mapping[term] += count;
        }
        else {
            mapping[term] = count;
        }
    }

    //////////////////////////////////////////////////////////////////////////
    // C Terms

    inline std::size_t calc_hash_c(const std::string& head, const TermCountMappping& args) {
        std::size_t seed = 0;
        boost::hash_combine(seed, head);
        for (const auto& arg : args) {
            boost::hash_combine(seed, arg.first);
            boost::hash_combine(seed, arg.second);
        }
        return seed;
    }
    
    class CTerm : public Term {
    private:
        TermCountMappping args;

    public:
        CTerm(const std::string& head);
        CTerm(const std::string& head, const TermCountMappping& c_args);
        // "CTerm(const std::string& head, TermCountMappping&& ac_args)" not needed
        

        const TermCountMappping& get_args() const;

        bool operator == (const Term& other) const;

        std::string to_string() const;

    };

    inline bool hash_value(const CTerm& term) {
        return term.hash_value();
    }

    //////////////////////////////////////////////////////////////////////////
    // AC Terms

    inline std::size_t calc_hash_ac(const std::string& head, const TermCountMappping& args) {
        std::size_t seed = 0;
        boost::hash_combine(seed, head);
        for (const auto& arg : args) {
            boost::hash_combine(seed, arg.first);
            boost::hash_combine(seed, arg.second);
        }
        return seed;
    }

    class ACTerm : public Term {
    private:
        TermCountMappping args;

    public:
        ACTerm(const std::string& head);
        ACTerm(const std::string& head, const TermCountMappping& ac_args);
        // "ACTerm(const std::string& head, TermCountMappping&& ac_args)" not needed
        

        const TermCountMappping& get_args() const;

        bool operator == (const Term& other) const;

        std::string to_string() const;

    };

    inline bool hash_value(const ACTerm& term) {
        return term.hash_value();
    }

}   // namespace ualg