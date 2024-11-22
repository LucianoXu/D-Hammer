#include "term.hpp"
#include "termbank.hpp"
#include "astparser.hpp"

// Transform code, or AST, to terms in the bank

namespace ualg {

    enum SymbolType {
        NORMAL,
        C,
        AC
    };

    template <class T>
    class Signature {
    protected:
        // The mapping from inner representations to head names
        std::map<T, std::string> head_naming;
        // The mapping from head names to inner representations
        std::map<std::string, T> head_mapping;

        // The mapping from inner representations to symbol types
        std::map<T, SymbolType> symbol_types;

    public:
        Signature(
            std::map<std::string, T> head_mapping,
            std::map<T, SymbolType> symbol_types
        ) : head_mapping(head_mapping), symbol_types(symbol_types) {
            for (const auto& [name, head] : head_mapping) {
                head_naming[head] = name;
            }
        }

        // copy constructor
        Signature(const Signature& other) {
            // deep copy the mappings
            head_naming = other.head_naming;
            head_mapping = other.head_mapping;
            symbol_types = other.symbol_types;
        }

        inline std::optional<T> find_repr(const std::string& name) const {
            auto find = head_mapping.find(name);
            if (find == head_mapping.end()) {
                return std::nullopt;
            }
            return find->second;
        }

        inline T get_repr(const std::string& name) const {
            return head_mapping.at(name);
        }

        inline std::optional<std::string> find_name(const T& head) const {
            auto find = head_naming.find(head);
            if (find == head_naming.end()) {
                return std::nullopt;
            }
            return find->second;
        }

        inline std::string get_name(const T& head) const {
            return head_naming.at(head);
        }

        inline std::optional<SymbolType> find_symbol_type(const T& head) const {
            auto find = symbol_types.find(head);
            if (find == symbol_types.end()) {
                return std::nullopt;
            }
            return find->second;
        }

        inline SymbolType get_symbol_type(const T& head) const {
            return symbol_types.at(head);
        }

        // Add a symbol to the signature
        inline void add_symbol(const std::string& name, const T& head, SymbolType symbol_type) {
            head_mapping[name] = head;
            head_naming[head] = name;
            symbol_types[head] = symbol_type;
        }

        inline std::string term_to_string(const Term<T>* term) const {
            return term->to_string(std::make_shared<const std::map<T, std::string>>(head_naming));
        }
    };

    template <class T>
    const Term<T>* ast2term(const Signature<T>& sig, TermBank<T>& bank, const astparser::AST& ast);

    template <class T>
    const Term<T>* parse(const Signature<T>& sig, TermBank<T>& bank, const std::string& code);


    class IntSignature : public Signature<int> {
    public:
        IntSignature(
            std::map<std::string, int> head_mapping,
            std::map<int, SymbolType> symbol_types
        ) : Signature<int>(head_mapping, symbol_types) {}
        
        /**
         * @brief Get the symbol repr.
         * 
         * It will add the symbol to the signature if it does not exist.
         * 
         * @param name 
         * @return int , the symbol representation.
         */
        int get_symbol_repr(const std::string& name) {
            auto find = head_mapping.find(name);
            if (find == head_mapping.end()) {
                int repr = head_mapping.size();
                add_symbol(name, repr, SymbolType::NORMAL);
                return repr;
            }

            return find->second;
        }

    };

    using StringSymbolType = std::vector<std::pair<std::string, SymbolType>>;

    /**
     * @brief Complie the Signature from a mapping of symbol names to symbol types.
     * 
     * @param symbol_types 
     * @return Signature<int> 
     */
    IntSignature compile_string_sig(const StringSymbolType& symbol_types);

    //////////////////////////////////////////////////////////
    // Implementation

    template <class T>
    const Term<T>* ast2term(const Signature<T>& sig, TermBank<T>& bank, const astparser::AST& ast) {
        auto repr_find = sig.find_repr(ast.head);
        if (repr_find == std::nullopt) {
            throw std::runtime_error("Unknown symbol: " + ast.head);
        }
        auto head = *repr_find;

        auto symbol_type = sig.get_symbol_type(head);

        switch (symbol_type) {

        case SymbolType::NORMAL:
            if (ast.children.size() == 0) {
                return bank.get_normal_term(head, {});
            }
            else {
                ListArgs<T> args;
                for (const auto& child : ast.children) {
                    args.push_back(ast2term(sig, bank, child));
                }
                return bank.get_normal_term(head, std::move(args));
            }
            break;

        case SymbolType::C:
            if (ast.children.size() == 0) {
                return bank.get_c_term(head, {});
            }
            else {
                TermCountMapping<T> args;
                for (const auto& child : ast.children) {
                    const Term<T>* child_term = ast2term(sig, bank, child);
                    add_TermCountMapping(args, child_term, 1);
                }
                return bank.get_c_term(head, std::move(args));
            }
            break;

        case SymbolType::AC:
            if (ast.children.size() == 0) {
                return bank.get_ac_term(head, {});
            }
            else {
                TermCountMapping<T> args;
                for (const auto& child : ast.children) {
                    const Term<T>* child_term = ast2term(sig, bank, child);
                    add_TermCountMapping(args, child_term, 1);
                }
                return bank.get_ac_term(head, std::move(args));
            }
            break;

        default:
            throw std::runtime_error("Unknown symbol type.");
        }
    }

    template <class T>
    const Term<T>* parse(const Signature<T>& sig, TermBank<T>& bank, const std::string& code) {
        auto ast = astparser::parse(code);
        return ast2term(sig, bank, ast);
    }

} // namespace ualg