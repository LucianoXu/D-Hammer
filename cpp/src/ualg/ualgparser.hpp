#include "term.hpp"
#include "termbank.hpp"
#include "astparser.hpp"

// Transform code, or AST, to terms in the bank

namespace ualg {

    template <class T>
    class Signature {
    protected:
        // The mapping from inner representations to head names
        std::map<T, std::string> head2name;

        // The mapping from head names to inner representations
        std::map<std::string, T> name2head;

    public:
        Signature(std::map<std::string, T> name2head) : name2head(name2head) {
            for (const auto& [name, head] : name2head) {
                head2name[head] = name;
            }
        }

        // copy constructor
        Signature(const Signature& other) {
            // deep copy the mappings
            head2name = other.head2name;
            name2head = other.name2head;
        }

        inline T register_symbol(const std::string& name) {
            auto find = name2head.find(name);

            if (find == name2head.end()) {
                if constexpr(std::is_same_v<T, int>) {
                    int repr = head2name.size();
                    add_symbol(name, repr);
                    return repr;
                }
                else if constexpr(std::is_same_v<T, std::string>) {
                    std::string repr = name;
                    add_symbol(name, repr);
                    return repr;
                }
                else {
                    throw std::runtime_error("Unknown symbol: " + name);
                }
            }

            return find->second;
        }

        inline std::optional<T> find_repr(const std::string& name) const {
            auto find = name2head.find(name);
            if (find == name2head.end()) {
                return std::nullopt;
            }
            return find->second;
        }

        inline T get_repr(const std::string& name) const {
            return name2head.at(name);
        }

        inline std::optional<std::string> find_name(const T& head) const {
            auto find = head2name.find(head);
            if (find == head2name.end()) {
                return std::nullopt;
            }
            return find->second;
        }

        inline std::string get_name(const T& head) const {
            return head2name.at(head);
        }

        // Add a symbol to the signature
        inline void add_symbol(const std::string& name, const T& head) {
            name2head[name] = head;
            head2name[head] = name;
        }

        inline std::string term_to_string(const Term<T>* term) const {
            return term->to_string(std::make_shared<const std::map<T, std::string>>(head2name));
        }
    };

    template <class T>
    const Term<T>* ast2term(Signature<T>& sig, TermBank<T>& bank, const astparser::AST& ast);

    template <class T>
    const Term<T>* parse(Signature<T>& sig, TermBank<T>& bank, const std::string& code);

    /**
     * @brief Complie the Signature from a mapping of symbol names to symbol types.
     * 
     * @param symbol_types 
     * @return Signature<int> 
     */
    Signature<int> compile_string_sig(const std::vector<std::string>& symbols);

    //////////////////////////////////////////////////////////
    // Implementation

    /**
     * @brief Transform the AST to a term in the bank.
     * 
     * Note that it will try to register new symbols (head of the terms) in the signature, using signature.register_symbol.
     * 
     * @tparam T 
     * @param sig 
     * @param bank 
     * @param ast 
     * @return const Term<T>* 
     */
    template <class T>
    const Term<T>* ast2term(Signature<T>& sig, TermBank<T>& bank, const astparser::AST& ast) {
        
        auto head = sig.register_symbol(ast.head);

        if (ast.children.size() == 0) {
            return bank.get_term(head);
        }
        else {
            ListArgs<T> args;
            for (const auto& child : ast.children) {
                args.push_back(ast2term(sig, bank, child));
            }
            return bank.get_term(head, std::move(args));
        }
    }

    /**
     * @brief Parse the code into a term in the bank.
     * 
     * @throws std::runtime_error if the code is not valid.
     * 
     * @tparam T 
     * @param sig 
     * @param bank 
     * @param code 
     * @return const Term<T>* 
     */
    template <class T>
    const Term<T>* parse(Signature<T>& sig, TermBank<T>& bank, const std::string& code) {
        auto ast = astparser::parse(code);
        if (!ast.has_value()) {
            throw std::runtime_error("Error: the code to parse is not valid.");
        }

        return ast2term(sig, bank, ast.value());
    }

    template <class T>
    const Term<T>* parse(Signature<T>& sig, TermBank<T>& bank, const astparser::AST& ast) {
        return ast2term(sig, bank, ast);
    }

} // namespace ualg