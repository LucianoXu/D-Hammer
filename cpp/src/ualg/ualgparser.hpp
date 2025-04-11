#include "term.hpp"
#include "astparser.hpp"

// Transform code, or AST, to terms in the bank

namespace ualg {

    template <class T>
    class Signature {
    protected:
        long long unique_var_id = 0;

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

        inline std::string unique_var() {
            return "$" + std::to_string(unique_var_id++);
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
                    throw std::runtime_error("Unimplemented error for symbol: " + name);
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

        inline std::string term_to_string(TermPtr<T> term) const {
            return term->to_string(&head2name);
        }

        astparser::AST term2ast(TermPtr<T> term) const;

        TermPtr<T> ast2term(const astparser::AST& ast);

        TermPtr<T> parse(const std::string& code);
    };


    /**
     * @brief Complie the Signature from a mapping of symbol names to symbol types.
     *
     * @param symbol_types
     * @return Signature<int>
     */
    Signature<int> compile_string_sig(const std::vector<std::string>& symbols);

    //////////////////////////////////////////////////////////
    // Implementation

    template <class T>
    TermPtr<T> Signature<T>::ast2term(const astparser::AST& ast) {

        auto head = register_symbol(ast.head);

        if (ast.children.size() == 0) {
            return std::make_shared<Term<T>>(head);
        }
        else {
            ListArgs<T> args;
            for (const auto& child : ast.children) {
                args.push_back(ast2term(child));
            }
            return std::make_shared<Term<T>>(head, std::move(args));
        }
    }

    template <class T>
    astparser::AST Signature<T>::term2ast(TermPtr<T> term) const {
        astparser::AST ast;
        ast.head = get_name(term->get_head());
        for (const auto& arg : term->get_args()) {
            ast.children.push_back(term2ast(arg));
        }
        return ast;
    }


    template <class T>
    TermPtr<T> Signature<T>::parse(const std::string& code) {
        auto ast = astparser::parse(code);
        if (!ast.has_value()) {
            throw std::runtime_error("Error: the code to parse is not valid.");
        }

        return ast2term(ast.value());
    }

} // namespace ualg