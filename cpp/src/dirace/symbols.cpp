#include "dirace.hpp"

namespace dirace {
    using namespace std;
    using namespace ualg;

    const int deBruijn_index_num = 1024;



    vector<string> gen_symbols() {

        vector<string> res = {};

        for (int i = 0; i < deBruijn_index_num; ++i) {
            res.push_back("$" + to_string(i));
        }
        
        vector<string> defined = {

            "GROUP"
            "DEF",
            "VAR",
            "CHECK",
            "SHOW",
            "SHOWALL",
            "NORMALIZE",
            "CHECKEQ",
            "TRACE",

            "COMPO",
            "ADDG",
            "STAR",
            "SSUM",

            "INDEX",
            "TYPE",

            "PROD",
            "QBIT", "BASIS0", "BASIS1",

            "BASIS",
            "STYPE",
            "KTYPE",
            "BTYPE",
            "OTYPE",
            "ARROW",
            "FORALL",
            "SET",
            
            "PAIR",
            "FUN",
            "IDX",
            "APPLY",
                    

            "0",
            "1",
            "Plus",
            "Times",
            "Conjugate",
            "DELTA",
            "DOT",

            "ZEROK",
            "ZEROB",
            "ZEROO",
            "ONEO",
            "KET",
            "BRA",
            "ADJ",
            "SCR",
            "ADD",
            "TSR",

            "MULK",
            "MULB",
            "OUTER",
            "MULO",

            "USET",
            "CATPROD",
            "SUM",

            "RSET",
            "DTYPE", "REG",
            "LKET", "LBRA", "SUBS", "LTSR", "LDOT"
        };

        for (const auto &s : defined) {
            res.push_back(s);
        }

        return res;
    }

    vector<string> dirace_symbols = gen_symbols();

    const Signature<int> dirace_sig = compile_string_sig(dirace_symbols);

    const auto COMPO = dirace_sig.get_repr("COMPO");
    const auto ADDG = dirace_sig.get_repr("ADDG");
    const auto STAR = dirace_sig.get_repr("STAR");
    const auto SSUM = dirace_sig.get_repr("SSUM");

    const auto INDEX = dirace_sig.get_repr("INDEX");
    const auto TYPE = dirace_sig.get_repr("TYPE");

    const auto PROD = dirace_sig.get_repr("PROD");
    const auto QBIT = dirace_sig.get_repr("QBIT");
    const auto BASIS0 = dirace_sig.get_repr("BASIS0");
    const auto BASIS1 = dirace_sig.get_repr("BASIS1");

    const auto BASIS = dirace_sig.get_repr("BASIS");
    const auto STYPE = dirace_sig.get_repr("STYPE");
    const auto KTYPE = dirace_sig.get_repr("KTYPE");
    const auto BTYPE = dirace_sig.get_repr("BTYPE");
    const auto OTYPE = dirace_sig.get_repr("OTYPE");
    const auto ARROW = dirace_sig.get_repr("ARROW");
    const auto FORALL = dirace_sig.get_repr("FORALL");
    const auto SET = dirace_sig.get_repr("SET");

    const auto PAIR = dirace_sig.get_repr("PAIR");
    const auto FUN = dirace_sig.get_repr("FUN");
    const auto IDX = dirace_sig.get_repr("IDX");
    const auto APPLY = dirace_sig.get_repr("APPLY");

    const auto ZERO = dirace_sig.get_repr("0");
    const auto ONE = dirace_sig.get_repr("1");
    const auto ADDS = dirace_sig.get_repr("Plus");
    const auto MULS = dirace_sig.get_repr("Times");
    const auto CONJ = dirace_sig.get_repr("Conjugate");
    const auto DELTA = dirace_sig.get_repr("DELTA");
    const auto DOT = dirace_sig.get_repr("DOT");

    const auto ZEROK = dirace_sig.get_repr("ZEROK");
    const auto ZEROB = dirace_sig.get_repr("ZEROB");
    const auto ZEROO = dirace_sig.get_repr("ZEROO");
    const auto ONEO = dirace_sig.get_repr("ONEO");
    const auto KET = dirace_sig.get_repr("KET");
    const auto BRA = dirace_sig.get_repr("BRA");
    const auto ADJ = dirace_sig.get_repr("ADJ");
    const auto SCR = dirace_sig.get_repr("SCR");
    const auto ADD = dirace_sig.get_repr("ADD");
    const auto TSR = dirace_sig.get_repr("TSR");

    const auto MULK = dirace_sig.get_repr("MULK");
    const auto MULB = dirace_sig.get_repr("MULB");
    const auto OUTER = dirace_sig.get_repr("OUTER");
    const auto MULO = dirace_sig.get_repr("MULO");

    const auto USET = dirace_sig.get_repr("USET");
    const auto CATPROD = dirace_sig.get_repr("CATPROD");
    const auto SUM = dirace_sig.get_repr("SUM");

    const auto RSET = dirace_sig.get_repr("RSET");
    const auto DTYPE = dirace_sig.get_repr("DTYPE");
    const auto REG = dirace_sig.get_repr("REG");
    const auto LKET = dirace_sig.get_repr("LKET");
    const auto LBRA = dirace_sig.get_repr("LBRA");
    const auto SUBS = dirace_sig.get_repr("SUBS");
    const auto LTSR = dirace_sig.get_repr("LTSR");
    const auto LDOT = dirace_sig.get_repr("LDOT");

    const std::set<int> a_symbols = {ADDS, MULS, ADD, LTSR};
    const std::set<int> c_symbols = {ADDS, MULS, DELTA, ADD, LTSR};

} // namespace dirace