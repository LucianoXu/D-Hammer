#include "dhammer.hpp"

namespace dhammer {
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
            "BIT", "BASIS0", "BASIS1",

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
            "ZEROD", "LKET", "LBRA", "SUBS", "LTSR", "LDOT"
        };

        for (const auto &s : defined) {
            res.push_back(s);
        }

        return res;
    }

    vector<string> dhammer_symbols = gen_symbols();

    const Signature<int> dhammer_sig = compile_string_sig(dhammer_symbols);

    const auto COMPO = dhammer_sig.get_repr("COMPO");
    const auto ADDG = dhammer_sig.get_repr("ADDG");
    const auto STAR = dhammer_sig.get_repr("STAR");
    const auto SSUM = dhammer_sig.get_repr("SSUM");

    const auto INDEX = dhammer_sig.get_repr("INDEX");
    const auto TYPE = dhammer_sig.get_repr("TYPE");

    const auto PROD = dhammer_sig.get_repr("PROD");
    const auto BIT = dhammer_sig.get_repr("BIT");
    const auto BASIS0 = dhammer_sig.get_repr("BASIS0");
    const auto BASIS1 = dhammer_sig.get_repr("BASIS1");

    const auto BASIS = dhammer_sig.get_repr("BASIS");
    const auto STYPE = dhammer_sig.get_repr("STYPE");
    const auto KTYPE = dhammer_sig.get_repr("KTYPE");
    const auto BTYPE = dhammer_sig.get_repr("BTYPE");
    const auto OTYPE = dhammer_sig.get_repr("OTYPE");
    const auto ARROW = dhammer_sig.get_repr("ARROW");
    const auto FORALL = dhammer_sig.get_repr("FORALL");
    const auto SET = dhammer_sig.get_repr("SET");

    const auto PAIR = dhammer_sig.get_repr("PAIR");
    const auto FUN = dhammer_sig.get_repr("FUN");
    const auto IDX = dhammer_sig.get_repr("IDX");
    const auto APPLY = dhammer_sig.get_repr("APPLY");

    const auto ZERO = dhammer_sig.get_repr("0");
    const auto ONE = dhammer_sig.get_repr("1");
    const auto ADDS = dhammer_sig.get_repr("Plus");
    const auto MULS = dhammer_sig.get_repr("Times");
    const auto CONJ = dhammer_sig.get_repr("Conjugate");
    const auto DELTA = dhammer_sig.get_repr("DELTA");
    const auto DOT = dhammer_sig.get_repr("DOT");

    const auto ZEROK = dhammer_sig.get_repr("ZEROK");
    const auto ZEROB = dhammer_sig.get_repr("ZEROB");
    const auto ZEROO = dhammer_sig.get_repr("ZEROO");
    const auto ONEO = dhammer_sig.get_repr("ONEO");
    const auto KET = dhammer_sig.get_repr("KET");
    const auto BRA = dhammer_sig.get_repr("BRA");
    const auto ADJ = dhammer_sig.get_repr("ADJ");
    const auto SCR = dhammer_sig.get_repr("SCR");
    const auto ADD = dhammer_sig.get_repr("ADD");
    const auto TSR = dhammer_sig.get_repr("TSR");

    const auto MULK = dhammer_sig.get_repr("MULK");
    const auto MULB = dhammer_sig.get_repr("MULB");
    const auto OUTER = dhammer_sig.get_repr("OUTER");
    const auto MULO = dhammer_sig.get_repr("MULO");

    const auto USET = dhammer_sig.get_repr("USET");
    const auto CATPROD = dhammer_sig.get_repr("CATPROD");
    const auto SUM = dhammer_sig.get_repr("SUM");

    const auto RSET = dhammer_sig.get_repr("RSET");
    const auto DTYPE = dhammer_sig.get_repr("DTYPE");
    const auto REG = dhammer_sig.get_repr("REG");
    const auto ZEROD = dhammer_sig.get_repr("ZEROD");
    const auto LKET = dhammer_sig.get_repr("LKET");
    const auto LBRA = dhammer_sig.get_repr("LBRA");
    const auto SUBS = dhammer_sig.get_repr("SUBS");
    const auto LTSR = dhammer_sig.get_repr("LTSR");
    const auto LDOT = dhammer_sig.get_repr("LDOT");

    const std::set<int> a_symbols = {ADDS, MULS, ADD, LTSR};
    const std::set<int> c_symbols = {ADDS, MULS, DELTA, ADD, LTSR};

} // namespace dhammer