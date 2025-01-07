#include "diracoq.hpp"

namespace diracoq {
    using namespace std;
    using namespace ualg;

    const int deBruijn_index_num = 1024;



    vector<string> gen_symbols() {

        vector<string> res = {};

        for (int i = 0; i < deBruijn_index_num; ++i) {
            res.push_back("@" + to_string(i));
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
            "ADDS",
            "MULS",
            "CONJ",
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
        };

        for (const auto &s : defined) {
            res.push_back(s);
        }

        return res;
    }

    vector<string> diracoq_symbols = gen_symbols();

    const Signature<int> diracoq_sig = compile_string_sig(diracoq_symbols);

    const auto COMPO = diracoq_sig.get_repr("COMPO");
    const auto ADDG = diracoq_sig.get_repr("ADDG");
    const auto STAR = diracoq_sig.get_repr("STAR");
    const auto SSUM = diracoq_sig.get_repr("SSUM");

    const auto INDEX = diracoq_sig.get_repr("INDEX");
    const auto TYPE = diracoq_sig.get_repr("TYPE");

    const auto PROD = diracoq_sig.get_repr("PROD");

    const auto BASIS = diracoq_sig.get_repr("BASIS");
    const auto STYPE = diracoq_sig.get_repr("STYPE");
    const auto KTYPE = diracoq_sig.get_repr("KTYPE");
    const auto BTYPE = diracoq_sig.get_repr("BTYPE");
    const auto OTYPE = diracoq_sig.get_repr("OTYPE");
    const auto ARROW = diracoq_sig.get_repr("ARROW");
    const auto FORALL = diracoq_sig.get_repr("FORALL");
    const auto SET = diracoq_sig.get_repr("SET");

    const auto PAIR = diracoq_sig.get_repr("PAIR");
    const auto FUN = diracoq_sig.get_repr("FUN");
    const auto IDX = diracoq_sig.get_repr("IDX");
    const auto APPLY = diracoq_sig.get_repr("APPLY");

    const auto ZERO = diracoq_sig.get_repr("0");
    const auto ONE = diracoq_sig.get_repr("1");
    const auto ADDS = diracoq_sig.get_repr("ADDS");
    const auto MULS = diracoq_sig.get_repr("MULS");
    const auto CONJ = diracoq_sig.get_repr("CONJ");
    const auto DELTA = diracoq_sig.get_repr("DELTA");
    const auto DOT = diracoq_sig.get_repr("DOT");

    const auto ZEROK = diracoq_sig.get_repr("ZEROK");
    const auto ZEROB = diracoq_sig.get_repr("ZEROB");
    const auto ZEROO = diracoq_sig.get_repr("ZEROO");
    const auto ONEO = diracoq_sig.get_repr("ONEO");
    const auto KET = diracoq_sig.get_repr("KET");
    const auto BRA = diracoq_sig.get_repr("BRA");
    const auto ADJ = diracoq_sig.get_repr("ADJ");
    const auto SCR = diracoq_sig.get_repr("SCR");
    const auto ADD = diracoq_sig.get_repr("ADD");
    const auto TSR = diracoq_sig.get_repr("TSR");

    const auto MULK = diracoq_sig.get_repr("MULK");
    const auto MULB = diracoq_sig.get_repr("MULB");
    const auto OUTER = diracoq_sig.get_repr("OUTER");
    const auto MULO = diracoq_sig.get_repr("MULO");

    const auto USET = diracoq_sig.get_repr("USET");
    const auto CATPROD = diracoq_sig.get_repr("CATPROD");
    const auto SUM = diracoq_sig.get_repr("SUM");

    const std::set<int> a_symbols = {ADDS, MULS, ADD};
    const std::set<int> c_symbols = {ADDS, MULS, DELTA, ADD};

} // namespace diracoq