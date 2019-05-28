#include "search.h"

using namespace std;

search::search(const int& n_, const double& tl, const bool& bm, const bool& dd, const bool& da, const bool& fa, const bool& verb): 
  n(n_), time_limit(tl), benchmark(bm), draw_derivation(dd), draw_all(da), formula(fa), verbose(verb) {
}

search::~search() {
}

Rcpp::List search::initialize() {

    info.to.a   = 0; info.to.b   = 0; info.to.c   = 0; info.to.d   = 0;
    info.from.a = 0; info.from.b = 0; info.from.c = 0; info.from.d = 0;
    info.rp.a   = 0; info.rp.b   = 0; info.rp.c   = 0; info.rp.d   = 0;
    info.ri.x   = 0; info.ri.y   = 0; info.ri.z   = 0; info.ri.u   = 0; info.ri.v = 0;
    info.valid = false; info.enumerate = false;

    string formula = "";
    string derivation = "";

    bool trivial = check_trivial();
    if ( trivial ) {
        return Rcpp::List::create(
            Rcpp::Named("identifiable") = false,
            Rcpp::Named("formula") = formula,
            Rcpp::Named("derivation") = derivation,
            Rcpp::Named("time") = 0,
            Rcpp::Named("rule_times") = rule_times
        );
    }

    z_sets = get_subsets(n);
    std::chrono::duration<double, std::milli> total_time;

    auto t1 = std::chrono::high_resolution_clock::now();
    auto t2 = std::chrono::high_resolution_clock::now();

    if ( !trivial_id ) {
        t1 = std::chrono::high_resolution_clock::now();
        find();
        t2 = std::chrono::high_resolution_clock::now();
    }
    total_time = t2 - t1;

    bool identifiable = target_dist.size() > 0;

    if ( identifiable ) formula = derive_formula(target_dist[0]);
    if ( draw_derivation ) {
        deriv->init();
        if ( draw_all ) {
            for ( const auto &d : L ) {
                draw(d.second, FALSE, *deriv);
            }
            if ( identifiable ) {
                draw(target_dist[0], FALSE, *deriv);
            }
        } else {
            if ( identifiable ) {
                draw(target_dist[0], TRUE, *deriv);
            }
        }
        deriv->finish();
        derivation = deriv->get();
    }

    return Rcpp::List::create(
        Rcpp::Named("identifiable") = identifiable,
        Rcpp::Named("formula") = formula,
        Rcpp::Named("derivation") = derivation,
        Rcpp::Named("time") = total_time.count(),
        Rcpp::Named("rule_times") = rule_times
    );

}

void search::find() {

    distr required;
    bool found = false;
    bool primi = true;
    unsigned int i = 1;
    unsigned int z_lim;
    unsigned int z_size = z_sets.size();
    int a, b, c, d, z, cd, req, ruleid, exist;
    int remaining = L.size();
    chrono::duration<double, std::ratio<3600>> total;

    if ( benchmark ) {

        chrono::high_resolution_clock::time_point t1, t2, t3;
        chrono::high_resolution_clock::time_point start;
        chrono::duration<double, std::milli> ms;

        while ( remaining > 0 && !found ) {

            distr& iquery = next_distribution(i);
            remaining--;
            a = iquery.pp.a;
            b = iquery.pp.b;
            c = iquery.pp.c;
            d = iquery.pp.d;
            primi = iquery.primitive;

            for ( unsigned int r = 0; r < rules.size(); r++ ) {

                t1 = chrono::high_resolution_clock::now();
                ruleid = rules[r];

                if ( !valid_rule(ruleid, a, b, c, d, primi) ) continue;

                z_lim = rule_limit(ruleid, z_size);

                for ( unsigned int z_ind = 0; z_ind < z_lim; z_ind++ ) {

                    t3 = chrono::high_resolution_clock::now();
                    total = t3 - start;
                    if ( total.count() > time_limit ) return;

                    required.primitive = TRUE;
                    z = z_sets[z_ind];
                    if ( !enumerate_distribution(ruleid, a, b, c, d, z, cd, exist, req, found, iquery, required, remaining) ) continue;

                }

                t2 = chrono::high_resolution_clock::now();
                ms = t2 - t1;
                rule_times[r] += ms.count();

                if ( found ) break;

            }

            i++;

        }

    } else {

        while ( remaining > 0 && !found ) {

            distr& iquery = next_distribution(i);
            remaining--;
            a = iquery.pp.a;
            b = iquery.pp.b;
            c = iquery.pp.c;
            d = iquery.pp.d;
            primi = iquery.primitive;

            for ( unsigned int r = 0; r < rules.size(); r++ ) {

                ruleid = rules[r];

                if ( !valid_rule(ruleid, a, b, c, d, primi) ) continue;

                z_lim = rule_limit(ruleid, z_size);

                for ( unsigned int z_ind = 0; z_ind < z_lim; z_ind++ ) {

                    required.primitive = TRUE;
                    z = z_sets[z_ind];
                    if ( !enumerate_distribution(ruleid, a, b, c, d, z, cd, exist, req, found, iquery, required, remaining) ) continue;

                }

                if ( found ) break;

            }

            i++;

        }

    }

}

bool search::enumerate_distribution(const int& ruleid, const int& a, const int& b, const int& c, const int& d, const int& z, int& cd, int& exist, int& req, bool& found, distr& iquery, distr& required, int& remaining) {

    apply_rule(ruleid, a, b, c, d, z);

    if ( !info.valid ) return false;;

    if ( info.enumerate ) {
        enumerate_candidates();
        cd = candidates.size();
        while ( cd > 0 && !found ) {
            cd--;
            get_candidate(required, candidates.top());
            candidates.pop();
            assign_candidate(required);
            exist = ps[make_key(info.to)];
            if ( exist == 0 ) derive_distribution(iquery, required, ruleid, remaining, found);
        }
    } else {
        exist = ps[make_key(info.to)];
        if ( exist > 0 ) return false;
        if ( info.ri.x > 0 ) {
            if ( !separation_criterion() ) return false;;
        }
        if ( info.rp.a > 0 ) {
            req = ps[make_key(info.rp)];
            if ( req > 0 ) {
                get_candidate(required, req);
                derive_distribution(iquery, required, ruleid, remaining, found);
            }
        } else {
            derive_distribution(iquery, required, ruleid, remaining, found);
        }
    }

    return true;

}

void search::set_derivation(derivation* d_) {
    deriv = d_;
}

void search::get_candidate(distr& required, const int& req) {
    distr& reqd = L[req];
    required.pp = reqd.pp;
    required.primitive = reqd.primitive;
    required.pa1 = reqd.pa1;
    required.pa2 = reqd.pa2;
    required.index = req;
    required.rule_num = reqd.rule_num;
    required.score = reqd.score;
}

string search::make_key(const p& pp) const {
    return std::to_string(pp.a) + "," + std::to_string(pp.b) + "," + std::to_string(pp.c) + "," + std::to_string(pp.d);
}

bool search::equal_p(const p& pp1, const p& pp2) const {
    return (pp1.a == pp2.a) && (pp1.b == pp2.b) && (pp1.c == pp2.c) && (pp1.d == pp2.d);
}

void search::draw(const distr& dist, const bool& recursive, derivation& d) {
    if ( dist.pa1 > 0 ) {
        distr& pa1 = L[dist.pa1];
        d.add_edge(to_string(pa1.pp), to_string(dist.pp), rule_name(dist.rule_num));
        if ( recursive ) draw(pa1, recursive, d);
        if ( dist.pa2 > 0 ) {
            distr& pa2 = L[dist.pa2];
            d.add_edge(to_string(pa2.pp), to_string(dist.pp), rule_name(dist.rule_num));
            if ( recursive ) draw(pa2, recursive, d);
        }
    }
}