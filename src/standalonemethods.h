#ifndef _STANDALONEMETHODS_H
#define _STANDALONEMETHODS_H

#include <Rcpp.h>
#include <vector>
#include <utility>
#include <algorithm>

Rcpp::NumericVector topNwordSeeds(Rcpp::NumericVector& wordParameters, int N);

#endif
