// This file contains standalone methods for the package that would otherwise be slow in R,
// but do not require a model by themselves.

#include "standalonemethods.h"

/*
method topNwordSeeds

This method takes returns the positions of the highest N parameters of the R NumericVector input.
Serves to improve the efficiency of the topNwords method. 

Arguments
Rcpp::NumericVector wordparameters - A vector of parameters from the phi data.frame
int                 N              - Integer. Represents the number of positions to return

Returns
Rcpp::NumericVector. A vector of the positions of the N highest values.

Used in: topNwords
*/

// [[Rcpp::export]]
Rcpp::NumericVector topNwordSeeds(Rcpp::NumericVector& wordParameters, int N) {
    Rcpp::NumericVector result(N);
    std::vector<std::pair<double,int> > parameterVector;
    parameterVector.resize(wordParameters.size());

    int rSeed = 1; //R arrays start at index 1, so so will this one.

    for (Rcpp::NumericVector::iterator it = wordParameters.begin(); it != wordParameters.end(); it++) {
        parameterVector.push_back(std::pair<double,int>(*it,rSeed));
        rSeed++;
    }

    std::sort(parameterVector.begin(),parameterVector.end()); //Note: Sorting is ascending by default
    //Note on sorting pairs. By default they are sorted by the first element and if that is equal by the second.
    //Therefore, the first element of the pair has to be the parameter value and te second the vector position.
    std::reverse(parameterVector.begin(),parameterVector.end()); //So we reverse the vector here.

    for (int i = 0; i < N; i++) {
        result.push_back(parameterVector[i].second); //Add the second element (the seed of the word in R) to our seeds
    }

    return result;
}


