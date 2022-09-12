#include <Rcpp.h>
using namespace Rcpp;

// bellow is a function ahta gets a numeric vector and return whether the value
// has down or up compairing with their previous value
// if the any of the compairing values is NA the value is 0 ( unconsider ), 1 for has up
// -1 for has down
// [[Rcpp::export]]
NumericVector stat_bllcorr_downOrUp(NumericVector x){
    NumericVector result {0};
    for ( int i=1;i < x.length(); i++ ) {
        double prevValue = x[i-1];
        double actualValue = x[i];
        if ( actualValue > prevValue ){
            result.push_back(1);
        }else if ( actualValue < prevValue ){
            result.push_back(-1);
        }else if ( ISNA(actualValue) || ISNA(prevValue) || actualValue == prevValue ){
            result.push_back(0);
        };
    };
    assert ( result.length() == x.length() );
    return result;
}
