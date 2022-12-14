// this file contains function especific to work with the R function 'stat.bllcorr'.
#include <Rcpp.h>
using namespace Rcpp;

//' @title stat_bllcorr_downOrUp: has the price fall or rised compaired to the previous price?
//' @description bellow is a function that gets a numeric vector and return whether the value
//' has down or up compairing with their previous value
//' if the any of the compairing values is NA the value is 0 ( unconsider ), 1 for has up
//' -1 for has down
//' @param x the a NumericVector with the values
//' @return a NumeriVector with the same size that the 'x' parameter with the values:
//' -1 (has fallen), 0 (unconsider), 1 (has raisen).
// [[Rcpp::export]]
NumericVector stat_bllcorr_downOrUp(NumericVector x){
    NumericVector result {0};
    for ( int i=1;i < x.size(); i++ ) {
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
    if ( result.size() != x.size() ){
        throw -100;
    }
    return result;
};

//' @title stat_bllcorr_doesExogPredictsEndog: compaire exog and endog and return a vector of the comparison,
//' should take proprocessed Vectors from the function 'stat_bllcorr_downOrUp'
//' @description the code bellow will do a comparison if the exog was capable to predict the endog
//' @param exog a lagged NumericVector to check whether can predicts endog
//' @param the NumericVector to check whether exog has predicted this
//' @return a NumericVector of the result of the coparison of the same size than input and
//' with values: -1 (oposite direction), 0 (unconsider), 1 (same direction).
//[[Rcpp::export]]
NumericVector stat_bllcorr_doesExogPredictsEndogCateg(NumericVector exog, NumericVector endog){
    // bellow will check if the size of exog is equal endog
    if ( exog.size() != endog.size() ){
        throw -100;
    };
    NumericVector result;
    // 1: 'same direction'
    // 0: 'unconsider'
    // -1: 'reverse direction'
    Rcout<< endog.size();
    for (int i=0; i< exog.size();i++ ){
        double valExog= exog[i];
        double valEndog= endog[i];
        if ( ISNA(valExog) || ISNA(valEndog) || valExog==0 || valEndog==0 ){
            result.push_back(0);
        }else if ( valExog==valEndog ){
            // has predicted same direction
            result.push_back(1);
        }else if ( (valExog==1 && valEndog==-1) || (valExog==-1 && valEndog==1) ){
            // has predicted reverse
            result.push_back(-1);
        };
    };
    // check if the size of result is equal the exog that is equal endog
    if ( result.size() != exog.size() ){
        throw -100;
    };
    return result;
};
