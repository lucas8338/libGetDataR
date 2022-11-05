// contains mathematical operations

#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
double Rcpp_math_mean(NumericVector data){
    int len=data.size();
    double sum=0;
    for ( int i=0;i<len;i++ ){
        sum+=data[i];
    };
    double calc= sum/len;
    return calc;
}

// [[Rcpp::export]]
double Rcpp_math_standardDeviation(NumericVector data){
    int len=data.size();
    double sum=0;
    double mean= Rcpp_math_mean(data);
    for ( int i=0;i<len;i++ ){
        sum+=pow((data[i]-mean),2);
    }
    double calc= sqrt( (sum / (len-1)) );
    return calc;
}