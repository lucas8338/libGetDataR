# this file contains checking of vector and varibles

#' @title check.is_characterNumeric: check if the character vector is numeric
#' @description this function will check if the character vector is composite
#' of numeric
#' @param data the vector
#' @return logical TRUE or FALSE
#' @export
check.is_characterNumeric<- function (data){
  # when is not convertible a character to numeric the function 'as.numeric' will
  # rise an warning, so i'm using this wargning to detect if the vector can be numeric
  result<- tryCatch(as.numeric(data),warning = function(e){as.logical(FALSE)})
  if ( !is.logical(result) ){
    result<- TRUE
  }
  result
}