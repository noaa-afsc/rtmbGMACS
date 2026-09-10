
#' @title Convert values to the logistic scale
#' @description Function to convert values to the logistic scale.
#' @param x - numeric object
#' @return 1.0/(1.0+exp(-x)), with same names and class as `x`
#' @details inverse of logit function.
#' @export
#'
logistic<-function(x){
  y = 1.0/(1.0+exp(-x));
  names(y) = names(x);
  return(y);
}

#'
#' @title Convert values to the logit scale
#' @description Function to convert values to the logit scale.
#' @param x - numeric object
#' @return log(x/(1-x)), with same names and class as `x`
#' @details inverse of logistic function. Values of `x` outside the
#' (0,1) range generate NaNs.
#' @export
#'
logit<-function(x){
  y = log(x/(1-x));
  names(y) = names(x);
  return(y);
}

#' @title Calculate a 0-symmetric logit function
#' @description Function to calculate a logit function centered on 0
#' @param x - numeric vector with values from -1.0 to 1.0 at which to calculate the symmetric logit function
#' @return a numeric vector with the same size and names (if any) as `x`
#' @details This is the inverse to `symlogistic` (see examples)
#' @examplesIf FALSE
#' ##--R context
#' x = seq(-0.99,0.99,0.01);
#' y = symlogit(x);
#' head(y);
#' xp = symlogistic(y) - x
#' ##--RTMB context
#' x = RTMB::AD(seq(-0.49,0.49,0.01),force=TRUE);
#' y = symlogit(x);
#' head(y)
#' @export
#'
symlogit<-function(x){
  y = (x+1)/2.0;
  y = log(y/(1-y));
  names(y) = names(x);
  return(y)
};

#' @title Calculate a 0-symmetric logistic function between -1 and 1
#' @description Function to calculate a logistic function centered on 0
#' @param x - numeric vector at which to calculate the symmetric logistic function
#' @return a numeric vector with the same size and names (if any) as `x`
#' @details This is the inverse to `symlogit` (see examples)
#' @examplesIf FALSE
#' ##--R context
#' x = seq(-5,5,0.1);
#' y = symlogistic(x);
#' head(y);
#' xp = symlogit(y) - x
#' ##--RTMB context
#' x = RTMB::AD(seq(-5,5,0.1),force=TRUE);
#' y = symlogistic(x);
#' head(y)
#' @export
#'
symlogistic<-function(x){
  y = 2.0*exp(x)/(1+exp(x))-1.0;
  names(y) = names(x);
  return(y);
}

