#--"enums" (most based on glmmTMB/R/enum.R)----
##--getValidLinks: based on glmmTMB/R/enum.R{.valid_link}
#'@title Named character vector of valid statistical family link function names
#'@return returns named character vector with names of valid link functions
#'@export
getValidLinks<-function(){
  .valid_link <- c(
    log                 = "log",       # 0
    logit               = "logit",     # 1
    probit              = "probit",    # 2
    inverse             = "inverse",   # 3
    cloglog             = "cloglog",   # 4
    identity            = "identity",  # 5
    sqrt                = "sqrt",      # 6
    lambertW            = "lambertW"   # 7
  );
  return(.valid_link);
}

##--getValidFamilies: based on glmmTMB/R/enum.R{.valid_family}
#'@title Named character vector of valid statistical family names
#'@return returns named character vector with names of valid statistical families
#'@export
getValidFamilies<-function(){
  .valid_family <- c(
    gaussian = "gaussian",                  #    0
    binomial = "binomial",                  #  100
    betabinomial ="betabinomial",           #  101
    beta         ="beta",                   #  200
    ordbeta = "ordbeta",                    #  201
    Gamma ="Gamma",                         #  300
    poisson ="poisson",                     #  400
    truncated_poisson ="truncated_poisson", #  401
    genpois ="genpois",                     #  402
    compois ="compois",                     #  403
    truncated_genpois ="truncated_genpois", #  404
    truncated_compois ="truncated_compois", #  405
    nbinom1 ="nbinom1",                     #  500
    nbinom2 ="nbinom2",                     #  501
    nbinom12 ="nbinom12",                   #  502
    truncated_nbinom1 ="truncated_nbinom1", #  550
    truncated_nbinom2 ="truncated_nbinom2", #  551
    t ="t",                                 #  600
    tweedie = "tweedie",                    #  700
    lognormal = "lognormal",                #  800
    skewnormal = "skewnormal",              #  900
    bell = "bell"                           # 1000
  )
  return(.valid_family);
}

##--.valid_covstruct: based on glmmTMB/R/enum.R{.valid_covstruct}
#'@title Named character vector of valid covariance structure names
#'@return returns named character vector with names of valid covariance structures
#'@export
getvalidCovStructs<-function(){
  .valid_covstruct <- c(
    diag    = "diag",     # 0,
    us      = "us",       # 1,
    cs      = "cs",       # 2,
    ar1     = "ar1",      # 3,
    ou      = "ou",       # 4,
    exp     = "exp",      # 5,
    gau     = "gau",      # 6,
    mat     = "mat",      # 7,
    toep    = "toep",     # 8,
    rr      = "rr",       # 9
    homdiag = "homdiag", # 10
    propto  = "propto",  # 11
    hetar1  = "hetar1",  # 12
    homcs   = "homcs",   # 13
    homtoep = "homtoep",  # 14
    rw = "rw"            # random walk (new)
  )
  return(.valid_covstruct);
}

##--.valid_smooths: character vector for valid smooth functions
#'@title Character vector of valid mgcv smooth function names
#'@details None.
#'@return returns named character vector with names of valid smooths
#'@export
getValidSmooths<-function(){
  .valid_smooths <- c("s",
                      "ti");
  return(.valid_smooths);
}

##--.valid_simcode: based on glmmTMB/R/enum.R{.valid_simcode}
#'@title Named character vector of valid simulation options
#'@return returns named character vector with names of valid simulation options
#'@export
getValidSimCodes<-function(){
  .valid_simcodes <- c(
    zero = "zero",    # 0,
    fix = "fix",      # 1,
    random = "random" # 2
  )
  return(.valid_simcodes);
}

