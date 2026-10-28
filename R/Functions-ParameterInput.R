#' @title Get column types as a readr::read_csv abbreviation string.
#' @examplesIf FALSE
#' # example code
#' test <- tibble::tibble(a = 10, b = "a", y = lubridate::ymd_hms("2025-01-01 01:02:03"))
#' tps = get_col_types_from_dataframe(test)
#' tps

get_col_types_from_dataframe <- function(df) {
    types <-structure(list(code = c("c", "i", "d", "l", "f", "D", "T", "t"), 
                           col_type = c("chr", "int", "dbl", "lgl", "fct", "date", "dttm", "time")),
                     class = c("tbl_df", "tbl", "data.frame"), 
                     row.names = c(NA, -8L));
    col_types = df |> 
                  dplyr::summarise(across(dplyr::everything(), pillar::type_sum)) |> 
                  tidyr::pivot_longer(dplyr::everything(), names_to = "col_names", values_to = "col_type") |> 
                  dplyr::left_join(types) |> 
                  dplyr::pull(code) |> 
                  stringr::str_c(collapse = "");
    return(col_types)
}

#'
#' @title Create an input section template for the values of coefficients defining the fixed effects part of a parameter
#' @description Function to create an input section template for the values of coefficients defining the fixed effects part of a parameter.
#' @param dfrMM - model matrix dataframe for parameter
#' @param transform - scale(s) on which coefficients are defined ("none" or "log", "logit","logistic")  
#' @param initial - initial value(s) on transform scale (default = 0)
#' @param lower - lower bound(s) of transform scale (default=-Inf)
#' @param upper - upper bound(s) on transform scale (default = +Inf)
#' @param phase - flag(s) indicating whether coefficients should be estimated
#' @param pdf - name(s) of prior probability function (default = "none")
#' @param p1 - expected value(s) for prior (default = 0)
#' @param p2 - dispersion(s) for prior (default = +Inf)
#' @return a dataframe resulting from binding the columns of dfrMM to those of a 
#' dataframe created by adding an id column ("pid) and a "mirror" column to the 
#' remainder of the inputs organized as a dataframe. 
#' @details The inputs other than `dfrMM` are used to create a secondary dataframe to 
#' be bound to `dfrMM` using [dplyr::bind_cols], adding a "pid" column that assigns an 
#' integer id to each coefficient and a "mirror" column that the user can use to link a 
#' coefficient value to another.
#' @examplesIf FALSE
#' # example code
#' ##--define model dimensions
#' fleets  = c("TCF","SCF","RKF");#--fleets
#' years   = 2020:2025;
#' seasons = 1:2;
#' sexes = c("male","female");
#' sizes = seq(27.5,182.5,5);
#' ##--create model matrix dataframe for fixed effects portion of parameter
#' dfrMM = tidyr::expand_grid(f=fleets[c(1,3)],
#'                            x=sexes);
#' dfrFPIT = createParameterInputTemplate_Fixed();
#' cat(writeToStr_Dataframe(dfrFPIT));
#' dfrFPIT = createParameterInputTemplate_Fixed(dfrMM);
#' cat(writeToStr_Dataframe(dfrFPIT));
#'
#' @importFrom dplyr bind_cols 
#' @importFrom tibble tibble
#' @export
createParameterInputTemplate_Fixed<-function(dfrMM=NULL,
                                             transform="none",
                                             initial=0,
                                             lower=-Inf,
                                             upper=Inf,
                                             phase=1,
                                             pdf="none",
                                             p1=0,
                                             p2=Inf){
  if (is.null(dfrMM)){
    pid = as.integer(0);
  } else {
    nr = nrow(dfrMM);
    pid = as.integer(1:nr);
  }
  e_cols=c("pid","mirror","tform","initial","lower","upper","phase","pdf","p1","p2");
  dfrE = tibble::tibble(pid=pid,mirror=as.integer(0),tform=transform,
                        initial=initial,lower=lower,upper=upper,phase=as.integer(phase),pdf=pdf,p1=p1,p2=p2);
  dfrI = dplyr::bind_cols(dfrMM,dfrE);
  return(dfrI);
}

#'
#' @title Create an input section template for the values of coefficients defining the environmental covariates part of a parameter
#' @description Function to create an input section template for the values of coefficients defining the environmental covariates part of a parameter.
#' @param dfrMM - model matrix dataframe for the environmental covariates part of a parameter
#' @param transform - scale(s) on which coefficients are defined ("none" or "log", "logit","logistic")  
#' @param initial - initial value(s) on transform scale (default = 0)
#' @param lower - lower bound(s) on transform scale (default=-Inf)
#' @param upper - upper bound(s) on transform scale (default = +Inf)
#' @param phase - flag(s) indicating whether coefficients should be estimated
#' @param pdf - name(s) of prior probability function (default = "none")
#' @param p1 - expected value(s) for prior (default = 0)
#' @param p2 - dispersion(s) for prior (default = +Inf)
#' @return a dataframe resulting from binding the columns of dfrMM to those of a 
#' dataframe created by adding an id column ("pid) and a "mirror" column to the 
#' remainder of the inputs organized as a dataframe. 
#' @details The inputs other than `dfrMM` are used to create a secondary dataframe to 
#' be bound to `dfrMM` using [dplyr::bind_cols], adding a "pid" column that assigns an 
#' integer id to each coefficient and a "mirror" column that the user can use to link a 
#' coefficient value to another.
#' 
#' `dfrMM` should include columns "eid", "y_lag", and "s_lag" indicating
#'   * eid: index of corresponding environmental covariate (integer \gt 0)
#'   * y_lag: year lag to apply to environmental covariate (integer \ge 0)
#'   * s_lag: season lag to apply to environmental covariate (integer \ge 0)
#'   
#' @examplesIf FALSE
#' # example code
#' ##--define model dimensions
#' fleets  = c("TCF","SCF","RKF");#--fleets
#' sexes = c("male","female");    #--sexes
#' eid = c(1,2);                  #--environmental index id's
#' ##--create model matrix dataframe for fixed effects portion of parameter
#' dfrMM = dplyr::bind_rows(
#'           tidyr::expand_grid(f=fleets[1],
#'                              x=sexes,
#'                              eid=eid[1],
#'                              y_lag=1,
#'                              s_lag=0),
#'           tidyr::expand_grid(f=fleets[2],
#'                              x=sexes,
#'                              eid=eid,
#'                              y_lag=c(0,1),
#'                              s_lag=c(1,0));
#' dfrECIT = createParameterInputTemplate_EnvCovs(dfrMM);
#' cat(writeToStr_Dataframe(dfrECIT));
#'
#' @importFrom dplyr bind_cols 
#' @importFrom tibble tibble
#' @export
createParameterInputTemplate_EnvCovs<-function(dfrMM=NULL,
                                               transform="none",
                                               initial=0,
                                               lower=-Inf,
                                               upper=Inf,
                                               phase=1,
                                               pdf="none",
                                               p1=0,
                                               p2=Inf){
  if (is.null(dfrMM)){
    pid = as.integer(0);
  } else {
    nr = nrow(dfrMM);
    pid = as.integer(1:nr);
  }
  e_cols=c("pid","mirror","tform","initial","lower","upper","phase","pdf","p1","p2");
  dfrE = tibble::tibble(pid=pid,mirror=as.integer(0),tform=transform,
                        initial=initial,lower=lower,upper=upper,phase=as.integer(phase),pdf=pdf,p1=p1,p2=p2);
  dfrP = tibble::tibble(type=c("eps","rho"));
  dfrI = dplyr::bind_cols(dfrMM,dfrP,dfrE);
  return(dfrI);
}

  