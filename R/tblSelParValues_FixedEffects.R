#' Insert variable names into an ordered vector
#'
#' Adds names not already present in an existing named character vector while
#' preserving the relative order specified by `newV`.
#'
#' @param newV A character vector of variable names in the desired relative
#'   order.
#' @param oldV A named character vector whose names are identical to its
#'   values, or `NULL`.
#'
#' @return A named character vector containing the values from `oldV` and any
#'   missing values from `newV`. Each element is named with its value.
#' @export
#'
#' @examples
#' oldV <- c(year = "year", sex = "sex");
#' newV <- c("year", "fleet", "sex");
#' insertVarNames(newV, oldV);
insertVarNames <- function(newV, oldV = NULL) {
  missingV <- newV[!newV %in% unname(oldV)];
  if (length(missingV) == 0) {
    return(oldV);
  }
  result <- oldV;
  for (value in missingV) {
    newPosition <- match(value, newV);
    followingV <- newV[seq_along(newV) > newPosition];
    followingPosition <- match(followingV, unname(result), nomatch = 0L);
    followingPosition <- followingPosition[followingPosition > 0L];
    value <- stats::setNames(value, value);
    if (length(followingPosition) == 0) {
      result <- c(result, value);
    } else {
      insertAfter <- min(followingPosition) - 1L;
      result <- append(result, value, after = insertAfter);
    }
  }
  return(result);
}

#' Create a fixed-effect selectivity parameter table
#'
#' Creates and combines fixed-effect parameter rows for all rows of an expanded
#' selectivity parameter specification table.
#'
#' @param tblExpSelPars An expanded selectivity parameter specification table.
#'   Each row must contain the inputs required by
#'   [tblSelParValues_FixedEffects()].
#'
#' @return A tibble containing parameter identifiers, model-frame variables,
#'   initial values, bounds, estimation phases, priors, and labels.
#' @export
tblSelParsValues_FixedEffects<-function(tblExpSelPars){
  lst  = list();
  vars = NULL;
  for (r in 1:nrow(tblExpSelPars)){
    rw = tblExpSelPars[r,];
    res = tblSelParValues_FixedEffects(rw); #--list(tbl,vars)
    vars = insertVarNames(res$vars,vars);
    lst[[r]] = res$tbl;
  }
  tbl = dplyr::bind_rows(lst);
  tbl = tbl[,c("pid","tid","param",vars,"init","lb","ub","phs","prior","p1","p2","label")];
  return(tbl);
}

#' Create fixed-effect selectivity parameter rows
#'
#' Creates fixed-effect parameter rows for one expanded selectivity parameter
#' specification.
#'
#' @param rw A one-row data frame containing `pid`, `tid`,`param`, `label`, and
#'   `lstFEs`. The `lstFEs` column must contain fixed-effect model-frame
#'   information, including `mfFEs` and `umfmFEs`.
#'
#' @return A list with `tbl`, a tibble of fixed-effect parameter rows, and
#'   `vars`, a character vector naming the model-frame variables.
#' @export
tblSelParValues_FixedEffects<-function(rw){
  lstFEs  = rw$lstFEs$lstFEs;
  nvar    = ncol(lstFEs$mfFEs);
  tblVars = lstFEs$umfmFEs[,1:nvar];
  strVars = names(tblVars);
  vecVars = apply(tblVars, 1, paste, collapse = "_");
  tblVars = tblVars |> dplyr::mutate(pid=rw$pid,tid=rw$tid,param=rw$param,.before=1);
  lstVals = list();
  for (r in 1:nrow(lstFEs$umfmFEs)){
    lstVals[[r]] = 
             tibble::tibble(init=0.0,
                            lb=-Inf,
                            ub=Inf,
                            phs=1,
                            prior="none",
                            p1=NA_real_,
                            p2=NA_real_,
                            label=paste0(rw$label,"_",vecVars[r]));
  }
  tblVals = dplyr::bind_rows(lstVals);
  tbl = dplyr::bind_cols(tblVars,tblVals)
  return(list(tbl=tbl,vars=strVars));
}
