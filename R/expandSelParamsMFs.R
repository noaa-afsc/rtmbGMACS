#' Expand selectivity/retention parameter model frames
#'
#' Builds a model frame for each row of a selectivity/retention parameter specification table by
#' combining its time frames, fleets, and population dimensions.
#'
#' @param tblSelPars A data frame containing parameter specifications. It must
#'   include the columns `pid`, `tid`, `timeFrames`, `fleets`, `popDims`, `FEs`,
#'   `ECs`, `REs`, and `label`.
#' @param lstMFs A named list of data frames referenced by values in the
#'   `timeFrames`, `fleets`, and `popDims` columns of `tblSelPars`.
#'
#' @return A tibble with one row per parameter specification and a list-column
#'   named `mf` containing the expanded model frames.
#' 
#' @details The columns in `tblSelPars` must include (others are ignored):
#' 
#'   * `pid` - an unique integer identifying the parameter definition
#'   * `tid` - an integer identifying the selectivity/retention function "type"
#'   * `timeFrames` - a tibble identifying time frames (years and seasons) and, possibly, time blocks to include in the model frame
#'   * `fleets` - a string or tibble identifying fleets to include in the model frame
#'   * `popDims` - a tibble identifying population dimensions and values to include in the model frame
#'   * `FEs` - a formula identifying fixed effects 
#'   * `ECs` - a formula identifying environmental covariates
#'   * `REs` - a formual idenitfying random effects
#'   * `label` - a label to use to identify the parameter definition
#' 
#' @export
#'
expandSelParamsMFs<-function(tblSelPars,lstMFs){
  evalTxt<-function(txt,lstMFs){
    if (is.na(txt)||stringr::str_equal(txt,"NA")) return(NULL);

    if (stringr::str_starts(txt,"tbl")){
      tbl = lstMFs[[txt]];
    } else if (stringr::str_detect(txt,"=")){
      tbl = eval(parse(text=paste0("tibble::tibble(",txt,")")));
    } else {
      #--the following should only occur if `txt` came from the "fleets" column
      tbl = tibble::tibble(f=txt);
    }
    return(tbl);
  }
  lstAll = list();
  for (r in 1:nrow(tblSelPars)){
    #--testing: r = 1;
    rw = tblSelPars[r,];
    tblYS   = evalTxt(rw$timeFrames,lstMFs);
    tblFlts = evalTxt(rw$fleets,lstMFs);
    tblPDs  = evalTxt(rw$popDims,lstMFs);
    tblMF   = tidyr::expand_grid(tblYS,tblFlts,tblPDs);
    lstAll[[r]] = tibble::tibble(pid=rw$pid,
                                 tid=rw$tid,
                                 param=rw$param,
                                 mf=list(mf=tblMF),
                                 FEs=rw$FEs,
                                 ECs=rw$ECs,
                                 REs=rw$REs,
                                 label=rw$label);

  }
  tblAll = dplyr::bind_rows(lstAll);
  return(tblAll);
}

