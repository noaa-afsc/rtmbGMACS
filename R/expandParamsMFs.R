#' Expand parameter model frames
#'
#' Builds a model frame for each row of a parameter specification table by
#' expanding its time frames, fleets, and population dimensions.
#'
#' @param tblSelPars A data frame containing parameter specifications. It must
#'   include the columns `pid`, `tid`, `timeFrames`, `fleets`, `popDims`, `FEs`,
#'   `ECs`, `REs`, and `label`.
#' @param lstMFs A named list of data frames referenced by values in the
#'   `timeFrames`, `fleets`, and `popDims` columns of `tblSelPars`.
#'
#' @return A tibble with one row per parameter specification and a list-column
#'   named `mf` containing the expanded model frames.
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
                                 mf=list(mf=tblMF),
                                 FEs=rw$FEs,
                                 ECs=rw$ECs,
                                 REs=rw$REs,
                                 label=rw$label);

  }
  tblAll = dplyr::bind_rows(lstAll);
  return(tblAll);
}

