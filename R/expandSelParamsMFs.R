#' Expand selectivity/retention parameter model frames
#'
#' Builds a model frame for each row of a selectivity/retention parameter specification table by
#' combining its time frames, fleets, and population dimensions.
#'
#' @param tblSelPars A data frame containing parameter specifications. It must
#'   include the columns `pid`, `tid`, `MFs`, `FEs`,`ECs`, `REs`, and `label`.
#' @param lstMFs A named list of model frames (tibbles) referenced by values in the
#'   `MFs` column of `tblSelPars`.
#'
#' @return A tibble with one row per parameter specification and a list-column
#'   named `mf` containing the expanded model frames.
#' 
#' @details The columns in `tblSelPars` must include (others are ignored):
#' 
#'   * `pid` - an unique integer identifying the parameter definition
#'   * `tid` - an integer identifying the selectivity/retention function "type"
#'   * `MFs` - a string identifying the model frames to include (see below)
#'   * `FEs` - a formula identifying fixed effects 
#'   * `ECs` - a formula identifying environmental covariates
#'   * `REs` - a formual idenitfying random effects
#'   * `label` - a label to use to identify the parameter definition
#' 
#' `MFs` is a string identifying the combination of model frames to use to expand the effects formulas. A single 
#' model frame can be specified simply by its name. Multiple model frames must be specified using the format 
#' [..mf1;..mf2;...], where mf1 and mf2 are names of model frames. The "."'s are optional spacers 
#' (and ignored when parsing this element), individual model frames are separated by semicolons (";").
#' 
#' @export
#'
expandSelParamsMFs<-function(tblSelPars,lstMFs){
  parse_MFs_string<-function(strMFs){
    txt = stringr::str_remove_all(strMFs,"[.\\[\\]]");
    v   = stringr::str_split_1(txt,";");
    return(v);
  }
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
    strMFs = parse_MFs_string(rw$MFs);
    tblMF = NULL;
    for (strMF in strMFs){
      tblMF = tidyr::expand_grid(tblMF,evalTxt(strMF,lstMFs));
    }
    # tblYS   = evalTxt(rw$timeFrames,lstMFs);
    # tblFlts = evalTxt(rw$fleets,lstMFs);
    # tblPDs  = evalTxt(rw$popDims,lstMFs);
    # tblMF   = tidyr::expand_grid(tblYS,tblFlts,tblPDs);
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

