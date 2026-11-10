expandParamsMFs<-function(tblSelPars,lstMFs){
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
    lstAll[[r]] = tibble::tibble(fid=rw$fid,
                                pid=rw$pid,
                                mf=list(mf=tblMF),
                                FEs=rw$FEs,
                                ECs=rw$ECs,
                                REs=rw$REs,
                                label=rw$label);

  }
  tblAll = dplyr::bind_rows(lstAll);
  return(tblAll);
}

