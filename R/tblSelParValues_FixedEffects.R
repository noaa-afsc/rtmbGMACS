# rw = tblExpSelPars[1,];
tblSelParValues_FixedEffects<-function(rw){
  pid_ = rw$pid;
  lbl  = rw$label;
  lstFEs = rw$lstFEs$lstFEs;
  nvar = ncol(lstFEs$mfFEs);
  tblVars = lstFEs$umfmFEs[,1:nvar];
  vecVars <- apply(tblVars, 1, paste, collapse = "_");
  tblVars = tblVars |> dplyr::mutate(pid=pid_,param=rw$param,.before=1);
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
  return(tbl);
}
