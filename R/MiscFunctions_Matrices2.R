#' @title Create a vector and assign names in one function call
#' @description Function to create a vector and assign names in one function call.
#' @param values - values for vector
#' @param names - names to assign to elements of vector
#' @return vector with names
#' @export
makeVector<-function(values,names=NULL){
  v = values; if (!is.null(names)) names(v)<-names;
  v;
}

#' @title Create a basic fixed-step transition matrix
#' @description
#' Function to create a basic fixed-step transition matrix for a set of levels.
#' @param lvls - vector of levels for which to create the TM.
#' @param step - transition step
#' @details The resulting matrix provides the transition matrix `m` from
#' level `i` to level `i+step` (`i=1:n`) for an `n`-level vector `v` such that
#' $v_{new} = m %*% v$. If , The row names and column names of the resulting matrix
#' will be the values of `lvls` if it is a character vector or the names of `lvls`
#' if it has names.
#'
#' @return the transition matrix.
#' @examplesIf FALSE
#' # example code
#'  lvls=1:5;
#'  makeTM_BasicFixedStep(lvls,step=0);
#'  makeTM_BasicFixedStep(lvls,step=1);
#'  makeTM_BasicFixedStep(lvls,step=-1);
#'  makeTM_BasicFixedStep(lvls,step=1) %*% lvls;
#'
#'  lvls=c("new","old 1","old 2","old 3");
#'  makeTM_BasicFixedStep(lvls,step=0);
#'  makeTM_BasicFixedStep(lvls,step=1);
#'  makeTM_BasicFixedStep(lvls,step=-1,accum=FALSE);
#'  v = makeVector(2*(1:length(lvls)),lvls);
#'  v;
#'  makeTM_BasicFixedStep(lvls,step=1) %*% v;
#' @export
#'
makeTM_BasicFixedStep<-function(lvls,step=1,accum=TRUE){
  nl = length(lvls);
  if (is.character(lvls))    {nms=lvls;} else
  if (!is.null(names(lvls))) {nms=names(lvls);} else
  {nms=1:nl;}
  m = matrix(0,nrow=nl,ncol=nl,dimnames=list(nms,nms));
  for (i in 1:nl) {if ((i-step)<=nl) {m[i,i-step] = 1;}}
  if (accum) {
    if (step>0) m[nl,nl] = 1;#--last state is accumulator
    if (step<0) m[1,1]   = 1;#--first state is accumulator
  }
  m;
}
if (FALSE){
  lvls=1:5;
  makeTM_BasicFixedStep(lvls,step=0);
  makeTM_BasicFixedStep(lvls,step=1);
  makeTM_BasicFixedStep(lvls,step=-1);

  lvls=makeVector(c("new","old 1","old 2","old 3"));
  makeTM_BasicFixedStep(lvls,step=0);
  makeTM_BasicFixedStep(lvls,step=1);
  makeTM_BasicFixedStep(lvls,step=-1,accum=FALSE);

  lvls=makeVector(stats::rnorm(4,0,1),c("new","old 1","old 2","old 3"));
  makeTM_BasicFixedStep(lvls,step=1) %*% lvls;
}

#' @title Expand a simple transition matrix to the full population class vector
#' @description Function to expand a simple transition matrix to the full population class vector.
#' @param tm - the simple transition matrix to expand
#' @param dfrC - the DimsMap dataframe object defining `nc` population classes
#' @return a `nc x nc` transition matrix for the full population vector
#' @examplesIf FALSE
#' # example code
#'   vRs="EBS";
#'   maz = as.character(seq(25,75,5));
#'   fiz = as.character(seq(25,65,5));
#'   fmz = as.character(seq(30,65,5));
#'   vXs=list(male=list(all=list(`new`=maz,
#'                                `old 1`=maz,
#'                                `old 2`=maz,
#'                                `old 3`=maz
#'                               )
#'                     ),
#'             female=list(imm=list(`new`=fiz,
#'                                  `old 1`=fiz,
#'                                  `old 2`=maz,
#'                                  `old 3`=maz
#'                                  ),
#'                         mat=list(`new`=fmz,
#'                                  `old 1`=fmz,
#'                                  `old 2`=maz,
#'                                  `old 3`=maz
#'                                  )
#'                        )
#'            ); attr(vXs,"dmnms")<-c("x","m","p","z");
#'   dmsC    = createSparseDimsMap(r=vRs,x=vXs);            #--DimsMap for stock categories
#'
#'   p=makeVector(attributes(dmsC)$dmlvs$p);
#'   pTM1 = makeTM_BasicFixedStep(p,step=1);
#'   pTM = expandTM(pTM1,dmsC);
#'
#' @export
#'
expandTM<-function(tm,dfrC){
  dfrC = dfrC |> dplyr::mutate(row=rownames(dfrC));
  dfrTM = tibble::as_tibble(tm) |>
            dplyr::mutate(row=rownames(tm)) |>
            tidyr::pivot_longer(colnames(tm),names_to="col",values_to="val") |>
            dplyr::filter(abs(val)>0);
  mC    = matrix(0,nrow(dfrC),nrow(dfrC),dimnames=list(dfrC$row,dfrC$row)) |>
            tibble::as_tibble() |>
            dplyr::mutate(row=dfrC$row) |>
            tidyr::pivot_longer(cols=dfrC$row,names_to="col",values_to="val") |>
            dplyr::select(!val) |>
            dplyr::mutate(rowDim=dfrC$p[as.integer(row)],
                          colDim=dfrC$p[as.integer(col)]) |>
            dplyr::left_join(dfrTM,by=c("rowDim"="row","colDim"="col")) |>
            dplyr::mutate(val=ifelse(is.na(val),0,val)) |>
            dplyr::select(!c(rowDim, colDim)) |>
            tidyr::pivot_wider(names_from=col,values_from=val) |>
            dplyr::select(!row) |>
            as.matrix();
  return(mC);
}
if (FALSE){
  dirPrj = rstudioapi::getActiveProject();
  source(file.path(dirPrj,"R/DimensionsFunctions.R"))
  source(file.path(dirPrj,"R/DimensionsUtilities.R"))
  vRs="EBS";
  maz = as.character(seq(25,75,5));
  fiz = as.character(seq(25,65,5));
  fmz = as.character(seq(30,65,5));
  vXs=list(male=list(all=list(`new`=maz,
                               `old 1`=maz,
                               `old 2`=maz,
                               `old 3`=maz
                              )
                    ),
            female=list(imm=list(`new`=fiz,
                                 `old 1`=fiz,
                                 `old 2`=maz,
                                 `old 3`=maz
                                 ),
                        mat=list(`new`=fmz,
                                 `old 1`=fmz,
                                 `old 2`=maz,
                                 `old 3`=maz
                                 )
                       )
           ); attr(vXs,"dmnms")<-c("x","m","p","z");
  dmsC    = createSparseDimsMap(r=vRs,x=vXs);            #--DimsMap for stock categories

  p=makeVector(attributes(dmsC)$dmlvs$p);
  pTM1 = makeTM_BasicFixedStep(p,step=1);
  pTM = expandTM(pTM1,dmsC);
}

