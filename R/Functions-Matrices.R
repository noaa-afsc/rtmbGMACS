#' Create a sparse aggregation matrix
#'
#' Creates a sparse matrix that maps rows of a data frame to groups defined by
#' one or more columns. Multiplying the matrix by a vector with one value per
#' input row calculates sums for each group.
#'
#' @param dfr A data frame containing the grouping columns.
#' @param ... Unquoted columns in `dfr` that define the groups. These arguments
#'   are passed to [dplyr::group_by()].
#'
#' @return A named list with two elements:
#'   \describe{
#'     \item{`aggM`}{A sparse matrix with one row per group and one column per
#'       row of `dfr`.}
#'     \item{`aggTbl`}{A data frame containing the unique grouping combinations,
#'       in the same order as the rows of `aggM`.}
#'   }
#' @examples
#' y <- as.character(11:20)
#' s <- as.character(1:3)
#' x <- as.character(1:2)
#' z <- seq(25, 50, 5)
#' dfr <- tidyr::expand_grid(y, s, x, z)
#'
#' lst <- createAggregationMatrix(dfr, y, s, x)
#' values <- as.matrix(dfr["z"])
#' group_sums <- lst$aggM %*% values
#'
#' lst$aggTbl |> dplyr::mutate(sum=as.vector(group_sums));
#' 
#' @export
createAggregationMatrix<-function(dfr,...){
  dfrGB = dfr   |> dplyr::group_by(...);                  #--group by variables in ...
  gid   = dfrGB |> dplyr:::group_indices.data.frame();    #--create vector of grouping indices
  dfrp  = dfr |> dplyr::mutate(.id=dplyr::row_number(),.gid=gid);    #--add grouping indices to dataframe
  dfrp  = dfrp |> dplyr::select(.id,.gid) |>                         #--pivot wider on .gid to identify matrix elements that will be 1's
            tidyr::pivot_wider(names_from=.gid,values_from=.gid) |> 
            dplyr::select(!.id) |> 
            dplyr::mutate(dplyr::across(dplyr::everything(),~ifelse(is.na(.x),0,1)));
  aggM = Matrix::Matrix(as.matrix(dfrp),sparse=TRUE);                                        #--convert to sparse matrix
  dfrGB = dfrGB |> dplyr::summarize(.cnt=dplyr::n(),.groups="drop") |> dplyr::select(!.cnt); #--create ungrouped tibble with grouping levels
  return(list(aggM=Matrix::t(aggM),aggTbl=dfrGB));
}

#' Add group indices to a data frame
#'
#' Adds an integer column identifying the group to which each row belongs.
#' Groups are defined by one or more columns and follow the grouping behavior of
#' [dplyr::group_by()].
#'
#' @param dfr A data frame containing the grouping columns.
#' @param ... Unquoted columns in `dfr` that define the groups. These arguments
#'   are passed to [dplyr::group_by()].
#'
#' @return `dfr` with its rows and existing columns preserved and an integer
#'   `.gid` column appended. Rows with the same values of the grouping columns
#'   receive the same group index. An existing `.gid` column is replaced.
#'
#' @examples
#' y <- as.character(11:20)
#' s <- as.character(1:3)
#' x <- as.character(1:2)
#' z <- seq(25, 50, 5)
#' dfr <- tidyr::expand_grid(y, s, x, z)
#'
#' dfrp <- addGroupIndices(dfr, y, s, x)
#' head(dfrp)
#' dplyr::count(dfrp, y, s, x, .gid)
#'
#' @export
addGroupIndices<-function(dfr,...){
  dfrGB = dfr   |> dplyr::group_by(...);                  #--group by variables in ...
  gid   = dfrGB |> dplyr:::group_indices.data.frame();    #--create vector of grouping indices
  dfrp  = dfr |> dplyr::mutate(.gid=gid);                 #--add grouping indices to dataframe
  return(dfrp);
}

# y = 1990:1995;
# s = 1:3;
# x = c("m","f");
# m = c("imm","mat");
# p = c("ns", "os1", "os2", "os3");
# z = seq(27.5,67.5,5);

# #--maturity depends on post-molt size, terminal molt
# ##--molt transition
#                     imm:ns            imm:os1           imm:os2
# trans_molt = matrix(  prMolt("ns")    prMolt("os")    prMolt("os2")         imm:ns
#                     1-prMolt("os1")      0                0                 imm:os1
#                          0          1-prMolt("os1")  1-prMolt("os2")        imm:os2
#                     )
#                     mat:ns            mat:os1           mat:os2
# trans_molt = matrix(  0                  0                 0         mat:ns
#                       1                  0                 0         mat:os1
#                       0                  1                 1         mat:os2
#                     )

#                       imm:ns           imm:os1      imm:os2
# trans_growth = matrix(prGr(z))           0            0        imm:ns
#                         0                1            0        imm:os1
#                         0                0            1        imm:os2
#                       )
#                     mat:ns            mat:os1           mat:os2
# trans_molt = matrix(  1                  0                 0         mat:ns
#                       0                  1                 0         mat:os1
#                       0                  0                 1         mat:os2
#                     )

#                       imm:ns           mat:ns   mat:os1  mat:02   mat:os3
# trans_mature = matrix(1-prMat(z)          0     imm:ns
#                       prMat(z)            0     mat:ns
#                      )


# tbl_ysxmpz = tidyr::expand_grid(y,s,x,m,p,z);

  
#' @title Create a state transitions map 
#' @description Function to create a state transitions map.
#' @param dfrSTF - complete state transitions map dataframe
#' @param dfrSTs - defined transitions map dataframe
#' @examplesIf FALSE
#' # example code
#' ##--define model dimensions
#' sex_states = c("m","f");      #--sexes
#' mat_states = c("imm","mat");  #--maturity state
#' pma_states = c("ns","os");    #--"post molt age" states (otherwise known as shell condition)
#' size_bins  = seq(27.5,52.5,5) #--size bins
#' ##--define model frame
#' dfrMF = tidyr::expand_grid(x=sex_states,m=mat_states,p=pma_states,z=size_bins) |> 
#'         dplyr::mutate(id=dplyr::row_number());
#' cat(writeToStr_Dataframe(dfrMF))
#' ##
#' ##--create state transitions associated with molting for Chionoecetes spp. (mature crab don't molt)
#' ###--immature, new shell crab may molt (imm,ns->imm,ns) or not (imm,ns->imm,os)
#' ###--immature, old shell crab may molt (imm,os->imm,ns) or not (imm,os->imm,os)
#' ###--mature, new shell crab don't change state (can't molt) (mat,ns->mat,ns)
#' ###--mature, old shell crab don't change state (can't molt) (mat,os->mat,os)
#' ###--create complete state transitions map dataframe for molt probabilities
#' dfrSTF = tidyr::expand_grid(m=mat_states,p=pma_states) |> 
#'          dplyr::mutate(sid=dplyr::row_number());
#' cat(writeToStr_Dataframe(dfrSTF))
#' ##--create defined state transitions map dataframe for Chionoecetes spp. 
#' ###--immature crab may molt, mature crab don't
#' dfrSTs = tibble::tribble(~from,~to,~val,
#'                             1,   1,  1,   # T(1,1) = p_molt(ns)              <= imm,ns can molt, stays imm,ns
#'                             1,   2, -1,   # T(1,2) = 1-Sum_{j ne 2}{T[2,j]}  <= imm,ns doesn't molt, becomes imm,os
#'                          #  1,   3,  0,   # T(1,3) = 0                       <= imm,ns can't become mat,ns
#'                          #  1,   4,  0,   # T(1,4) = 0                       <= imm,ns can't become mat,os
#'                             2,   1,  1,   # T(2,1) = p_molt(os)              <= imm,os can molt, becomes imm,ns
#'                             2,   2, -1);  # T(2,2) = 1-Sum_{j ne 2}{T[2,j]}  <= imm,os doesn't molt, stays imm,os
#'                          #  2,   3,  0,   # T(2,3) = 0                       <= imm,os can't become mat,ns
#'                          #  2,   4,  0,   # T(2,4) = 0                       <= imm,os can't become mat,os
#'                          #  3,   1,  0,   # T(3,1) = 0                       <= mat,ns can't become imm,ns
#'                          #  3,   2,  0,   # T(3,2) = 0                       <= mat,ns can't become imm,os
#'                          #  3,   3,  0,   # T(3,3) = 1                       <= mat,ns stays mat,ns
#'                          #  3,   4,  0,   # T(3,4) = 0                       <= mat,ns can't become mat,os
#'                          #  4,   1,  0,   # T(4,1) = 0                       <= mat,os can't become imm,ns
#'                          #  4,   2,  0,   # T(4,2) = 0                       <= mat,os can't become imm,os
#'                          #  4,   3,  0,   # T(4,3) = 0                       <= mat,os can't become mat,ns
#'                          #  4,   4,  0,   # T(4,4) = 1                       <= mat,os stays mat,os
#'  cat(writeToStr_Dataframe(dfrSTs))
#'  lst = createStateTransitionMatrixMap(dfrSTF,dfrSTs);
#'  cat(writeToStr_Dataframe(lst$dfrT))
#'  cat(writeToStr_Matrix(lst$mT))
#' ##
#' ##--create state transitions associated with maturation for Chionoecetes spp.
#' ###--immature, new shell crab may mature (imm,ns->mat,ns) or stay immature (imm,ns->imm,ns)
#' ###--immature, old shell crab can not mature (didn't molt) (imm,os->imm,os)
#' ###--mature, new shell crab become old shell (didn't molt) (mat,ns->mat,os)
#' ###--mature, old shell crab stay old shell (didn't molt) (mat,os->mat,os)
#' ###--create complete state transitions map dataframe for maturity probabilities
#' dfrSTF = tidyr::expand_grid(m=mat_states,p=pma_states) |> 
#'          dplyr::mutate(sid=dplyr::row_number());
#' cat(writeToStr_Dataframe(dfrSTF))
#' ##--create defined state transitions map dataframe
#' dfrSTs = tibble::tribble(~from,~to,~val,
#'                             1,   1, -1,   # T(1,1) = 1-Sum_{j ne 1}{T[1,j]} <= imm,ns stays imm,ns w/ prob 1-p_mature
#'                          #  1,   2,  0,   # T(1,2) = 0                      <= imm,ns can't become imm,os
#'                             1,   3,  1,   # T(1,3) = p_mature               <= imm,ns becomes mat,ns w/ prob p_mature
#'                          #  1,   4,  0,   # T(1,4) = 0                      <= imm,ns can't become mat,os
#'                          #  2,   1,  0,   # T(2,1) = 0                      <= imm,os can't become imm,ns
#'                          #  2,   2,  1,   # T(2,2) = 1                      <= imm,os stays imm,os
#'                          #  2,   3,  0,   # T(2,3) = 0                      <= imm,os can't become mat,ns
#'                          #  2,   4,  0,   # T(2,4) = 0                      <= imm,os can't become mat,os
#'                          #  3,   1,  0,   # T(3,1) = 0                      <= mat,ns can't become imm,ns
#'                          #  3,   2,  0,   # T(3,2) = 0                      <= mat,ns can't become imm,os
#'                          #  3,   3,  0,   # T(3,3) = 0                      <= mat,ns can't stay mat,ns
#'                             3,   4,  1);  # T(3,4) = 1                      <= mat,ns must become mat,os
#'                          #  4,   1,  0,   # T(4,1) = 0                      <= mat,os can't become imm,ns
#'                          #  4,   2,  0,   # T(4,2) = 0                      <= mat,os can't become imm,os
#'                          #  4,   3,  0,   # T(4,3) = 0                      <= mat,os can't become mat,ns
#'                          #  4,   4,  1,   # T(4,4) = 1                      <= mat,os stays mat,os
#' cat(writeToStr_Dataframe(dfrSTs))
#' lst = createStateTransitionMatrixMap(dfrSTF,dfrSTs);
#' cat(writeToStr_Dataframe(lst$dfrT))
#' cat(writeToStr_Matrix(lst$mT))
#' ##
#' ## Add a third shell condition and create a molt 
#' pma_states = c("ns","os1","os2");    #--"post molt age" states
#' ##--create (new) model frame
#' dfrMF = tidyr::expand_grid(x=sex_states,m=mat_states,p=pma_states,z=size_bins) |> 
#'         dplyr::mutate(id=dplyr::row_number());
#' ##--create state transitions associated with molting based Chionoecetes spp. (mature crab don't molt)
#' ###--imm, ns crab  may molt (imm,ns ->imm,ns) or not (imm,ns ->imm,os1)
#' ###--imm, os1 crab may molt (imm,os1->imm,ns) or not (imm,os1->imm,os2)
#' ###--imm, os2 crab may molt (imm,os2->imm,ns) or not (imm,os2->imm,os2)
#' ###--mature crab don't molt (can't change state) (mat,.->mat,.)
#' ###--create complete state transitions map dataframe for molt probabilities
#' dfrSTF = tidyr::expand_grid(m=mat_states,p=pma_states) |> 
#'          dplyr::mutate(sid=dplyr::row_number());
#' cat(writeToStr_Dataframe(dfrSTF))
#' ##--create defined state transitions map dataframe for Chionoecetes spp. 
#' ###--immature crab may molt, mature crab don't
#' dfrSTs = tibble::tribble(~from,~to,~val,
#'                             1,   1,  1,   # T(1,1) = p_molt(ns)              <= imm,ns can molt, stays imm,ns
#'                             1,   2, -1,   # T(1,2) = 1-Sum_{j ne 2}{T[1,j]}  <= imm,ns doesn't molt, becomes imm,os1
#'                             2,   1,  1,   # T(2,1) = p_molt(os)              <= imm,os1 can molt, becomes imm,ns
#'                             2,   3, -1,   # T(2,3) = 1-Sum_{j ne 3}{T[2,j]}  <= imm,os1 doesn't molt, becomes imm,os2
#'                             3,   1,  1,   # T(3,1) = p_molt(os)              <= imm,os2 can molt, becomes imm,ns
#'                             3,   3, -1);  # T(3,3) = 1-Sum_{j ne 3}{T[3,j]}  <= imm,os2 doesn't molt, stays imm,os2
#'  cat(writeToStr_Dataframe(dfrSTs))
#'  lst = createStateTransitionMatrixMap(dfrSTF,dfrSTs);
#'  cat(writeToStr_Dataframe(lst$dfrT))
#'  cat(writeToStr_Matrix(lst$mT))
#'  dfrExpMF = dplyr::left_join(dfrMF,lst$dfrT,by=c("m","p"));
#'  cat(writeToStr_Dataframe(dfrExpMF)) 
#'  
#'  @import dplyr
#'  @export
#'  
createStateTransitionMatrixMap<-function(dfrSTF,dfrSTs=NULL,verbose=FALSE){
  ##--create dataframe representing transition process
  ns = nrow(dfrSTF); #--total number of states
  ###--start with no transitions
  dfrT = dplyr::cross_join(dfrSTF,dfrSTF,suffix=c("_from","_to")) |> 
           dplyr::mutate(idx=0);
  if (verbose) {
    cat("dataframe with no transitions defined:\n")
    cat(writeToStr_Dataframe(dfrT));
  }
  ###--add in defined transitions
  if (!is.null(dfrSTs)){
    for (r in 1:nrow(dfrSTs)){
      rw = dfrSTs[r,];
      dfrT$idx[(dfrT$sid_from==rw$from[1])&(dfrT$sid_to==rw$to[1])] = rw$val[1];
    }
    if (verbose) {
      cat("dataframe with defined transitions only:\n")
      cat(writeToStr_Dataframe(dfrT));
    }
  }
  ###--add in self-only transitions
  dfrSelfIDs = dfrT |> 
                 dplyr::summarize(idx=sum(abs(idx)),.by=sid_from) |> 
                 dplyr::filter(idx==0) |> dplyr::select(from=sid_from);
  for (r in 1:nrow(dfrSelfIDs)){
    rw = dfrSelfIDs[r,];
    dfrT$idx[(dfrT$sid_from==rw$from[1])&(dfrT$sid_to==rw$from[1])] = 1;
  }
  if (verbose) {
    cat("dataframe with defined and implicit self transitions defined:\n")
    cat(writeToStr_Dataframe(dfrT));
  }
  ##--create transition matrix map (need to take transpose so row number indexes "to" state)
  mT = t(matrix(data=dfrT$idx,nrow=ns,ncol=ns,byrow=TRUE));
  if (verbose){
    cat("matrix map with defined and implicit self transitions defined:\n")
    for (r in 1:ns){
      cat(mT[r,],"\n")
    }
  }
  colnames(mT) = as.character(1:ns);
  dfrT_wide=dplyr::bind_cols(dfrSTF,mT)
  return(list(dfrT=dfrT_wide,dfrT_long=dfrT,mT=mT));
}



