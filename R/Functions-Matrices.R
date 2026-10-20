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

