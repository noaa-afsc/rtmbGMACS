#' Remove columns containing only missing values
#'
#' Checks every column in a table and removes columns for which all values are
#' `NA`.
#'
#' @param tbl A data frame or tibble.
#'
#' @return `tbl` without columns containing only missing values.
#' @export
#'
#' @examples
#' tbl <- tibble::tibble(x = 1:3, y = c(NA, NA, NA));
#' remove_all_NA_columns(tbl);
remove_aLL_NA_columns<-function(tbl){
  cols = names(tbl);
  for (col in cols) tbl = remove_all_NA_column(tbl,col);
  return(tbl);
}

#' Remove a column if all its values are missing
#'
#' Removes a specified column from a table when every value in that column is
#' `NA`. Otherwise, the table is returned unchanged.
#'
#' @param tbl A data frame or tibble.
#' @param column A single character string naming the column to check.
#'
#' @return `tbl` without `column` if all its values are missing; otherwise,
#'   `tbl` unchanged.
#' @export
#'
#' @examples
#' tbl <- tibble::tibble(x = 1:3, y = c(NA, NA, NA));
#' remove_all_NA_column(tbl, "y");
remove_all_NA_column <- function(tbl, column) {
  if (all(is.na(tbl[[column]]))) {
    tbl <- dplyr::select(tbl, -dplyr::all_of(column));
  }
  return(tbl);
}

#' Combine parameters defined by different model-frame variables
#'
#' Combines parameter-value columns whose non-missing values occur in disjoint
#' rows because the parameters use different sets of model-frame variables.
#' Parameters are aligned by their shared model-frame variables and by their
#' order within each group of shared values.
#'
#' @param tblPars A data frame or tibble. Model-frame columns must precede the
#'   parameter-value columns.
#' @param npid A positive integer giving the number of parameter-value columns
#'   at the end of `tblPars`.
#'
#' @return A tibble with one set of aligned rows containing the relevant
#'   model-frame variables and parameter values.
#' @export
#'
#' @examples
#' tbl <- tibble::tibble(
#'   f = rep("TCF", 4),
#'   tb = c(1, 3, NA, NA),
#'   tb1 = c(NA, NA, 1, 2),
#'   param9 = c(145, 130, NA, NA),
#'   param10 = c(NA, NA, 15, 10)
#' );
#' combine_disjoint_parameter_values(tbl, npid = 2);
combine_disjoint_parameter_values<-function(tblPars,npid){
    par_cols = tail(names(tblPars),npid);
    mf_cols = head(names(tblPars),-npid);
    par_mf_cols = lapply(par_cols,function(par_col){
      rows = !is.na(tblPars[[par_col]]);
      return(mf_cols[vapply(tblPars[rows,mf_cols,drop=FALSE],function(x) any(!is.na(x)),logical(1))]);
    });
    common_mf_cols = Reduce(intersect,par_mf_cols);
    join_cols = c(common_mf_cols,".obs");
    par_tbls = Map(function(par_col,active_mf_cols){
      out = tblPars |>
              dplyr::filter(!is.na(.data[[par_col]])) |>
              dplyr::select(dplyr::all_of(c(active_mf_cols,par_col)));
      if (length(common_mf_cols)>0){
        out = out |>
                dplyr::group_by(dplyr::across(dplyr::all_of(common_mf_cols))) |>
                dplyr::mutate(.obs=dplyr::row_number()) |>
                dplyr::ungroup();
      } else {
        out = out |> dplyr::mutate(.obs=dplyr::row_number());
      }
      return(out);
    },par_cols,par_mf_cols);
    tblPars = Reduce(function(x,y) dplyr::full_join(x,y,by=join_cols),par_tbls) |>
                dplyr::select(-.obs) |>
                dplyr::select(dplyr::all_of(unique(unlist(par_mf_cols))),dplyr::all_of(par_cols));
  return(tblPars);
}
