#' @title Get standard population dimensions as a tibble 
#' @description Function to get standard population dimensions as a tibble. 
#' @returns A [tibble::tibble] with columns "variable" and "description" 
#' describing the standard population dimensions
#' @details The tibble lists "standard" dimensions that define a population at a 
#' single time. None of these other than "z" (size) is required.
#' @export
getTbl_PopDimsStd<-function(){
  tbl = tibble::tribble(~dimension,~description,
                        "x","sex",
                        "m","maturity state",
                        "c","shell condition",
                        "z","size"
                       );
  return(tbl);
}

#' @title Get "extra" population dimensions as a tibble 
#' @description Function to get "extra" population dimensions as a tibble. 
#' @returns A [tibble::tibble] with columns "variable" and "description" 
#' describing the standard population dimensions
#' @details The tibble lists "extra" dimensions that may define a population at a 
#' single time. 
#' @export
getTbl_PopDimsXtra<-function(){
  tbl = tibble::tribble(~dimension,~description,
                        "r","region or area",
                        "a","age",
                        "p","post-molt age"
                       );
  return(tbl);
}
#' @title Get standard population dimensions as a tibble 
#' @description Function to get standard population dimensions as a tibble. 
#' @returns A [tibble::tibble] with columns "variable" and "description" 
#' describing the standard population dimensions
#' @details The tibble lists "standard" dimensions that define a population at a 
#' single time. None of these other than "z" (size) is required.
#' @export
getTbl_StdModDims<-function(){
  tbl = tibble::tribble(~dimension,~description,
                        "y","year",
                        "s","season",
                        "f","fleet"
                       );
  tbl = dplyr::bind_rows(tbl,getTbl_StdPopDims);
  return(tbl);
}

#' @title Get model dimension names
#' @description Function to get model dimension names.
#' @return character vector with model dimension names
#' @export
getDimNames<-function(){
  return(c("f","y","s","r","x","m","p","z"));
}

#' @title Get which model dimensions are identified in a character vector
#' @description Function to get which model dimensions are identified in a character vector.
#' @param char_vec - character vector to check for model dimension names
#' @return character vector with identifed model dimension names
#' @export
whichDims<-function(char_vec){
  return(char_vec[char_vec %in% getDimNames()]);
}

