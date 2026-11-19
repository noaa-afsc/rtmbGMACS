#' Construct a rtmbGMACS model matrix
#'
#' Constructs a model matrix from a formula and data. Factor and logical
#' predictors are assigned identity contrast matrices if contrasts for these 
#' have not already been assigned, 
#' retaining one column for every factor level, including factors with a single level 
#' (in contrast to [stats::model.matrix()], which drops single-level factors).
#'
#' @param object A model formula or terms object.
#' @param data A data frame or model frame containing the variables in
#'   `object`. By default, the environment of `object` is used.
#' @param contrasts.arg An optional named list specifying contrasts for factor
#'   variables.
#' @param xlev An optional named list of factor levels to use when constructing
#'   a model frame.
#' @param ... Additional arguments. Currently unused.
#'
#' @return A numeric model matrix. Its `assign` attribute identifies the
#'   formula term associated with each column, and its `contrasts` attribute
#'   records contrasts used for factor and logical predictors.
#' @export
#'
#' @examples
#' data <- data.frame(
#'   group = factor(c("a", "b", "a")),
#'   value = c(1, 2, 3)
#' );
#' gmacs.model.matrix(object=~ 0 + group + value, data = data);
#' #--with a missing level
#' data <- data.frame(
#'   group = factor(c("a", "b", "a"),levels=c("a","b","c")),
#'   value = c(1, 2, 3)
#' );
#' gmacs.model.matrix(object=~ 0 + group + value, data = data);
#' #--note that "c" is included in the contrasts for `group` even though it does not appear 
#' #--in the model matrix.
#' 
#' #--changing the contrasts for the `group` factor
#' gmacs.model.matrix(object=~ 0 + group + value, data = data,contrasts.arg=list(group="contr.treatment"));
#' 
gmacs.model.matrix<-function (object, 
                              data = environment(object), 
                              contrasts.arg = NULL, 
                              xlev = NULL, ...) {
  #--get terms
  t <- if (missing(data)) {terms(object)} else {terms(object, data = data)}

  if (is.null(attr(data, "terms"))) {
      data <- model.frame(object, data, xlev = xlev)
  } else {
    reorder <- match(vapply(attr(t, "variables"), stats:::deparse2, 
        "")[-1L], names(data))
    if (anyNA(reorder)) 
        stop("model frame and formula mismatch in model.matrix()")
    if (!identical(reorder, seq_len(ncol(data)))) 
        data <- data[, reorder, drop = FALSE]
  }
  
  int <- attr(t, "response")
  if (length(data)) { #--ncol(data)>0
    #contr.funs <- as.character(getOption("contrasts"))
    namD <- names(data)
    for (i in namD){ 
      if (is.character(data[[i]]))
        data[[i]] <- factor(data[[i]]);
    }
    isF <- vapply(data, function(x) is.factor(x) || is.logical(x), NA);
    isF[int] <- FALSE;
    isOF <- vapply(data, is.ordered, NA);
    for (nn in namD[isF]) {
      if (is.null(attr(data[[nn]], "contrasts"))) {
        #contrasts(data[[nn]]) <- contr.funs[1 + isOF[nn]]
        nlev = length(levels(data[[nn]]));
        attr(data[[nn]], "contrasts") <- diag(1,nlev);
      }
    }
    if (!is.null(contrasts.arg)) {
      if (!is.list(contrasts.arg)){ 
        warning("non-list contrasts argument ignored");
      } else {
        if (is.null(namC <- names(contrasts.arg))) 
          stop("'contrasts.arg' argument must be named");
        for (nn in namC) {
          if (is.na(ni <- match(nn, namD))) {
            warning(gettextf("variable '%s' is absent, its contrast will be ignored",nn), 
                    domain = NA);
          } else {
            ca <- contrasts.arg[[nn]];
            if (is.matrix(ca)){ 
              contrasts(data[[ni]], ncol(ca)) <- ca;
            } else contrasts(data[[ni]]) <- ca;
          }
        }
      }
    }
  } else { 
    ncol(data)==0;
    isF <- FALSE;
    data[["x"]] <- raw(nrow(data));
  }
  ans <- .External2(stats:::C_modelmatrix, t, data);
  if (any(isF)) 
      attr(ans, "contrasts") <- lapply(data[isF], attr, "contrasts");
  ans
}
