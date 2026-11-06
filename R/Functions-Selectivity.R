#' Logistic selectivity
#'
#' Calculates logistic selectivity using the GMACS `LogisticCurve` parameterization.
#'
#' @param z data: Numeric vector of sizes.
#' @param p param: Numeric vector containing the mean and standard deviation,
#'   in that order.
#'
#' @return A numeric vector of selectivity values with the same length as `z`.
#' @keywords rtmbFunction
#' @export
#'
#' @examples
#' sel_logistic(z = seq(20, 100, by = 10), p = c(50, 8))
sel_logistic <- function(z, p) {
  "[<-" <- ADoverload("[<-");
  "c" <- ADoverload("c");
  "diag<-" <- ADoverload("diag<-");

  if (length(p) != 2L) {
    stop("`p` must have length 2.", call. = FALSE);
  };

  result <- 1 / (1 + exp(-(z - p[1]) / p[2]));
  return(result);
};

#' Stacked logistic selectivity
#'
#' Calculates a weighted average of two logistic selectivity curves using the
#' GMACS `StackedLogistic` parameterization.
#'
#' @param z data: Numeric vector of sizes.
#' @param p param: Numeric vector containing the weight, first mean, first
#'   standard deviation, second mean, and second standard deviation, in that
#'   order.
#'
#' @return A numeric vector of selectivity values with the same length as `z`.
#' @keywords rtmbFunction
#' @export
#'
#' @examples
#' sel_stacked_logistic(
#'   z = seq(20, 100, by = 10),
#'   p = c(0.6, 40, 6, 70, 10)
#' )
sel_stacked_logistic <- function(z, p) {
  "[<-" <- ADoverload("[<-");
  "c" <- ADoverload("c");
  "diag<-" <- ADoverload("diag<-");

  if (length(p) != 5L) {
    stop("`p` must have length 5.", call. = FALSE);
  };

  first <- 1 / (1 + exp(-(z - p[2]) / p[3]));
  second <- 1 / (1 + exp(-(z - p[4]) / p[5]));
  result <- p[1] * first + (1 - p[1]) * second;
  return(result);
};

#' Logistic selectivity parameterized by 50 and 95 percent selection
#'
#' @param z data: Numeric vector of sizes.
#' @param p param: Numeric vector containing size at 50 percent selection and
#'   size at 95 percent selection, in that order.
#'
#' @return A numeric vector of selectivity values with the same length as `z`.
#' @keywords rtmbFunction
#' @export
#'
#' @examples
#' sel_logistic95(z = seq(20, 100, by = 10), p = c(50, 70))
sel_logistic95 <- function(z, p) {
  "[<-" <- ADoverload("[<-");
  "c" <- ADoverload("c");
  "diag<-" <- ADoverload("diag<-");

  if (length(p) != 2L) {
    stop("`p` must have length 2.", call. = FALSE);
  };

  result <- 1 / (1 + exp(-log(19) * (z - p[1]) / (p[2] - p[1])));
  return(result);
};

#' Selectivity coefficients
#'
#' Exponentiates log-scale coefficients. If fewer coefficients than sizes are
#' supplied, the final coefficient is repeated for the remaining sizes.
#'
#' @param z data: Numeric vector of sizes.
#' @param p param: Numeric vector of log-scale selectivity coefficients.
#'
#' @return A numeric vector of selectivity values with the same length as `z`.
#' @keywords rtmbFunction
#' @export
#'
#' @examples
#' sel_coefficients(z = seq(20, 70, by = 10), p = log(c(0.2, 0.5, 0.9)))
sel_coefficients <- function(z, p) {
  "[<-" <- ADoverload("[<-");
  "c" <- ADoverload("c");
  "diag<-" <- ADoverload("diag<-");

  if (length(p) < 1L || length(p) > length(z)) {
    stop("`p` must have length between 1 and `length(z)`.", call. = FALSE);
  };

  result <- rep(exp(p[length(p)]), length(z));
  result[seq_along(p)] <- exp(p);
  return(result);
};

#' Nonparametric selectivity
#'
#' Calculates one selectivity value per size from log-scale parameters.
#'
#' @param z data: Numeric vector of sizes.
#' @param p param: Numeric vector of log-scale selectivity parameters, with one
#'   value for each element of `z`.
#'
#' @return A numeric vector of selectivity values with the same length as `z`.
#' @keywords rtmbFunction
#' @export
#'
#' @examples
#' sel_nonparametric(
#'   z = seq(20, 60, by = 10),
#'   p = log(c(0.1, 0.4, 0.8, 1, 0.9))
#' )
sel_nonparametric <- function(z, p) {
  "[<-" <- ADoverload("[<-");
  "c" <- ADoverload("c");
  "diag<-" <- ADoverload("diag<-");

  if (length(p) != length(z)) {
    stop("`p` and `z` must have the same length.", call. = FALSE);
  };

  return(exp(p));
};

#' Uniform selectivity
#'
#' Returns the parameter value for every size.
#'
#' @param z data: Numeric vector of sizes.
#' @param p param: Numeric vector containing the uniform selectivity value.
#'
#' @return A numeric vector of selectivity values with the same length as `z`.
#' @keywords rtmbFunction
#' @export
#'
#' @examples
#' sel_uniform(z = seq(20, 60, by = 10), p = 0.8)
sel_uniform <- function(z, p) {
  "[<-" <- ADoverload("[<-");
  "c" <- ADoverload("c");
  "diag<-" <- ADoverload("diag<-");

  if (length(p) != 1L) {
    stop("`p` must have length 1.", call. = FALSE);
  };

  result <- p[1] + 0 * z;
  return(result);
};

#' One-parameter logistic selectivity
#'
#' Calculates GMACS `LogisticCurveOne` selectivity from one slope parameter. The
#' selectivity at the final size is 0.999.
#'
#' @param z data: Numeric vector of sizes.
#' @param p param: Numeric vector containing the logistic slope.
#'
#' @return A numeric vector of selectivity values with the same length as `z`.
#' @keywords rtmbFunction
#' @export
#'
#' @examples
#' sel_logistic1(z = seq(20, 60, by = 10), p = 0.2)
sel_logistic1 <- function(z, p) {
  "[<-" <- ADoverload("[<-");
  "c" <- ADoverload("c");
  "diag<-" <- ADoverload("diag<-");

  if (length(p) != 1L) {
    stop("`p` must have length 1.", call. = FALSE);
  };

  result <- 1 / (1 + exp(p[1] * (-z + z[length(z)]) + log(1 / 0.999 - 1)));
  return(result);
};

#' Declining logistic selectivity
#'
#' Calculates declining logistic selectivity and replaces its first values
#' with exponentiated log-scale coefficients.
#'
#' @param z data: Numeric vector of sizes.
#' @param p param: Numeric vector containing the logistic mean, logistic slope,
#'   and zero or more log-scale coefficients for the first size classes.
#'
#' @return A numeric vector of selectivity values with the same length as `z`.
#' @keywords rtmbFunction
#' @export
#'
#' @examples
#' sel_declining_logistic(
#'   z = seq(20, 80, by = 10),
#'   p = c(60, 0.12, log(c(0.1, 0.25)))
#' )
sel_declining_logistic <- function(z, p) {
  "[<-" <- ADoverload("[<-");
  "c" <- ADoverload("c");
  "diag<-" <- ADoverload("diag<-");

  if (length(p) < 2L || length(p) > length(z) + 2L) {
    stop(
      "`p` must contain a mean, slope, and at most `length(z)` coefficients.",
      call. = FALSE
    );
  };

  result <- 1 / (1 + exp(p[2] * (z - p[1])));
  number_coefficients <- length(p) - 2L;
  if (number_coefficients > 0L) {
    indices <- seq_len(number_coefficients);
    result[indices] <- exp(p[indices + 2L]);
  };

  return(result);
};

#' Three-parameter double-normal selectivity
#'
#' @param z data: Numeric vector of sizes.
#' @param p param: Numeric vector containing ascending width, peak size, and
#'   descending width, in that order.
#'
#' @return A numeric vector of selectivity values with the same length as `z`.
#' @keywords rtmbFunction
#' @export
#'
#' @examples
#' sel_dbl_normal3(z = seq(20, 100, by = 10), p = c(12, 60, 18))
sel_dbl_normal3 <- function(z, p) {
  "[<-" <- ADoverload("[<-");
  "c" <- ADoverload("c");
  "diag<-" <- ADoverload("diag<-");

  if (length(p) != 3L) {
    stop("`p` must have length 3.", call. = FALSE);
  };

  ascending <- exp(-0.5 * ((z - p[2]) / p[1])^2);
  descending <- exp(-0.5 * ((z - p[2]) / p[3])^2);
  join <- 1 / (1 + exp(-5 * (z - p[2])));
  result <- ((1 - join) * ascending + join) *
    (join * descending + (1 - join));
  return(result);
};

#' Four-parameter double-normal selectivity
#'
#' @param z data: Numeric vector of sizes.
#' @param p param: Numeric vector containing ascending width, ascending peak
#'   size, descending width, and descending peak size, in that order.
#'
#' @return A numeric vector of selectivity values with the same length as `z`.
#' @keywords rtmbFunction
#' @export
#'
#' @examples
#' sel_dbl_normal4(z = seq(20, 100, by = 10), p = c(12, 50, 18, 75))
sel_dbl_normal4 <- function(z, p) {
  "[<-" <- ADoverload("[<-");
  "c" <- ADoverload("c");
  "diag<-" <- ADoverload("diag<-");

  if (length(p) != 4L) {
    stop("`p` must have length 4.", call. = FALSE);
  };

  result <- double_normal_plateau(z, p[1], p[2], p[3], p[4]);
  return(result);
};

#' Scaled four-parameter double-normal selectivity
#'
#' The descending peak is calculated by scaling the distance from the ascending
#' peak to the maximum possible size.
#'
#' @param z data: Numeric vector of sizes.
#' @param p param: Numeric vector containing ascending width, ascending peak
#'   size, descending width, scaled increment, and maximum possible size, in
#'   that order.
#'
#' @return A numeric vector of selectivity values with the same length as `z`.
#' @keywords rtmbFunction
#' @export
#'
#' @examples
#' sel_dbl_normal4a(z = seq(20, 100, by = 10), p = c(12, 50, 18, 0.5, 100))
sel_dbl_normal4a <- function(z, p) {
  "[<-" <- ADoverload("[<-");
  "c" <- ADoverload("c");
  "diag<-" <- ADoverload("diag<-");

  if (length(p) != 5L) {
    stop("`p` must have length 5.", call. = FALSE);
  };

  descending_peak <- p[2] + p[4] * (p[5] - p[2]);
  result <- double_normal_plateau(z, p[1], p[2], p[3], descending_peak);
  return(result);
};

double_normal_plateau <- function(
  z,
  ascending_width,
  ascending_peak,
  descending_width,
  descending_peak
) {
  "[<-" <- ADoverload("[<-");
  "c" <- ADoverload("c");
  "diag<-" <- ADoverload("diag<-");

  ascending <- exp(-0.5 * ((z - ascending_peak) / ascending_width)^2);
  descending <- exp(-0.5 * ((z - descending_peak) / descending_width)^2);
  ascending_join <- 1 / (1 + exp(5 * (z - ascending_peak)));
  descending_join <- 1 / (1 + exp(-5 * (z - descending_peak)));
  result <- (ascending_join * ascending + (1 - ascending_join)) *
    (descending_join * descending + (1 - descending_join));
  return(result);
};

#' Ascending-normal selectivity
#'
#' @param z data: Numeric vector of sizes.
#' @param p param: Numeric vector containing ascending-limb width and size at
#'   full selection, in that order.
#'
#' @return A numeric vector of selectivity values with the same length as `z`.
#' @keywords rtmbFunction
#' @export
#'
#' @examples
#' sel_asc_normal(z = seq(20, 100, by = 10), p = c(12, 60))
sel_asc_normal <- function(z, p) {
  "[<-" <- ADoverload("[<-");
  "c" <- ADoverload("c");
  "diag<-" <- ADoverload("diag<-");

  if (length(p) != 2L) {
    stop("`p` must have length 2.", call. = FALSE);
  };

  ascending <- exp(-0.5 * ((z - p[2]) / p[1])^2);
  join <- 1 / (1 + exp(-5 * (z - p[2])));
  result <- (1 - join) * ascending + join;
  return(result);
};

#' Cubic-spline selectivity
#'
#' Fits a cubic spline through logit-scale values using zero first derivatives
#' at both endpoints, evaluates it at each size, and applies the logistic
#' transformation.
#'
#' @param z data: Numeric vector of sizes.
#' @param p param: Numeric vector formed by concatenating knot locations and the
#'   same number of logit-scale values: `c(knots, values)`.
#'
#' @return A numeric vector of selectivity values with the same length as `z`.
#' @keywords rtmbFunction selectivity
#' @export
#'
#' @examples
#' sel_cubic_spline(
#'   z = seq(20, 100, by = 10),
#'   p = c(c(20, 50, 80, 100), stats::qlogis(c(0.1, 0.8, 1 - 1e-4, 0.6)))
#' )
sel_cubic_spline <- function(z, p) {
  "[<-" <- ADoverload("[<-");
  "c" <- ADoverload("c");
  "diag<-" <- ADoverload("diag<-");

  if (length(p) < 4L || length(p) %% 2L != 0L) {
    stop("`p` must contain equal-length knot and value vectors.", call. = FALSE);
  };

  number_knots <- length(p) / 2L;
  knots <- p[seq_len(number_knots)];
  values <- p[number_knots + seq_len(number_knots)];
  if (inherits(knots, "advector")) {
    knot_values <- RTMB:::getValues(knots);
  } else {
    knot_values <- knots;
  };
  if (is.unsorted(knot_values, strictly = TRUE)) {
    stop("Knot locations in `p` must be strictly increasing.", call. = FALSE);
  };

  second_derivatives <- clamped_spline_second_derivatives(knots, values);
  interval <- findInterval(z, knot_values, all.inside = TRUE);
  interval <- pmin(interval, number_knots - 1L);
  width <- knots[interval + 1L] - knots[interval];
  left_weight <- (knots[interval + 1L] - z) / width;
  right_weight <- (z - knots[interval]) / width;
  interpolated <- left_weight *
    values[interval] +
    right_weight * values[interval + 1L] +
    ((left_weight^3 - left_weight) *
      second_derivatives[interval] +
      (right_weight^3 - right_weight) *
        second_derivatives[interval + 1L]) *
      width^2 /
      6;
  result <- 1 / (1 + exp(-interpolated));
  return(result);
};

clamped_spline_second_derivatives <- function(x, y) {
  "[<-" <- ADoverload("[<-");
  "c" <- ADoverload("c");
  "diag<-" <- ADoverload("diag<-");

  number_knots <- length(x);
  widths <- diff(x);
  slopes <- diff(y) / widths;
  system_matrix <- matrix(0, nrow = number_knots, ncol = number_knots);
  right_hand_side <- numeric(number_knots);

  system_matrix[1, 1:2] <- c(2 * widths[1], widths[1]);
  right_hand_side[1] <- 6 * slopes[1];

  if (number_knots > 2L) {
    for (i in 2:(number_knots - 1L)) {
      system_matrix[i, (i - 1L):(i + 1L)] <- c(
        widths[i - 1L],
        2 * (widths[i - 1L] + widths[i]),
        widths[i]
      );
      right_hand_side[i] <- 6 * (slopes[i] - slopes[i - 1L]);
    };
  };

  system_matrix[number_knots, (number_knots - 1L):number_knots] <- c(
    widths[number_knots - 1L],
    2 * widths[number_knots - 1L]
  );
  right_hand_side[number_knots] <- -6 * slopes[number_knots - 1L];
  result <- solve(system_matrix, right_hand_side);
  return(result);
};
