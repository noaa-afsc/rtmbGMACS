testthat::test_that("sel_logistic works with RTMB autodifferentiation", {
  library(RTMB);

  z <- seq(20, 100, by = 10);
  p <- c(50, 8);
  valsR <- sel_logistic(z, p);

  data <- list(z = z);
  params <- list(p = p);

  fcn1 <- function(data, params) {
    val <- sel_logistic(data$z, params$p);
    return(val);
  };

  encf <- function(f, data) {
    return(function(params) {
      return(f(data, list(p = params)));
    });
  };

  N <- sum(sapply(params, length));
  F <- RTMB::MakeTape(encf(fcn1, data), numeric(N));
  p <- as.numeric(unlist(params));
  valRTMB <- F(p);

  testthat::expect_equal(as.numeric(valRTMB), valsR);
});

testthat::test_that("sel_stacked_logistic works with RTMB autodifferentiation", {
  library(RTMB);

  z <- seq(20, 100, by = 10);
  p <- c(0.6, 40, 6, 70, 10);
  valsR <- sel_stacked_logistic(z, p);

  data <- list(z = z);
  params <- list(p = p);

  fcn1 <- function(data, params) {
    val <- sel_stacked_logistic(data$z, params$p);
    return(val);
  };

  encf <- function(f, data) {
    return(function(params) {
      return(f(data, list(p = params)));
    });
  };

  N <- sum(sapply(params, length));
  F <- RTMB::MakeTape(encf(fcn1, data), numeric(N));
  p <- as.numeric(unlist(params));
  valRTMB <- F(p);

  testthat::expect_equal(as.numeric(valRTMB), valsR);
});

testthat::test_that("sel_logistic95 works with RTMB autodifferentiation", {
  library(RTMB);

  z <- seq(20, 100, by = 10);
  p <- c(50, 70);
  valsR <- sel_logistic95(z, p);

  data <- list(z = z);
  params <- list(p = p);

  fcn1 <- function(data, params) {
    val <- sel_logistic95(data$z, params$p);
    return(val);
  };

  encf <- function(f, data) {
    return(function(params) {
      return(f(data, list(p = params)));
    });
  };

  N <- sum(sapply(params, length));
  F <- RTMB::MakeTape(encf(fcn1, data), numeric(N));
  p <- as.numeric(unlist(params));
  valRTMB <- F(p);

  testthat::expect_equal(as.numeric(valRTMB), valsR);
});

testthat::test_that("sel_coefficients works with RTMB autodifferentiation", {
  library(RTMB);

  z <- seq(20, 70, by = 10);
  p <- log(c(0.2, 0.5, 0.9));
  valsR <- sel_coefficients(z, p);

  data <- list(z = z);
  params <- list(p = p);

  fcn1 <- function(data, params) {
    val <- sel_coefficients(data$z, params$p);
    return(val);
  };

  encf <- function(f, data) {
    return(function(params) {
      return(f(data, list(p = params)));
    });
  };

  N <- sum(sapply(params, length));
  F <- RTMB::MakeTape(encf(fcn1, data), numeric(N));
  p <- as.numeric(unlist(params));
  valRTMB <- F(p);

  testthat::expect_equal(as.numeric(valRTMB), valsR);
});

testthat::test_that("sel_nonparametric works with RTMB autodifferentiation", {
  library(RTMB);

  z <- seq(20, 60, by = 10);
  p <- log(c(0.1, 0.4, 0.8, 1, 0.9));
  valsR <- sel_nonparametric(z, p);

  data <- list(z = z);
  params <- list(p = p);

  fcn1 <- function(data, params) {
    val <- sel_nonparametric(data$z, params$p);
    return(val);
  };

  encf <- function(f, data) {
    return(function(params) {
      return(f(data, list(p = params)));
    });
  };

  N <- sum(sapply(params, length));
  F <- RTMB::MakeTape(encf(fcn1, data), numeric(N));
  p <- as.numeric(unlist(params));
  valRTMB <- F(p);

  testthat::expect_equal(as.numeric(valRTMB), valsR);
});

testthat::test_that("sel_uniform works with RTMB autodifferentiation", {
  library(RTMB);

  z <- seq(20, 60, by = 10);
  p <- 0.8;
  valsR <- sel_uniform(z, p);

  data <- list(z = z);
  params <- list(p = p);

  fcn1 <- function(data, params) {
    val <- sel_uniform(data$z, params$p);
    return(val);
  };

  encf <- function(f, data) {
    return(function(params) {
      return(f(data, list(p = params)));
    });
  };

  N <- sum(sapply(params, length));
  F <- RTMB::MakeTape(encf(fcn1, data), numeric(N));
  p <- as.numeric(unlist(params));
  valRTMB <- F(p);

  testthat::expect_equal(as.numeric(valRTMB), valsR);
});

testthat::test_that("sel_logistic1 works with RTMB autodifferentiation", {
  library(RTMB);

  z <- seq(20, 60, by = 10);
  p <- 0.2;
  valsR <- sel_logistic1(z, p);

  data <- list(z = z);
  params <- list(p = p);

  fcn1 <- function(data, params) {
    val <- sel_logistic1(data$z, params$p);
    return(val);
  };

  encf <- function(f, data) {
    return(function(params) {
      return(f(data, list(p = params)));
    });
  };

  N <- sum(sapply(params, length));
  F <- RTMB::MakeTape(encf(fcn1, data), numeric(N));
  p <- as.numeric(unlist(params));
  valRTMB <- F(p);

  testthat::expect_equal(as.numeric(valRTMB), valsR);
});

testthat::test_that("sel_declining_logistic works with RTMB autodifferentiation", {
  library(RTMB);

  z <- seq(20, 80, by = 10);
  p <- c(60, 0.12, log(c(0.1, 0.25)));
  valsR <- sel_declining_logistic(z, p);

  data <- list(z = z);
  params <- list(p = p);

  fcn1 <- function(data, params) {
    val <- sel_declining_logistic(data$z, params$p);
    return(val);
  };

  encf <- function(f, data) {
    return(function(params) {
      return(f(data, list(p = params)));
    });
  };

  N <- sum(sapply(params, length));
  F <- RTMB::MakeTape(encf(fcn1, data), numeric(N));
  p <- as.numeric(unlist(params));
  valRTMB <- F(p);

  testthat::expect_equal(as.numeric(valRTMB), valsR);
});

testthat::test_that("sel_dbl_normal3 works with RTMB autodifferentiation", {
  library(RTMB);

  z <- seq(20, 100, by = 10);
  p <- c(12, 60, 18);
  valsR <- sel_dbl_normal3(z, p);

  data <- list(z = z);
  params <- list(p = p);

  fcn1 <- function(data, params) {
    val <- sel_dbl_normal3(data$z, params$p);
    return(val);
  };

  encf <- function(f, data) {
    return(function(params) {
      return(f(data, list(p = params)));
    });
  };

  N <- sum(sapply(params, length));
  F <- RTMB::MakeTape(encf(fcn1, data), numeric(N));
  p <- as.numeric(unlist(params));
  valRTMB <- F(p);

  testthat::expect_equal(as.numeric(valRTMB), valsR);
});

testthat::test_that("sel_dbl_normal4 works with RTMB autodifferentiation", {
  library(RTMB);

  z <- seq(20, 100, by = 10);
  p <- c(12, 50, 18, 75);
  valsR <- sel_dbl_normal4(z, p);

  data <- list(z = z);
  params <- list(p = p);

  fcn1 <- function(data, params) {
    val <- sel_dbl_normal4(data$z, params$p);
    return(val);
  };

  encf <- function(f, data) {
    return(function(params) {
      return(f(data, list(p = params)));
    });
  };

  N <- sum(sapply(params, length));
  F <- RTMB::MakeTape(encf(fcn1, data), numeric(N));
  p <- as.numeric(unlist(params));
  valRTMB <- F(p);

  testthat::expect_equal(as.numeric(valRTMB), valsR);
});

testthat::test_that("sel_dbl_normal4a works with RTMB autodifferentiation", {
  library(RTMB);

  z <- seq(20, 100, by = 10);
  p <- c(12, 50, 18, 0.5, 100);
  valsR <- sel_dbl_normal4a(z, p);

  data <- list(z = z);
  params <- list(p = p);

  fcn1 <- function(data, params) {
    val <- sel_dbl_normal4a(data$z, params$p);
    return(val);
  };

  encf <- function(f, data) {
    return(function(params) {
      return(f(data, list(p = params)));
    });
  };

  N <- sum(sapply(params, length));
  F <- RTMB::MakeTape(encf(fcn1, data), numeric(N));
  p <- as.numeric(unlist(params));
  valRTMB <- F(p);

  testthat::expect_equal(as.numeric(valRTMB), valsR);
});

testthat::test_that("sel_asc_normal works with RTMB autodifferentiation", {
  library(RTMB);

  z <- seq(20, 100, by = 10);
  p <- c(12, 60);
  valsR <- sel_asc_normal(z, p);

  data <- list(z = z);
  params <- list(p = p);

  fcn1 <- function(data, params) {
    val <- sel_asc_normal(data$z, params$p);
    return(val);
  };

  encf <- function(f, data) {
    return(function(params) {
      return(f(data, list(p = params)));
    });
  };

  N <- sum(sapply(params, length));
  F <- RTMB::MakeTape(encf(fcn1, data), numeric(N));
  p <- as.numeric(unlist(params));
  valRTMB <- F(p);

  testthat::expect_equal(as.numeric(valRTMB), valsR);
});

testthat::test_that("sel_cubic_spline works with RTMB autodifferentiation", {
  library(RTMB);

  z <- seq(20, 100, by = 10);
  p <- c(c(20, 50, 80, 100), stats::qlogis(c(0.1, 0.8, 1 - 1e-4, 0.6)));
  valsR <- sel_cubic_spline(z, p);

  data <- list(z = z);
  params <- list(p = p);

  fcn1 <- function(data, params) {
    val <- sel_cubic_spline(data$z, params$p);
    return(val);
  };

  encf <- function(f, data) {
    return(function(params) {
      return(f(data, list(p = params)));
    });
  };

  N <- sum(sapply(params, length));
  # Knot locations determine spline intervals and must be available while taping.
  F <- RTMB::MakeTape(encf(fcn1, data), as.numeric(unlist(params)));
  p <- as.numeric(unlist(params));
  valRTMB <- F(p);

  testthat::expect_equal(as.numeric(valRTMB), valsR);
});

testthat::test_that("selectivity functions reproduce defining values", {
  library(RTMB);
  z <- c(40, 50, 70);

  testthat::expect_equal(sel_logistic(z, c(50, 10))[2], 0.5);
  testthat::expect_equal(sel_logistic95(z, c(50, 70))[2:3], c(0.5, 0.95));
  testthat::expect_equal(
    sel_stacked_logistic(z, c(0.25, 50, 10, 50, 10)),
    sel_logistic(z, c(50, 10))
  );
  testthat::expect_equal(sel_uniform(z, 0.8), rep(0.8, length(z)));
  testthat::expect_equal(sel_logistic1(z, 0.2)[length(z)], 0.999);
  testthat::expect_equal(
    sel_nonparametric(z, log(c(0.2, 0.5, 1))),
    c(0.2, 0.5, 1)
  );
  testthat::expect_equal(
    sel_coefficients(z, log(c(0.2, 0.5))),
    c(0.2, 0.5, 0.5)
  );
});

testthat::test_that("double-normal and ascending curves peak as defined", {
  library(RTMB);
  testthat::expect_equal(sel_dbl_normal3(50, c(10, 50, 15)), 1);
  testthat::expect_equal(
    sel_dbl_normal4(c(50, 70), c(10, 50, 15, 70)),
    c(1, 1)
  );
  testthat::expect_equal(
    sel_dbl_normal4a(c(50, 75), c(10, 50, 15, 0.5, 100)),
    c(1, 1)
  );
  testthat::expect_equal(sel_asc_normal(50, c(10, 50)), 1);
});

testthat::test_that("spline selectivity interpolates knot values", {
  library(RTMB);
  knots <- c(20, 50, 80);
  selectivity <- c(0.1, 0.8, 0.4);

  testthat::expect_equal(
    sel_cubic_spline(knots, c(knots, stats::qlogis(selectivity))),
    selectivity
  );
});
