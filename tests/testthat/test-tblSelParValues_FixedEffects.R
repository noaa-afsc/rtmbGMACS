testthat::test_that("insertVarNames initializes a named vector", {
  newV <- c("year", "sex");

  testthat::expect_identical(
    insertVarNames(newV, NULL),
    c(year = "year", sex = "sex")
  );
});

testthat::test_that("insertVarNames inserts missing values in newV order", {
  oldV <- c(year = "year", sex = "sex", maturity = "maturity");
  newV <- c("sex", "fleet", "maturity", "area");

  testthat::expect_identical(
    insertVarNames(newV, oldV),
    c(
      year = "year",
      sex = "sex",
      fleet = "fleet",
      maturity = "maturity",
      area = "area"
    )
  );
});

testthat::test_that("insertVarNames returns oldV when it contains newV", {
  oldV <- c(year = "year", sex = "sex");

  testthat::expect_identical(insertVarNames("sex", oldV), oldV);
});
