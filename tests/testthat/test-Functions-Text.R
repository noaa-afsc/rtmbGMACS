testthat::test_that("whitespace groups become commas and newlines remain", {
  input <- "a  b\tc\nd e";
  expected <- "a,b,c\nd,e";

  testthat::expect_identical(whitespace_groups_to_commas(input), expected);
});
