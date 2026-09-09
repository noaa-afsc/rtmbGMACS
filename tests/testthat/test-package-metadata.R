test_that("package metadata is available", {
  desc <- utils::packageDescription("rtmbGMACS")

  expect_equal(desc$Package, "rtmbGMACS")
})
