test_that("output is numeric", {
  out <- get_sparsity(sc_gold)
  expect_numeric(x = out, len = 1)
})