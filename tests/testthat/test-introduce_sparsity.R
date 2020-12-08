test_that("output is a dataframe", {
  out <- introduce_sparsity(0.01)
  expect_data_frame(out)
})
