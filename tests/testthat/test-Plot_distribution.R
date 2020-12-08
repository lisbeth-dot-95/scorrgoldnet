test_that("Printing ggplot object actually works",{
  p <- Plot_distribution(sc_gold)
  expect_error(print(p), NA)
})