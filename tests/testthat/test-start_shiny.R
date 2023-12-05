test_that("start_shiny works", {
  result <- start_shiny()
  expect_true(class(result) == "shiny.appobj")
})
