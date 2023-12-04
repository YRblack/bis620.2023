test_that("create endpoint histogram works", {
  plot <- create_endpoint_histogram( studies = studies,
                                      sponsor_type = "FED",
                                      status_type = "Terminated",
                                      endpoints = endpoints,
                                      kw =  "nash"
  )

  # Check if the function returns a ggplot object
  expect_true(ggplot2::is.ggplot(plot))
})
