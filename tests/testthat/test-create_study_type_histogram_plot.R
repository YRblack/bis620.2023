test_that("create study type histogram plot works", {
  plot <- create_study_type_histogram_plot( studies = studies,
                                            sponsor_type = "FED",
                                            status_type = "Terminated",
                                            brief_title_kw = "nash"
  )

  # Check if the function returns a ggplot object
  expect_true(ggplot2::is.ggplot(plot))
})
