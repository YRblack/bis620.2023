test_that("create interventions pie chart works", {
  plot <- create_interventions_pie_chart( studies = studies,
                                          interventions = interventions,
                                          sponsor_type = "FED",
                                          status_type = "Terminated",
                                          brief_title_kw = "nash"
  )

  # Check if the function returns a ggplot object
  expect_true(ggplot2::is.ggplot(plot))
})
