test_that("create_word_cloud_plot works", {
  data(studies)
  expect_snapshot(create_word_cloud_plot(studies,"FED","Terminated", "nash"))
})
