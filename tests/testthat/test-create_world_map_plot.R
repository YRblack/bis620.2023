test_that("create world map plot works", {
  result <- create_world_map(studies,1,1,"nash",countries)
  expect_true(class(result) == "list")
})
