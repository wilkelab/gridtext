test_that("get_file works", {
  # skip test on cran because the url could be broken in the future
  skip_on_cran()
  # get_file returns raw data if it's an url and a character path if it's
  # a local path. That's why we test it with the function read_image that calls it
  expect_identical(
    read_image("https://upload.wikimedia.org/wikipedia/commons/thumb/2/27/20140427Iris_versicolor1.jpg/320px-20140427Iris_versicolor1.jpg"),
    read_image("../figs/test_image.jpg")
  )
})
