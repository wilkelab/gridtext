context("grid constructors")

test_that("unit_pt", {
  expect_equal(
    unit_pt(10),
    grid::unit(10, "pt")
  )

  expect_identical(
    unit_pt(1:10),
    grid::unit(1:10, "pt")
  )
})

test_that("gpar_empty", {
  expect_identical(
    gpar_empty(),
    grid::gpar()
  )
})

test_that("text_grob", {
  # basic functionality, gp is set to gpar() if not provided
  expect_identical(
    text_grob("test", 10, 20, name = "abc"),
    textGrob(
      "test",
      x = unit(10, "pt"), y = unit(20, "pt"),
      hjust = 0, vjust = 0,
      gp = gpar(),
      name = "abc"
    )
  )

  # basic functionality, x and y are set to 0 if not provided
  expect_identical(
    text_grob("test", name = "abc"),
    textGrob(
      "test",
      x = unit(0, "pt"), y = unit(0, "pt"),
      hjust = 0, vjust = 0,
      gp = gpar(),
      name = "abc"
    )
  )

  # gp is set as requested
  gp <- gpar(col = "blue", fill = "red")
  expect_identical(
    text_grob("test", 10, 20, gp = gp, name = "abc"),
    textGrob(
      "test",
      x = unit(10, "pt"), y = unit(20, "pt"),
      hjust = 0, vjust = 0,
      gp = gp,
      name = "abc"
    )
  )

  # if no name is provided, different names are assigned
  g1 <- text_grob("test")
  g2 <- text_grob("test")
  expect_false(identical(g1$name, g2$name))

  # function is not vectorized
  expect_error(
    text_grob(c("test", "test"), 10, 20),
    "not vectorized"
  )

  expect_error(
    text_grob("test", 1:5, 20),
    "not vectorized"
  )

  expect_error(
    text_grob("test", 10, 1:5),
    "not vectorized"
  )

  # arguments of length 0 are also disallowed
  expect_error(
    text_grob("test", numeric(0), 5),
    "not vectorized"
  )
})

test_that("rect_grob", {
  # basic functionality, gp is set to gpar() if not provided
  expect_identical(
    rect_grob(10, 20, 100, 140, name = "abc"),
    rectGrob(
      x = unit(10, "pt"), y = unit(20, "pt"),
      width = unit(100, "pt"), height = unit(140, "pt"),
      hjust = 0, vjust = 0,
      gp = gpar(),
      name = "abc"
    )
  )

  # gp is set as requested
  gp <- gpar(col = "blue", fill = "red")
  expect_identical(
    rect_grob(10, 20, 100, 140, gp = gp, name = "abc"),
    rectGrob(
      x = unit(10, "pt"), y = unit(20, "pt"),
      width = unit(100, "pt"), height = unit(140, "pt"),
      hjust = 0, vjust = 0,
      gp = gp,
      name = "abc"
    )
  )

  # if no name is provided, different names are assigned
  g1 <- rect_grob()
  g2 <- rect_grob()
  expect_false(identical(g1$name, g2$name))

  # function is not vectorized
  expect_error(
    rect_grob(c(10, 20), 20, 100, 140),
    "not vectorized"
  )

  expect_error(
    rect_grob(10, numeric(0), 100, 140),
    "not vectorized"
  )
})

test_that("set_grob_coords", {
  g <- list(x = 0, y = 0)

  # setting coords as numbers
  expect_identical(
    set_grob_coords(g, x = 20, y = 40),
    list(x = 20, y = 40)
  )

  # setting coords as units
  expect_identical(
    set_grob_coords(g, x = unit_pt(20), y = unit_pt(40)),
    list(x = unit_pt(20), y = unit_pt(40))
  )
})
