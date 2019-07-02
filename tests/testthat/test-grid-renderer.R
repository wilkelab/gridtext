context("grid renderer")

test_that("basic functioning", {
  r <- grid_renderer()
  g <- grid_renderer_collect_grobs(r)

  # without any grobs rendered, we get an empty list of class gList
  expect_equal(length(g), 0)
  expect_true(inherits(g, "gList"))

  # grobs get added in order
  grid_renderer_text(r, "abcd", 100, 100)
  grid_renderer_rect(r, 100, 100, 200, 200)
  g <- grid_renderer_collect_grobs(r)
  expect_equal(length(g), 2)
  expect_true(inherits(g, "gList"))
  expect_true(inherits(g[[1]], "text"))
  expect_true(inherits(g[[2]], "rect"))

  # internal state gets reset after calling collect_grobs()
  g <- grid_renderer_collect_grobs(r)
  expect_equal(length(g), 0)
  expect_true(inherits(g, "gList"))
})

test_that("smart rendering of rects", {
  r <- grid_renderer()
  # add normal rect
  grid_renderer_rect(r, 100, 100, 200, 200)
  # add rect with rounded corners
  grid_renderer_rect(r, 100, 100, 200, 200, r = 5)
  # add rect that is invisible, gets removed automatically
  grid_renderer_rect(r, 100, 100, 200, 200, color = NA_character_, fill = NA_character_)
  g <- grid_renderer_collect_grobs(r)
  expect_equal(length(g), 2)
  expect_true(inherits(g, "gList"))
  expect_true(inherits(g[[1]], "rect"))
  expect_true(inherits(g[[2]], "roundrect"))
})

test_that("visual tests", {
  draw_grob <- function(g) {
    grid.newpage()
    grid.draw(g)
    invisible()
  }

  r <- grid_renderer()
  grid_renderer_text(r, "blue", 10, 400, color = "blue")
  grid_renderer_text(r, "red bold", 20, 380, color = "red", fontface = "bold")
  grid_renderer_text(r, "roman", 30, 360, fontfamily = "Times")
  g <- grid_renderer_collect_grobs(r)
  vdiffr::expect_doppelganger("Text in different stylings", draw_grob(g))

  r <- grid_renderer()
  grid_renderer_rect(r, 100, 400, 200, 20, color = "blue")
  grid_renderer_rect(r, 100, 200, 300, 30, fill = "cornsilk", r = 8)
  grid_renderer_text(r, "text 1, square box blue", 100, 400, fontsize = 20)
  grid_renderer_text(r, "text 2, rounded box filled", 100, 200, fontsize = 20)
  g <- grid_renderer_collect_grobs(r)
  vdiffr::expect_doppelganger("Mixing text and boxes", draw_grob(g))
})
