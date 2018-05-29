context("grob descent")

test_that("grob_descent_pt", {

  # descent is independent of string
  t1 <- textGrob("abcd")
  t2 <- textGrob("gjqp")
  expect_equal(grob_descent_pt(t1), grob_descent_pt(t2))

  # descent scales with font size
  t2 <- editGrob(t1, gp = gpar(fontfamily = "Helvetica", fontsize = 20))
  t1 <- editGrob(t1, gp = gpar(fontfamily = "Helvetica", fontsize = 10))
  expect_equal(2 * grob_descent_pt(t1), grob_descent_pt(t2))

  # descent changes with font
  t2 <- editGrob(t1, gp = gpar(fontfamily = "Times", fontsize = 10))
  expect_false(grob_descent_pt(t1) == grob_descent_pt(t2))
})

test_that("grob_descent", {

  # descent() is equal to descent_pt()
  t1 <- textGrob("abcd")
  expect_equal(convertHeight(grob_descent(t1), "pt", valueOnly = TRUE), grob_descent_pt(t1))
})

test_that("font_details_pt", {

  # font details are identical to what we would get from an actual grob
  gp <- gpar(fontfamily = "Times", fontsize = 10)
  t1 <- textGrob("abcd", gp = gp)
  details <- font_details_pt(gp)
  expect_equal(details$descent_pt, grob_descent_pt(t1))
  expect_equal(details$height_pt, grob_height_pt(t1))
})
