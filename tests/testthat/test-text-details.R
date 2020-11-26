test_that("text_details() calculates info correctly", {
  # descent and space are independent of string
  gp1 <- gpar(fontfamily = "Helvetica", fontface = "plain", fontsize = 10)
  t1 <- text_details("abcd", gp = gp1)
  t2 <- text_details("gjqp", gp = gp1)
  expect_equal(t1$descent_pt, t2$descent_pt)
  expect_equal(t1$space_pt, t2$space_pt)

  # recalculating the same details gives same results (tests caching)
  t2 <- text_details("abcd", gp = gp1)
  expect_equal(t1$width_pt, t2$width_pt)
  expect_equal(t1$ascent_pt, t2$ascent_pt)
  expect_equal(t1$descent_pt, t2$descent_pt)
  expect_equal(t1$space_pt, t2$space_pt)

  # all parameters scale with font size
  gp2 <- gpar(fontfamily = "Helvetica", fontface = "plain", fontsize = 20)
  t2 <- text_details("abcd", gp = gp2)
  expect_equal(2 * t1$width_pt, t2$width_pt)
  expect_equal(2 * t1$ascent_pt, t2$ascent_pt)
  expect_equal(2 * t1$descent_pt, t2$descent_pt)
  expect_equal(2 * t1$space_pt, t2$space_pt)

  # parameters change with font
  gp2 <- gpar(fontfamily = "Times", fontface = "plain", fontsize = 10)
  t2 <- text_details("abcd", gp = gp2)
  expect_false(t1$width_pt == t2$width_pt)
  expect_false(t1$ascent_pt == t2$ascent_pt)
  expect_false(t1$descent_pt == t2$descent_pt)
  expect_false(t1$space_pt == t2$space_pt)

  # font details are identical to what we would get from an actual grob
  g <- textGrob("Qbcd", gp = gp1)
  t1 <- text_details("Qbcd", gp = gp1)
  expect_equal(t1$ascent_pt, convertHeight(grobHeight(g), "pt", valueOnly = TRUE))
  expect_equal(t1$width_pt, convertWidth(grobWidth(g), "pt", valueOnly = TRUE))
})
