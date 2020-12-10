context("richtext-grob")

test_that("grobheight and grobwidth work", {
  # width is the same for textGrob and richtext_grob
  g <- textGrob("test")
  g2 <- richtext_grob("test")
  w <- convertWidth(grobWidth(g), "pt", valueOnly = TRUE)
  w2 <- convertWidth(grobWidth(g2), "pt", valueOnly = TRUE)
  expect_equal(w, w2)

  # height is slightly larger for richtext_grob, b/c descent is considered
  h <- convertHeight(grobHeight(g), "pt", valueOnly = TRUE)
  h2 <- convertHeight(grobHeight(g2), "pt", valueOnly = TRUE)
  expect_lt(h, h2)

  # width and height are flipped after rotating 90 degrees
  g <- textGrob("test", rot = 90)
  g2 <- richtext_grob("test", rot = 90)
  w <- convertWidth(grobWidth(g), "pt", valueOnly = TRUE)
  w2 <- convertWidth(grobWidth(g2), "pt", valueOnly = TRUE)
  expect_lt(w, w2)

  # height is slightly larger for richtext_grob, b/c descent is considered
  h <- convertHeight(grobHeight(g), "pt", valueOnly = TRUE)
  h2 <- convertHeight(grobHeight(g2), "pt", valueOnly = TRUE)
  expect_equal(h, h2)

  # position of multiple labels is taken into account
  g <- textGrob("test", x = unit(0, "pt"), y = unit(80, "pt"))
  g2 <- textGrob(c("test", "test"), x = unit(c(0, 50), "pt"), y = unit(c(80, 40), "pt"))
  w <- convertWidth(grobWidth(g), "pt", valueOnly = TRUE)
  w2 <- convertWidth(grobWidth(g2), "pt", valueOnly = TRUE)
  expect_equal(w + 50, w2)
  h <- convertHeight(grobHeight(g), "pt", valueOnly = TRUE)
  h2 <- convertHeight(grobHeight(g2), "pt", valueOnly = TRUE)
  expect_equal(h + 40, h2)

  # multiple labels, w rotation
  g <- textGrob("test", x = unit(0, "pt"), y = unit(80, "pt"), rot = 45)
  g2 <- textGrob(c("test", "test"), x = unit(c(0, 50), "pt"), y = unit(c(80, 40), "pt"), rot = 45)
  w <- convertWidth(grobWidth(g), "pt", valueOnly = TRUE)
  w2 <- convertWidth(grobWidth(g2), "pt", valueOnly = TRUE)
  expect_equal(w + 50, w2)
  h <- convertHeight(grobHeight(g), "pt", valueOnly = TRUE)
  h2 <- convertHeight(grobHeight(g2), "pt", valueOnly = TRUE)
  expect_equal(h + 40, h2)

  # grob height and width are identical with and without debug info
  text <- c(
    "Some text **in bold.**<br>(centered)", "Linebreaks<br>Linebreaks<br>Linebreaks",
    "*x*<sup>2</sup> + 5*x* + *C*<sub>i</sub><br>*a* = 5"
  )
  x <- c(.4, .3, .8)
  y <- c(.8, .5, .3)
  rot <- c(0, -45, 45)
  halign <- c(0.5, 0, 1)
  valign <- c(0.5, 1, 0)

  g1 <- richtext_grob(
    text, x, y, halign = halign, valign = valign, rot = rot,
    padding = unit(c(6, 6, 4, 6), "pt"),
    r = unit(c(0, 4, 8), "pt"),
    debug = FALSE
  )

  g2 <- richtext_grob(
    text, x, y, halign = halign, valign = valign, rot = rot,
    padding = unit(c(6, 6, 4, 6), "pt"),
    r = unit(c(0, 4, 8), "pt"),
    debug = TRUE
  )

  w1 <- convertWidth(grobWidth(g1), "pt", valueOnly = TRUE)
  w2 <- convertWidth(grobWidth(g2), "pt", valueOnly = TRUE)
  expect_equal(w1, w2)

  h1 <- convertHeight(grobHeight(g1), "pt", valueOnly = TRUE)
  h2 <- convertHeight(grobHeight(g2), "pt", valueOnly = TRUE)
  expect_equal(h1, h2)
})

test_that("misc. tests", {
  # empty strings work
  expect_silent(richtext_grob(""))
  expect_silent(richtext_grob(" "))

  # NAs work
  expect_silent(richtext_grob(c(" ", "abc", NA)))
})

test_that("visual tests", {
  draw_labels <- function() {
    function() {
      text <- c(
        "**Various text boxes in different stylings**",
        "Some text **in bold.**<br>(centered)", "Linebreaks<br>Linebreaks<br>Linebreaks",
        "*x*<sup>2</sup> + 5*x* + *C*<sub>i</sub><br>*a* = 5"
      )

      x <- c(0, .4, .3, .8)
      y <- c(1, .8, .5, .3)
      rot <- c(0, 0, -45, 45)
      gp = gpar(col = c("black", "red"))
      box_gp = gpar(col = "black", fill = c(NA, "cornsilk", NA, "lightblue1"), lty = c(0, 1, 1, 1))
      hjust <- c(0, 0.5, 0, 1)
      vjust <- c(1, 0.5, 1, 0)

      g <- richtext_grob(
        text, x, y, hjust = hjust, vjust = vjust, rot = rot,
        padding = unit(c(6, 6, 4, 6), "pt"),
        r = unit(c(0, 0, 4, 8), "pt"),
        gp = gp, box_gp = box_gp
      )
      grid.draw(g)
      grid.points(x, y, default.units = "npc", pch = 19, size = unit(5, "pt"))
      invisible()
    }
  }

  expect_doppelganger("Various text boxes", draw_labels())

  draw_labels_debug <- function() {
    function() {
      text <- c(
        "Some text **in bold.**<br>(centered)", "Linebreaks<br>Linebreaks<br>Linebreaks",
        "*x*<sup>2</sup> + 5*x* + *C*<sub>i</sub><br>*a* = 5"
      )

      x <- c(.4, .3, .8)
      y <- c(.8, .5, .3)
      rot <- c(0, -45, 45)
      gp = gpar(col = c("black", "red", "black"))
      box_gp = gpar(col = "black", fill = c("cornsilk", NA, "lightblue1"), lty = c(1, 1, 1))
      hjust <- c(0.5, 0, 1)
      vjust <- c(0.5, 1, 0)

      g <- richtext_grob(
        text, x, y, hjust = hjust, vjust = vjust, rot = rot,
        padding = unit(c(6, 6, 4, 6), "pt"),
        r = unit(c(0, 4, 8), "pt"),
        gp = gp, box_gp = box_gp,
        debug = TRUE
      )
      grid.draw(g)
      grid.points(x, y, default.units = "npc", pch = 19, size = unit(5, "pt"))
      invisible()
    }
  }

  expect_doppelganger("Various text boxes w/ debug", draw_labels_debug())

  draw_aligned_heights <- function() {
    function() {
      text <- c(
        "Some text **in bold.**<br>(centered)", "Linebreaks<br>Linebreaks<br>Linebreaks",
        "*x*<sup>2</sup> + 5*x* + *C*<sub>i</sub><br>*a* = 5"
      )

      x <- c(.4, .3, .8)
      y <- c(.8, .5, .3)
      rot <- c(0, -45, 45)
      gp = gpar()
      box_gp = gpar(col = "black", fill = c("cornsilk", NA, "lightblue1"))
      hjust <- c(0.5, 0, 1)
      vjust <- c(0.5, 1, 0)

      g <- richtext_grob(
        text, x, y, halign = 0.5, valign = 0.5,
        hjust = hjust, vjust = vjust, rot = rot,
        align_heights = TRUE,
        padding = unit(c(6, 6, 4, 6), "pt"),
        r = unit(c(0, 4, 8), "pt"),
        gp = gp, box_gp = box_gp
      )
      grid.draw(g)
      grid.text("Box heights aligned, content centered", gp = gpar(fontface = "bold"), 0.02, 1, hjust = 0, vjust = 1.2)
      grid.points(x, y, default.units = "npc", pch = 19, size = unit(5, "pt"))
      invisible()
    }
  }

  expect_doppelganger("Aligned heights", draw_aligned_heights())

  draw_aligned_widths <- function() {
    function() {
      text <- c(
        "Some text **in bold.**<br>(centered)", "Linebreaks<br>Linebreaks<br>Linebreaks",
        "*x*<sup>2</sup> + 5*x* + *C*<sub>i</sub><br>*a* = 5"
      )

      x <- c(.4, .3, .8)
      y <- c(.8, .5, .3)
      rot <- c(0, -45, 45)
      gp = gpar()
      box_gp = gpar(col = "black", fill = c("cornsilk", NA, "lightblue1"))
      hjust <- c(0.5, 0, 1)
      vjust <- c(0.5, 1, 0)

      g <- richtext_grob(
        text, x, y, halign = 0.5, valign = 0.5,
        hjust = hjust, vjust = vjust, rot = rot,
        align_widths = TRUE,
        padding = unit(c(6, 6, 4, 6), "pt"),
        r = unit(c(0, 4, 8), "pt"),
        gp = gp, box_gp = box_gp
      )
      grid.draw(g)
      grid.text("Box widths aligned, content centered", gp = gpar(fontface = "bold"), 0.02, 1, hjust = 0, vjust = 1.2)
      grid.points(x, y, default.units = "npc", pch = 19, size = unit(5, "pt"))
      invisible()
    }
  }

  expect_doppelganger("Aligned widths", draw_aligned_widths())

})
