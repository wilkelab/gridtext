context("rich text grob")

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

      grid.newpage()
      g <- rich_text_grob(
        text, x, y, hjust = hjust, vjust = vjust, rot = rot,
        padding = unit(c(6, 6, 4, 6), "pt"),
        r = unit(c(0, 0, 4, 8), "pt"),
        gp = gp, box_gp = box_gp)
      grid.draw(g)
      grid.points(x, y, default.units = "npc", pch = 19, size = unit(5, "pt"))
      invisible()
    }
  }

  vdiffr::expect_doppelganger("Various text boxes", draw_labels())
})
