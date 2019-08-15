context("textbox grob")

test_that("visual tests", {
  draw_box <- function() {
    function() {
      g <- textbox_grob(
        "**The quick brown fox jumps over the lazy dog.**<br><br>
  The quick brown fox jumps over the lazy dog.
  The **quick <span style='color:brown;'>brown fox</span>** jumps over the lazy dog.
  The quick brown fox jumps over the lazy dog.",
        y = unit(0.9, "npc"), vjust = 1,
        gp = gpar(fontsize = 15),
        box_gp = gpar(col = "black", fill = "lightcyan1"),
        r = unit(5, "pt"),
        padding = unit(c(10, 10, 10, 10), "pt"),
        margin = unit(c(0, 10, 0, 10), "pt")
      )
      grid.draw(g)
      grid.text("  Box spanning entire viewport, with margins", 0, 1, hjust = 0, vjust = 1.2)
      invisible()
    }
  }

  vdiffr::expect_doppelganger("Box spanning entire viewport, with margins", draw_box())
})
