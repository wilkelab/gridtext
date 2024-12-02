context("textbox-grob")

test_that("misc. tests", {
  # empty strings work
  expect_silent(textbox_grob(""))
  expect_silent(textbox_grob(" "))

  # NAs work
  expect_silent(textbox_grob(NA))
})

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
      grid.points(0.5, 0.9, default.units = "npc", pch = 19, size = unit(5, "pt"))
      invisible()
    }
  }

  expect_doppelganger("Box spanning entire viewport, with margins", draw_box())

  draw_align_upright <- function() {
    function() {
      g1 <- textbox_grob(
        "The quick brown fox jumps over the lazy dog.",
        hjust = 0, vjust = 1, valign = 1, halign = 0,
        width = unit(1.5, "inch"), height = unit(1.5, "inch"),
        box_gp = gpar(col = "black", fill = "cornsilk"),
        padding = unit(c(2, 2, 2, 2), "pt"),
        margin = unit(c(5, 5, 5, 5), "pt")
      )
      g2 <- textbox_grob(
        "The quick brown fox jumps over the lazy dog.",
        hjust = 1, vjust = 1, valign = 0.5, halign = 0.5,
        width = unit(1.5, "inch"), height = unit(1.5, "inch"),
        box_gp = gpar(col = "black", fill = "cornsilk"),
        padding = unit(c(2, 2, 2, 2), "pt"),
        margin = unit(c(5, 5, 5, 5), "pt")
      )
      g3 <- textbox_grob(
        "The quick brown fox jumps over the lazy dog.",
        hjust = 0, vjust = 0, valign = 1, halign = 1,
        width = unit(1.5, "inch"), height = unit(1.5, "inch"),
        box_gp = gpar(col = "black", fill = "cornsilk"),
        padding = unit(c(2, 2, 2, 2), "pt"),
        margin = unit(c(5, 5, 5, 5), "pt")
      )
      g4 <- textbox_grob(
        "The quick brown fox jumps over the lazy dog.",
        hjust = 1, vjust = 0, valign = 0, halign = 0,
        width = unit(1.5, "inch"), height = unit(1.5, "inch"),
        box_gp = gpar(col = "black", fill = "cornsilk"),
        padding = unit(c(2, 2, 2, 2), "pt"),
        margin = unit(c(5, 5, 5, 5), "pt")
      )
      grid.draw(g1)
      grid.draw(g2)
      grid.draw(g3)
      grid.draw(g4)
      invisible()
    }
  }

  expect_doppelganger("Multiple boxes, internal alignment", draw_align_upright())


  draw_align_left_rotated <- function() {
    function() {
      g1 <- textbox_grob(
        "The quick brown fox jumps over the lazy dog.",
        hjust = 0, vjust = 1, valign = 1, halign = 0,
        width = unit(1.5, "inch"), height = unit(1.5, "inch"),
        orientation = "left-rotated",
        box_gp = gpar(col = "black", fill = "cornsilk"),
        padding = unit(c(2, 2, 2, 2), "pt"),
        margin = unit(c(5, 5, 5, 5), "pt")
      )
      g2 <- textbox_grob(
        "The quick brown fox jumps over the lazy dog.",
        hjust = 1, vjust = 1, valign = 0.5, halign = 0.5,
        width = unit(1.5, "inch"), height = unit(1.5, "inch"),
        orientation = "left-rotated",
        box_gp = gpar(col = "black", fill = "cornsilk"),
        padding = unit(c(2, 2, 2, 2), "pt"),
        margin = unit(c(5, 5, 5, 5), "pt")
      )
      g3 <- textbox_grob(
        "The quick brown fox jumps over the lazy dog.",
        hjust = 0, vjust = 0, valign = 1, halign = 1,
        width = unit(1.5, "inch"), height = unit(1.5, "inch"),
        orientation = "left-rotated",
        box_gp = gpar(col = "black", fill = "cornsilk"),
        padding = unit(c(2, 2, 2, 2), "pt"),
        margin = unit(c(5, 5, 5, 5), "pt")
      )
      g4 <- textbox_grob(
        "The quick brown fox jumps over the lazy dog.",
        hjust = 1, vjust = 0, valign = 0, halign = 0,
        width = unit(1.5, "inch"), height = unit(1.5, "inch"),
        orientation = "left-rotated",
        box_gp = gpar(col = "black", fill = "cornsilk"),
        padding = unit(c(2, 2, 2, 2), "pt"),
        margin = unit(c(5, 5, 5, 5), "pt")
      )
      grid.draw(g1)
      grid.draw(g2)
      grid.draw(g3)
      grid.draw(g4)
      invisible()
    }
  }

  expect_doppelganger("Multiple boxes left rotated, internal alignment", draw_align_left_rotated())


  draw_align_right_rotated <- function() {
    function() {
      g1 <- textbox_grob(
        "The quick brown fox jumps over the lazy dog.",
        hjust = 0, vjust = 1, valign = 1, halign = 0,
        width = unit(1.5, "inch"), height = unit(1.5, "inch"),
        orientation = "right-rotated",
        box_gp = gpar(col = "black", fill = "cornsilk"),
        padding = unit(c(2, 2, 2, 2), "pt"),
        margin = unit(c(5, 5, 5, 5), "pt")
      )
      g2 <- textbox_grob(
        "The quick brown fox jumps over the lazy dog.",
        hjust = 1, vjust = 1, valign = 0.5, halign = 0.5,
        width = unit(1.5, "inch"), height = unit(1.5, "inch"),
        orientation = "right-rotated",
        box_gp = gpar(col = "black", fill = "cornsilk"),
        padding = unit(c(2, 2, 2, 2), "pt"),
        margin = unit(c(5, 5, 5, 5), "pt")
      )
      g3 <- textbox_grob(
        "The quick brown fox jumps over the lazy dog.",
        hjust = 0, vjust = 0, valign = 1, halign = 1,
        width = unit(1.5, "inch"), height = unit(1.5, "inch"),
        orientation = "right-rotated",
        box_gp = gpar(col = "black", fill = "cornsilk"),
        padding = unit(c(2, 2, 2, 2), "pt"),
        margin = unit(c(5, 5, 5, 5), "pt")
      )
      g4 <- textbox_grob(
        "The quick brown fox jumps over the lazy dog.",
        hjust = 1, vjust = 0, valign = 0, halign = 0,
        width = unit(1.5, "inch"), height = unit(1.5, "inch"),
        orientation = "right-rotated",
        box_gp = gpar(col = "black", fill = "cornsilk"),
        padding = unit(c(2, 2, 2, 2), "pt"),
        margin = unit(c(5, 5, 5, 5), "pt")
      )
      grid.draw(g1)
      grid.draw(g2)
      grid.draw(g3)
      grid.draw(g4)
      invisible()
    }
  }

  expect_doppelganger("Multiple boxes right rotated, internal alignment", draw_align_right_rotated())


  draw_align_inverted <- function() {
    function() {
      g1 <- textbox_grob(
        "The quick brown fox jumps over the lazy dog.",
        hjust = 0, vjust = 1, valign = 1, halign = 0,
        width = unit(1.5, "inch"), height = unit(1.5, "inch"),
        orientation = "inverted",
        box_gp = gpar(col = "black", fill = "cornsilk"),
        padding = unit(c(2, 2, 2, 2), "pt"),
        margin = unit(c(5, 5, 5, 5), "pt")
      )
      g2 <- textbox_grob(
        "The quick brown fox jumps over the lazy dog.",
        hjust = 1, vjust = 1, valign = 0.5, halign = 0.5,
        width = unit(1.5, "inch"), height = unit(1.5, "inch"),
        orientation = "inverted",
        box_gp = gpar(col = "black", fill = "cornsilk"),
        padding = unit(c(2, 2, 2, 2), "pt"),
        margin = unit(c(5, 5, 5, 5), "pt")
      )
      g3 <- textbox_grob(
        "The quick brown fox jumps over the lazy dog.",
        hjust = 0, vjust = 0, valign = 1, halign = 1,
        width = unit(1.5, "inch"), height = unit(1.5, "inch"),
        orientation = "inverted",
        box_gp = gpar(col = "black", fill = "cornsilk"),
        padding = unit(c(2, 2, 2, 2), "pt"),
        margin = unit(c(5, 5, 5, 5), "pt")
      )
      g4 <- textbox_grob(
        "The quick brown fox jumps over the lazy dog.",
        hjust = 1, vjust = 0, valign = 0, halign = 0,
        width = unit(1.5, "inch"), height = unit(1.5, "inch"),
        orientation = "inverted",
        box_gp = gpar(col = "black", fill = "cornsilk"),
        padding = unit(c(2, 2, 2, 2), "pt"),
        margin = unit(c(5, 5, 5, 5), "pt")
      )
      grid.draw(g1)
      grid.draw(g2)
      grid.draw(g3)
      grid.draw(g4)
      invisible()
    }
  }
  expect_doppelganger("Multiple boxes inverted, internal alignment", draw_align_inverted())

  draw_rotated_fixedpoint <- function() {
    function() {
      g1 <- textbox_grob(
        "The quick brown fox jumps over the lazy dog.",
        hjust = 0, vjust = 1,
        x = 0.4, y = 0.6,
        width = unit(1.5, "inch"), height = unit(1.5, "inch"),
        orientation = "upright",
        box_gp = gpar(col = "black", fill = "cornsilk"),
        padding = unit(c(2, 2, 2, 2), "pt"),
        margin = unit(c(5, 0, 5, 0), "pt")
      )
      g2 <- textbox_grob(
        "The quick brown fox jumps over the lazy dog.",
        hjust = 0, vjust = 1,
        x = 0.4, y = 0.6,
        width = unit(1.5, "inch"), height = unit(1.5, "inch"),
        orientation = "left-rotated",
        box_gp = gpar(col = "black", fill = "cornsilk"),
        padding = unit(c(2, 2, 2, 2), "pt"),
        margin = unit(c(5, 0, 5, 0), "pt")
      )
      g3 <- textbox_grob(
        "The quick brown fox jumps over the lazy dog.",
        hjust = 0, vjust = 1,
        x = 0.4, y = 0.6,
        width = unit(1.5, "inch"), height = unit(1.5, "inch"),
        orientation = "right-rotated",
        box_gp = gpar(col = "black", fill = "cornsilk"),
        padding = unit(c(2, 2, 2, 2), "pt"),
        margin = unit(c(5, 0, 5, 0), "pt")
      )
      g4 <- textbox_grob(
        "The quick brown fox jumps over the lazy dog.",
        hjust = 0, vjust = 1,
        x = 0.4, y = 0.6,
        width = unit(1.5, "inch"), height = unit(1.5, "inch"),
        orientation = "inverted",
        box_gp = gpar(col = "black", fill = "cornsilk"),
        padding = unit(c(2, 2, 2, 2), "pt"),
        margin = unit(c(5, 0, 5, 0), "pt")
      )
      grid.draw(g1)
      grid.draw(g2)
      grid.draw(g3)
      grid.draw(g4)
      grid.points(0.4, 0.6, default.units = "npc", pch = 19, size = unit(5, "pt"))
      invisible()
    }
  }
  expect_doppelganger("Rotation around fixed point", draw_rotated_fixedpoint())

})

describe("text breaking behavior", {
  get_labels <- function(x) {
    g <- textbox_grob(x)
    labels <- sapply(grid.force(g)$children, function(x) x$label)
    return(labels)
  }

  it("does not break no-space text",
    expect_contains(
      get_labels(" ThisIsALongWordThatShouldNotBreak "),
      c("ThisIsALongWordThatShouldNotBreak"))
  )

  it("does break spaced text",
    expect_contains(
      get_labels( "This Is Text That Should Break" ),
      c("This", "Is", "Text", "That", "Should", "Break"))
  )

  it("does not break at non-breaking space but still at regular space",
    expect_contains(
      get_labels("  This&nbsp;Is&nbsp;Text That&nbsp;Should\n\nNot&nbsp;Break  "),
      c("This\u00A0Is\u00A0Text",
        "That\u00A0Should",
        "Not\u00A0Break"))
  )

  it("preserves non-breaking space at start and end",
    expect_contains(
      get_labels("   &nbsp;Beginning Should Not Trim Nor End&nbsp;   "),
      c("\u00A0Beginning",
        "Should", "Not", "Trim", "Nor",
        "End\u00A0"))
  )
})
