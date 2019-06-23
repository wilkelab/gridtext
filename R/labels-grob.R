#' Like the margin function in ggplot2
#'
#' @param t,r,b,l top, right, bottom, left margins
#' @param unit unit in which margins are measured
#' @export
mar <- function(t = 0, r = 0, b = 0, l = 0, unit = "pt") {
  structure(unit(c(t, r, b, l), unit), class = c("margin", "unit"))
}

#' Create grob representing one or more labels
#'
#' The function `labels_grob()` creates one or more labels. All label data is provided
#' in the form of a data frame/tibble holding aesthetic values as columns and label
#' information as rows. The padding and margin information needs to be provided in a
#' list column.
#'
#' @param label_data Tibble holding the label data. At a minimum, needs a
#'   `label` column holding the text labels to be drawn
#' @param width,height Width and height of the box collection grob. If either is set
#'   to `NULL` (the default), it is calculated from the box grobs.
#' @param align_frame_widths,align_frame_heights Bools indicating whether frames
#'   should have equal widths/heights or not.
#' @param debug Bool indicating whether debugging info should be drawn.
#' @examples
#' library(grid)
#' library(tibble)
#'
#' label_data <- tibble(
#'   label = c("Descenders: pgqjy", "This is a label\nwith two lines", "Hello!"),
#'   x = unit(.4, "npc"),
#'   y = unit(c(.9, .5, .3), "npc"),
#'   hjust = 0,
#'   vjust = 0.5,
#'   hjust_int = 1, # internal hjust, vjust, defines how text label
#'   vjust_int = 1, # is positioned inside the box
#'   color = "blue",
#'   fill = "azure1",
#'   fontsize = 10,
#'   fontfamily = "Comic Sans MS",
#'   angle = c(0, 45, -45),
#'   padding = list(mar(5, 5, 3, 5)),
#'   margin = list(mar(5, 5, 5, 5))
#' )
#'
#' grid.newpage()
#' g <- labels_grob(label_data, align_frame_heights = TRUE)
#' grid.draw(g)
#'
#'
#' label_data <- tibble(
#'   label = c("This", "is", "a", "test", "x-axis"),
#'   x = unit(c(.1, .3, .5, .7, .9), "npc"),
#'   y = unit(0.5, "npc"),
#'   hjust = 0.5,
#'   vjust = 1,
#'   hjust_int = 0.5,
#'   vjust_int = 1,
#'   angle = 0,
#'   fontsize = 10, fontfamily = "Comic Sans MS",
#'   padding = list(mar(5, 5, 3, 5)),
#'   margin = list(mar(5, 5, 5, 5))
#' )
#' grid.newpage()
#' g <- labels_grob(label_data, align_frame_widths = TRUE, debug = TRUE)
#' grid.draw(g)
#'
#'
#' label_data <- tibble(label = "abcqgy", fontsize = 50, fontfamily = "Comic Sans MS")
#' grid.newpage()
#' grid.draw(labels_grob(label_data))
#' @export
labels_grob <- function(label_data, width = NULL, height = NULL,
                        align_frame_widths = FALSE, align_frame_heights = FALSE,
                        debug = FALSE) {
  txt_grobs <- pmap(label_data, make_label_grob)
  width_pt <- vapply(txt_grobs, grob_width_pt, numeric(1))
  height_pt <- vapply(txt_grobs, grob_height_pt, numeric(1))
  descent_pt <- vapply(txt_grobs, grob_descent_pt, numeric(1))

  # do we want equal frame widths/heights
  if (isTRUE(align_frame_widths)) {
    width_pt <- max(width_pt)
  }
  if (isTRUE(align_frame_heights)) {
    height_pt <- max(height_pt)
  }

  txt_data <- tibble(
    grob = txt_grobs,
    width_pt = width_pt,
    height_pt = height_pt,
    descent_pt = descent_pt
  )

  txt_data <- cbind(txt_data, label_data)

  # Here we would like to write something like thefollowing:
  #   grobs <- pmap(txt_data, make_box_grob, debug = debug)
  # However, this doesn't work because pmap turns units into
  # numerics, so we need a for loop.
  grobs <- list()
  for (i in 1:nrow(txt_data)) {
    row <- txt_data[i, ]
    grobs[[i]] <- do.call(make_box_grob, c(row, list(debug = debug)))
  }

  # enclose all grobs in a box collection
  do.call(box_collection_grob, c(grobs, list(width = width, height = height, debug = debug)))
}

#' create individual labels for labels_grob
make_label_grob <- function(label, hjust_int = 0.5, vjust_int = 0.5,
                            fontfamily = "", fontface = "plain", fontsize = 12,
                            lineheight = 1.1, color = "black", ...) {
  gp <- gpar(
    col = color,
    fontfamily = fontfamily,
    fontface = fontface,
    fontsize = fontsize,
    lineheight = lineheight
  )
  textGrob(label, x = hjust_int, y = vjust_int, hjust = hjust_int, vjust = vjust_int, gp = gp)
}

#' make frame around text grobs
make_box_grob <- function(grob, width_pt, height_pt, descent_pt,
                          x = unit(0.5, "npc"), y = unit(0.5, "npc"),
                          hjust = 0.5, vjust = 0.5, padding = list(mar(0, 0, 0, 0)),
                          margin = list(mar(0, 0, 0, 0)), angle = 0, fill = NA, color = NA,
                          frame_color = NULL,
                          ..., debug = FALSE) {
  # data from list columns arrive enclosed in lists, and we need to undo that first
  grob <- grob[[1]]
  margin <- margin[[1]]
  padding <- padding[[1]]

  # add descent to padding
  padding[3] <- padding[3] + unit(descent_pt, "pt")

  color <- frame_color %||% color

  width <- margin[2] + margin[4] + padding[2] + padding[4] + unit(width_pt, "pt")
  height <- margin[1] + margin[3] + padding[1] + padding[3] + unit(height_pt, "pt")

  box_grob(
    grob,
    width = width, height = height,
    x = x, y = y,
    hjust = hjust, vjust = vjust,
    padding = padding, margin = margin, angle = angle,
    fill = fill, color = color, debug = debug
  )
}

