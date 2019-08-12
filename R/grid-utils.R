# various functions to help with grid functionality

# from: https://github.com/thomasp85/ggforce/blob/cba71550606d18b4f4b245cb097aee5eeeec52a8/R/textbox.R#L290-L295
with_unit <- function(x, default) {
  if (!is.null(x) && !is.unit(x)) {
    x <- unit(x, default)
  }
  x
}

# inspired by: https://github.com/thomasp85/ggforce/blob/cba71550606d18b4f4b245cb097aee5eeeec52a8/R/textbox.R#L193-L207
# calculate the current width of a grob, in pt
current_width_pt <- function(grob = NULL, width = NULL) {
  if (is.null(width)) {
    if (is.null(grob) || is.null(grob$vp)) {
      convertWidth(unit(1, 'npc'), 'pt', TRUE)
    } else {
      if (is.null(grob$vp$layout.pos.col)) {
        convertWidth(grob$vp$width, 'pt', TRUE)
      } else {
        cur_vp <- current.viewport()
        span <- do.call(seq, as.list(range(grob$vp$layout.pos.col)))
        convertWidth(unit(1, 'npc'), 'pt', TRUE) - sum(convertWidth(cur_vp$layout$widths[-span], 'pt', TRUE))
      }
    }
  } else {
    convertWidth(width, 'pt', TRUE)
  }
}
