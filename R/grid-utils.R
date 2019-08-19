# various functions to help with grid functionality

# from: https://github.com/thomasp85/ggforce/blob/cba71550606d18b4f4b245cb097aee5eeeec52a8/R/textbox.R#L290-L295
# code taken with permission (https://twitter.com/thomasp85/status/1160989815657119747)
with_unit <- function(x, default) {
  if (!is.null(x) && !is.unit(x)) {
    x <- unit(x, default)
  }
  x
}

# calculate the current width of a grob, in pt
# @flip are width and height flipped?
# @convert_null should null values be converted or kept as they are?
current_width_pt <- function(grob = NULL, width = NULL, flip = FALSE, convert_null = TRUE) {
  if (is.null(width)) {
    if (isTRUE(convert_null)) {
      width <- unit(1, 'npc')
    } else {
      return(NULL)
    }
  }

  if (isTRUE(flip)) {
    convert <- convertHeight
  } else {
    convert <- convertWidth
  }

  if (is.null(grob$vp)) {
    width_pt <- convert(width, 'pt', TRUE)
  } else {
    # If the grob has its own viewport then we need to push it and
    # afterwards pop it. For this to work in the general case
    # (stacked viewports, etc), we need to keep track of the depth
    # of the current viewport stack and pop appropriately.
    n <- current.vpPath()$n %||% 0
    pushViewport(grob$vp)
    width_pt <- convert(width, 'pt', TRUE)
    popViewport(current.vpPath()$n - n)
  }

  width_pt
}

# calculate the current height of a grob, in pt
current_height_pt <- function(grob = NULL, height = NULL, flip = FALSE, convert_null = TRUE) {
  if (is.null(height)) {
    if (isTRUE(convert_null)) {
      height <- unit(1, 'npc')
    } else {
      return(NULL)
    }
  }

  if (isTRUE(flip)) {
    convert <- convertWidth
  } else {
    convert <- convertHeight
  }

  if (is.null(grob$vp)) {
    height_pt <- convert(height, 'pt', TRUE)
  } else {
    # If the grob has its own viewport then we need to push it and
    # afterwards pop it. For this to work in the general case
    # (stacked viewports, etc), we need to keep track of the depth
    # of the current viewport stack and pop appropriately.
    n <- current.vpPath()$n %||% 0
    pushViewport(grob$vp)
    height_pt <- convert(height, 'pt', TRUE)
    popViewport(current.vpPath()$n - n)
  }

  height_pt
}
