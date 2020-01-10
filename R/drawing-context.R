# create drawing context with defined state
# halign defines horizontal text alignment (0 = left aligned, 0.5 = centered, 1 = right aligned)
setup_context <- function(fontsize = 12, fontfamily = "", fontface = "plain", color = "black",
                          lineheight = 1.2, halign = 0, word_wrap = TRUE, gp = NULL) {
  if (is.null(gp)) {
    gp <- gpar(
      fontsize = fontsize, fontfamily = fontfamily, fontface = fontface,
      col = color, cex = 1, lineheight = lineheight
    )
  }
  gp <- update_gpar(get.gpar(), gp)

  set_context_gp(list(yoff_pt = 0, halign = halign, word_wrap = word_wrap), gp)
}

# update a given drawing context with the values provided via ...
update_context <- function(drawing_context, ...) {
  dc_new <- list(...)
  names_new <- names(dc_new)
  names_old <- names(drawing_context)
  drawing_context[intersect(names_old, names_new)] <- NULL
  c(drawing_context, dc_new)
}

set_style <- function(drawing_context, style = NULL) {
  if (is.null(style)) return(drawing_context)

  css <- parse_css(style)

  if (!is.null(css$`font-size`)) {
    font_size = convert_css_unit_pt(css$`font-size`)
  } else {
    font_size = NULL
  }

  drawing_context <- set_context_gp(
    drawing_context,
    gpar(col = css$color, fontfamily = css$`font-family`, fontsize = font_size)
  )
}


# helper functions --------------------------------------------------------

# update a gpar object with new values
update_gpar <- function(gp, gp_new) {
  names_new <- names(gp_new)
  names_old <- names(gp)
  gp[c(intersect(names_old, names_new), "font")] <- NULL
  gp_new["font"] <- NULL
  do.call(gpar, c(gp, gp_new))
}

# update the gpar object of a drawing context
set_context_gp <- function(drawing_context, gp = NULL) {
  gp <- update_gpar(drawing_context$gp, gp)
  font_info <- text_details("", gp)
  linespacing_pt <- gp$lineheight * gp$fontsize
  em_pt <- gp$fontsize

  update_context(
    drawing_context,
    gp = gp,
    ascent_pt = font_info$ascent_pt,
    descent_pt = font_info$descent_pt,
    linespacing_pt = linespacing_pt,
    em_pt = em_pt
  )
}

# update the fontface of a drawing context
set_context_fontface <- function(drawing_context, fontface = "plain", overwrite = FALSE) {
  fontface_old <- drawing_context$gp$fontface

  # combine bold and italic if needed
  if (!isTRUE(overwrite)) {
    if (isTRUE(fontface == "italic") && isTRUE(fontface_old == "bold")) {
      fontface <- "bold.italic"
    } else if (isTRUE(fontface == "bold") && isTRUE(fontface_old == "italic")) {
      fontface <- "bold.italic"
    }
  }

  set_context_gp(drawing_context, gpar(fontface = fontface))
}

