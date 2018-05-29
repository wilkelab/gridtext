update_gpar <- function(gp, gp_new) {
  names_new <- names(gp_new)
  names_old <- names(gp)
  gp[c(intersect(names_old, names_new), "font")] <- NULL
  gp_new["font"] <- NULL
  do.call(gpar, c(gp, gp_new))
}

update_context <- function(drawing_context, ...) {
  dc_new <- list(...)
  names_new <- names(dc_new)
  names_old <- names(drawing_context)
  drawing_context[intersect(names_old, names_new)] <- NULL
  c(drawing_context, dc_new)
}

setup_context <- function(fontsize = 12, fontfamily = "", fontface = "plain", color = "black",
                                  cex = 1, lineheight = 1.2) {
  gp <- gpar(
    fontsize = fontsize, fontfamily = fontfamily, fontface = fontface,
    col = color, cex = cex, lineheight = lineheight
  )
  gp <- update_gpar(get.gpar(), gp)

  set_context_gp(list(yoff_pt = 0), gp)
}

set_context_gp <- function(drawing_context, gp = NULL) {
  gp <- update_gpar(drawing_context$gp, gp)
  font_info <- font_details_pt(gp)
  linespacing_pt <- gp$lineheight * gp$fontsize
  em_pt <- gp$fontsize

  update_context(
    drawing_context,
    gp = gp,
    height_pt = font_info$height_pt,
    descent_pt = font_info$descent_pt,
    linespacing_pt = linespacing_pt,
    em_pt = em_pt
  )
}

set_context_fontface <- function(drawing_context, fontface = "plain", overwrite = FALSE) {
  fontface_old <- drawing_context$gp$fontface

  # combine bold and italic if needed
  if (!isTRUE(overwrite)) {
    if (fontface == "italic" && fontface_old == "bold") {
      fontface <- "bold.italic"
    } else if (fontface == "bold" && fontface_old == "italic") {
      fontface <- "bold.italic"
    }
  }

  set_context_gp(drawing_context, gpar(fontface = fontface))
}


set_style <- function(drawing_context, style = NULL) {
  if (is.null(style)) return(drawing_context)

  css <- parse_css(style)

  if (!is.null(css$`font-size`)) {
    font_size = as.numeric(css$`font-size`)
  } else {
    font_size = NULL
  }

  drawing_context <- set_context_gp(
    drawing_context,
    gpar(col = css$color, fontfamily = css$`font-family`, fontsize = font_size)
  )
}

