#' Calculate descent and other text properties, either from grob or from graphical
#' parameters
#' @param grob The grob for which we want information
#' @export
grob_descent <- function(grob) {
  unit(grob_descent_pt(grob), "pt")
}

#' @rdname grob_descent
#' @export
grob_descent_pt <- function(grob) {
  if (inherits(grob, "text")) {
    font_details_pt(grob$gp)$descent
  } else 0
}

#' Calculates generic font height and descent from given graphical parameters
#'
#' @param gp Graphical parameters
#' @export
font_details_pt <- function(gp = gpar()) {
  lookup_font_details(gp)
}

# environment to cache font details so we don't have to recalculate over and over
font_env <- new.env(parent = emptyenv())
font_env$font_details <- list()

lookup_font_details <- function(gp) {
  fontfamily <- gp$fontfamily %||% ""
  fontface <- gp$fontface %||% "plain"
  fontsize <- gp$fontsize %||% 12
  key <- paste0(fontfamily, fontface, fontsize)

  details <- font_env$font_details[[key]]

  if (is.null(details)) {
    details <- calc_font_details(gp)

    font_env$font_details <- c(
      font_env$font_details,
      setNames(list(details), key)
    )
  }
  details
}

calc_font_details <- function(gp) {
  pushViewport(viewport(gp = gp)) # change viewport to get correct font settings
  grob <- textGrob(label = "gjpqyQ")
  height <- convertHeight(heightDetails(grob), "pt", valueOnly = TRUE)
  descent <- convertHeight(descentDetails(grob), "pt", valueOnly = TRUE)
  popViewport()
  list(height_pt = height, descent_pt = descent)
}
