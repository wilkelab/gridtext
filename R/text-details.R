#' Calculate text details for a given text label
#'
#' Calculate text details for a given text label
#' @param label Character vector containing the label. Can handle only one label at a time.
#' @param gp Grid graphical parameters defining the font (`fontfamily`, `fontface`, and
#'   `fontface` should be defined).
#' @examples
#' text_details("Hello world!", grid::gpar(fontfamily = "", fontface = "plain", fontsize = 12))
#' text_details("Hello world!", grid::gpar(fontfamily = "", fontface = "plain", fontsize = 24))
#' text_details(
#'   "Hello world\nwith newline",
#'   grid::gpar(fontfamily = "", fontface = "plain", fontsize = 12)
#' )
#' @noRd
text_details <- function(label, gp = gpar()) {
  fontfamily <- gp$fontfamily %||% grid::get.gpar("fontfamily")$fontfamily
  fontface <- gp$fontface %||% grid::get.gpar("fontface")$fontface
  fontsize <- gp$fontsize %||% grid::get.gpar("fontsize")$fontsize

  devname <- names(grDevices::dev.cur())
  fontkey <- paste0(devname, fontfamily, fontface, fontsize)
  if (devname == "null device") {
    cache <- FALSE   # don't cache if no device open
  } else {
    cache <- TRUE
  }

  if (length(fontkey) != 1 || length(label) != 1) {
    stop("Function `text_details()` is not vectorized.", call. = FALSE)
  }

  # ascent and width depend on label and font
  l1 <- text_info(label, fontkey, fontfamily, fontface, fontsize, cache)
  # descent and space width depend only on font
  l2 <- font_info(fontkey, fontfamily, fontface, fontsize, cache)

  # concatenate, result is a list with four members, width_pt, ascent_pt, descent_pt, space_pt
  c(l1, l2)
}

font_info_cache <- new.env(parent = emptyenv())
font_info <- function(fontkey, fontfamily, fontface, fontsize, cache) {
  info <- font_info_cache[[fontkey]]

  if (is.null(info)) {
    descent_pt <- convertHeight(grobDescent(textGrob(
      label = "gjpqyQ",
      gp = gpar(
        fontsize = fontsize,
        fontfamily = fontfamily,
        fontface = fontface,
        cex = 1
      )
    )), "pt", valueOnly = TRUE)

    space_pt <- convertWidth(grobWidth(textGrob(
      label = " ",
      gp = gpar(
        fontsize = fontsize,
        fontfamily = fontfamily,
        fontface = fontface,
        cex = 1
      )
    )), "pt", valueOnly = TRUE)

    info <- list(descent_pt = descent_pt, space_pt = space_pt)

    if (cache) {
      font_info_cache[[fontkey]] <- info
    }
  }
  info
}

text_info_cache <- new.env(parent = emptyenv())
text_info <- function(label, fontkey, fontfamily, fontface, fontsize, cache) {
  key <- paste0(label, fontkey)
  info <- text_info_cache[[key]]

  if (is.null(info)) {
    ascent_pt <- convertHeight(grobHeight(textGrob(
      label = label,
      gp = gpar(
        fontsize = fontsize,
        fontfamily = fontfamily,
        fontface = fontface,
        cex = 1
      )
    )), "pt", valueOnly = TRUE)

    width_pt <- convertWidth(grobWidth(textGrob(
      label = label,
      gp = gpar(
        fontsize = fontsize,
        fontfamily = fontfamily,
        fontface = fontface,
        cex = 1
      )
    )), "pt", valueOnly = TRUE)

    info <- list(width_pt = width_pt, ascent_pt = ascent_pt)

    if (cache) {
      text_info_cache[[key]] <- info
    }
  }
  info
}
