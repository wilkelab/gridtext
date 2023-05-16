#' Sets the default options for the gridtext  package.
#'
#' Options:
#'   gridtext.stop_if_tag_is_unimplemented with default behaviour: Stop if an unimplemented tag is incountered.
#'
#' @param overwrite Overwrite all options with gridtext default settings
#' @return NULL
#' @examples
#' options(gridtext.stop_if_tag_is_unimplemented = FALSE)
#' @export
set_gridtext_options <- function(overwrite=FALSE) {
  if(is.null(getOption("gridtext.stop_if_tag_is_unimplemented")) | overwrite) {
    options(gridtext.stop_if_tag_is_unimplemented = TRUE)
  }
}

.onLoad <- function(libname, pkgname) {
  set_gridtext_options()
}
