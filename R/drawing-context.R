update_gpar <- function(gp, ...) {
  gp_new <- list(...)
  names_new <- names(gp_new)
  names_old <- names(gp)
  if ("fontface" %in% names_new || "fontface" %in% names_old) { # have to unset font when we're setting fontface
    names_new <- c(names_new, "font")
  }
  gp[intersect(names_old, names_new)] <- NULL
  do.call(gpar, c(gp, gp_new))
}
