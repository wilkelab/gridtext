# takes a graphical parameters object gp and returns a list of
# length n of appropriately recycled elements from gp
recycle_gpar <- function(gp = NULL, n = 1) {
  make_gpar <- function(n, ...) {
    structure(
      list(...),
      class = "gpar"
    )
  }

  args <- c(list(make_gpar, n = 1:n), gp, list(SIMPLIFY = FALSE))
  do.call(mapply, args)
}

# converts a unit vector into a list of individual unit objects
unit_to_list <- function(u)
{
  lapply(seq_along(u), function(i) u[i])
}
