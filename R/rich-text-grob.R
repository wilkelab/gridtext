#' Rich-text grob
#'
#' @param text Character vector containing markdown/html string to draw.
#' @param x,y Unit objects specifying the location of the reference point.
#' @param hjust,vjust Numerical values specifying the location of the grob
#'   relative to the reference point.
#' @param rot Angle of rotation for text, in degrees.
#' @param default.units Units of `x` and `y` if these are provided only as
#'   numerical values.
#' @param name Name of the grob.
#' @param gp Other graphical parameters for drawing.
#' @param vp Viewport.
#' @param use_markdown Should the `text` input be treated as markdown?
#' @examples
#' library(grid)
#'
#' text <- c(
#'   "Some text **in bold.**", "Linebreaks<br>Linebreaks<br>Linebreaks",
#'   "*x*<sup>2</sup> + 5*x* + *C*<sub>i</sub>",
#'   "Mixing **it** all *up,<br> for good measure.*"
#' )
#'
#' x <- c(.2, .1, .7, .9)
#' y <- c(.8, .4, .2, .6)
#' rot <- c(0, 0, 45, -45)
#' gp = gpar(col = c("black", "red"), fontfamily = c("Palatino", "Courier", "Times", "Helvetica"))
#' hjust <- c(0.5, 0, 0, 1)
#' vjust <- c(0.5, 1, 0, 0.5)
#'
#' grid.newpage()
#' g <- rich_text_grob(text, x, y, hjust = hjust, vjust = vjust, rot = rot, gp = gp)
#' grid.draw(g)
#' grid.points(x, y, default.units = "npc")
#' @export
rich_text_grob <- function(text, x = unit(0.5, "npc"), y = unit(0.5, "npc"),
                           hjust = 0.5, vjust = 0.5, rot = 0, default.units = "npc",
                           name = NULL, gp = gpar(), vp = NULL, use_markdown = TRUE) {
  # make sure x and y are units
  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)

  # make sure all input has same length
  n <- unique(length(text), length(x), length(y))
  if (length(n) > 1) {
    stop("Arguments `text`, `x`, and `y` must have the same length.", call. = FALSE)
  }
  hjust <- rep_len(hjust, n)
  vjust <- rep_len(vjust, n)
  rot <- rep_len(rot, n)
  use_markdown <- rep_len(use_markdown, n)
  gp_list <- recycle_gpar(gp, n)

  grobs <- mapply(
    make_rich_text_grob,
    text,
    x,
    y,
    hjust,
    vjust,
    rot,
    use_markdown,
    gp_list,
    SIMPLIFY = FALSE
  )

  children <- do.call(gList, grobs)

  gTree(
    gp = gp,
    vp = vp,
    name = name,
    children = children,
    cl = "rich_text_grob"
  )
}

make_rich_text_grob <- function(text, x, y, hjust, vjust, rot, use_markdown, gp) {
  if (use_markdown) {
    text <- markdown::markdownToHTML(text = text, options = c("use_xhtml", "fragment_only"))
  }
  doctree <- read_html(text)

  drawing_context <- setup_context(gp = gp)
  boxlist <- process_tags(xml2::as_list(doctree)$html$body, drawing_context)
  vbox <- bl_make_vbox(boxlist, width_pt = 0, hjust = hjust, vjust = vjust, width_policy = "native")
  bl_calc_layout(vbox, 0, 0)
  grobs <- bl_render(vbox, 0, 0)

  gTree(
    children = grobs,
    vp = viewport(x = x, y = y, just = c(0, 0), angle = rot)
  )
}
