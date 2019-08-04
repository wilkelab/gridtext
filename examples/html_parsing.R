process_text <- function(node, drawing_context) {
  tokens <- stringr::str_split(stringr::str_squish(node), "[[:space:]]+")[[1]]

  # make interior boxes
  boxes <- lapply(tokens,
    function(token) {
      list(
        gridtext:::bl_make_text_box(token, drawing_context$gp, drawing_context$yoff_pt),
        gridtext:::bl_make_regular_space_glue(drawing_context$gp)
      )
    }
  )

  # if node starts with space, add glue at beginning
  if (isTRUE(grepl("^[[:space:]]", node))) {
    boxes <- c(list(gridtext:::bl_make_regular_space_glue(drawing_context$gp)), boxes)
  }

  boxes <- unlist(boxes, recursive = FALSE)

  # if node doesn't end with space, remove glue at end
  if (!isTRUE(grepl("[[:space:]]$", node))) {
    boxes[[length(boxes)]] <- NULL
  }
  boxes
}

process_tag_br <- function(node, drawing_context) {
  list(
    gridtext:::bl_make_text_box("", drawing_context$gp),
    gridtext:::bl_make_forced_break_penalty()
  )
}

process_tag_b <- function(node, drawing_context) {
  # temporarily disabled
  #attr <- attributes(node)
  #drawing_context <- set_style(drawing_context, attr$style)

  process_tags(node, gridtext:::set_context_fontface(drawing_context, "bold"))
}

process_tag_i <- function(node, drawing_context) {
  # temporarily disabled
  #attr <- attributes(node)
  #drawing_context <- set_style(drawing_context, attr$style)

  process_tags(node, gridtext:::set_context_fontface(drawing_context, "italic"))
}


process_tag_p <- function(node, drawing_context) {
  # temporarily disabled
  #attr <- attributes(node)
  #drawing_context <- set_style(drawing_context, attr$style)

  boxes <- unlist(
    list(
      process_tags(node, drawing_context),
      process_tag_br(NULL, drawing_context)
    ),
    recursive = FALSE
  )
  gridtext:::bl_make_par_box(boxes, drawing_context$linespacing_pt)
}

process_tag_sub <- function(node, drawing_context) {
  # modify fontsize before processing style, to allow for manual overriding
  drawing_context <- gridtext:::set_context_gp(drawing_context, gpar(fontsize = 0.8*drawing_context$gp$fontsize))
  # temporarily disabled
  #attr <- attributes(node)
  #drawing_context <- set_style(drawing_context, attr$style)

  # move drawing half a character below baseline
  drawing_context$yoff_pt <- drawing_context$yoff_pt - drawing_context$height_pt / 2
  process_tags(node, drawing_context)
}

dispatch_tag <- function(node, tag, drawing_context) {
  if (is.null(tag) || tag == "") {
    process_text(node, drawing_context)
  } else {
    switch(
      tag,
      "b"    = process_tag_b(node, drawing_context),
      "strong" = process_tag_b(node, drawing_context),
      "br"   = process_tag_br(node, drawing_context),
      "i"    = process_tag_i(node, drawing_context),
      "em"   = process_tag_i(node, drawing_context),
      "p"    = process_tag_p(node, drawing_context),
#      "span" = process_tag_span(node, drawing_context),
#      "sup"  = process_tag_sup(node, drawing_context),
      "sub"  = process_tag_sub(node, drawing_context),
      stop("unknown tag: ", tag)
    )
  }
}

process_tags <- function(node, drawing_context) {
  tags <- names(node)
  boxes <- list()
  for (i in seq_along(node)) {
    boxes[[i]] <- dispatch_tag(node[[i]], tags[i], drawing_context)
  }
  unlist(boxes, recursive = FALSE)
}



draw_rich_text <- function(contents, x_pt = 50, y_pt = 100, width_pt = 300, newpage = TRUE) {
  doctree <- read_html(contents)

  drawing_context <- gridtext:::setup_context()

  boxlist <- process_tags(xml2::as_list(doctree)$html$body, drawing_context)
  vbox <- gridtext:::bl_make_vbox(boxlist, width = width_pt, hjust = 0, vjust = 0, width_policy = "fixed")

  gridtext:::bl_calc_layout(vbox, width_pt, 0)
  grob <- gridtext:::bl_render(vbox, x_pt, y_pt)

  if (isTRUE(newpage)) grid.newpage()
  grid.draw(grob)
}

library(xml2)
library(gridtext)
#text <- "Some <span style='color:red'>red</span> text <b>in bold.</b><br>And <i>more</i> text.<br>And some <span style='font-size:18'>large</span> text."
text <- "<html>The quick<sub>j</sub> brown <b>fox</b><br> jumps <i>over</i> the lazy dog. The quick brown <b>fox</b><br> jumps <i>over</i> the lazy dog.</html>"
draw_rich_text(text, width_pt = 170)

