process_text <- function(node, drawing_context) {
  make_text_grobs(node, drawing_context)
}

process_tag_b <- function(node, drawing_context) {
  attr <- attributes(node)
  drawing_context <- set_style(drawing_context, attr$style)

  process_tags(node, set_context_fontface(drawing_context, "bold"))
}

process_tag_br <- function(node, drawing_context) {
  make_line_break(drawing_context)
}

process_tag_i <- function(node, drawing_context) {
  attr <- attributes(node)
  drawing_context <- set_style(drawing_context, attr$style)

  process_tags(node, set_context_fontface(drawing_context, "italic"))
}

process_tag_p <- function(node, drawing_context) {
  attr <- attributes(node)
  drawing_context <- set_style(drawing_context, attr$style)

  rbind(
    process_tags(node, drawing_context),
    make_line_break(drawing_context)
  )
}

process_tag_span <- function(node, drawing_context) {
  attr <- attributes(node)
  drawing_context <- set_style(drawing_context, attr$style)

  process_tags(node, drawing_context)
}

process_tag_sup <- function(node, drawing_context) {
  # modify fontsize before processing style, to allow for manual overriding
  drawing_context <- set_context_gp(drawing_context, gpar(fontsize = 0.8*drawing_context$gp$fontsize))
  attr <- attributes(node)
  drawing_context <- set_style(drawing_context, attr$style)

  # move drawing half a character above baseline
  drawing_context$yoff_pt <- drawing_context$yoff_pt + drawing_context$height_pt / 2
  process_tags(node, drawing_context)
}

process_tag_sub <- function(node, drawing_context) {
  # modify fontsize before processing style, to allow for manual overriding
  drawing_context <- set_context_gp(drawing_context, gpar(fontsize = 0.8*drawing_context$gp$fontsize))
  attr <- attributes(node)
  drawing_context <- set_style(drawing_context, attr$style)

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
      "span" = process_tag_span(node, drawing_context),
      "sup"  = process_tag_sup(node, drawing_context),
      "sub"  = process_tag_sub(node, drawing_context),
      stop("unknown tag: ", tag)
    )
  }
}


process_tags <- function(node, drawing_context) {
  tags <- names(node)
  grobs <- tibble()
  for (i in seq_along(node)) {
    grobs <- rbind(grobs, dispatch_tag(node[[i]], tags[i], drawing_context))
  }
  grobs
}

