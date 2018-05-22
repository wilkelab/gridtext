process_text <- function(node, drawing_context) {
  make_text_grobs(node, drawing_context)
}

process_tag_b <- function(node, drawing_context) {
  process_tags(node, set_context_fontface(drawing_context, "bold"))
}

process_tag_br <- function(node, drawing_context) {
  make_line_break(drawing_context)
}

process_tag_i <- function(node, drawing_context) {
  process_tags(node, set_context_fontface(drawing_context, "italic"))
}

process_tag_font <- function(node, drawing_context) {
  attr <- attributes(node)
  col <- attr$color
  fontfamily <- attr$face
  fontsize <- if (!is.null(attr$size)) as.numeric(attr$size) else NULL
  drawing_context <- set_context_gp(
    drawing_context,
    gpar(col = col, fontfamily = fontfamily, fontsize = fontsize)
  )
  process_tags(node, drawing_context)
}

process_tag_p <- function(node, drawing_context) {
  rbind(
    process_tags(node, drawing_context),
    make_line_break(drawing_context)
  )
}

dispatch_tag <- function(node, tag, drawing_context) {
  if (is.null(tag) || tag == "") {
    process_text(node, drawing_context)
  } else {
    switch(
      tag,
      "b"    = process_tag_b(node, drawing_context),
      "br"   = process_tag_br(node, drawing_context),
      "i"    = process_tag_i(node, drawing_context),
      "font" = process_tag_font(node, drawing_context),
      "p"    = process_tag_p(node, drawing_context),
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

