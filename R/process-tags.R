process_text <- function(node, drawing_context) {
  cat("TEXT: ", node, "\n")
  make_line(node, drawing_context)
}

process_tag_br <- function(node, drawing_context) {
  cat("BR\n")
  tibble(grob = list(zeroGrob()), width_pt = 0, height_pt = 0, descent_pt = 0, type = "br")
}

process_tag_b <- function(node, drawing_context) {
  cat("B\n")
  drawing_context$gp <- update_gpar(drawing_context$gp, fontface = "bold")
  result <- process_tags(node, drawing_context)
  cat("/B\n")
  result
}

process_tag_p <- function(node, drawing_context) {
  cat("P\n")
  result <- process_tags(node, drawing_context)
  cat("/P\n")
  rbind(result, tibble(grob = list(zeroGrob()), width_pt = 0, height_pt = 0, descent_pt = 0, type = "br"))
}


dispatch_tag <- function(node, tag, drawing_context) {
  if (is.null(tag) || tag == "") {
    process_text(node, drawing_context)
  } else {
    switch(
      tag,
      "br" = process_tag_br(node, drawing_context),
      "b" = process_tag_b(node, drawing_context),
      "p" = process_tag_p(node, drawing_context),
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

