process_text <- function(node, drawing_context) {
  tokens <- stringr::str_split(stringr::str_squish(node), "[[:space:]]+")[[1]]

  # make interior boxes
  boxes <- lapply(tokens,
    function(token) {
      list(
        bl_make_text_box(token, drawing_context$gp, drawing_context$yoff_pt),
        bl_make_regular_space_glue(drawing_context$gp)
      )
    }
  )

  # if node starts with space, add glue at beginning
  if (isTRUE(grepl("^[[:space:]]", node))) {
    boxes <- c(list(bl_make_regular_space_glue(drawing_context$gp)), boxes)
  }

  boxes <- unlist(boxes, recursive = FALSE)

  # if node doesn't end with space, remove glue at end
  if (!isTRUE(grepl("[[:space:]]$", node))) {
    boxes[[length(boxes)]] <- NULL
  }
  boxes
}

process_tag_b <- function(node, drawing_context) {
  attr <- attributes(node)
  drawing_context <- set_style(drawing_context, attr$style)

  process_tags(node, set_context_fontface(drawing_context, "bold"))
}

process_tag_br <- function(node, drawing_context) {
  list(
    bl_make_text_box("", drawing_context$gp),
    bl_make_forced_break_penalty()
  )
}

process_tag_i <- function(node, drawing_context) {
  attr <- attributes(node)
  drawing_context <- set_style(drawing_context, attr$style)

  process_tags(node, set_context_fontface(drawing_context, "italic"))
}

process_tag_img <- function(node, drawing_context) {
  attr <- attributes(node)

  height <- attr$height
  if (is.null(height)) {
    height <- 0
    height_policy <- "native"
  } else {
    height <- as.numeric(height)
    height_policy <- "fixed"
  }

  width <- attr$width
  if (is.null(width)) {
    width <- 0
    width_policy <- "native"
  } else {
    width <- as.numeric(width)
    width_policy <- "fixed"
  }

  if (height_policy == "fixed" && width_policy == "fixed") {
    respect_asp <- FALSE
  } else {
    respect_asp <- TRUE
  }

  # read image
  img <- read_image(attr$src)

  # dpi = 72.27 turns lengths in pixels to lengths in pt
  rb <- bl_make_raster_box(
    img, width, height, width_policy, height_policy,
    respect_aspect = respect_asp, dpi = 72.27
  )

  list(rb)
}

process_tag_p <- function(node, drawing_context) {
  attr <- attributes(node)
  drawing_context <- set_style(drawing_context, attr$style)

  boxes <- unlist(
    list(
      process_tags(node, drawing_context),
      process_tag_br(NULL, drawing_context)
    ),
    recursive = FALSE
  )

  # word wrapping corresponds to width_policy = "relative".
  if (isTRUE(drawing_context$word_wrap)) {
    bl_make_par_box(
      boxes, drawing_context$linespacing_pt, width_policy = "relative",
      hjust = drawing_context$halign
    )
  } else {
    bl_make_par_box(
      boxes, drawing_context$linespacing_pt, width_policy = "native",
      hjust = drawing_context$halign
    )
  }
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
  drawing_context$yoff_pt <- drawing_context$yoff_pt + drawing_context$ascent_pt / 2
  process_tags(node, drawing_context)
}

process_tag_sub <- function(node, drawing_context) {
  # modify fontsize before processing style, to allow for manual overriding
  drawing_context <- set_context_gp(drawing_context, gpar(fontsize = 0.8*drawing_context$gp$fontsize))
  attr <- attributes(node)
  drawing_context <- set_style(drawing_context, attr$style)

  # move drawing half a character below baseline
  drawing_context$yoff_pt <- drawing_context$yoff_pt - drawing_context$ascent_pt / 2
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
      "img"  = process_tag_img(node, drawing_context),
      "em"   = process_tag_i(node, drawing_context),
      "p"    = process_tag_p(node, drawing_context),
      "span" = process_tag_span(node, drawing_context),
      "sup"  = process_tag_sup(node, drawing_context),
      "sub"  = process_tag_sub(node, drawing_context),
      stop(
        paste0("gridtext has encountered a tag that isn't supported yet: <", tag, ">\n",
               "Only a very limited number of tags are currently supported."),
        call. = FALSE
      )
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

