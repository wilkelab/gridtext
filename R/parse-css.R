# Parse css
#
# A very simple css parser that can parse `key:value;` pairs.
#
# @param text The css text to parse
parse_css <- function(text) {
  # break into separate lines; for now, ignore the possibility of
  # quoted or escaped semicolon
  lines <- strsplit(text, ";", fixed = TRUE)[[1]]

  # parse each line and return list of key--value pairs
  unlist(lapply(lines, parse_css_line), recursive = FALSE)
}

parse_css_line <- function(line) {
  pattern <- "\\s*(\\S+)\\s*:\\s*(\"(.*)\"|'(.*)'|(\\S*))\\s*"
  m <- attributes(regexpr(pattern, line, perl = TRUE))
  if (m$capture.start[1] > 0) {
    key <- substr(line, m$capture.start[1], m$capture.start[1] + m$capture.length[1] - 1)
  } else key <- NULL

  if (m$capture.start[3] > 0) {
    value <- substr(line, m$capture.start[3], m$capture.start[3] + m$capture.length[3] - 1)
  } else if (m$capture.start[4] > 0) {
    value <- substr(line, m$capture.start[4], m$capture.start[4] + m$capture.length[4] - 1)
  } else if (m$capture.start[5] > 0) {
    value <- substr(line, m$capture.start[5], m$capture.start[5] + m$capture.length[5] - 1)
  } else value <- NULL

  if (is.null(key)) list()
  else list2(!!key := value)
}

parse_css_unit <- function(x) {
  pattern <- "^((-?\\d+\\.?\\d*)(%|[a-zA-Z]+)|(0))$"
  m <- attributes(regexpr(pattern, x, perl = TRUE))
  if (m$capture.start[4] > 0) {
    # matched null value
    return(list(value = 0, unit = "pt"))
  } else {
    if (m$capture.start[2] > 0) {
      value <- as.numeric(
        substr(x, m$capture.start[2], m$capture.start[2] + m$capture.length[2] - 1)
      )
      if (m$capture.start[3] > 0) {
        unit <- substr(x, m$capture.start[3], m$capture.start[3] + m$capture.length[3] - 1)
        return(list(value = value, unit = unit))
      }
    }
  }
  stop(paste0("The string '", x, "' does not represent a valid CSS unit."), call. = FALSE)
}

convert_css_unit_pt <- function(x) {
  u <- parse_css_unit(x)
  switch(
    u$unit,
    pt = u$value,
    px = (72/96)*u$value,
    `in` = 72*u$value,
    cm = (72/2.54)*u$value,
    mm = (72/25.4)*u$value,
    stop(paste0("Cannot convert ", u$value, u$unit, " to pt."), call. = FALSE)
  )
}

