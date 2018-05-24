#' Parse css
#'
#' A very simple css parser that can parse `key:value;` pairs.
#'
#' @param text The css text to parse
#' @export
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
