read_image <- function(path) {
  if (isTRUE(grepl("\\.png$", path, ignore.case = TRUE))) {
    img <- png::readPNG(get_file(path), native = TRUE)
  } else if (isTRUE(grepl("(\\.jpg$)|(\\.jpeg)", path, ignore.case = TRUE))) {
    img <- jpeg::readJPEG(get_file(path), native = TRUE)
  } else {
    warning(paste0("Image type not supported: ", path), call. = FALSE)
    img <- grDevices::as.raster(matrix(0, 10, 10))
  }
}

get_file <- function(path) {
  if (is_url(path)) {
    RCurl::getBinaryURL(path)
  } else {
    path
  }
}

is_url <- function(path)
{
  grepl("https?://", path)
}
