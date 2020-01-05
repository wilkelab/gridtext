#' Improved text rendering support for grid graphics
#'
#' The gridtext package provides two new grobs, [`richtext_grob()`] and
#' [`textbox_grob()`], which support drawing of formatted text labels and
#' formatted text boxes, respectively.
#' @name gridtext
#' @docType package
#' @useDynLib gridtext, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @import grid
#' @import rlang
#' @importFrom xml2 read_html
NULL
