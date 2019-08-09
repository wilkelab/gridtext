# An empty grob of no extent
#
# An empty grob of no extent. Useful when a grob is needed but no
# content is desired.
# @keywords internal
# @export
zeroGrob <- function() .zeroGrob

.zeroGrob <- grid::grob(cl = "zeroGrob", name = "NULL")

widthDetails.zeroGrob <- function(x) unit(0, "cm")
heightDetails.zeroGrob <- function(x) unit(0, "cm")
grobWidth.zeroGrob <- function(x) unit(0, "cm")
grobHeight.zeroGrob <- function(x) unit(0, "cm")
drawDetails.zeroGrob <- function(x, recording) {}
