library(ggplot2)
library(grid)
library(gridtext)
library(rlang)
library(tibble)

# define new theme element that inherits from `element_text()`
element_label <- function(family = NULL, face = NULL, colour = NULL, size = NULL,
                          hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL,
                          color = NULL, margin = NULL,
                          debug = FALSE, inherit.blank = FALSE) {
  if (!is.null(color))
    colour <- color
  structure(
    list(
      family = family, face = face, colour = colour,
      size = size, hjust = hjust, vjust = vjust, angle = angle,
      lineheight = lineheight, margin = margin, debug = debug,
      inherit.blank = inherit.blank),
    class = c("element_label", "element_text", "element")
  )
}

# helper function to set up data frame with graphical parameters
list_to_table <- function(l) {
  lengths <- vapply(l, length, numeric(1))

  # remove length 0 cases
  l[lengths == 0L] <- NULL

  do.call(tibble, l)
}

# rendering of the theme element is handled by `labels_grob()`
element_grob.element_label <- function(element, label = "", x = NULL, y = NULL,
                                       family = NULL, face = NULL, colour = NULL, size = NULL,
                                       hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL,
                                       margin = NULL, margin_x = FALSE, margin_y = FALSE, ...) {
  if (is.null(label))
    return(ggplot2:::zeroGrob())

  vj <- vjust %||% element$vjust
  hj <- hjust %||% element$hjust
  margin <- margin %||% element$margin %||% ggplot2::margin(0, 0, 0, 0)
  angle <- angle %||% element$angle %||% 0

  x <- x %||% hj
  if (!is.unit(x))
    x <- unit(x, "npc")
  y <- y %||% vj
  if (!is.unit(y))
    y <- unit(y, "npc")

  # The gp settings can override element_gp
  gp <- gpar(
    fontsize = size %||% element$size,
    col = colour %||% element$colour,
    fontfamily = family %||% element$family,
    fontface = face %||% element$face,
    lineheight = lineheight %||% element$lineheight
  )

  gl <- list(
    label = label,
    x = x,
    y = y,
    hjust = hj,
    vjust = vj,
    angle = angle,
    color = gp$col,
    fontsize = gp$fontsize,
    fontfamily = gp$fontfamily,
    fontface = gp$fontface,
    lineheight = gp$lineheight,
    padding = list(margin)
  )
  label_data <- list_to_table(gl)

  labels_grob(label_data, debug = element$debug)
}

# draw plot with alternative axis rendering
ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
  geom_point() +
  ggtitle("Iris data") +
  theme_minimal() +
  theme(
    axis.text.x = element_label(
      hjust = 0.5, vjust = 1, angle = 0, debug = TRUE,
      margin = ggplot2::margin(10, 2, 10, 2)
    )
  )

# draw plot with alternative title rendering
ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
  geom_point() +
  ggtitle("Iris data") +
  theme_minimal() +
  theme(
    plot.title = element_label(
      hjust = 0, vjust = 0.5, angle = 0, debug = TRUE,
      margin = ggplot2::margin(5, 5, 5, 5)
    )
  )

library(ggplot2)
library(grid)
library(gridtext)
library(rlang)

# define new theme element that inherits from `element_text()`
element_markdown <- function(family = NULL, face = NULL, colour = NULL, size = NULL,
                          hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL,
                          color = NULL, margin = NULL,
                          debug = FALSE, inherit.blank = FALSE) {
  if (!is.null(color))
    colour <- color
  structure(
    list(
      family = family, face = face, colour = colour,
      size = size, hjust = hjust, vjust = vjust, angle = angle,
      lineheight = lineheight, margin = margin, debug = debug,
      inherit.blank = inherit.blank),
    class = c("element_markdown", "element_text", "element")
  )
}

# rendering of the theme element is handled by `labels_grob()`
element_grob.element_markdown <- function(element, label = "", x = NULL, y = NULL,
                                          family = NULL, face = NULL, colour = NULL, size = NULL,
                                          hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL,
                                          margin = NULL, margin_x = FALSE, margin_y = FALSE, ...) {
  if (is.null(label))
    return(ggplot2:::zeroGrob())

  vj <- vjust %||% element$vjust
  hj <- hjust %||% element$hjust
  margin <- margin %||% element$margin %||% ggplot2::margin(0, 0, 0, 0)
  angle <- angle %||% element$angle %||% 0

  x <- x %||% hj
  if (!is.unit(x))
    x <- unit(x, "npc")
  y <- y %||% vj
  if (!is.unit(y))
    y <- unit(y, "npc")

  # The gp settings can override element_gp
  gp <- gpar(
    fontsize = size %||% element$size,
    col = colour %||% element$colour,
    fontfamily = family %||% element$family,
    fontface = face %||% element$face,
    lineheight = lineheight %||% element$lineheight
  )

  markdown_grob(
    label, x = x, y = y, hjust = hj, vjust = vj, angle = angle,
    padding = margin, gp = gp, debug = element$debug
  )
}

# draw plot with alternative title rendering
ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
  geom_point() +
  ggtitle("The **famous** Fischer *Iris* dataset") +
  theme_minimal() +
  theme(
    plot.title = element_markdown(
      hjust = 0, vjust = 0.5, size = 14, margin = ggplot2::margin(5, 0, 5, 0)
    )
  )


ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
  geom_point(show.legend=TRUE, size = 4) +
  scale_color_brewer(
    palette = "Set2",
    labels = c("*I. setosa*", "*I. versicolor*", "*I. virginica*")
  ) +
  labs(
    title = "Sepal length and sepal width of<br>various *Iris* species",
    x = "Sepal **length** (cm)", y = "Sepal **width** (cm)"
  ) +
  geom_smooth(show.legend = FALSE, alpha = 0.15) +
  theme_minimal() +
  theme(
    plot.title = element_markdown(
      colour = "#3A4A60",
      margin = ggplot2::margin(5, 0, 5, 0),
      lineheight = 1.2
    ),
    legend.text = element_markdown(),
    axis.title.x = element_markdown(colour = "#3A4A60"),
    axis.title.y = element_markdown(
      colour = "#3A4A60",
      vjust = 0.5, hjust = 0.5,
      margin = margin(b = 5.5)
    )
  )

