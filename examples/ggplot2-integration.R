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

# rendering of the theme element is handled by `richtext_grob()`
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

  richtext_grob(
    label, x = x, y = y, hjust = hj, vjust = vj, rot = angle,
    padding = margin, gp = gp
  )
}

ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
  geom_point(show.legend=TRUE, size = 4) +
  scale_color_brewer(
    palette = "Set2",
    labels = c("*I. setosa*", "*I. versicolor*", "*I. virginica*")
  ) +
  labs(
    title = "Sepal **length** and sepal **width** of various *Iris* species",
    x = "Sepal **length** (cm)", y = "Sepal **width** (cm)"
  ) +
  geom_smooth(show.legend = FALSE, alpha = 0.15) +
  theme_minimal() +
  theme(
    plot.title = element_markdown(
      colour = "#3A4A60",
      margin = ggplot2::margin(5, 0, 5, -35),
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

labels <- c(
  setosa = "<img src='https://upload.wikimedia.org/wikipedia/commons/thumb/5/56/Kosaciec_szczecinkowaty_Iris_setosa.jpg/300px-Kosaciec_szczecinkowaty_Iris_setosa.jpg'
    width='100' /><br>*I. setosa*",
  virginica = "<img src='https://upload.wikimedia.org/wikipedia/commons/thumb/9/9f/Iris_virginica.jpg/300px-Iris_virginica.jpg'
    width='100' /><br>*I. virginica*",
  versicolor = "<img src='https://upload.wikimedia.org/wikipedia/commons/thumb/4/41/Iris_versicolor_3.jpg/320px-Iris_versicolor_3.jpg'
    width='100' /><br>*I. versicolor*"
)

ggplot(iris, aes(Species, Sepal.Width)) +
  geom_boxplot() +
  scale_x_discrete(
    labels = labels
  ) +
  theme(
    axis.text.x = element_markdown(color = "black", size = 11)
  )



