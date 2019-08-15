library(xml2)
library(gridtext)

img_src <- system.file("extdata", "Rlogo.png", package = "gridtext")
text <- glue::glue("This is an image. <img src='{img_src}' width='100' height='50'/> And some more text.")
grid.newpage()
grid.draw(richtext_grob(text, y = 0.3))

text <- glue::glue("This is an image. <img src='{img_src}' width='100'/> And some more text.")
grid.draw(richtext_grob(text, y = 0.7))


text <- "An image tag. <img src='smiley.gif' height='42' width='42'>"
doctree <- read_html(text)
node <- xml2::as_list(doctree)$html$body
names(node)
