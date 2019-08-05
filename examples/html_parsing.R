
library(xml2)
library(gridtext)
text <- "<html>The quick<sub>j</sub> brown <b>fox</b><br> jumps <i>over</i> the lazy dog. The quick brown <b>fox</b><br> jumps <i>over</i> the lazy dog.</html>"
draw_rich_text(text, width_pt = 200)


text1 <- "<span style='font-family:\"Comic Sans MS\"; font-size:20; color:orange'>Comic Sans! Yay!</span>"
text2 <- "Some <span style='color:red'>red</span> text <b>in bold.</b><br>And <i>more</i> text.<br>And some <span style='font-size:18'>large</span> text."
text3 <- "5<i>x</i><sup>2</sup><span style='color:blue'> + 7<i>x</i></span> - <i>α<sub>i</sub></i>"

draw_rich_text("<p><span>Comic Sans! Yay!</span></p>", width_pt = 800)


doctree <- read_html(text3)
node <- xml2::as_list(doctree)$html$body
names(node)

img_src <- system.file("extdata", "Rlogo.png", package = "gridtext")
text <- glue::glue("This is an image. ![]({img_src}) And some more text.")
html <- markdown::markdownToHTML(text = text, options = c("use_xhtml", "fragment_only"))
draw_rich_text(html, width_pt = 200)
