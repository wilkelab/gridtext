
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gridtext

Improved text rendering support for grid graphics in R, written by Claus
O. Wilke

This is an experiment. Most features are broken. The API is not stable.
Please do not use this in any context where you need your code to work.
No user support will be provided.

## Installation

``` r
devtools::install_github("clauswilke/gridtext")
```

## Examples

``` r
library(grid)
library(gridtext)
text1 <- "<span style='font-family:\"Comic Sans MS\"; font-size:20; color:orange'>Comic Sans! Yay!</span>"
text2 <- "Some <span style='color:red'>red</span> text <b>in bold.</b><br>And <i>more</i> text.<br>And some <span style='font-size:18'>large</span> text."
text3 <- "5<i>x</i><sup>2</sup><span style='color:blue'> + 7<i>x</i></span> - <i>α<sub>i</sub></i>"

grid.newpage()
grid.draw(rich_text_grob(text1, x = 0.25, y = 0.9))
grid.draw(rich_text_grob(text2, x = 0.3, y = 0.3, hjust = 0, angle = 45))
grid.draw(rich_text_grob(text2, x = 0.8, y = 0.7, hjust = 1, angle = -90))
grid.draw(rich_text_grob(text3, x = 0.1, y = 0.1))
```

![](man/figures/README-unnamed-chunk-3-1.png)<!-- -->
