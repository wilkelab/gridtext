library(Rcpp)

unit_pt <- function(x) {
  structure(x, class = "unit", valid.unit = 8L, unit = "pt")
}

cppFunction('NumericVector unit_pt_cpp(NumericVector x) {
  NumericVector out(x);
  out.attr("class") = "unit";
  out.attr("valid.unit") = IntegerVector(1, 8);
  out.attr("unit") = "pt";

  return out;
}')

identical(unit(10, "pt"), unit_pt(10))
identical(unit(10, "pt"), unit_pt_cpp(10))

microbenchmark::microbenchmark(unit(10, "pt"), unit_pt(10), unit_pt_cpp(10))

text_grob <- function(x_pt, y_pt, gp = gpar()) {
  textGrob("abcd", x = unit(x_pt, "pt"), y = unit(y_pt, "pt"), hjust = 0, vjust = 0, gp = gp, name = "1234")
}

text_grob2 <- function(x_pt, y_pt, gp = gpar()) {
  structure(
    list(
      label = "abcd",
      x = unit_pt(x_pt),
      y = unit_pt(y_pt),
      just = "centre",
      hjust = 0,
      vjust = 0,
      rot = 0,
      check.overlap = FALSE,
      name = "1234",
      gp = gp,
      vp = NULL
    ),
    class = c("text", "grob", "gDesc")
  )
}

cppFunction('List text_grob3(NumericVector x_pt, NumericVector y_pt, List gp) {
  NumericVector x(x_pt), y(y_pt);
  x.attr("class") = "unit";
  x.attr("valid.unit") = IntegerVector(1, 8);
  x.attr("unit") = "pt";
  y.attr("class") = "unit";
  y.attr("valid.unit") = IntegerVector(1, 8);
  y.attr("unit") = "pt";

  List out = List::create(
    _["label"] = "abcd", _["x"] = x, _["y"] = y, _["just"] = "centre",
    _["hjust"] = 0., _["vjust"] = 0., _["rot"] = 0.,
    _["check.overlap"] = false, _["name"] = "1234", _["gp"] = gp, _["vp"] = R_NilValue
  );

  Rcpp::StringVector cl(3);
  cl(0) = "text";
  cl(1) = "grob";
  cl(2) = "gDesc";

  out.attr("class") = cl;

  return out;
}')

identical(text_grob(10, 10, gpar()), text_grob2(10, 10, gpar()))
identical(text_grob(10, 10, gpar()), text_grob3(10, 10, gpar()))

gp = gpar()

microbenchmark::microbenchmark(
  text_grob(10, 10, gp),
  text_grob2(10, 10, gp),
  text_grob3(10, 10, gp)
)
