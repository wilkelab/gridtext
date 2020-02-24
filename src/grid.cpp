#include "grid.h"

NumericVector unit_pt(NumericVector x) {
  // create unit vector by calling back to R
  Environment env = Environment::namespace_env("grid");
  Function unit = env["unit"];
  return unit(x, "pt");
}

NumericVector unit_pt(Length x) {
  NumericVector out(1, x);
  return unit_pt(out);
}

List gpar_empty() {
  List out;
  out.attr("class") = "gpar";

  return out;
}

List text_grob(CharacterVector label, NumericVector x_pt, NumericVector y_pt, RObject gp, RObject name) {
  if (label.size() != 1 || x_pt.size() != 1 || y_pt.size() != 1) {
    stop("Function text_grob() is not vectorized.\n");
  }

  if (gp.isNULL()) {
    gp = gpar_empty();
  }

  // need to produce a unique name for each grob, otherwise grid gets grumpy
  static int tg_count = 0;
  if (name.isNULL()) {
    tg_count += 1;
    string s("gridtext.text.");
    s = s + to_string(tg_count);
    CharacterVector vs;
    vs.push_back(s);
    name = vs;
  }

  List out = List::create(
    _["label"] = label, _["x"] = unit_pt(x_pt), _["y"] = unit_pt(y_pt), _["just"] = "centre",
    _["hjust"] = 0., _["vjust"] = 0., _["rot"] = 0.,
    _["check.overlap"] = false, _["name"] = name, _["gp"] = gp, _["vp"] = R_NilValue
  );

  Rcpp::StringVector cl(3);
  cl(0) = "text";
  cl(1) = "grob";
  cl(2) = "gDesc";

  out.attr("class") = cl;

  return out;
}

List raster_grob(RObject image, NumericVector x_pt, NumericVector y_pt, NumericVector width_pt, NumericVector height_pt,
                 LogicalVector interpolate, RObject gp, RObject name) {
  if (x_pt.size() != 1 || y_pt.size() != 1 || width_pt.size() != 1 || height_pt.size() != 1) {
    stop("Function raster_grob() is not vectorized.\n");
  }

  // need to produce a unique name for each grob, otherwise grid gets grumpy
  static int tg_count = 0;
  if (name.isNULL()) {
    tg_count += 1;
    string s("gridtext.raster.");
    s = s + to_string(tg_count);
    CharacterVector vs;
    vs.push_back(s);
    name = vs;
  }

  RObject raster = image;
  if (!raster.inherits("nativeRaster")) {
    // convert to raster by calling grDevices::as.raster()
    Environment env = Environment::namespace_env("grDevices");
    Function as_raster = env["as.raster"];
    raster = as_raster(image);
  }

  List out = List::create(
    _["raster"] = raster,
    _["x"] = unit_pt(x_pt), _["y"] = unit_pt(y_pt),
    _["width"] = unit_pt(width_pt), _["height"] = unit_pt(height_pt),
    _["just"] = "centre", _["hjust"] = 0., _["vjust"] = 0.,
    _["interpolate"] = interpolate,
    _["name"] = name, _["gp"] = gp, _["vp"] = R_NilValue
  );

  Rcpp::StringVector cl(3);
  cl(0) = "rastergrob";
  cl(1) = "grob";
  cl(2) = "gDesc";

  out.attr("class") = cl;

  return out;
}



List rect_grob(NumericVector x_pt, NumericVector y_pt, NumericVector width_pt, NumericVector height_pt,
               RObject gp, RObject name) {
  if (x_pt.size() != 1 || y_pt.size() != 1 || width_pt.size() != 1 || height_pt.size() != 1) {
    stop("Function rect_grob() is not vectorized.\n");
  }

  if (gp.isNULL()) {
    gp = gpar_empty();
  }

  // need to produce a unique name for each grob, otherwise grid gets grumpy
  static int tg_count = 0;
  if (name.isNULL()) {
    tg_count += 1;
    string s("gridtext.rect.");
    s = s + to_string(tg_count);
    CharacterVector vs;
    vs.push_back(s);
    name = vs;
  }

  List out = List::create(
    _["x"] = unit_pt(x_pt), _["y"] = unit_pt(y_pt),
    _["width"] = unit_pt(width_pt), _["height"] = unit_pt(height_pt),
    _["just"] = "centre", _["hjust"] = 0., _["vjust"] = 0.,
    _["name"] = name, _["gp"] = gp, _["vp"] = R_NilValue
  );

  Rcpp::StringVector cl(3);
  cl(0) = "rect";
  cl(1) = "grob";
  cl(2) = "gDesc";

  out.attr("class") = cl;

  return out;
}

List roundrect_grob(NumericVector x_pt, NumericVector y_pt, NumericVector width_pt, NumericVector height_pt,
                    NumericVector r_pt, RObject gp, RObject name) {
  if (x_pt.size() != 1 || y_pt.size() != 1 || width_pt.size() != 1 || height_pt.size() != 1 || r_pt.size() != 1) {
    stop("Function roundrect_grob() is not vectorized.\n");
  }

  if (gp.isNULL()) {
    gp = gpar_empty();
  }

  // need to produce a unique name for each grob, otherwise grid gets grumpy
  static int tg_count = 0;
  if (name.isNULL()) {
    tg_count += 1;
    string s("gridtext.roundrect.");
    s = s + to_string(tg_count);
    CharacterVector vs;
    vs.push_back(s);
    name = vs;
  }

  NumericVector justv(2);  // c(0, 0)

  List out = List::create(
    _["x"] = unit_pt(x_pt), _["y"] = unit_pt(y_pt),
    _["width"] = unit_pt(width_pt), _["height"] = unit_pt(height_pt),
    _["r"] = unit_pt(r_pt),
    _["just"] = justv,
    _["name"] = name, _["gp"] = gp, _["vp"] = R_NilValue
  );

  Rcpp::StringVector cl(3);
  cl(0) = "roundrect";
  cl(1) = "grob";
  cl(2) = "gDesc";

  out.attr("class") = cl;

  return out;
}


RObject set_grob_coords(RObject grob, NumericVector x, NumericVector y) {
  as<List>(grob)["x"] = x;
  as<List>(grob)["y"] = y;

  return grob;
}
