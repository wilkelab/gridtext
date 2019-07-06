#ifndef GRID_H
#define GRID_H

#include <Rcpp.h>
using namespace Rcpp;

#include <string>
using namespace std;

#include "length.h"

// This file defines a number of convenience functions that allow for the rapid construction
// or manipulation of grid units or grobs. Each could be replaced by a simple R call to a
// corresponding grid function (e.g., unit_pt(x) is equivalent to unit(x, "pt")), but in general
// the C++ version here is much faster, in particular because it skips extensive input validation.

// replacement for unit(x, "pt")
// [[Rcpp::export]]
NumericVector unit_pt(NumericVector x);

// Overloaded version for Length
NumericVector unit_pt(Length x);

// replacement for gpar() with no arguments
// [[Rcpp::export]]
List gpar_empty();

// replacement for textGrop(label, x_pt, y_pt, gp = gpar(), hjust = 0, vjust = 0, default.units = "pt", name = NULL)
// [[Rcpp::export]]
List text_grob(CharacterVector label, NumericVector x_pt = 0, NumericVector y_pt = 0,
               RObject gp = R_NilValue, RObject name = R_NilValue);

// replacement for rasterGrop(image, x_pt, y_pt, width_pt, height_pt, gp = gpar(), hjust = 0, vjust = 0, default.units = "pt", interpolate = TRUE, name = NULL)
// [[Rcpp::export]]
List raster_grob(RObject image, NumericVector x_pt = 0, NumericVector y_pt = 0, NumericVector width_pt = 0, NumericVector height_pt = 0,
                 LogicalVector interpolate = true, RObject gp = R_NilValue, RObject name = R_NilValue);

// replacement for rectGrop(x_pt, y_pt, width_pt, height_pt, gp = gpar(), hjust = 0, vjust = 0, default.units = "pt", name = NULL)
// [[Rcpp::export]]
List rect_grob(NumericVector x_pt = 0, NumericVector y_pt = 0, NumericVector width_pt = 0, NumericVector height_pt = 0,
               RObject gp = R_NilValue, RObject name = R_NilValue);

// replacement for roundrectGrop(x_pt, y_pt, width_pt, height_pt, r = unit(r_pt, "pt), gp = gpar(), just = c(0, 0), default.units = "pt", name = NULL)
// [[Rcpp::export]]
List roundrect_grob(NumericVector x_pt = 0, NumericVector y_pt = 0, NumericVector width_pt = 0, NumericVector height_pt = 0,
                    NumericVector r_pt = 5, RObject gp = R_NilValue, RObject name = R_NilValue);

// replacement for editGrob(grob, x = x, y = y)
// [[Rcpp::export]]
RObject set_grob_coords(RObject grob, NumericVector x, NumericVector y);

#endif
