#include <Rcpp.h>
using namespace Rcpp;

#include "layout.h"
#include "null-box.h"
#include "par-box.h"
#include "raster-box.h"
#include "rect-box.h"
#include "text-box.h"
#include "vbox.h"
#include "grid-renderer.h"

// for testing and debugging only
// [[Rcpp::export]]
XPtr<BoxPtr<GridRenderer> > bl_make_null_ptr() {
  XPtr<BoxPtr<GridRenderer> > p(new BoxPtr<GridRenderer>(nullptr));

  StringVector cl = {"bl_null_ptr", "bl_box", "bl_node"};
  p.attr("class") = cl;

  return p;
}

// [[Rcpp::export]]
XPtr<BoxPtr<GridRenderer> > bl_make_null_box(double width_pt = 0, double height_pt = 0) {
  XPtr<BoxPtr<GridRenderer> > p(new BoxPtr<GridRenderer> (new NullBox<GridRenderer>(width_pt, height_pt)));

  StringVector cl = {"bl_null_box", "bl_box", "bl_node"};
  p.attr("class") = cl;

  return p;
}

// [[Rcpp::export]]
XPtr<BoxPtr<GridRenderer> > bl_make_par_box(XPtr<BoxList<GridRenderer> > nodes, double vspacing_pt, double hspacing_pt) {
  XPtr<BoxPtr<GridRenderer> > p(new BoxPtr<GridRenderer>(new ParBox<GridRenderer>(*nodes, vspacing_pt, hspacing_pt)));

  StringVector cl = {"bl_par_box", "bl_box", "bl_node"};
  p.attr("class") = cl;

  return p;
}


Margin convert_margin(NumericVector margin) {
  if (margin.size() != 4) {
    stop("Margin must have exactly four elements.");
  }

  return Margin(margin[0], margin[1], margin[2], margin[3]);
}

SizePolicy convert_size_policy(String size_policy) {
  // we identify the size policy simply by its first letter
  switch (size_policy.get_cstring()[0]) {
  case 'n':
    return SizePolicy::native;
  case 'e':
    return SizePolicy::expand;
  case 'r':
    return SizePolicy::relative;
  case 'f':
  default:
    return SizePolicy::fixed;
  }
}

// [[Rcpp::export]]
XPtr<BoxPtr<GridRenderer> > bl_make_rect_box(XPtr<BoxPtr<GridRenderer> > content, double width_pt, double height_pt,
                               NumericVector margin, NumericVector padding, List gp,
                               double content_hjust = 0, double content_vjust = 1, String width_policy = "fixed",
                               String height_policy = "fixed", double r = 0) {
  if (!content.inherits("bl_box")) {
    stop("Contents must be of type 'bl_box'.");
  }

  Margin marg = convert_margin(margin);
  Margin pad = convert_margin(padding);
  SizePolicy w_policy = convert_size_policy(width_policy);
  SizePolicy h_policy = convert_size_policy(height_policy);

  XPtr<BoxPtr<GridRenderer> > p(new BoxPtr<GridRenderer>(new RectBox<GridRenderer>(
      *content, width_pt, height_pt, marg, pad, gp, content_hjust, content_vjust, w_policy, h_policy, r
    )));

  StringVector cl = {"bl_rect_box", "bl_box", "bl_node"};
  p.attr("class") = cl;

  return p;
}

// [[Rcpp::export]]
XPtr<BoxPtr<GridRenderer> > bl_make_text_box(String label, List gp, double voff_pt = 0) {
  XPtr<BoxPtr<GridRenderer> > p(new BoxPtr<GridRenderer>(new TextBox<GridRenderer>(label, gp, voff_pt)));

  StringVector cl = {"bl_text_box", "bl_box", "bl_node"};
  p.attr("class") = cl;

  return p;
}


//RasterBox(RObject image, Length width, Length height, const typename Renderer::GraphicsContext &gp,
//          SizePolicy width_policy = SizePolicy::fixed, SizePolicy height_policy = SizePolicy::fixed,
//          bool interpolate = true) :

// [[Rcpp::export]]
XPtr<BoxPtr<GridRenderer> > bl_make_raster_box(RObject image, double width_pt, double height_pt,
                                               String width_policy = "fixed", String height_policy = "fixed",
                                               bool interpolate = true, List gp = R_NilValue) {
  SizePolicy w_policy = convert_size_policy(width_policy);
  SizePolicy h_policy = convert_size_policy(height_policy);

  XPtr<BoxPtr<GridRenderer> > p(new BoxPtr<GridRenderer>(new RasterBox<GridRenderer>(
      image, width_pt, height_pt, gp, w_policy, h_policy, interpolate)));

  StringVector cl = {"bl_raster_box", "bl_box", "bl_node"};
  p.attr("class") = cl;

  return p;
}

// [[Rcpp::export]]
XPtr<BoxPtr<GridRenderer> > bl_make_vbox(XPtr<BoxList<GridRenderer> > nodes, double width_pt = 0,
                                         double hjust = 0, double vjust = 1, String width_policy = "native") {
  SizePolicy w_policy = convert_size_policy(width_policy);

  XPtr<BoxPtr<GridRenderer> > p(new BoxPtr<GridRenderer>(new VBox<GridRenderer>(
      *nodes, width_pt, hjust, vjust, w_policy)));

  StringVector cl = {"bl_vbox", "bl_box", "bl_node"};
  p.attr("class") = cl;

  return p;
}

// [[Rcpp::export]]
XPtr<BoxList<GridRenderer> > bl_make_node_list(List nodes) {
  XPtr<BoxList<GridRenderer> > nlist(new BoxList<GridRenderer>());

  for (auto i_node = nodes.begin(); i_node != nodes.end(); i_node++) {
    RObject obj(static_cast<RObject>(*i_node));
    if (!obj.inherits("bl_node")) {
      stop("All list elements must be of type 'bl_node'.");
    }
    XPtr<BoxPtr<GridRenderer> > p(obj);
    nlist->push_back(*p);
  }

  StringVector cl = {"bl_node_list"};
  nlist.attr("class") = cl;

  return nlist;
}

// [[Rcpp::export]]
double bl_box_width(XPtr<BoxPtr<GridRenderer> > node) {
  if (!node.inherits("bl_box")) {
    stop("Node must be of type 'bl_box'.");
  }

  return (*node)->width();
}

// [[Rcpp::export]]
double bl_box_height(XPtr<BoxPtr<GridRenderer> > node) {
  if (!node.inherits("bl_box")) {
    stop("Node must be of type 'bl_box'.");
  }

  return (*node)->height();
}

// [[Rcpp::export]]
double bl_box_ascent(XPtr<BoxPtr<GridRenderer> > node) {
  if (!node.inherits("bl_box")) {
    stop("Node must be of type 'bl_box'.");
  }

  return (*node)->ascent();
}

// [[Rcpp::export]]
double bl_box_descent(XPtr<BoxPtr<GridRenderer> > node) {
  if (!node.inherits("bl_box")) {
    stop("Node must be of type 'bl_box'.");
  }

  return (*node)->descent();
}

// [[Rcpp::export]]
double bl_box_voff(XPtr<BoxPtr<GridRenderer> > node) {
  if (!node.inherits("bl_box")) {
    stop("Node must be of type 'bl_box'.");
  }

  return (*node)->voff();
}

// [[Rcpp::export]]
void bl_calc_layout(XPtr<BoxPtr<GridRenderer> > node, double width_pt, double height_pt = 0) {
  if (!node.inherits("bl_box")) {
    stop("Node must be of type 'bl_box'.");
  }

  (*node)->calc_layout(width_pt, height_pt);
}

// [[Rcpp::export]]
void bl_place(XPtr<BoxPtr<GridRenderer> > node, double x_pt, double y_pt) {
  if (!node.inherits("bl_box")) {
    stop("Node must be of type 'bl_box'.");
  }

  (*node)->place(x_pt, y_pt);
}


// [[Rcpp::export]]
RObject bl_render(XPtr<BoxPtr<GridRenderer> > node, double x_pt, double y_pt) {
  if (!node.inherits("bl_box")) {
    stop("Node must be of type 'bl_box'.");
  }

  GridRenderer gr;
  (*node)->render(gr, x_pt, y_pt);
  return gr.collect_grobs();
}
