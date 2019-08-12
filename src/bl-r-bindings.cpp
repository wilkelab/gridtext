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

/* Various helper functions (not exported) */

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

BoxList<GridRenderer> make_node_list(const List &nodes) {
  BoxList<GridRenderer> nlist;
  nlist.reserve(nodes.size());

  for (auto i_node = nodes.begin(); i_node != nodes.end(); i_node++) {
    RObject obj(static_cast<RObject>(*i_node));
    if (!obj.inherits("bl_node")) {
      stop("All list elements must be of type 'bl_node'.");
    }
    BoxPtr<GridRenderer> p(obj);
    nlist.push_back(p);
  }
  return nlist;
}

/* Exported R bindings */

/*
 * Constructors for boxes
 */

// [[Rcpp::export]]
BoxPtr<GridRenderer> bl_make_null_box(double width_pt = 0, double height_pt = 0) {
  BoxPtr<GridRenderer> p(new NullBox<GridRenderer>(width_pt, height_pt));

  StringVector cl = {"bl_null_box", "bl_box", "bl_node"};
  p.attr("class") = cl;

  return p;
}

// [[Rcpp::export]]
BoxPtr<GridRenderer> bl_make_par_box(const List &node_list, double vspacing_pt, String width_policy = "native",
                                     RObject hjust = R_NilValue) {
  SizePolicy w_policy = convert_size_policy(width_policy);

  double hjust_val = 0;
  double use_hjust = false;
  if (!hjust.isNULL()) {
    NumericVector hj = as<NumericVector>(hjust);
    if (hj.size() > 0 && !NumericVector::is_na(hj[0])) {
      hjust_val = hj[0];
      use_hjust = true;
    }
  }

  BoxList<GridRenderer> nodes(make_node_list(node_list));
  BoxPtr<GridRenderer> p(new ParBox<GridRenderer>(nodes, vspacing_pt, w_policy, hjust_val, use_hjust));

  StringVector cl = {"bl_par_box", "bl_box", "bl_node"};
  p.attr("class") = cl;

  return p;
}


// [[Rcpp::export]]
BoxPtr<GridRenderer> bl_make_rect_box(RObject content, double width_pt, double height_pt,
                               NumericVector margin, NumericVector padding, List gp,
                               double content_hjust = 0, double content_vjust = 1, String width_policy = "fixed",
                               String height_policy = "fixed", double r = 0) {
  if (!content.isNULL() && !content.inherits("bl_box")) {
    stop("Contents must be of type 'bl_box'.");
  }

  Margin marg = convert_margin(margin);
  Margin pad = convert_margin(padding);
  SizePolicy w_policy = convert_size_policy(width_policy);
  SizePolicy h_policy = convert_size_policy(height_policy);

  StringVector cl = {"bl_rect_box", "bl_box", "bl_node"};

  if (content.isNULL()) {
    // R doesn't like null pointers, so we have to create
    // a null box instead
    BoxPtr<GridRenderer> nb(new NullBox<GridRenderer>(0, 0));
    BoxPtr<GridRenderer> p(new RectBox<GridRenderer>(
      nb, width_pt, height_pt, marg, pad, gp,
      content_hjust, content_vjust, w_policy, h_policy, r
    ));

    p.attr("class") = cl;
    return p;
  } else {
    BoxPtr<GridRenderer> p(new RectBox<GridRenderer>(
      as<BoxPtr<GridRenderer>>(content),
      width_pt, height_pt, marg, pad, gp, content_hjust, content_vjust, w_policy, h_policy, r
    ));

    p.attr("class") = cl;
    return p;
  }
}

// [[Rcpp::export]]
BoxPtr<GridRenderer> bl_make_text_box(const CharacterVector &label, List gp, double voff_pt = 0) {
  if (label.size() != 1) {
    stop("TextBox requires a label vector of length 1.");
  }

  BoxPtr<GridRenderer> p(new TextBox<GridRenderer>(label, gp, voff_pt));

  StringVector cl = {"bl_text_box", "bl_box", "bl_node"};
  p.attr("class") = cl;

  return p;
}


// [[Rcpp::export]]
BoxPtr<GridRenderer> bl_make_raster_box(RObject image, double width_pt = 0, double height_pt = 0,
                                        String width_policy = "native", String height_policy = "native",
                                        bool respect_aspect = true, bool interpolate = true, double dpi = 150,
                                        List gp = R_NilValue) {
  SizePolicy w_policy = convert_size_policy(width_policy);
  SizePolicy h_policy = convert_size_policy(height_policy);

  BoxPtr<GridRenderer> p(new RasterBox<GridRenderer>(
      image, width_pt, height_pt, gp, w_policy, h_policy, respect_aspect, interpolate, dpi));

  StringVector cl = {"bl_raster_box", "bl_box", "bl_node"};
  p.attr("class") = cl;

  return p;
}

// [[Rcpp::export]]
BoxPtr<GridRenderer> bl_make_vbox(const List &node_list, double width_pt = 0,
                                  double hjust = 0, double vjust = 1, String width_policy = "native") {
  SizePolicy w_policy = convert_size_policy(width_policy);

  BoxList<GridRenderer> nodes(make_node_list(node_list));
  BoxPtr<GridRenderer> p(new VBox<GridRenderer>(nodes, width_pt, hjust, vjust, w_policy));

  StringVector cl = {"bl_vbox", "bl_box", "bl_node"};
  p.attr("class") = cl;

  return p;
}

/*
 * Constructors for glue
 */

// [[Rcpp::export]]
BoxPtr<GridRenderer> bl_make_regular_space_glue(List gp, double stretch_ratio = 0.5, double shrink_ratio = 0.333333) {
  BoxPtr<GridRenderer> p(new RegularSpaceGlue<GridRenderer>(gp, stretch_ratio, shrink_ratio));

  StringVector cl = {"bl_regular_space_glue", "bl_glue", "bl_node"};
  p.attr("class") = cl;

  return p;
}


/*
 * Constructors for penalties
 */

// [[Rcpp::export]]
BoxPtr<GridRenderer> bl_make_forced_break_penalty() {
  BoxPtr<GridRenderer> p(new ForcedBreakPenalty<GridRenderer>());

  StringVector cl = {"bl_forced_break_penalty", "bl_penalty", "bl_node"};
  p.attr("class") = cl;

  return p;
}

// [[Rcpp::export]]
BoxPtr<GridRenderer> bl_make_never_break_penalty() {
  BoxPtr<GridRenderer> p(new NeverBreakPenalty<GridRenderer>());

  StringVector cl = {"bl_never_break_penalty", "bl_penalty", "bl_node"};
  p.attr("class") = cl;

  return p;
}

/*
 * Call member functions
 */

// [[Rcpp::export]]
double bl_box_width(BoxPtr<GridRenderer> node) {
  if (!node.inherits("bl_node")) {
    stop("Node must be of type 'bl_node'.");
  }

  return node->width();
}

// [[Rcpp::export]]
double bl_box_height(BoxPtr<GridRenderer> node) {
  if (!node.inherits("bl_node")) {
    stop("Node must be of type 'bl_node'.");
  }

  return node->height();
}

// [[Rcpp::export]]
double bl_box_ascent(BoxPtr<GridRenderer> node) {
  if (!node.inherits("bl_node")) {
    stop("Node must be of type 'bl_node'.");
  }

  return node->ascent();
}

// [[Rcpp::export]]
double bl_box_descent(BoxPtr<GridRenderer> node) {
  if (!node.inherits("bl_node")) {
    stop("Node must be of type 'bl_node'.");
  }

  return node->descent();
}

// [[Rcpp::export]]
double bl_box_voff(BoxPtr<GridRenderer> node) {
  if (!node.inherits("bl_node")) {
    stop("Node must be of type 'bl_node'.");
  }

  return node->voff();
}

// [[Rcpp::export]]
void bl_calc_layout(BoxPtr<GridRenderer> node, double width_pt = 0, double height_pt = 0) {
  if (!node.inherits("bl_node")) {
    stop("Node must be of type 'bl_node'.");
  }

  node->calc_layout(width_pt, height_pt);
}

// [[Rcpp::export]]
void bl_place(BoxPtr<GridRenderer> node, double x_pt, double y_pt) {
  if (!node.inherits("bl_node")) {
    stop("Node must be of type 'bl_node'.");
  }

  node->place(x_pt, y_pt);
}


// [[Rcpp::export]]
RObject bl_render(BoxPtr<GridRenderer> node, double x_pt = 0, double y_pt = 0) {
  if (!node.inherits("bl_node")) {
    stop("Node must be of type 'bl_node'.");
  }

  GridRenderer gr;
  node->render(gr, x_pt, y_pt);
  return gr.collect_grobs();
}
