#include <Rcpp.h>
using namespace Rcpp;

#include "layout.h"
#include "null-box.h"
#include "par-box.h"
#include "rect-box.h"
#include "text-box.h"
#include "grid-renderer.h"

// [[Rcpp::export]]
XPtr<NodePtr> bl_make_null_box() {
  XPtr<NodePtr> p(new NodePtr(new NullBox<GridRenderer>()));

  StringVector cl = {"bl_null_box", "bl_box", "bl_node"};
  p.attr("class") = cl;

  return p;
}

// [[Rcpp::export]]
XPtr<NodePtr> bl_make_par_box(XPtr<NodeList> nodes, double vspacing_pt, double hspacing_pt) {
  XPtr<NodePtr> p(new NodePtr(new ParBox<GridRenderer>(*nodes, vspacing_pt, hspacing_pt)));

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

LayoutNode::SizePolicy convert_size_policy(String size_policy) {
  // we identify the size policy simply by its first letter
  switch (size_policy.get_cstring()[0]) {
  case 'n':
    return LayoutNode::native;
  case 'e':
    return LayoutNode::expand;
  case 'r':
    return LayoutNode::relative;
  case 'f':
  default:
    return LayoutNode::fixed;
  }
}

// [[Rcpp::export]]
XPtr<NodePtr> bl_make_rect_box(XPtr<NodePtr> content, double width_pt, double height_pt,
                               NumericVector margin, NumericVector padding, List gp,
                               double content_hjust = 0, double content_vjust = 1, String width_policy = "fixed",
                               String height_policy = "fixed", double r = 0) {
  if (!content.inherits("bl_box")) {
    stop("Contents must be of type 'bl_box'.");
  }

  Margin marg = convert_margin(margin);
  Margin pad = convert_margin(padding);
  LayoutNode::SizePolicy w_policy = convert_size_policy(width_policy);
  LayoutNode::SizePolicy h_policy = convert_size_policy(height_policy);

  XPtr<NodePtr> p(new NodePtr(new RectBox<GridRenderer>(
      *content, width_pt, height_pt, marg, pad, gp, content_hjust, content_vjust, w_policy, h_policy, r
    )));

  StringVector cl = {"bl_rect_box", "bl_box", "bl_node"};
  p.attr("class") = cl;

  return p;
}

// [[Rcpp::export]]
XPtr<NodePtr> bl_make_text_box(String label, List gp, double voff_pt) {
  XPtr<NodePtr> p(new NodePtr(new TextBox<GridRenderer>(label, gp, voff_pt)));

  StringVector cl = {"bl_text_box", "bl_box", "bl_node"};
  p.attr("class") = cl;

  return p;
}

// [[Rcpp::export]]
XPtr<NodeList> bl_make_node_list(List nodes) {
  XPtr<NodeList> nlist(new NodeList());

  for (auto i_node = nodes.begin(); i_node != nodes.end(); i_node++) {
    RObject obj(static_cast<RObject>(*i_node));
    if (!obj.inherits("bl_node")) {
      stop("All list elements must be of type 'bl_node'.");
    }
    XPtr<NodePtr> p(obj);
    nlist->push_back(*p);
  }

  StringVector cl = {"bl_node_list"};
  nlist.attr("class") = cl;

  return nlist;
}

// [[Rcpp::export]]
void bl_calc_layout(XPtr<NodePtr> node, double width_pt, double height_pt = 0) {
  if (!node.inherits("bl_box")) {
    stop("Node must be of type 'bl_box'.");
  }

  static_pointer_cast<Box<GridRenderer> >(*node)->calc_layout(width_pt, height_pt);
}

// [[Rcpp::export]]
RObject bl_render(XPtr<NodePtr> node, double x_pt, double y_pt) {
  if (!node.inherits("bl_box")) {
    stop("Node must be of type 'bl_box'.");
  }

  GridRenderer gr;
  static_pointer_cast<Box<GridRenderer> >(*node)->render(gr, x_pt, y_pt);
  return gr.collect_grobs();
}
