#include <Rcpp.h>
using namespace Rcpp;

#include "layout.h"
#include "hbox.h"
#include "text-box.h"
#include "grid-renderer.h"

// [[Rcpp::export]]
XPtr<NodePtr> bl_make_text_box(String label, double voff_pt) {
  XPtr<NodePtr> p(new NodePtr(new TextBox<GridRenderer>(label, voff_pt)));

  StringVector cl = {"bl_text_box", "bl_box", "bl_node"};
  p.attr("class") = cl;

  return p;
}

// [[Rcpp::export]]
XPtr<NodePtr> bl_make_hbox(XPtr<NodeList> nodes, double vspacing_pt, double hspacing_pt) {
  XPtr<NodePtr> p(new NodePtr(new HBox<GridRenderer>(*nodes, vspacing_pt, hspacing_pt)));

  StringVector cl = {"bl_hbox", "bl_box", "bl_node"};
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
