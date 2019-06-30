#include "hbox.h"

// [[Rcpp::export]]
RObject test_hbox(List grobs, NumericVector widths, double box_width, double x, double y,
                  double vspacing, double hspacing) {
  // build list of grob boxes
  NodeList nodes;

  int i_width = 0;
  for (auto i_grob = grobs.begin(); i_grob != grobs.end(); i_grob++) {
    nodes.push_back(NodePtr(new GrobBox(*i_grob, widths[i_width])));
    i_width++;
  }

  HBox hb(nodes, vspacing, hspacing);
  hb.calc_layout(box_width);
  return hb.render(x, y);
}
