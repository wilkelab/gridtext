/* Various test routines.
 * To be deleted eventually.
 */

#include "grid-renderer.h"

// [[Rcpp::export]]
List test_grid_renderer() {
  GridRenderer r;

  r.rect(50, 160, 100, 14, "black", "cornsilk", 1, 1, 3);
  r.text("Hello", 10, 200);
  r.rect(20, 200, 100, 14, NA_STRING, NA_STRING);
  r.rect(30, 220, 100, 14, NA_STRING, "cornsilk", 0);
  r.rect(30, 220, 100, 14, "black", NA_STRING, 0);
  r.text("World", 10, 180, "red", 14, "bold");
  r.rect(10, 180, 100, 14);
  r.text("Some more text", 50, 160, "blue");
  return r.collect_grobs();
}


#include "hbox.h"
/*
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
*/
