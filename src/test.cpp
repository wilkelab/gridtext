/* Various test routines.
 * To be deleted eventually.
 */

#include "hbox.h"
#include "rect-box.h"
#include "text-box.h"
#include "grid-renderer.h"

// [[Rcpp::export]]
RObject test_hbox(CharacterVector tokens, double box_width, double x, double y, List gp) {
  // build list of grob boxes
  NodeList nodes;

  int i_width = 0;
  for (auto i_token = tokens.begin(); i_token != tokens.end(); i_token++) {
    nodes.push_back(NodePtr(new TextBox<GridRenderer>(*i_token, gp, 0)));
    i_width++;
  }

  TextDetails td = GridRenderer::text_details("abc", gp);
  Length hspacing = td.space;
  Length vspacing = 1.2*(td.ascent + td.descent);

  HBox<GridRenderer> hb(nodes, vspacing, hspacing);
  hb.calc_layout(box_width);

  RectBox<GridRenderer> rb(box_width, 200, Margin(0, -10, -150, -10), Margin(), gp);

  GridRenderer rd;
  rb.render(rd, x, y);
  hb.render(rd, x, y);
  return rd.collect_grobs();
}

