/* Various test routines.
 * To be deleted eventually.
 */

#include "text-box.h"
#include "hbox.h"
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

  GridRenderer rd;
  hb.render(rd, x, y);
  return rd.collect_grobs();
}

