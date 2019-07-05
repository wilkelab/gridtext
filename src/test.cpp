/* Various test routines.
 * To be deleted eventually.
 */

#include "par-box.h"
#include "rect-box.h"
#include "text-box.h"
#include "grid-renderer.h"

// [[Rcpp::export]]
RObject test_par_box(CharacterVector tokens, double box_width, double x, double y, List gp) {
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

  NodePtr pb(new ParBox<GridRenderer>(nodes, vspacing, hspacing));

  RectBox<GridRenderer> rb(pb, box_width, 200, Margin(0, 0, 0, 0), Margin(10, 10, 10, 10), gp,
                           0, 0.5, LayoutNode::fixed, LayoutNode::native);
  rb.calc_layout(box_width, 200);

  GridRenderer rd;
  rb.render(rd, x, y);
  return rd.collect_grobs();
}

