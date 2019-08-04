/* Various test routines.
 * To be deleted eventually.
 */

#include "par-box.h"
#include "rect-box.h"
#include "text-box.h"
#include "glue.h"
#include "grid-renderer.h"

// [[Rcpp::export]]
RObject test_par_box(CharacterVector tokens, double box_width, double x, double y, List gp) {
  List gp2 = List::create(_["fontsize"] = 40.);
  StringVector cl = {"gpar"};
  gp2.attr("class") = cl;

  // build list of grob boxes
  BoxList<GridRenderer> nodes;

  int i = 0;
  for (auto i_token = tokens.begin(); i_token != tokens.end(); i_token++) {
    nodes.push_back(BoxPtr<GridRenderer>(new TextBox<GridRenderer>(*i_token, gp, 0)));
    nodes.push_back(BoxPtr<GridRenderer>(new RegularSpaceGlue<GridRenderer>(gp)));
    i++;
    if (i == 15) {
      nodes.push_back(BoxPtr<GridRenderer>(new ForcedBreakPenalty<GridRenderer>()));
      nodes.push_back(BoxPtr<GridRenderer>(new TextBox<GridRenderer>("abc", gp2, 0)));
      nodes.push_back(BoxPtr<GridRenderer>(new RegularSpaceGlue<GridRenderer>(gp)));
    }
    if (i == 30) {
      nodes.push_back(BoxPtr<GridRenderer>(new ForcedBreakPenalty<GridRenderer>()));
      nodes.push_back(BoxPtr<GridRenderer>(new ForcedBreakPenalty<GridRenderer>()));
    }
  }

  TextDetails td = GridRenderer::text_details("abc", gp);
  Length vspacing = 1.2*(td.ascent + td.descent);

  BoxPtr<GridRenderer> pb(new ParBox<GridRenderer>(nodes, vspacing));

  RectBox<GridRenderer> rb(pb, box_width, 200, Margin(0, 0, 0, 0), Margin(10, 10, 10, 10), gp,
                           0, 0.5, SizePolicy::fixed, SizePolicy::native);
  rb.calc_layout(box_width, 200);

  GridRenderer rd;
  rb.render(rd, x, y);
  return rd.collect_grobs();
}

