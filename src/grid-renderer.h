#ifndef GRID_RENDERER_H
#define GRID_RENDERER_H

#include <Rcpp.h>
using namespace Rcpp;

#include <string>
using namespace std;

#include "grid.h"
#include "length.h"

class GridRenderer {
private:
  List m_grobs;
  Environment m_grid_env;

public:
  GridRenderer() {
    m_grid_env = Environment::namespace_env("grid");
  }

  void text(String label, Length x, Length y, String color = "#000000",
            double fontsize = 12, String fontface = "plain", String fontfamily = "") {
    NumericVector xv(1, x), yv(1, y);
    NumericVector sizev(1, fontsize);

    // make gp
    Function gpar = m_grid_env["gpar"];
    List gp = gpar(_["col"] = color, _["fontfamily"] = fontfamily, _["fontface"] = fontface, _["fontsize"] = sizev);

    m_grobs.push_back(text_grob(CharacterVector(label), xv, yv, gp));
  }

  void rect(Length x, Length y, Length width, Length height, String color = "#000000",
            String fill = NA_STRING, int linetype = 1, double linewidth = 1, Length r = 0,
            String linejoin = "round", double linemitre = 1) {
    // skip drawing if nothing would show anyways
    if (fill == NA_STRING && (color == NA_STRING || linetype == 0)) {
      return;
    }

    NumericVector xv(1, x), yv(1, y), widthv(1, width), heightv(1, height);
    NumericVector lwdv(1, linewidth), mitrev(1, linemitre);
    IntegerVector ltyv(1, linetype);

    // make gp
    Function gpar = m_grid_env["gpar"];
    List gp = gpar(_["col"] = color, _["fill"] = fill, _["lty"] = ltyv, _["lwd"] = lwdv,
                   _["linejoin"] = linejoin, _["linemitre"] = mitrev);

    // draw simple rect grob or rounded rect grob depending on provided radius
    if (r < 0.01) {
      m_grobs.push_back(rect_grob(xv, yv, widthv, heightv, gp));
    } else {
      NumericVector rv(1, r);
      m_grobs.push_back(roundrect_grob(xv, yv, widthv, heightv, rv, gp));
    }
  }


  List collect_grobs() {
    List out = m_grobs;
    // turn list into gList to keep grid happy
    out.attr("class") = "gList";
    m_grobs = List();

    return out;
  }
};

#endif
