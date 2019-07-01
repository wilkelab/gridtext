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

  void text(CharacterVector label, Length x, Length y, CharacterVector color = "#000000",
            double fontsize = 12, CharacterVector fontface = "plain", CharacterVector fontfamily = "") {
    NumericVector xv(1, x), yv(1, y);
    NumericVector sizev(1, fontsize);

    // make gp
    Function gpar = m_grid_env["gpar"];
    List gp = gpar(_["col"] = color, _["fontfamily"] = fontfamily, _["fontface"] = fontface, _["fontsize"] = sizev);

    m_grobs.push_back(text_grob(label, xv, yv, gp));
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
