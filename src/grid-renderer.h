#ifndef GRID_RENDERER_H
#define GRID_RENDERER_H

#include <Rcpp.h>
using namespace Rcpp;

#include <vector>

#include "grid.h"
#include "length.h"
#include "layout.h"

class GridRenderer {
public:
  // GridRenderer stores its graphics context in a grid gpar() list
  typedef List GraphicsContext;

private:
  vector<RObject> m_grobs;

  RObject gpar_lookup(List gp, const char* element) {
    if (!gp.containsElementNamed(element)) {
      return R_NilValue;
    } else {
      return gp[element];
    }
  }

public:
  GridRenderer() {
  }

  static TextDetails text_details(const CharacterVector &label, GraphicsContext gp) {
    // call R function to look up text info
    Environment env = Environment::namespace_env("gridtext");

    Function td = env["text_details"];
    List info = td(label, gp);
    RObject width_pt = info["width_pt"];
    RObject ascent_pt = info["ascent_pt"];
    RObject descent_pt = info["descent_pt"];
    RObject space_pt = info["space_pt"];
    return TextDetails(
      NumericVector(width_pt)[0],
      NumericVector(ascent_pt)[0],
      NumericVector(descent_pt)[0],
      NumericVector(space_pt)[0]
    );
  }

  void text(const CharacterVector &label, Length x, Length y, const GraphicsContext &gp) {
    m_grobs.push_back(text_grob(label, NumericVector(1, x), NumericVector(1, y), gp));
  }

  void raster(RObject image, Length x, Length y, Length width, Length height, bool interpolate = true,
              const GraphicsContext &gp = R_NilValue) {
    if (!image.isNULL()) {
      m_grobs.push_back(
        raster_grob(
          image, NumericVector(1, x), NumericVector(1, y),
          NumericVector(1, width), NumericVector(1, height), LogicalVector(1, interpolate, gp)
        )
      );
    }
  }

  void rect(Length x, Length y, Length width, Length height, const GraphicsContext &gp, Length r = 0) {
    // skip drawing if nothing would show anyways

    // default assumption is we don't have a fill color but we do have line color and type
    bool have_fill_col = false;
    bool have_line_col = true;
    bool have_line_type = true;
    RObject fill_obj = gpar_lookup(gp, "fill");

    if (!fill_obj.isNULL()) {
      CharacterVector fill(fill_obj);
      if (fill.size() > 0 && !CharacterVector::is_na(fill[0])) {
        have_fill_col = true;
      }
    }

    // if we have a fill color, further checks don't matter
    if (!have_fill_col) {
      RObject color = gpar_lookup(gp, "col");
      if (!color.isNULL()) {
        CharacterVector col(color);
        if (col.size() == 0 || CharacterVector::is_na(col[0])) {
          have_line_col = false;
        }
      }
    }

    // if we don't have a fill color but do have a line color,
    // need to check line type
    if (!have_fill_col && have_line_col) {
      RObject linetype = gpar_lookup(gp, "lty");
      if (!linetype.isNULL()) {
        NumericVector lty(linetype);
        if (lty.size() == 0 || lty[0] == 0) {
          have_line_type = false;
        }
      }
    }

    if (!have_fill_col && (!have_line_col || !have_line_type)) {
      return;
    }

    // now that we know we should draw, go ahead

    NumericVector xv(1, x), yv(1, y), widthv(1, width), heightv(1, height);

    // draw simple rect grob or rounded rect grob depending on provided radius
    if (r < 0.01) {
      m_grobs.push_back(rect_grob(xv, yv, widthv, heightv, gp));
    } else {
      NumericVector rv(1, r);
      m_grobs.push_back(roundrect_grob(xv, yv, widthv, heightv, rv, gp));
    }
  }


  List collect_grobs() {
    // turn vector of grobs into list; doing it this way avoids
    // List.push_back() which is slow.
    List out(m_grobs.size());

    size_t i = 0;
    for (auto i_grob = m_grobs.begin(); i_grob != m_grobs.end(); i_grob++) {
      out[i] = *i_grob;
      i++;
    }
    // clear internal grobs list; the renderer is reset with each collect_grobs() call
    m_grobs.clear();

    // turn list into gList to keep grid happy
    out.attr("class") = "gList";

    return out;
  }
};

#endif
