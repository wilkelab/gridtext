#ifndef TEXT_BOX_H
#define TEXT_BOX_H

#include <Rcpp.h>
using namespace Rcpp;

#include "layout.h"

// A box holding a single R grob
template <class Renderer>
class TextBox : public Box<Renderer> {
private:
  String m_label;
  typename Renderer::GraphicsContext m_gp;
  Length m_width;
  Length m_ascent;
  Length m_descent;
  Length m_voff;
  // position of the box in enclosing box, modulo vertical offset (voff),
  // which gets added to m_y;
  // the box reference point is the leftmost point of the baseline.
  Length m_x, m_y;

public:
  TextBox(String label, const typename Renderer::GraphicsContext &gp, Length voff = 0) :
    m_label(label), m_gp(gp), m_width(0), m_ascent(0), m_descent(0), m_voff(voff),
    m_x(0), m_y(0) {
    /*
    Environment env = Environment::namespace_env("gridtext");
    Function text_details = env["text_details"];
    List info = text_details(
      _["label"] = label, _["fontfamily"] = "",
      _["fontface"] = "plain", _["fontsize"] = 12
    );
    m_width = info["width_pt"];
    m_ascent = info["ascent_pt"];
    m_descent = info["descent_pt"];
     */
  }
  ~TextBox() {};

  Length width() { return m_width; }
  Length ascent() { return m_ascent; }
  Length descent() { return m_descent; }
  Length voff() { return m_voff; }

  // nothing to be done for a text box
  void calc_layout(Length, Length) {;}

  // place box in internal coordinates used in enclosing box
  void place(Length x, Length y) {
    m_x = x;
    m_y = y;
  }

  // render into absolute coordinates, using the reference coordinates
  // from the enclosing box
  void render(Renderer &r, Length xref, Length yref) {
    Length x = m_x + xref;
    Length y = m_y + m_voff + yref;

    r.text(m_label, x, y, m_gp);
      // String color = "#000000",
      // double fontsize = 12, String fontface = "plain", String fontfamily = "")
  }
};

#endif
