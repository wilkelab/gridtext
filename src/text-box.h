#ifndef TEXT_BOX_H
#define TEXT_BOX_H

#include <Rcpp.h>
using namespace Rcpp;

#include "gridtext_types.h"
#include "layout.h"
#include "grid-renderer.h"

// A box holding a single R grob
template <class Renderer = GridRenderer>
class TextBox : public Box<Renderer> {
private:
  String m_label;
  Length m_width;
  Length m_ascent;
  Length m_descent;
  Length m_voff;
  // position of the box in enclosing box, modulo vertical offset (voff),
  // which gets added to m_y;
  // the box reference point is the leftmost point of the baseline.
  Length m_x, m_y;

public:
  TextBox(String label, Length voff = 0) :
    m_label(label), m_width(0), m_ascent(0), m_descent(0), m_voff(voff),
    m_x(0), m_y(0) {}
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

    r.text(m_label, x, y);
      // String color = "#000000",
      // double fontsize = 12, String fontface = "plain", String fontfamily = "")
  }
};

#endif
