#ifndef TEXT_BOX_H
#define TEXT_BOX_H

#include <Rcpp.h>
using namespace Rcpp;

#include "layout.h"

// A box holding a single text label
template <class Renderer>
class TextBox : public Box<Renderer> {
private:
  CharacterVector m_label;
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
  TextBox(const CharacterVector &label, const typename Renderer::GraphicsContext &gp, Length voff = 0) :
    m_label(label), m_gp(gp), m_width(0), m_ascent(0), m_descent(0), m_voff(voff),
    m_x(0), m_y(0) {}
  ~TextBox() {}

  Length width() { return m_width; }
  Length ascent() { return m_ascent; }
  Length descent() { return m_descent; }
  Length voff() { return m_voff; }

  // width and height are only defined once `calc_layout()` has been called
  void calc_layout(Length, Length) {
    TextDetails td = Renderer::text_details(m_label, m_gp);
    m_width = td.width;
    m_ascent = td.ascent;
    m_descent = td.descent;
  }

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
  }
};

#endif
