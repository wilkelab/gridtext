#ifndef GLUE_H
#define GLUE_H

#include <Rcpp.h>
using namespace Rcpp;

#include "layout.h"

template <class Renderer> class Glue : public BoxNode<Renderer> {
  Length m_width, m_stretch, m_shrink;
  double m_r; // adjustment ratio

public:
  static constexpr double infinity = 1e9; // maximum adjustment ratio

  Glue(Length width = 0, Length stretch = 0, Length shrink = 0) :
  m_width(width), m_stretch(stretch), m_shrink(shrink), m_r(0) {}
  virtual ~Glue() {}
  NodeType type() {return NodeType::glue;}

  Length width() {return compute_width(m_r);}
  Length ascent() {return 0;}
  Length descent() {return 0;}
  Length voff() {return 0;}

  void calc_layout(Length, Length) {}
  void place(Length, Length) {}
  void render(Renderer &, Length, Length) {}

  /* The remaining functions are not virtual,
   * don't override.
   */

  Length stretch() {return m_stretch;}
  Length shrink() {return m_shrink;}

  // calculate the width of the glue for given adjustment ratio
  Length compute_width(double r) {
    if (r < 0) {
      return m_width + r*m_shrink;
    } else {
      return m_width + r*m_stretch;
    }
  }
};

#endif
