#ifndef GLUE_H
#define GLUE_H

#include <Rcpp.h>
using namespace Rcpp;

#include "layout.h"

template <class Renderer> class Glue : public BoxNode<Renderer> {
protected:
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

  Length default_width() {return m_width;}
  Length stretch() {return m_stretch;}
  Length shrink() {return m_shrink;}

  void set_r(double r) {m_r = r;}

  // calculate the width of the glue for given adjustment ratio
  Length compute_width(double r) {
    if (r < 0) {
      return m_width + r*m_shrink;
    } else {
      return m_width + r*m_stretch;
    }
  }
};


// Glue corresponding to a regular space in text
template <class Renderer>
class RegularSpaceGlue : public Glue<Renderer> {
private:
  typename Renderer::GraphicsContext m_gp;
  double m_stretch_ratio, m_shrink_ratio; // used to convert width of space character into stretch and shrink

  // pull protected members from superclass explicitly into scope
  using Glue<Renderer>::m_width;
  using Glue<Renderer>::m_stretch;
  using Glue<Renderer>::m_shrink;

public:
  RegularSpaceGlue(const typename Renderer::GraphicsContext &gp,
                   double stretch_ratio = 0.5, double shrink_ratio = 0.333333) :
    m_gp(gp), m_stretch_ratio(stretch_ratio), m_shrink_ratio(shrink_ratio) {}
  ~RegularSpaceGlue() {}

  // width, stretch, and shrink are only defined once `calc_layout()` has been called
  void calc_layout(Length, Length) {
    TextDetails td = Renderer::text_details(" ", m_gp);
    m_width = td.space;
    m_stretch = m_width * m_stretch_ratio;
    m_shrink = m_width * m_shrink_ratio;
  }
};

#endif
