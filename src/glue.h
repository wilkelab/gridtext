#ifndef GLUE_H
#define GLUE_H

#include <Rcpp.h>
using namespace Rcpp;

#include "layout.h"

template <class Renderer> class Glue : public BoxNode<Renderer> {
  Length m_width, m_stretch, m_shrink;

public:
  Glue(Length width = 0, Length stretch = 0, Length shrink = 0) :
  m_width(width), m_stretch(stretch), m_shrink(shrink) {}
  virtual ~Glue() {}
  NodeType type() {return NodeType::glue;}

  Length width() {return m_width;}
  Length ascent() {return 0;}
  Length descent() {return 0;}
  Length voff() {return 0;}

  void calc_layout(Length, Length) {}
  void place(Length, Length) {}
  void render(Renderer &, Length, Length) {}
};

#endif
