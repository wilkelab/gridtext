#ifndef PENALTY_H
#define PENALTY_H

#include <Rcpp.h>
using namespace Rcpp;

#include "layout.h"

template <class Renderer> class Penalty : public BoxNode<Renderer> {
  Length m_width;
  int m_penalty;
  bool m_flagged;


public:
  static constexpr int infinity = 10000; // maximum penalty

  Penalty(Length width = 0, int penalty = 0, bool flagged = false) :
    m_width(width), m_penalty(penalty), m_flagged(flagged) {}
  virtual ~Penalty() {}
  NodeType type() {return NodeType::penalty;}

  Length width() {return m_width;}
  Length ascent() {return 0;}
  Length descent() {return 0;}
  Length voff() {return 0;}

  void calc_layout(Length, Length) {}
  void place(Length, Length) {}
  void render(Renderer &, Length, Length) {}

  /* The remaining functions are not virtual,
   * don't override.
   */

  int penalty() {return m_penalty;}
  bool flagged() {return m_flagged;}
};

#endif
