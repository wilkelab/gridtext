#ifndef PENALTY_H
#define PENALTY_H

#include <Rcpp.h>
using namespace Rcpp;

#include "layout.h"

template <class Renderer> class Penalty : public BoxNode<Renderer> {
private:
  int m_penalty;
  Length m_width;
  bool m_flagged;

public:
  static constexpr int infinity = 10000; // maximum penalty

  Penalty(int penalty = 0, Length width = 0, bool flagged = false) :
    m_penalty(penalty), m_width(width), m_flagged(flagged) {}
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

// Penalty that causes a forced break
template <class Renderer> class ForcedBreakPenalty : public Penalty<Renderer> {
public:
  ForcedBreakPenalty() : Penalty<Renderer>(-1*Penalty<Renderer>::infinity) {}
};

// Penalty that prevents a break at this position
template <class Renderer> class NeverBreakPenalty : public Penalty<Renderer> {
public:
  NeverBreakPenalty() : Penalty<Renderer>(Penalty<Renderer>::infinity) {}
};


#endif
