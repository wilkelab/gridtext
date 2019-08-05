#ifndef VBOX_H
#define VBOX_H

#include <Rcpp.h>
using namespace Rcpp;

#include "layout.h"

/* The VBox class takes a list of boxes and lays them out
 * horizontally, breaking lines if necessary. The reference point
 * is the lower left corner of the box.
 */

template <class Renderer>
class VBox : public Box<Renderer> {
private:
  BoxList<Renderer> m_nodes;
  Length m_width;
  Length m_height;
  SizePolicy m_width_policy; // width policy; height policy is always "native"
  // reference point of the box
  Length m_x, m_y;
  // justification of box relative to reference
  Length m_hjust, m_vjust;
  double m_rel_width; // used to store relative width when needed

public:
  VBox(const BoxList<Renderer>& nodes, Length width = 0, double hjust = 0, double vjust = 1,
       SizePolicy width_policy = SizePolicy::native) :
    m_nodes(nodes),
    m_width(width), m_height(0),
    m_width_policy(width_policy),
    m_x(0), m_y(0),
    m_hjust(hjust), m_vjust(vjust),
    m_rel_width(0) {
    if (m_width_policy == SizePolicy::relative) {
      m_rel_width = m_width/100;
    }
  }
  ~VBox() {};

  Length width() { return m_width; }
  Length ascent() { return m_height; }
  Length descent() { return 0; }
  Length voff() { return 0; }

  void calc_layout(Length width_hint, Length height_hint) {
    switch(m_width_policy) {
    case SizePolicy::expand:
      m_width = width_hint;
      break;
    case SizePolicy::relative:
      m_width = width_hint * m_rel_width;
      width_hint = m_width;
      break;
    case SizePolicy::fixed:
      width_hint = m_width;
      break;
    case SizePolicy::native:
    default:
      // nothing to be done for native policy, will be handled below
      break;
    }

    // y offset as we layout
    Length y_off = 0;
    // calculated box width
    Length width = 0;

    for (auto i_node = m_nodes.begin(); i_node != m_nodes.end(); i_node++) {
      auto b = (*i_node);
      // we propagate width and height hints to all child nodes,
      // in case they are useful there
      b->calc_layout(width_hint, height_hint);
      y_off -= b->ascent();
      // place node, ignoring any vertical offset from baseline
      // (we stack boxes vertically, baselines don't matter here)
      b->place(0, y_off - b->voff());
      y_off -= b->descent(); // account for box descent if any

      // record width
      if (b->width() > width) {
        width = b->width();
      }
    }

    if (m_width_policy == SizePolicy::native) {
      // we record the calculated width for native width policy
      // in all other cases, it's already set
      m_width = width;
    }
    m_height = -y_off;
  }

  void place(Length x, Length y) {
    m_x = x;
    m_y = y;
  }

  void render(Renderer &r, Length xref, Length yref) {
    // render all grobs in the list
    for (auto i_node = m_nodes.begin(); i_node != m_nodes.end(); i_node++) {
      (*i_node)->render(
            r,
            xref + m_x - m_hjust*m_width,
            yref + m_height + m_y - m_vjust*m_height
      );
    }
  }
};

#endif
