#ifndef VBOX_H
#define VBOX_H

#include <Rcpp.h>
using namespace Rcpp;

#include "layout.h"

/* The VBox class takes a list of boxes and lays them out
 * horizontally, breaking lines if necessary. The reference point
 * is the left end point of the baseline of the last line.
 */

template <class Renderer>
class VBox : public Box<Renderer> {
private:
  NodeList m_nodes;
  Length m_width;
  Length m_height;
  // reference point of the box
  Length m_x, m_y;
  // justification of box relative to reference
  Length m_hjust, m_vjust;

public:
  VBox(const NodeList& nodes, double hjust, double vjust) :
    m_nodes(nodes),
    m_width(0), m_height(0),
    m_x(0), m_y(0),
    m_hjust(hjust), m_vjust(vjust) {}
  ~VBox() {};

  Length width() { return m_width; }
  Length ascent() { return m_height; }
  Length descent() { return 0; }
  Length voff() { return 0; }

  void calc_layout(Length width_hint, Length height_hint) {
    // y offset as we layout
    Length y_off = 0;
    // calculated box width
    Length width = 0;

    for (auto i_node = m_nodes.begin(); i_node != m_nodes.end(); i_node++) {
      LayoutNode::NodeType nt = (*i_node)->type();

      if (nt == LayoutNode::box) {
        auto b = static_pointer_cast<Box<Renderer> >(*i_node);
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
    }
    m_width = width;
    m_height = -y_off;
  }

  void place(Length x, Length y) {
    m_x = x;
    m_y = y;
  }

  void render(Renderer &r, Length xref, Length yref) {
    // render all grobs in the list
    for (auto i_node = m_nodes.begin(); i_node != m_nodes.end(); i_node++) {
      if ((*i_node)->type() == LayoutNode::box) {
        static_pointer_cast<Box<Renderer> >(*i_node)->render(
            r,
            xref + m_x - m_hjust*m_width,
            yref + m_height + m_y - m_vjust*m_height
        );
      }
    }
  }
};

#endif
