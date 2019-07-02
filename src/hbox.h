#ifndef HBOX_H
#define HBOX_H

#include <Rcpp.h>
using namespace Rcpp;

#include "grid.h"
#include "gridtext_types.h"
#include "layout.h"

template <class Renderer = GridRenderer>
class HBox : public Box<Renderer> {
private:
  NodeList m_nodes;
  Length m_vspacing;
  Length m_hspacing;
  Length m_width;
  Length m_ascent;
  Length m_descent;
  Length m_voff;
  Length m_x, m_y;  // calculated left baseline corner of the box after layouting

public:
  HBox(const NodeList& nodes, Length vspacing, Length hspacing) :
    m_nodes(nodes), m_vspacing(vspacing), m_hspacing(hspacing),
    m_width(0), m_ascent(0), m_descent(0), m_voff(0),
    m_x(0), m_y(0) {}
  ~HBox() {};

  Length width() { return m_width; }
  Length ascent() { return m_ascent; }
  Length descent() { return m_descent; }
  Length voff() { return m_voff; }

  void calc_layout(Length width_hint = 0, Length = 0) {
    // x and y offset as we layout
    Length x_off = 0, y_off = 0;

    for (auto i_node = m_nodes.begin(); i_node != m_nodes.end(); i_node++) {
      LayoutNode::NodeType nt = (*i_node)->type();

      if (nt == LayoutNode::box) {
        auto b = static_pointer_cast<Box<Renderer> >(*i_node);
        b->calc_layout(width_hint);
        b->place(x_off, y_off);
        x_off += b->width();
        if (x_off > width_hint) { // simple wrapping, no fancy logic
          x_off = 0;
          y_off = y_off - m_vspacing;
        } else {
          // add space, this needs to be replaced by glue
          x_off += m_hspacing;
        }
      } else if (nt == LayoutNode::glue) {
        // not implemented
      }
    }
  }

  void place(Length x, Length y) {
    m_x = x;
    m_y = y;
  }

  void render(Renderer &r, Length xref, Length yref) {
    // render all grobs in the list
    for (auto i_node = m_nodes.begin(); i_node != m_nodes.end(); i_node++) {
      if ((*i_node)->type() == LayoutNode::box) {
        static_pointer_cast<Box<Renderer> >(*i_node)->render(r, xref, yref);
      }
    }
  }
};

#endif
