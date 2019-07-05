#ifndef VBOX_H
#define VBOX_H

#include <Rcpp.h>
using namespace Rcpp;

#include "grid.h"
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
/*
        // x and y offset as we layout
    Length x_off = 0, y_off = 0;

    int lines = 0;
    Length ascent = 0;
    Length descent = 0;

    for (auto i_node = m_nodes.begin(); i_node != m_nodes.end(); i_node++) {
      LayoutNode::NodeType nt = (*i_node)->type();

      if (nt == LayoutNode::box) {
        auto b = static_pointer_cast<Box<Renderer> >(*i_node);
        // we propagate width and height hints to all child nodes,
        // in case they are useful there
        b->calc_layout(width_hint, height_hint);
        if (x_off + b->width() > width_hint) { // simple wrapping, no fancy logic
          x_off = 0;
          y_off = y_off - m_vspacing;
          lines += 1;
          descent = 0; // reset descent when starting new line
          // we don't reset ascent because we only record it for the first line
        }
        b->place(x_off, y_off);
        x_off += b->width();
        // add space, this needs to be replaced by glue
        x_off += m_hspacing;

        // record ascent and descent
        if (b->descent() > descent) {
          descent = b->descent();
        }
        if (lines == 0 && b->ascent() > ascent) {
          ascent = b->ascent();
        }
      } else if (nt == LayoutNode::glue) {
        // not implemented
      }
    }
    m_multiline_shift = lines*m_vspacing; // multi-line boxes need to be shifted upwards
    m_ascent = ascent + m_multiline_shift;
    m_descent = descent;
    m_width = width_hint;
 */
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
            yref + m_y - m_vjust*m_height
        );
      }
    }
  }
};

#endif
