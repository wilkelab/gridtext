#ifndef PAR_BOX_H
#define PAR_BOX_H

#include <Rcpp.h>
using namespace Rcpp;

#include "grid.h"
#include "layout.h"

/* The ParBox class takes a list of boxes and lays them out
 * horizontally, breaking lines if necessary. The reference point
 * is the left end point of the baseline of the last line.
 */

template <class Renderer>
class ParBox : public Box<Renderer> {
private:
  NodeList m_nodes;
  Length m_vspacing;
  Length m_hspacing;
  Length m_width;
  Length m_ascent;
  Length m_descent;
  Length m_voff;
  // vertical shift if paragraph contains more than one line; is used to make sure the
  // bottom line in the box is used as the box baseline (all lines above are folded
  // into the ascent)
  Length m_multiline_shift;
  // calculated left baseline corner of the box after layouting
  Length m_x, m_y;

public:
  ParBox(const NodeList& nodes, Length vspacing, Length hspacing) :
    m_nodes(nodes), m_vspacing(vspacing), m_hspacing(hspacing),
    m_width(0), m_ascent(0), m_descent(0), m_voff(0),
    m_x(0), m_y(0) {}
  ~ParBox() {};

  Length width() { return m_width; }
  Length ascent() { return m_ascent; }
  Length descent() { return m_descent; }
  Length voff() { return m_voff; }

  void calc_layout(Length width_hint = 0, Length = 0) {
    // x and y offset as we layout
    Length x_off = 0, y_off = 0;

    int lines = 0;
    Length ascent = 0;
    Length descent = 0;
    Length width = 0;

    for (auto i_node = m_nodes.begin(); i_node != m_nodes.end(); i_node++) {
      LayoutNode::NodeType nt = (*i_node)->type();

      if (nt == LayoutNode::box) {
        auto b = static_pointer_cast<Box<Renderer> >(*i_node);
        b->calc_layout(width_hint);
        if (x_off + b->width() > width_hint) { // simple wrapping, no fancy logic
          x_off = 0;
          y_off = y_off - m_vspacing;
          lines += 1;
          descent = 0; // reset descent when starting new line
          // we don't reset ascent because we only record it for the first line
        }
        b->place(x_off, y_off);
        x_off += b->width();
        if (x_off > width) { // record box width
          width = x_off;
        }
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
    m_width = width;
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
            xref + m_x,
            yref + m_voff + m_y + m_multiline_shift
        );
      }
    }
  }
};

#endif
