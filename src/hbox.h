#ifndef HBOX_H
#define HBOX_H

#include <Rcpp.h>
using namespace Rcpp;

#include "grid.h"
#include "gridtext_types.h"

// A box holding a single R grob
class GrobBox : public Box {
private:
  RObject m_grob;
  Length m_width;
  Length m_ascent;
  Length m_descent;
  Length m_voff;
  // position of the box in enclosing box, modulo vertical offset (voff),
  // which gets added to m_y;
  // the box reference point is the leftmost point of the baseline.
  Length m_x, m_y;

public:
  GrobBox(RObject grob, Length width = 0, Length ascent = 0,
          Length descent = 0, Length voff = 0) :
    m_grob(grob), m_width(width), m_ascent(ascent), m_descent(descent), m_voff(voff),
    m_x(0), m_y(0) {}
  ~GrobBox() {};

  Length width() { return m_width; }
  Length ascent() { return m_ascent; }
  Length descent() { return m_descent; }
  Length voff() { return m_voff; }

  // nothing to be done for a box that contains a grob
  void calc_layout(Length, Length) {;}

  // place box in internal coordinates used in enclosing box
  void place(Length x, Length y) {
    m_x = x;
    m_y = y;
  }

  // render into absolute coordinates, using the reference coordinates
  // from the enclosing box
  RObject render(Length xref, Length yref) {
    Length x = m_x + xref;
    Length y = m_y + m_voff + yref;

    return set_grob_coords(m_grob, unit_pt(x), unit_pt(y));
  }
};

class HBox : public Box {
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
        auto b = static_pointer_cast<Box>(*i_node);
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

  virtual RObject render(Length xref, Length yref) {
    List out;

    // render all grobs in the list
    for (auto i_node = m_nodes.begin(); i_node != m_nodes.end(); i_node++) {
      if ((*i_node)->type() == box) {
        out.push_back(static_pointer_cast<Box>(*i_node)->render(xref, yref));
      }
    }

    // turn list into gList to keep grid happy
    out.attr("class") = "gList";
    return out;
  }
};

#endif
