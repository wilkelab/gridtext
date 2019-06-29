#include <Rcpp.h>
using namespace Rcpp;

#include "grid.h"
#include "length-type.h"
#include "layout.h"

// A box holding a single R grob
class GrobBox : public Box {
private:
  RObject m_grob;
  length_type m_width;
  length_type m_ascent;
  length_type m_descent;
  length_type m_voff;
  // position of the box in enclosing box, modulo vertical offset (voff),
  // which gets added to m_y;
  // the box reference point is the leftmost point of the baseline.
  length_type m_x, m_y;

public:
  GrobBox(RObject grob, length_type width = 0, length_type ascent = 0,
          length_type descent = 0, length_type voff = 0) :
    m_grob(grob), m_width(width), m_ascent(ascent), m_descent(descent), m_voff(voff),
    m_x(0), m_y(0) {}
  ~GrobBox() {};

  length_type width() { return m_width; }
  length_type ascent() { return m_ascent; }
  length_type descent() { return m_descent; }
  length_type voff() { return m_voff; }

  // nothing to be done for a box that contains a grob
  void calc_layout(length_type, length_type) {;}

  // place box in internal coordinates used in enclosing box
  void place(length_type x, length_type y) {
    m_x = x;
    m_y = y;
  }

  // render into absolute coordinates, using the reference coordinates
  // from the enclosing box
  RObject render(length_type xref, length_type yref) {
    length_type x = m_x + xref;
    length_type y = m_y + m_voff + yref;

    return set_grob_coords(m_grob, unit_pt(x), unit_pt(y));
  }
};

class HBox : public Box {
private:
  node_list m_nodes;
  length_type m_width;
  length_type m_ascent;
  length_type m_descent;
  length_type m_yoff;
  length_type m_x, m_y;  // calculated left baseline corner of the box after layouting

public:
  HBox(const node_list& nodes) :
    m_nodes(nodes), m_width(0), m_ascent(0), m_descent(0), m_yoff(0),
    m_x(0), m_y(0) {}
  ~HBox() {};

  length_type width() { return m_width; }
  length_type ascent() { return m_ascent; }
  length_type descent() { return m_descent; }
  length_type yoff() { return m_yoff; }

  void calc_layout(length_type width = 0, length_type = 0) {
    // hardcoded for now
    length_type linespacing = 14;

    // x and y offset as we layout
    length_type x_off = 0, y_off = 0;

    for (auto i_node = m_nodes.begin(); i_node != m_nodes.end(); i_node++) {
      node_type nt = (*i_node)->type();

      if (nt == box) {
        auto b = static_pointer_cast<Box>(*i_node);
        b->calc_layout(width);
        b->place(x_off, y_off);
        x_off += b->width();
        if (x_off > width) { // simple wrapping, no fancy logic
          x_off = 0;
          y_off = y_off - linespacing;
        } else {
          // add space, this needs to be replaced by glue
          x_off += 4; // hardcoded to 4 pts
        }
      } else if (nt == glue) {
        // not implemented
      }
    }
  }

  void place(length_type x, length_type y) {
    m_x = x;
    m_y = y;
  }

  virtual RObject render(length_type xref, length_type yref) {
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
