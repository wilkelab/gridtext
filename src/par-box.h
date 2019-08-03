#ifndef PAR_BOX_H
#define PAR_BOX_H

#include <Rcpp.h>
using namespace Rcpp;

#include <iostream>

#include "grid.h"
#include "layout.h"
//#include "glue.h"
//#include "penalty.h"
#include "line-breaker.h"


/* The ParBox class takes a list of boxes and lays them out
 * horizontally, breaking lines if necessary. The reference point
 * is the left end point of the baseline of the last line.
 */

template <class Renderer>
class ParBox : public Box<Renderer> {
private:
  BoxList<Renderer> m_nodes;
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
  ParBox(const BoxList<Renderer>& nodes, Length vspacing, Length hspacing) :
    m_nodes(nodes), m_vspacing(vspacing), m_hspacing(hspacing),
    m_width(0), m_ascent(0), m_descent(0), m_voff(0),
    m_x(0), m_y(0) {
  }
  ~ParBox() {};

  Length width() { return m_width; }
  Length ascent() { return m_ascent; }
  Length descent() { return m_descent; }
  Length voff() { return m_voff; }

  /*
  void calc_layout_old(Length width_hint, Length height_hint) {
    // x and y offset as we layout
    Length x_off = 0, y_off = 0;

    int lines = 0;
    Length ascent = 0;
    Length descent = 0;

    for (auto i_node = m_nodes.begin(); i_node != m_nodes.end(); i_node++) {
      NodeType nt = (*i_node)->type();

      if (nt == NodeType::box) {
        // we propagate width and height hints to all child nodes,
        // in case they are useful there
        (*i_node)->calc_layout(width_hint, height_hint);
        if (x_off + (*i_node)->width() > width_hint) { // simple wrapping, no fancy logic
          x_off = 0;
          y_off = y_off - m_vspacing;
          lines += 1;
          descent = 0; // reset descent when starting new line
          // we don't reset ascent because we only record it for the first line
        }
        (*i_node)->place(x_off, y_off);
        x_off += (*i_node)->width();
        // add space, this needs to be replaced by glue
        x_off += m_hspacing;

        // record ascent and descent
        if ((*i_node)->descent() > descent) {
          descent = (*i_node)->descent();
        }
        if (lines == 0 && (*i_node)->ascent() > ascent) {
          ascent = (*i_node)->ascent();
        }
      } else if (nt == NodeType::glue) {
        // not implemented
      }
    }
    m_multiline_shift = lines*m_vspacing; // multi-line boxes need to be shifted upwards
    m_ascent = ascent + m_multiline_shift;
    m_descent = descent;
    m_width = width_hint;
  }
  */

  void calc_layout(Length width_hint, Length height_hint) {
    // first make sure all child nodes are in a defined state
    // we propagate width and height hints to all child nodes,
    // in case they are useful there
    for (auto i_node = m_nodes.begin(); i_node != m_nodes.end(); i_node++) {
      (*i_node)->calc_layout(width_hint, height_hint);
    }

    // calculate line breaks
    vector<Length> line_lengths = {width_hint};
    LineBreaker<Renderer> lb(m_nodes, line_lengths);
    vector<LineBreakInfo> line_breaks;
    lb.compute_line_breaks(line_breaks);

    //for (size_t i = 0; i < line_breaks.size(); i++) {
    //  cout << i << " " << line_breaks[i].start << " " << line_breaks[i].end << endl;
    //}

    // now place all nodes according to line breaks
    Length x_off = 0, y_off = 0; // x and y offset as we layout

    int lines = 0;
    Length ascent = 0;
    Length descent = 0;

    for (auto i_line = line_breaks.begin(); i_line != line_breaks.end(); i_line++) {
      // reset variables for new line
      x_off = 0;
      descent = 0;

      // loop over all boxes in each line and place
      for (size_t i = i_line->start; i != i_line->end; i++) {
        auto node = m_nodes[i];
        node->place(x_off, y_off);
        x_off += node->width();

        // record ascent and descent
        Length descent_new = node->descent();
        if (descent_new > descent) {
          descent = descent_new;
        }
        if (lines == 0) {
          Length ascent_new = node->ascent();
          if (ascent_new > ascent) {
            ascent = ascent_new;
          }
        }
      }

      // advance line
      lines += 1;
      y_off = y_off - m_vspacing;
    }

    if (lines > 0) { // at least one line?
      m_multiline_shift = (lines-1)*m_vspacing; // multi-line boxes need to be shifted upwards
      m_ascent = ascent + m_multiline_shift;
      m_descent = descent;
      m_width = width_hint;
    } else {
      m_multiline_shift = 0;
      m_ascent = 0;
      m_descent = 0;
      m_width = width_hint;
    }
  }

  void place(Length x, Length y) {
    m_x = x;
    m_y = y;
  }

  void render(Renderer &r, Length xref, Length yref) {
    // render all grobs in the list
    for (auto i_node = m_nodes.begin(); i_node != m_nodes.end(); i_node++) {
      (*i_node)->render(r, xref + m_x, yref + m_voff + m_y + m_multiline_shift);
    }
  }
};

#endif
