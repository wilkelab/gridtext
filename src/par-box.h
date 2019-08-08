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
  Length m_width;
  Length m_ascent;
  Length m_descent;
  Length m_voff;
  SizePolicy m_width_policy;
  double m_hjust; // horizontal adjustment; can be used to override text adjustment
  bool m_use_hjust; // should text adjustment be overridden or not?
  // vertical shift if paragraph contains more than one line; is used to make sure the
  // bottom line in the box is used as the box baseline (all lines above are folded
  // into the ascent)
  Length m_multiline_shift;
  // calculated left baseline corner of the box after layouting
  Length m_x, m_y;

public:
  ParBox(const BoxList<Renderer>& nodes, Length vspacing, SizePolicy width_policy = SizePolicy::native,
         double hjust = 0, bool use_hjust = false) :
    m_nodes(nodes), m_vspacing(vspacing),
    m_width(0), m_ascent(0), m_descent(0), m_voff(0),
    m_width_policy(width_policy),
    m_hjust(hjust), m_use_hjust(use_hjust),
    m_multiline_shift(0), m_x(0), m_y(0) {
  }
  ~ParBox() {};

  Length width() { return m_width; }
  Length ascent() { return m_ascent; }
  Length descent() { return m_descent; }
  Length voff() { return m_voff; }

  void calc_layout(Length width_hint, Length height_hint) {
    // first make sure all child nodes are in a defined state
    // we propagate width and height hints to all child nodes,
    // in case they are useful there
    for (auto i_node = m_nodes.begin(); i_node != m_nodes.end(); i_node++) {
      (*i_node)->calc_layout(width_hint, height_hint);
    }

    // choose breaking parameters based on size policy
    bool word_wrap = true;
    if (m_width_policy == SizePolicy::native) {
      // for native policy, we don't wrap words and we allow lines to be arbitrarily long
      word_wrap = false;
      width_hint = Glue<Renderer>::infinity;
    }

    // calculate line breaks
    vector<Length> line_lengths = {width_hint};
    LineBreaker<Renderer> lb(m_nodes, line_lengths, word_wrap);
    vector<LineBreakInfo> line_breaks;
    lb.compute_line_breaks(line_breaks);

    // now get the true line length for native size policy,
    // by finding the longest line
    if (m_width_policy == SizePolicy::native) {
      width_hint = 0;
      for (auto i_line = line_breaks.begin(); i_line != line_breaks.end(); i_line++) {
        if (width_hint < i_line->width) {
          width_hint = i_line->width;
        }
      }
    }

    // now place all nodes according to line breaks
    Length x_off = 0, y_off = 0; // x and y offset as we layout

    int lines = 0;
    Length first_ascent = 0; // ascent of the first line
    Length descent = 0;

    for (auto i_line = line_breaks.begin(); i_line != line_breaks.end(); i_line++) {
      // reset x_off for new line, potentially overriding alignment
      if (m_use_hjust) {
        x_off = m_hjust*(width_hint - i_line->width);
      } else {
        x_off = 0;
      }

      // we first get the ascent of each box in the line, to make sure there is
      // vertical space if some boxes are very tall
      Length ascent = 0;
      for (size_t i = i_line->start; i != i_line->end; i++) {
        auto node = m_nodes[i];
        Length ascent_new = node->ascent() + node->voff();
        if (ascent_new > ascent) {
          ascent = ascent_new;
        }
      }
      if (lines == 0) { // are we rendering the first line?
        // yes, record ascent for first line
        first_ascent = ascent;
      } else {
        // no, adjust y_offset as needed
        if (ascent + descent > m_vspacing) {
          y_off = y_off - (ascent + descent);
        } else {
          y_off = y_off - m_vspacing;
        }
      }

      // reset descent for new line
      descent = 0;

      // now loop over all boxes in each line and place
      for (size_t i = i_line->start; i != i_line->end; i++) {
        auto node = m_nodes[i];
        node->place(x_off, y_off);
        x_off += node->width();

        // record new descent
        Length descent_new = node->descent() - node->voff();
        if (descent_new > descent) {
          descent = descent_new;
        }
      }

      // advance line
      lines += 1;
    }

    if (lines > 0) { // at least one line?
      m_multiline_shift = -1 * y_off; // multi-line boxes need to be shifted upwards
      m_ascent = first_ascent - y_off;
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
