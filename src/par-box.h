#ifndef PAR_BOX_H
#define PAR_BOX_H

#include <Rcpp.h>
using namespace Rcpp;

#include "grid.h"
#include "layout.h"
#include "glue.h"
#include "penalty.h"

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

  vector<Length> m_sum_widths, m_sum_stretch, m_sum_shrink;

public:
  ParBox(const BoxList<Renderer>& nodes, Length vspacing, Length hspacing) :
    m_nodes(nodes), m_vspacing(vspacing), m_hspacing(hspacing),
    m_width(0), m_ascent(0), m_descent(0), m_voff(0),
    m_x(0), m_y(0) {
    m_sum_widths.resize(nodes.size());
    m_sum_stretch.resize(nodes.size());
    m_sum_shrink.resize(nodes.size());
  }
  ~ParBox() {};

  Length width() { return m_width; }
  Length ascent() { return m_ascent; }
  Length descent() { return m_descent; }
  Length voff() { return m_voff; }

  void calc_layout(Length width_hint, Length height_hint) {
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

  bool is_feasible_breakpoint(size_t i) {
    // we can break at position i if either i is a penalty less than infinity
    // or if it is a glue and the previous node is a box
    auto node = m_nodes[i];
    if (node->type() == NodeType::penalty) {
      if (static_cast<Penalty<Renderer>*>(node)->penalty() < Penalty<Renderer>::infinity) {
        return true;
      }
    }
    else if (i > 0 && node->type() == NodeType::glue) {
      if (m_nodes[i-1]->type() == NodeType::box) {
        return true;
      }
    }
    return false;
  }

  bool is_forced_break(size_t i) {
    // a penalty of -infinity is a forced break
    auto node = m_nodes[i];
    if (node->type() == NodeType::penalty) {
      if (static_cast<Penalty<Renderer>*>(node)->penalty() <= -1*Penalty<Renderer>::infinity) {
        return true;
      }
    }
    return false;
  }

  Length measure_width(size_t i1, size_t i2) {
    return m_sum_widths[i2] - m_sum_widths[i1];
  }

  Length measure_stretch(size_t i1, size_t i2) {
    return m_sum_stretch[i2] - m_sum_stretch[i1];
  }

  Length measure_shrink(size_t i1, size_t i2) {
    return m_sum_shrink[i2] - m_sum_shrink[i1];
  }

  double compute_adjustment_ratio(size_t i1, size_t i2, size_t line, const vector<Length> &line_lengths) {
    Length len = measure_width(i1, i2);

    // TODO: Are these two lines correct? They seem strange; needs to be checked.
    if (m_nodes[i2]->type() == NodeType::penalty) {
      len = len + m_nodes[i2]->width();
    }

    // we obtain the available length of the current line
    // from the vector of line lengths or, if we have used them up,
    // from the last line length
    Length len_avail;
    if (line < line_lengths.size()) {
      len_avail = line_lengths[line];
    } else {
      len_avail = line_lengths.back();
    }

    double r = 0; // adjustment ratio
    if (len < len_avail) { // if length is smaller than available length, need to stretch
      Length stretch =  measure_stretch(i1, i2);
      if (stretch > 0) {
        r = (len_avail - len)/stretch;
      } else {
        r = Glue<Renderer>::infinity;
      }
    } else if (len > len_avail) { // if length is larger than available length, need to shrink
      Length shrink =  measure_shrink(i1, i2);
      if (shrink > 0) {
        r = (len_avail - len)/shrink;
      } else {
        r = Glue<Renderer>::infinity; // TODO: Should this be -infinity?
      }
    }
    // r = 0 if len == len_avail

    return r;
  }
};

#endif
