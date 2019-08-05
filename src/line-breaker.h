#ifndef LINE_BREAKER_H
#define LINE_BREAKER_H

#include <Rcpp.h>
using namespace Rcpp;

#include <iostream>

#include "layout.h"
#include "glue.h"
#include "penalty.h"


// helper class to record start and end points of lines to render
class LineBreakInfo {
public:
  size_t start; // first node in the line
  size_t end;   // one past the last node in the line
  double r;     // adjustment ratio
  Length width; // width of the line

  LineBreakInfo(size_t _start, size_t _end, double _r, Length _width) :
    start(_start), end(_end), r(_r), width(_width) {}

};


// naive line breaker

template <class Renderer>
class LineBreaker {
private:
  const BoxList<Renderer> &m_nodes;
  const vector<Length> &m_line_lengths;
  bool m_word_wrap; // do we break at any feasible position or only at forced positions?
  vector<Length> m_sum_widths;

  // get width of node i
  Length get_width(size_t i) {
    if (i >= m_nodes.size()) {
      return 0;
    }

    auto node = m_nodes[i];
    auto type = node->type();

    if (type == NodeType::box) {
      return node->width();
    } else if (type == NodeType::glue) {
      return static_cast<Glue<Renderer>*>(node.get())->default_width();
    } else {
      // penalties have width 0 unless they get rendered
      return 0;
    }
  }

  // measure width from point a to point b, excluding b
  Length measure_width(size_t a, size_t b) {
    return m_sum_widths[b] - m_sum_widths[a];
  }

  // calculate the length of the current line
  Length line_length(size_t line) {
    if (line < m_line_lengths.size()) {
      return m_line_lengths[line];
    } else {
      return m_line_lengths.back();
    }
  }

  // determine whether we can break at position i
  bool is_feasible_breakpoint(size_t i) {
    // if word wrap is off, only forced breaks are feasible breaks
    if (!m_word_wrap) {
      return is_forced_break(i);
    }

    // if we have run out of nodes we definitely want to break
    if (i >= m_nodes.size()) {
      return true;
    }

    // we can break at position i if either i is a penalty less than infinity
    // or if it is a glue and the previous node is a box
    auto node = m_nodes[i];
    if (node->type() == NodeType::penalty) {
      if (static_cast<Penalty<Renderer>*>(node.get())->penalty() < Penalty<Renderer>::infinity) {
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

  // determine whether we must break at position i
  bool is_forced_break(size_t i) {
    // if we have run out of nodes we definitely want to break
    if (i >= m_nodes.size()) {
      return true;
    }

    // a penalty of -infinity is a forced break
    auto node = m_nodes[i];
    if (node->type() == NodeType::penalty) {
      if (static_cast<Penalty<Renderer>*>(node.get())->penalty() <= -1*Penalty<Renderer>::infinity) {
        return true;
      }
    }
    return false;
  }

  // determine whether we remove this node at the beginning of a line
  bool is_removable_whitespace(size_t i) {
    if (i >= m_nodes.size()) {
      return false;
    }

    auto node = m_nodes[i];
    auto type = node->type();
    if (type == NodeType::penalty) {
      // we cannot remove a forced break
      if (static_cast<Penalty<Renderer>*>(node.get())->penalty() <= -1*Penalty<Renderer>::infinity) {
        return false;
      } else {
        return true;
      }
    } else if (type == NodeType::glue) {
      return true;
    }
    return false;
  }

  // advances i until the next possible point to start a line; used to
  // skip penalties and glue at beginning of a line
  size_t find_next_startpoint(size_t i) {
    while (i < m_nodes.size() && is_removable_whitespace(i)) {
      i++;
    }
    return i;
  }

  // advances i until the next feasible breakpoint
  size_t find_next_feasible_breakpoint(size_t i) {
    while (i < m_nodes.size() && !is_feasible_breakpoint(i)) {
      i++;
    }
    return i;
  }


  // to write unit tests that have access to private members
  friend class TestLineBreaker;

public:
  LineBreaker(const BoxList<Renderer>& nodes, const vector<Length> &line_lengths,
              bool word_wrap = true) :
    m_nodes(nodes), m_line_lengths(line_lengths), m_word_wrap(word_wrap) {

    // calculate sums of widths
    size_t m = m_nodes.size();
    m_sum_widths.resize(m + 1);
    Length running_sum_w = 0;
    for (size_t i = 0; i < m + 1; i++) {
      m_sum_widths[i] = running_sum_w;
      running_sum_w += get_width(i);
    }
  }


  void compute_line_breaks(vector<LineBreakInfo> &line_breaks) {
    line_breaks.clear(); // this is how we return the results; hence, clear first

    size_t a = 0; // starting point of the current line
    size_t line = 0; // current line we are processing
    while (a < m_nodes.size()) {
      //cout << "start" << " " << a << " " << m_nodes.size() << endl;
      a = find_next_startpoint(a); // skip whitespace at beginning of line
      size_t b = find_next_feasible_breakpoint(a);
      Length width = measure_width(a, b); // calculate width from a to b, excluding b
      Length linelen = line_length(line);

      // at a minimum, the current line contains material from a to b; however, if
      // b is not a forced break and the next piece fits, we can add it
      while (b < m_nodes.size() && !is_forced_break(b)) {
        //cout << "end" << " " << b << " " << m_nodes.size() << endl;

        size_t b_new = find_next_feasible_breakpoint(b + 1);
        Length width_delta = measure_width(b, b_new);

        // does the next piece fit?
        if (width + width_delta < linelen) {
          // yes, continue
          width += width_delta;
          b = b_new;
        } else {
          // no, exit inner loop
          break;
        }
      }
      // now we have a line from a to b
      // but place only if we're not past the end of the nodes list
      if (a < m_nodes.size()) {
        line_breaks.emplace_back(a, b, 0, width);
        //cout << line << " " << a << " " << b << endl;
        line++;

        // if b is a forced break, we need to advance by 1 to make sure
        // the break penalty gets skipped in the next line
        if (is_forced_break(b)) {
          b++;
        }
        a = b;
      } else {
        break; // exit outer loop, we're done
      }
    }
  }
};

/***************************************************************
 This code was never fully implemented and is currently disabled.

#include <cmath>
#include <algorithm>
#include <memory>
#include <list>
#include <array>
using namespace std;

// The LineBreaker class implements the Knuth-Plass algorithm, as described in
// Knuth & Plass 1981


// support class representing an active breakpoint
struct Breakpoint {
  size_t position, line;
  int fitness_class;
  // these are in the original paper but are not needed
  //Length totalwidth, totalstretch, totalshrink;
  int demerits;
  Breakpoint* previous;

  Breakpoint(size_t _position, size_t _line, int _fitness_class,
             //Length _totalwidth, Length _totalstretch, Length _totalshrink,
             int _demerits, Breakpoint* _previous = nullptr) :
    position(_position), line(_line), fitness_class(_fitness_class),
    //totalwidth(_totalwidth), totalstretch(_totalstretch), totalshrink(_totalshrink),
    demerits(_demerits), previous(_previous)
  {};
};


template <class Renderer>
class LineBreaker {
private:
  const BoxList<Renderer> &m_nodes;
  const vector<Length> &m_line_lengths;
  double m_tolerance;
  double m_fitness_demerit, m_flagged_demerit;

  // vectors w, y, z, p, f as in the paper
  vector<Length> m_w, m_y, m_z;
  vector<double> m_p;
  vector<bool> m_f;

  // sums of widths, stretch, and shrink
  vector<Length> m_sum_widths, m_sum_stretch, m_sum_shrink;

  // active and passive nodes
  list<unique_ptr<Breakpoint>> m_active_nodes;
  vector<unique_ptr<Breakpoint>> m_passive_nodes;


  void add_active_node(unique_ptr<Breakpoint> node) {
    // find the first position at which the line number of the new node
    // exceeds the line number of the node in the list
    auto i_node = m_active_nodes.begin();
    while (i_node != m_active_nodes.end() && (*i_node)->line < node->line) {
      i_node++;
    }
    auto i_node_insert = i_node; // store insertion point

    // now check if there's another node with the same line number,
    // position, and fitness; if yes, drop the new node
    while (i_node != m_active_nodes.end() && (*i_node)->line == node->line) {
      if ((*i_node)->fitness_class == node->fitness_class &&
          (*i_node)->position == node->position) {
        return;
      }
      i_node++;
    }

    m_active_nodes.insert(i_node_insert, node);
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

  double compute_adjustment_ratio(size_t i1, size_t i2, size_t line) {
    Length len = measure_width(i1, i2);

    // if we're breaking at a penalty and it has a width, need to account for it
    // TODO: check if this is correct. Why don't we have to account for the widths of other box types?
    if (m_nodes[i2]->type() == NodeType::penalty) {
      len = len + m_nodes[i2]->width();
    }

    // we obtain the available length of the current line
    // from the vector of line lengths or, if we have used them up,
    // from the last line length
    Length len_avail;
    if (line < m_line_lengths.size()) {
      len_avail = m_line_lengths[line];
    } else {
      len_avail = m_line_lengths.back();
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
        r = -1*Glue<Renderer>::infinity;
      }
    }
    // r = 0 if len == len_avail

    return r;
  }

  int compute_fitness_class(double r) {
    // the fitness class of the line with adjustment ratio r (very tight, tight, loose, very loose)
    int fitness_class;
    if  (r < -.5) fitness_class = 0;
    else if (r <= .5) fitness_class = 1;
    else if (r <= 1) fitness_class = 2;
    else fitness_class = 3;

    return fitness_class;
  }

  double compute_demerits(size_t i, size_t i_active, double r, int fitness_class, int fitness_class_active) {
    double demerits;

    if (m_p[i] >= 0) {
      demerits = pow(1 + 100 * pow(abs(r), 3) + m_p[i], 3);
    } else if (is_forced_break(i)) {
      demerits = pow(1 + 100 * pow(abs(r), 3), 2) - pow(m_p[i], 2);
    } else {
      demerits = pow(1 + 100 * pow(abs(r), 3), 2);
    }

    // adjust demeteris for flagged items
    demerits = demerits + (m_flagged_demerit * m_f[i] * m_f[i_active]);

    // add demerits for changes in fitness class
    if (abs(fitness_class - fitness_class_active) > 1) {
      demerits = demerits + m_fitness_demerit;
    }

    return demerits;
  }

  // to write unit tests that have access to private members
  friend class TestLineBreaker;

public:
  LineBreaker(const BoxList<Renderer>& nodes, const vector<Length> &line_lengths, double tolerance = 1,
              double fitness_demerit = 100, double flagged_demerit = 100) :
    m_nodes(nodes), m_line_lengths(line_lengths), m_tolerance(tolerance), m_fitness_demerit(fitness_demerit),
    m_flagged_demerit(flagged_demerit) {
    // tolerance is \rho in the paper
    // fitness_demerit is \gamma in the paper

    size_t m = nodes.size();

    // set up vectors with the five possible values (w, y, z, p, f)
    // for each node
    m_w.reserve(m);
    m_y.reserve(m);
    m_z.reserve(m);
    m_p.reserve(m);
    m_f.reserve(m);

    for (auto i_node = m_nodes.begin(); i_node != m_nodes.end(); i_node++) {
      m_w.push_back((*i_node)->width());
      if ((*i_node)->type() == NodeType::glue) {
        m_y.push_back(static_cast<Glue<Renderer>*>(i_node->get())->stretch());
        m_z.push_back(static_cast<Glue<Renderer>*>(i_node->get())->shrink());
        m_p.push_back(0);
        m_f.push_back(false);
      } else if ((*i_node)->type() == NodeType::penalty) {
        m_y.push_back(0);
        m_z.push_back(0);
        m_p.push_back(static_cast<Penalty<Renderer>*>(i_node->get())->penalty());
        m_f.push_back(static_cast<Penalty<Renderer>*>(i_node->get())->flagged());
      } else {
        m_y.push_back(0);
        m_z.push_back(0);
        m_p.push_back(0);
        m_f.push_back(false);
      }
    }

    // pre-compute sums
    m_sum_widths.resize(m);
    m_sum_stretch.resize(m);
    m_sum_shrink.resize(m);
    Length widths_sum = 0;
    Length stretch_sum = 0;
    Length shrink_sum = 0;
    for (size_t i = 0; i < m; i++) {
      m_sum_widths[i] = widths_sum;
      m_sum_stretch[i] = stretch_sum;
      m_sum_shrink[i] = shrink_sum;

      widths_sum = widths_sum + m_w[i];
      stretch_sum = stretch_sum + m_y[i];
      shrink_sum  = shrink_sum + m_z[i];
    }
  }

  void compute_breaks(vector<size_t> &final_breaks) {
    // we return the final breaks via this argument;
    // clear this vector to make sure we're starting from a clean slate
    final_breaks.clear();

    size_t m = m_nodes.size();

    // if there are no nodes we have no breaks
    if (m == 0) {
      return;
    }

    // set up list of active nodes, initialize with
    // break at beginning of text
    m_active_nodes.clear();
    m_passive_nodes.clear();
    auto ptr = unique_ptr<Breakpoint>(new Breakpoint(0, 0, 1, 0));
    m_active_nodes.push_back(ptr);

    for (size_t b = 0; b < m; b++) {
      // we can only break at feasible breakpoints
      if (is_feasible_breakpoint(b)) {
        // main loop, p. 1159
        auto i_active = m_active_nodes.begin();
        do {
          array<double, 4> Dc = {Penalty<Renderer>::infinity, Penalty<Renderer>::infinity,
                                 Penalty<Renderer>::infinity, Penalty<Renderer>::infinity};
          double D = Penalty<Renderer>::infinity;
          array<Breakpoint*, 4> Ac = {nullptr, nullptr, nullptr, nullptr};

          // iterate over all currently active nodes and evaluate breaking
          // between there and b; we need to pay attention because we modify
          // the list as we iterate
          while (i_active != m_active_nodes.end()) {
            double r = compute_adjustment_ratio((*i_active)->position, b, (*i_active)->line);

            // remove active nodes when overfull line or forced break
            if (r < -1 || is_forced_break(b)) {
              m_passive_nodes.push_back(*i_active);
              i_active = m_active_nodes.erase(i_active); // this advances the iterator
              continue; // to avoid incrementing i_active twice
            }
            if (-1 <= r <= m_tolerance) {
              int fitness_class = compute_fitness_class(r);
              double d = compute_demerits(b, (*i_active)->position, r, fitness_class, (*i_active)->fitness_class);

              if (d < Dc[fitness_class]) {
                Dc[fitness_class] = d;
                Ac[fitness_class] = (i_active);
                if (d < D) D = d; // keep track of overall minimum demerits
              }
            }
            i_active++;
            // the paper lists a shortcut that we skip for now:
            //   if line(a) >= j and j < j_0 then exit loop
            // should this be j > j_0?
          }
          if (D < Penalty<Renderer>::infinity) {
            // insert new active nodes for breaks from A_c to b
            for (int c = 0; c < 4; c++) {
              if (Dc[c] < D + m_fitness_demerit) {
                auto ptr = unique_ptr<Breakpoint>(
                  new Breakpoint(b, Ac[c]->line + 1, c, Dc[c], Ac[c])
                );
                m_active_nodes.insert(i_active, ptr);
              }
            }
          }
        } while (i_active != m_active_nodes.end());
      }

      if (m_active_nodes.size() == 0) {
        // TODO
        // do something drastic since there is no feasible solution
      }

      // find the active node with the lowest number of demerits
      auto i_active = m_active_nodes.begin();
      double min_demerits = (*i_active)->demerits;
      auto i_min = i_active;
      while (true) {
        i_active++;
        if (i_active == m_active_nodes.end()) {
          break;
        }
        if ((*i_active)->demerits < min_demerits) {
          min_demerits = (*i_active)->demerits;
          i_min = i_active;
        }
      }

      // now build a list of break points going backwards from minimum
      // demerits node to beginning
      Breakpoint *p_node = *i_min;
      while (p_node != nullptr) {
        final_breaks.push_back(p_node->position);
        p_node = p_node->previous;
      }
      reverse(final_breaks.begin(), final_breaks.end());
    }
  }
};


*/
#endif
