#ifndef LAYOUT_H
#define LAYOUT_H

#include <Rcpp.h>
using namespace Rcpp;

#include <vector>
#include <memory>
using namespace std;

#include "length.h"

enum class NodeType {
  none,
  box,
  glue,
  penalty
};

enum class SizePolicy {
  fixed,    // box size is fixed upon construction
  native,   // box determines its own ideal size
  expand,   // box expands as much as possible
  relative  /* box takes up a set proportion of the size hint
 * provided to calc_layout(); in this case
 * Length units are interpreted as percent, i.e.,
 * a Length of 100 means full size
 */
};


// base class for a generic node in the
// layout tree
template <class Renderer> class BoxNode {
public:
  BoxNode() {}
  virtual ~BoxNode() {}

  // returns the node type (box, glue, penalty)
  virtual NodeType type() = 0;

  // width of the box
  virtual Length width() = 0;
  // ascent of the box (height measured from baseline)
  virtual Length ascent() = 0;
  // descent of the box (height below the baseline)
  virtual Length descent() = 0;
  // vertical offset (vertical shift of baseline)
  virtual Length height() {
    return ascent() + descent();
  }
  virtual Length voff() = 0;

  // calculate the internal layout of the box
  // in the general case, we may provide the box with a width and
  // a height to render into, though boxes may ignore these
  virtual void calc_layout(Length width_hint = 0, Length height_hint = 0) = 0;

  // place box in internal coordinates used in enclosing box
  virtual void place(Length x, Length y) = 0;

  // render into absolute coordinates, using the reference coordinates
  // from the enclosing box
  virtual void render(Renderer &r, Length xref, Length yref) = 0;
};

template <class Renderer> class Box : public BoxNode<Renderer> {
  Length m_width, m_stretch, m_shrink;

public:
  Box() {}
  ~Box() {}
  NodeType type() {return NodeType::box;}
};

// box list (vector of pointers to boxes)
template <class Renderer>
using BoxPtr = XPtr<BoxNode<Renderer>>;

template <class Renderer>
using BoxList = vector<BoxPtr<Renderer>>;

// struct that holds width, ascent, etc. data for text labels
struct TextDetails {
  Length width;    // width of the label
  Length ascent;   // ascent from baseline
  Length descent;  // descent below baseline
  Length space;    // width of a space

  TextDetails(Length w = 0, Length a = 0, Length d = 0, Length s = 0) :
    width(w), ascent(a), descent(d), space(s) {}
};

// struct that holds margin or padding info, in the form top, right, bottom, left
struct Margin {
  Length top;
  Length right;
  Length bottom;
  Length left;

  Margin(Length t = 0, Length r = 0, Length b = 0, Length l = 0) :
    top(t), right(r), bottom(b), left(l) {}
};

#endif
