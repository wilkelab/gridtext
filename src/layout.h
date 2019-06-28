#ifndef LAYOUT_H
#define LAYOUT_H

#include <Rcpp.h>
using namespace Rcpp;

#include <vector>
#include <memory>
using namespace std;

#include "length-type.h"

enum node_type {
  none,
  box,
  glue,
  penalty
};

// base class for a generic node in the
// layout tree
class LayoutNode {
protected:
  node_type m_type;

public:
  LayoutNode(node_type type = none) : m_type(type) {}
  virtual ~LayoutNode() = 0;
  node_type type() {return m_type;}
};

// generic box class
class Box : public LayoutNode {
public:
  Box() : LayoutNode(box) {}
  virtual ~Box() = 0;

  // width of the box
  virtual length_type width() = 0;
  // ascent of the box (height measured from baseline)
  virtual length_type ascent() = 0;
  // descent of the box (height below the baseline)
  virtual length_type descent() = 0;
  // yoffset (vertical shift of baseline)
  virtual length_type yoff() = 0;

  virtual RObject render_grob() = 0;
};

class Glue : public LayoutNode {
  length_type m_width, m_stretch, m_shrink;

public:
  Glue(length_type width = 0, length_type stretch = 0, length_type shrink = 0) :
    LayoutNode(glue), m_width(width), m_stretch(stretch), m_shrink(shrink) {}
  virtual ~Glue() {}

  length_type width() {return m_width;}
};


// node list (vector of pointers to layout nodes)
typedef  vector<shared_ptr<LayoutNode> > node_list;

#endif
