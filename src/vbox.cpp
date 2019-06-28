#include <Rcpp.h>
using namespace Rcpp;

#include "length-type.h"
#include "layout.h"

// A box holding a single R grob
class GrobBox : public Box {
private:
  RObject m_grob;
  length_type m_width;
  length_type m_ascent;
  length_type m_descent;
  length_type m_yoff;

public:
  GrobBox(RObject grob, length_type width = 0, length_type ascent = 0,
          length_type descent = 0, length_type yoff = 0) :
    m_grob(grob), m_width(width), m_ascent(ascent), m_descent(descent), m_yoff(yoff) {}
  virtual ~GrobBox() {};

  virtual length_type width() { return m_width; }
  virtual length_type ascent() { return m_ascent; }
  virtual length_type descent() { return m_descent; }
  virtual length_type yoff() { return m_yoff; }

  virtual RObject render_grob() {return m_grob; }
};

class HBox : public Box {
private:
  node_list m_nodes;
  length_type m_width;
  length_type m_ascent;
  length_type m_descent;
  length_type m_yoff;

public:
  HBox(const node_list& nodes) :
    m_nodes(nodes), m_width(0), m_ascent(0), m_descent(0), m_yoff(0) {}
  virtual ~HBox() {};

  virtual length_type width() { return m_width; }
  virtual length_type ascent() { return m_ascent; }
  virtual length_type descent() { return m_descent; }
  virtual length_type yoff() { return m_yoff; }

  virtual RObject render_grob() {
    List out;

    // render all grobs in the list
    for (auto i_node = m_nodes.begin(); i_node != m_nodes.end(); i_node++) {
      if ((*i_node)->type() == box) {
        out.push_back(static_pointer_cast<Box>(*i_node)->render_grob());
      }
    }

    // turn list into gList to keep grid happy
    out.attr("class") = "gList";
    return out;
  }
};
