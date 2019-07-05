#ifndef NULL_BOX_H
#define NULL_BOX_H

#include <Rcpp.h>
using namespace Rcpp;

#include "layout.h"

/* The NullBox draws nothing. If given a width or a height,
 * it can be used as a spacer. The reference point of
 * the NullBox is the lower left corner.
 */

template <class Renderer>
class NullBox : public Box<Renderer> {
private:
  Length m_width;
  Length m_height;

public:
  NullBox(Length width = 0, Length height = 0) :
    m_width(width), m_height(height) {}
  ~NullBox() {}

  Length width() { return m_width; }
  Length ascent() { return m_height; }
  Length descent() { return 0; }
  Length voff() { return 0; }

  // nothing to be done
  void calc_layout(Length, Length) {}

  // nothing to be done
  void place(Length, Length) {}

  // nothing to be done
  void render(Renderer &, Length, Length) {}
};

#endif
