#ifndef NULL_BOX_H
#define NULL_BOX_H

#include <Rcpp.h>
using namespace Rcpp;

#include "layout.h"

// A box holding nothing and having no extent
template <class Renderer>
class NullBox : public Box<Renderer> {
public:
  NullBox() {}
  ~NullBox() {}

  Length width() { return 0; }
  Length ascent() { return 0; }
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
