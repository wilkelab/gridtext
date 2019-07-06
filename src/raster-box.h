#ifndef RASTER_BOX_H
#define RASTER_BOX_H

#include <Rcpp.h>
using namespace Rcpp;

#include <iostream>

#include "layout.h"

// A box holding a single image
template <class Renderer>
class RasterBox : public Box<Renderer> {
private:
  RObject m_image;
  typename Renderer::GraphicsContext m_gp;
  Length m_width, m_height;
  SizePolicy m_width_policy, m_height_policy;
  // position of the box in enclosing box
  // the box reference point is the leftmost point of the baseline.
  Length m_x, m_y;
  bool m_interpolate;

public:
  RasterBox(RObject image, Length width, Length height, const typename Renderer::GraphicsContext &gp,
            SizePolicy width_policy = SizePolicy::fixed, SizePolicy height_policy = SizePolicy::fixed,
            bool interpolate = true) :
    m_image(image), m_gp(gp), m_width(width), m_height(height),
    m_width_policy(width_policy), m_height_policy(height_policy),
    m_x(0), m_y(0), m_interpolate(interpolate) {
  }
  ~RasterBox() {};

  Length width() { return m_width; }
  Length ascent() { return m_height; }
  Length descent() { return 0; }
  Length voff() { return 0; }

  // nothing to be done for a raster box
  void calc_layout(Length, Length) {}

  // place box in internal coordinates used in enclosing box
  void place(Length x, Length y) {
    m_x = x;
    m_y = y;
  }

  // render into absolute coordinates, using the reference coordinates
  // from the enclosing box
  void render(Renderer &r, Length xref, Length yref) {
    Length x = m_x + xref;
    Length y = m_y + yref;

    r.raster(m_image, x, y, m_width, m_height, m_interpolate, m_gp);
  }
};

#endif
