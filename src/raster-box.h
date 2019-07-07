#ifndef RASTER_BOX_H
#define RASTER_BOX_H

#include <Rcpp.h>
using namespace Rcpp;

#include <utility> // for pair<>
using namespace std;

#include "layout.h"

pair<double, double> image_dimensions(RObject image) {
  Environment env = Environment::namespace_env("base");
  Function dim = env["dim"];

  NumericVector dims = dim(image);
  if (dims.size() < 2) {
    stop("Cannot extract image dimensions. Image must be a matrix, raster, or nativeRaster object.");
  }

  // first dimension is rows (height), second is columns (width)
  return pair<double, double>(dims[1], dims[0]);
}


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
  bool m_respect_asp; // if `true`, always plots image with correct aspect ratio, regardless of box dimensions
  bool m_interpolate; // if `true`, interpolates raster images
  double m_dpi; // dots per inch to determine native image sizes
  double m_rel_width, m_rel_height; // used to store relative width and height when needed
  Length m_native_width, m_native_height; // native width and height of image, in pt

public:
  RasterBox(RObject image, Length width, Length height, const typename Renderer::GraphicsContext &gp,
            SizePolicy width_policy = SizePolicy::native, SizePolicy height_policy = SizePolicy::native,
            bool respect_aspect = true, bool interpolate = true, double dpi = 150) :
    m_image(image), m_gp(gp), m_width(width), m_height(height),
    m_width_policy(width_policy), m_height_policy(height_policy),
    m_x(0), m_y(0), m_respect_asp(respect_aspect), m_interpolate(interpolate),
    m_dpi(dpi), m_rel_width(0), m_rel_height(0),
    m_native_width(0), m_native_height(0) {
    pair<double, double> d = image_dimensions(image);

    // there are 72.27 pt in each in
    m_native_width = d.first * 72.27 / m_dpi;
    m_native_height = d.second * 72.27 / m_dpi;

    if (m_width_policy == SizePolicy::relative) {
      m_rel_width = m_width/100;
    }
    if (m_height_policy == SizePolicy::relative) {
      m_rel_height = m_height/100;
    }
  }
  ~RasterBox() {};

  Length width() { return m_width; }
  Length ascent() { return m_height; }
  Length descent() { return 0; }
  Length voff() { return 0; }

  void calc_layout(Length width_hint, Length height_hint) {
    if (m_width_policy == SizePolicy::native && m_height_policy == SizePolicy::native) {
      m_width = m_native_width;
      m_height = m_native_height;
      return;
    }

    switch(m_width_policy) {
    case SizePolicy::expand:
      m_width = width_hint;
      break;
    case SizePolicy::relative:
      m_width = width_hint * m_rel_width;
      break;
    case SizePolicy::fixed:
    default:
      break;
    }

    switch(m_height_policy) {
    case SizePolicy::expand:
      m_height = height_hint;
      break;
    case SizePolicy::relative:
      m_height = height_hint * m_rel_height;
      break;
    case SizePolicy::native:
      m_height = m_width * m_native_height / m_native_width;
      break;
    case SizePolicy::fixed:
    default:
      break;
    }

    // can only do this calculation after height is set
    if (m_width_policy == SizePolicy::native) {
      m_width = m_height * m_native_width / m_native_height;
    }
  }

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

    // adjust for aspect ratio if necessary
    if (!m_respect_asp || (m_width/m_height == m_native_width/m_native_height)) {
      r.raster(m_image, x, y, m_width, m_height, m_interpolate, m_gp);
    } else {
      // do we need to adjust the height or the width of the image?
      if (m_height_policy == SizePolicy::native ||
          (m_width/m_height > m_native_width/m_native_height &&
            !(m_width_policy == SizePolicy::native))) {
        // adjust imate width if box is wider than image or native image height is requested
        Length width = m_height * m_native_width / m_native_height;
        Length xoff = (m_width - width)/2;
        r.raster(m_image, x + xoff, y, width, m_height, m_interpolate, m_gp);
      } else {
        // otherwise adjust image height
        Length height = m_width * m_native_height / m_native_width;
        Length yoff = (m_height - height)/2;
        r.raster(m_image, x, y + yoff, m_width, height, m_interpolate, m_gp);
      }
    }
  }
};

#endif
