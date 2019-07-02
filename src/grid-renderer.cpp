/* R bindings to grid renderer, for unit testing */

#include "grid-renderer.h"

// [[Rcpp::export]]
XPtr<GridRenderer> grid_renderer() {
  XPtr<GridRenderer> gr(new GridRenderer());

  return gr;
}

// [[Rcpp::export]]
void grid_renderer_text(XPtr<GridRenderer> gr, String label, Length x, Length y, List gp) {
//                        String color = "#000000", double fontsize = 12,
//                        String fontface = "plain", String fontfamily = ""
  return gr->text(label, x, y, gp);
}

// [[Rcpp::export]]
void grid_renderer_rect(XPtr<GridRenderer> gr, Length x, Length y, Length width, Length height, List gp, Length r = 0) {
//                        String color = "#000000", String fill = NA_STRING, int linetype = 1,
//                        double linewidth = 1, String linejoin = "round",
//                        double linemitre = 1) {
  return gr->rect(x, y, width, height, gp, r);
}

// [[Rcpp::export]]
List grid_renderer_collect_grobs(XPtr<GridRenderer> gr) {
  return gr->collect_grobs();
}

