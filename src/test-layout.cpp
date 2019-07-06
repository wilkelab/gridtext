#include <testthat.h>

#include "layout.h"
#include "text-box.h"
#include "glue.h"
#include "grid-renderer.h"

context("Node list") {
  test_that("Create polymorphic node list") {
    BoxPtr<GridRenderer> b1(new Glue<GridRenderer>());
    BoxPtr<GridRenderer> b2(new TextBox<GridRenderer>("abcd", List()));

    BoxList<GridRenderer> l;
    l.push_back(b1);
    l.push_back(b2);

    expect_true(l[0]->type() == NodeType::glue);
    expect_true(l[1]->type() == NodeType::box);
  }
}
