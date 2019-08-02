#include <testthat.h>

#include <iostream>

#include "layout.h"
#include "null-box.h"
#include "glue.h"
#include "grid-renderer.h"
#include "line-breaker.h"

class TestLineBreaker {
public:
  static void test1() {
    BoxPtr<GridRenderer> b1(new Glue<GridRenderer>(5));
    BoxPtr<GridRenderer> b2(new NullBox<GridRenderer>(3));

    BoxList<GridRenderer> l;
    l.push_back(b1);
    l.push_back(b2);

    vector<Length> line_lengths = {20};
    LineBreaker<GridRenderer> lb(l, line_lengths);

    expect_true(lb.m_nodes.size() == 2);
  }
};

context("Line breaker") {
  test_that("Create linebreaker class") {
    TestLineBreaker::test1();
  }
}
