#include <testthat.h>

#include "layout.h"
#include "text-box.h"

context("Node list") {
  test_that("Create polymorphic node list") {
    NodePtr n1(new Glue());
    NodePtr n2(new TextBox<>("abcd"));

    NodeList l;
    l.push_back(n1);
    l.push_back(n2);

    expect_true(l[0]->type() == LayoutNode::glue);
    expect_true(l[1]->type() == LayoutNode::box);
  }
}
