# nolint start
context("util_facetplot")

l2 <- grdmap
l3 <- rndmap

bri1 <- raster::brick(l2, l3)
p1 <- util_facetplot(bri1)

lst1 <- list(lay1 = l2,
             lay2 = l3,
             lay3 = l2,
             lay4 = l3)
p2 <- util_facetplot(lst1)

test_that("basic functionality", {
  expect_error(util_facetplot(bri1), NA)
  expect_error(util_facetplot(lst1), NA)
})

test_that("util_plot behaves like it should", {
  expect_equal(class(p1), c("gg","ggplot"))
  expect_equal(class(p2), c("gg","ggplot"))
})

# nolint end
