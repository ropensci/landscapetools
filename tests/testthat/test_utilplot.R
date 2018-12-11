# nolint start
context("show_landscape")

x <- rndmap

p <- show_landscape(x)

test_that("basic functionality", {
  expect_error(show_landscape(x), NA)
  expect_error(show_landscape(x, discrete = TRUE), NA)
})

test_that("show_landscape behaves like it should", {
  expect_equal(class(p), c("gg","ggplot"))
})

l2 <- grdmap
l3 <- rndmap

bri1 <- raster::brick(l2, l3)
p1 <- show_landscape(bri1)

lst1 <- list(lay1 = l2,
             lay2 = l3,
             lay3 = l2,
             lay4 = l3)
p2 <- show_landscape(lst1)

test_that("basic functionality", {
  expect_error(show_landscape(bri1), NA)
  expect_error(show_landscape(lst1), NA)
})

test_that("util_plot behaves like it should", {
  expect_equal(class(p1), c("gg","ggplot"))
  expect_equal(class(p2), c("gg","ggplot"))
})

# nolint end
