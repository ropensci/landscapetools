# nolint start
context("show_landscape")

x <- random_landscape
y <- util_classify(gradient_landscape,  n = 3, level_names = c("Land Use 1", "Land Use 2", "Land Use 3"))

p <- show_landscape(x)
p_discrete <- show_landscape(y, discrete = TRUE)

test_that("basic functionality", {
  expect_error(show_landscape(x), NA)
  expect_error(show_landscape(x, discrete = TRUE), NA)
})

test_that("show_landscape behaves like it should", {
  expect_equal(class(p), c("gg","ggplot"))
  expect_equal(class(p_discrete), c("gg","ggplot"))
})

l2 <- gradient_landscape
l3 <- random_landscape

bri1 <- raster::brick(l2, l3)
p1 <- show_landscape(bri1)

lst1 <- list(lay1 = l2,
             lay2 = l3,
             lay3 = l2,
             lay4 = l3)

lst2 <- list(lay1 = y,
             lay2 = y)


p2 <- show_landscape(lst1)
p3 <- show_landscape(lst2)

p4 <- show_landscape(lst2, unique_scales = TRUE)

test_that("basic functionality", {
  expect_error(show_landscape(bri1), NA)
  expect_error(show_landscape(lst1), NA)
  expect_error(show_landscape(lst2), NA)
})

test_that("util_plot behaves like it should", {
  expect_equal(class(p1), c("gg","ggplot"))
  expect_equal(class(p2), c("gg","ggplot"))
  expect_equal(class(p3), c("gg","ggplot"))
})

# nolint end
