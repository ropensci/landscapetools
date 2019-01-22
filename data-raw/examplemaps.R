# Example maps from NLMR -------

fractal_landscape <- NLMR::nlm_fbm(ncol = 150, nrow = 150, fract_dim = 0.8)
usethis::use_data(fractal_landscape, overwrite = TRUE)

gradient_landscape <- NLMR::nlm_planargradient(150, 150)
usethis::use_data(gradient_landscape, overwrite = TRUE)

random_landscape <- NLMR::nlm_random(150, 150)
usethis::use_data(random_landscape, overwrite = TRUE)

# Classify the map into land uses
classified_landscape <- util_classify(fractal_landscape,
                        n = 3,
                        level_names = c("Land Use 1",
                                        "Land Use 2",
                                        "Land Use 3"))


usethis::use_data(classified_landscape, overwrite = TRUE)
