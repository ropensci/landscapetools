# Example maps from nlmr -------

fbmmap <- nlmr::nlm_fbm(ncol = 200, nrow = 200, fract_dim = 0.8)
devtools::use_data(fbmmap, overwrite = TRUE)

grdmap <- nlmr::nlm_planargradient(150, 150)
devtools::use_data(grdmap, overwrite = TRUE)

rndmap <- nlmr::nlm_random(150, 150)
devtools::use_data(rndmap, overwrite = TRUE)
