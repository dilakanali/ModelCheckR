library(usethis)
library(devtools)
library(roxygen2)
library(testthat)

usethis::use_mit_license()

usethis::use_testthat()

usethis::use_roxygen_md()

usethis::use_readme_rmd()

usethis::use_vignette("ModelCheckR")

usethis::use_pkgdown()
usethis::use_test("normality_check")
usethis::use_test("hetero_check")
usethis::use_test("multicol_check")
usethis::use_test("outlier_check")
usethis::use_test("model_diagnostics")
