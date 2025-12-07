library(usethis)
library(devtools)
library(roxygen2)
library(testthat)
library(knitr)
library(rmarkdown)
library(styler)

usethis::use_r("sample_size_auto")
usethis::use_r("missing_helpers")
usethis::use_r("relation_infer")
usethis::use_r("joinless")

devtools::document()

usethis::use_testthat()
usethis::use_test("joinless")

usethis::use_readme_rmd()
devtools::build_readme()

install.packages("styler")
styler::style_pkg()

devtools::check()
devtools::check(args = "--as-cran")









