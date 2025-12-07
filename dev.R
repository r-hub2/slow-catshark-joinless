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

rhub::rc_submit()

library(rhub)

## 1) Registrar el token que te llegó por email ----
rhub::rc_new_token(
  email = "braylinjr1511@gmail.com",
  token = "73cf8ba4-6b7c-49e1-bfac-143b7c5612b1"
)

## 2) (Opcional) Verificar que el token quedó guardado ----
rhub::rc_list_local_tokens()

## 3) Enviar el paquete a R-hub para chequearlo ----
rhub::rc_submit()






