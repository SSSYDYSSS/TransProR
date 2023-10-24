# pipe
usethis::use_pipe(export = TRUE)

## code to prepare MAKEDATA.R
usethis::use_data_raw("MAKEDATA")

devtools::load_all()
devtools::document()
devtools::check()
