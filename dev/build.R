# pipe
usethis::use_pipe(export = TRUE)

## code to prepare MAKEDATA.R
usethis::use_data_raw("MAKEDATA")


devtools::document()
devtools::check()


devtools::load_all()
