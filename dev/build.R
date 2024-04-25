# pipe
usethis::use_pipe(export = TRUE)

## code to prepare MAKEDATA.R
usethis::use_data_raw("MAKEDATA")

devtools::load_all()


devtools::document()
devtools::check()


# Ensure that all dependencies are correctly installed, and generate the pak lock file.
#pak::pak()
#pak::lockfile_create()# for github action
#file.rename("pkg.lock", ".github/pkg.lock")



