# Data that the package will use, and is explorable by users:
usethis::use_data(x) # This will create a file at data/x.Rda

# Data that the package will use, and is not explorable by users, internal data:
usethis::use_data(y, internal = TRUE) # This will create a file at R/sysdata.rda. Note, this data does not require documentation, nor does it need to be exported, it is not visible to users.
