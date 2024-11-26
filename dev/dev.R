# dependency package
usethis::use_package("stats")
usethis::use_package("utils")
usethis::use_package("DESeq2")
usethis::use_package("BiocGenerics")
usethis::use_package("ggplot2")
usethis::use_package("ggpubr")
usethis::use_package("tibble")

# license
usethis::use_mit_license("Dongyue Yu")

# readme
usethis::use_readme_md()

# adds a Code of Conduct
usethis::use_code_of_conduct(contact = "yudongyue@mail.nankai.edu.cn")

# vignette
usethis::use_vignette("a_example_workflow") # optional
devtools::install(build_vignettes = TRUE) # optional

## a website
# Run once to configure your package to use pkgdown
usethis::use_pkgdown() # optional
pkgdown::build_site() # optional

## add git
usethis::use_git()
usethis::use_github()

# CI
usethis::use_github_action_check_standard()

# CRAN
# Spell check
devtools::spell_check()
# Regular local tests
devtools::check()
# rhub cross-platform tests
rhub::check_for_cran()
# Windows platform-only test
devtools::check_win_devel() # optional
# rhub Windows platform-only test
rhub::check_for_cran(
  platform="windows-x86_64-devel",
  env_vars=c(R_COMPILE_AND_INSTALL_PACKAGES = "always")
) # optional



# Check before release
devtools::release()
