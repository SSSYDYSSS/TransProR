# pipe
usethis::use_pipe(export = TRUE)

## code to prepare MAKEDATA.R
usethis::use_data_raw("MAKEDATA")

devtools::load_all()


devtools::document()
devtools::check()


#BiocManager::install("BiocCheck")
#BiocCheck::BiocCheck()



# Ensure that all dependencies are correctly installed, and generate the pak lock file.
pak::pak()
pak::pak("jokergoo/ComplexHeatmap")# 单独安装
pak::lockfile_create()# for github action
file.rename("pkg.lock", ".github/pkg.lock")


# NEWS.md
usethis::use_news_md() #生成一个标准模板。

