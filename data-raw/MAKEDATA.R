# 包要使用的数据,且用户能探查：
usethis::use_data(x)# 会产生文件在data/x.Rda

# 包要使用的数据,且用户不能探查，内部数据：
usethis::use_data(y, internal = TRUE)# 会产生文件在r/sysdata.rda，注意，此数据无需文档，也无需导出，用户不可见。
