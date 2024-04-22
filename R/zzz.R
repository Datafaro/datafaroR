if (!requireNamespace("renviron", quietly = TRUE)) {
  install.packages("renviron", repos = c("https://adatar-do.r-universe.dev"))
}


.DatafaroSessionID <- uuid::UUIDgenerate()
.DatafaroBoard <- pins::board_folder(path.expand("~/.DatafaroCache"))


# 1. Definir la nueva clase
setClass("datalight", contains = "tbl_df")
