if (!requireNamespace("renviron", quietly = TRUE)) {
  utils::install.packages("renviron", repos = c("https://adatar-do.r-universe.dev"))
}


.DatafaroSessionID <- uuid::UUIDgenerate()


# 1. Definir la nueva clase
library(tibble)
setClass("datalight", contains = "tbl_df")
