yen_mensual_ecb <- function(indicador){
  Period <- NULL
  file <- downloader(indicador)
  table <- rvest::html_nodes(file, 'table')
  table <- rvest::html_table(file, fill = TRUE)
  table <- table[[6]]
  table <- table[-c(1:2),1:2]
  table[1,1] <- 'Period'
  names(table) <- table[1,]
  table <- table[-1,]
  table <- tidyr::separate(table, Period, c('ano', 'mes'))
  table <- Dmisc::vars_to_date(table, ano = 1, mes = 2, tipo_mes = 'num')
  table
}
