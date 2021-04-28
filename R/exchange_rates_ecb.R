exchange_rates_ecb <- function(indicador){
  file <- downloader(indicador)
  table <- rvest::html_nodes(file, 'table')
  table <- rvest::html_nodes(table, 'tbody')
  table <- rvest::html_nodes(table, 'tr')
  table <- rvest::html_nodes(table, 'td')
}
