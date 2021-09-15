#' Tipo de cambio libra esterlina
#'
#'  \lifecycle{experimental}
#'
#' @param indicador Vea \code{\link{downloader}}
#'
#' @return [data.frame]: los datos del indicador en forma tabular
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   pound <- tipo_cambio_gbp_eur_mensual()
#' }
tipo_cambio_gbp_eur_mensual <- function(indicador = NULL, metadata = FALSE){
  if(metadata){
    return(
      tibble::tribble(
        ~col, ~name, ~unit, ~dtype, ~key,
        "date", "Fecha", "Mensual", "mdate", 1,
        "value", "Tipo de cambio", "", "f2", 0
      )
    )
  }
  if(is.null(indicador)){
    indicador = c(
      original_url = "https://sdw.ecb.europa.eu/quickview.do?SERIES_KEY=120.EXR.M.GBP.EUR.SP00.A",
      file_ext = "html"
    )
  }
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
  table <- Dmisc::vars_to_date(table, year = 1, month = 2)
  table
}

##########################################################################################################

#' Tipo de cambio USD/Euro Mensual
#'
#' @param indicador Vea \code{\link{downloader}}
#'
#' @return [data.frame]: los datos del indicador en forma tabular
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  usd_eur <- tipo_cambio_usd_eur_mensual()
#' }
tipo_cambio_usd_eur_mensual <- function(indicador = NULL, metadata = FALSE){
  if(metadata){
    return(
      tibble::tribble(
        ~col, ~name, ~unit, ~dtype, ~key,
        "date", "Fecha", "Mensual", "mdate", 1,
        "value", "Tipo de cambio", "", "f2", 0
      )
    )
  }
  if(is.null(indicador)){
    indicador = c(
      original_url = "https://sdw.ecb.europa.eu/quickview.do?SERIES_KEY=120.EXR.M.USD.EUR.SP00.A",
       file_ext = "html"
    )
  }
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
  table <- Dmisc::vars_to_date(table, year = 1, month = 2)
  table
}

####################################################################################################################

#' Tipo de cambio Yen Japonés /Euro Mensual
#'
#' @param indicador Vea \code{\link{downloader}}
#'
#' @return [data.frame]: los datos del indicador en forma tabular
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  yen <- tipo_cambio_jpy_eur_mensual()
#' }
tipo_cambio_jpy_eur_mensual <- function(indicador = NULL, metadata = FALSE){
  if(metadata){
    return(
      tibble::tribble(
        ~col, ~name, ~unit, ~dtype, ~key,
        "date", "Fecha", "Mensual", "mdate", 1,
        "value", "Tipo de cambio", "", "f2", 0
      )
    )
  }
  if(is.null(indicador)){
    indicador = c(
      original_url = "https://sdw.ecb.europa.eu/quickview.do?SERIES_KEY=120.EXR.M.JPY.EUR.SP00.A",
       file_ext = "html"
    )
  }
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
  table <- Dmisc::vars_to_date(table, year = 1, month = 2)
  table
}
