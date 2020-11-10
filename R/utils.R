utils::globalVariables(c("ano", "mes", 'domar_list', '...2', 'V1', 'V2'))


#' Descarga de archivos
#'
#' @param indicador el vector de datos correspondiente al indicador en cuestion
#'
#' @return la ruta de un archivo temporal que contiene el archivo descargado
#'
#' @export
#'
#' @examples
#' \dontrun{
#' downloader(pib_gasto)
#' }
downloader <- function(indicador){
  httr::GET(indicador$original_url,
            httr::write_disk(tf <- tempfile(fileext = indicador$file_ext)))
  tf
}




#' Multiple variables to unique date variable
#'
#' @param data data.frame the target dataframe
#' @param ano numeric the character name of variable corresponding to year
#' @param mes character the character name of variable corresponding to month
#' @param dia numeric the character name of variable corresponding to day
#'
#' @return data.frame a new dataframe with the compute variable
#'
#' @export
#'
#' @examples
#' \dontrun{
#' df <- to_date(df)
#' }
to_date <- function(data, ano = 'ano', mes = 'mes', dia = NULL) {
  if(is.null(dia)){
    data <- data
    data <- dplyr::mutate(data,
                          mes = stringr::str_trim(mes),
                          mes = stringr::str_sub(mes, 1, 3),
                          mes = stringr::str_to_title(mes),
                          mes = dplyr::case_when(
                           mes == 'Ene' ~ 'Jan',
                           mes == 'Abr' ~ 'Apr',
                           mes == 'Ago' ~ 'Aug',
                           mes == 'Dic' ~ 'Dec',
                           TRUE ~ mes
                          ),
                          fecha = zoo::as.Date(zoo::as.yearmon(paste(mes, ano))))
    data[,mes] <- NULL
    data[,ano] <- NULL
    data
  }
}
