#' Descarga de archivos
#'
#' @param indicador el vector de datos correspondiente al indicador en cuestion.
#'     Vea la sección \code{details} para más detalles.
#'
#' @return la ruta de un archivo temporal que contiene el archivo descargado
#'
#' @details El indicador pasado a la función es un vector de datos que debe
#'   contener por lo menos:
#'
#'   \itemize{
#'     \item{Extensión del archivo \code{(file_ext)}: uno de 'xlsx', 'xls',
#'      'csv', 'pdf', 'dir', 'sql'}
#'      \item{Enlace directo a la descarga \code{(original_url)}}
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' downloader(pib_gasto)
#' }
downloader <- function(indicador) {
  file_ext = as.character(indicador[["file_ext"]])
  original_url <- as.character(indicador[["original_url"]])
  #return(class(original_url))
  #return(length(file_ext))
  if (file_ext == "html") {
    xml2::read_html(original_url)
  } else {
    httr::GET(
      original_url,
      httr::write_disk(tf <- tempfile(fileext = paste0(".", file_ext)))
    )
    tf
  }
}



nivelador <- function(tbl){
  orden <- NULL
  tmax <- trunc(max(as.numeric(tbl$orden), na.rm = T))
  tbl <- tbl %>%
    dplyr::mutate(
      nivel0 = stringr::str_detect(orden, "\\-"),
      nivel = dplyr::if_else(
        tmax %in% c(99, 999, 9999) & tidyr::replace_na(as.numeric(orden), 0) >= tmax, tmax,
        stringr::str_count(orden, stringr::regex("\\.")) + 1
      ),
      nivel = dplyr::if_else(nivel0, -1*nivel, nivel),
      nivel0 = NULL
    )
  tbl$orden <- c(1:nrow(tbl))
  tbl
}

#' Ordenes y niveles de los indicadores
#'
#'
#' @format [data.frame]
"nvl_balanza_pagos"
"nvl_balanza_pagos_trim"
"nvl_pib_gasto"



#' Ejecutar funciones desde Python
#'
#'   Recibe los keys y valores de un diccionario python y lo convierte en un
#'   vector con nombres en R, que luego pasa a la función del indicador.
#'
#' @param keys [character]: keys del diccionario python que se utilizan como nombres del vector.
#' @param values values del diccionario python que se utilizan como el contenido del vector.
#'
#' @return Un vector con nombres que se pasa a la función del indicador.
#'
#' @export
from_python <- function(keys, values, data = NULL, metadata = FALSE){
  indicador <- values
  names(indicador) <- keys
  #indicador <- type.convert(indicador)
  if(!is.null(data)){
    get(stringr::str_replace_all(indicador$id, "-", "_"))(data, metadata = metadata)
  } else {
    get(stringr::str_replace_all(indicador$id, "-", "_"))(indicador, metadata = metadata)
  }
}



#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL


download_domar <- function(id){
  id = stringr::str_replace_all(id, "_", "-")
  readr::read_csv(url(glue::glue("{info$domar_url}/app/datos/{id}/d?out=csv&t={info$token}"))) %>%
    type.convert(as.is = T)
}
