#' Balanza de pagos Anual
#'
#' @param indicador  Vea \code{\link{downloader}}
#'
#' @return [data.frame]: los datos del indicador en forma tabular
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   bpa <- balanza_pagos_anual()
#' }
balanza_pagos_anual <- function(indicador = NULL){
  `2010` <- NULL
  conceptos <- NULL
  orden <- NULL
  nivel <- NULL
  Conceptos <- NULL
  if(is.null(indicador)){
    indicador <-  c(
      original_url = "https://cdn.bancentral.gov.do/documents/estadisticas/sector-externo/documents/bpagos_6.xls",
      file_ext = "xls"
    )
  }
  fecha <- NULL
  file <- downloader(indicador)
  #file <- "/mnt/d/Descargas/bpagos_6 (1).xls"
  datos <- readxl::read_excel(file, skip = 5)
  datos <- tidyr::drop_na(datos, `2010`)
  datos <- dplyr::bind_cols(dplyr::select(domar::nvl_balanza_pagos_anual, -conceptos), datos)
  datos <- tidyr::pivot_longer(datos, -c(orden, nivel, Conceptos), names_to = "ano", values_to = "valor")
  datos
}
