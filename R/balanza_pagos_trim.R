#' Balanza de pagos Trimestral
#'
#' @param indicador  Vea \code{\link{downloader}}
#'
#' @return [data.frame]: los datos del indicador en forma tabular
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   bpt <- balanza_pagos_trim()
#' }
balanza_pagos_trim <- function(indicador = NULL){
  V1 <- NULL
  V2 <- NULL
  . <- NULL
  name <- NULL
  if(is.null(indicador)){
    indicador <- c(
      original_url = "https://cdn.bancentral.gov.do/documents/estadisticas/sector-externo/documents/bpagos__trim_6.xls",
      file_ext = "xls"
    )
  }
  file <- downloader(indicador)
  #file <- "/mnt/d/Descargas/bpagos__trim_6 (4).xls"
  datos <- readxl::read_excel(file, skip = 4, col_names = F) %>%
    tidyr::drop_na(3) %>%
    t() %>%
    tibble::as.tibble() %>%
    dplyr::filter(is.na(V2) | !(V2 %in% c("Ene-Jun", "Ene-Sep", "Ene-Dic"))) %>%
    dplyr::mutate(
      V2 = stringr::str_remove(V2, stringr::regex("[a-zA-Z]*-")),
      V1 = paste(V1, V2)
    ) %>%
    dplyr::select(-V2) %>%
    t() %>%
    tibble::as.tibble() %>%
    janitor::row_to_names(1) %>%
    dplyr::bind_cols(domar::nvl_balanza_pagos, .)
  if(sum(datos[, "conceptos"] == datos[, "Conceptos NA"]) == 57){
    datos %>%
      dplyr::select(-"Conceptos NA") %>%
      tidyr::pivot_longer(-c(1:3), values_to = "valor") %>%
      tidyr::separate(name, into = c("ano", "mes")) %>%
      Dmisc::vars_to_date(year = "ano", month = "mes") %>%
      utils::type.convert()
  } else {
    stop("Nombres del indicador no machean.")
  }
}
