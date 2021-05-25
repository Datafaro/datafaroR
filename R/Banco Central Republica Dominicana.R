#' Balanza de pagos Anual
#'
#' \lifecycle{experimental}
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
  . <- NULL
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
  unlink(file)
  datos <- tidyr::drop_na(datos, `2010`)
  datos <- datos %>%
    dplyr::bind_cols(domar::nvl_balanza_pagos, .)
  if(sum(datos[, "conceptos"] == datos[, "Conceptos"]) == 57){
    datos %>%
      dplyr::select(-"Conceptos") %>%
      tidyr::pivot_longer(-c(1:3), names_to = "ano", values_to = "valor")
  }
}

#' Balanza de pagos Trimestral
#'
#'  \lifecycle{experimental}
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

#' IMAE Mensual
#'
#'  \lifecycle{experimental}
#'
#' @param indicador Vea \code{\link{downloader}}
#'
#' @return [data.frame]: los datos del IMAE en forma tabular
#'
#' @export
#'
#' @examples
#' \dontrun{
#' imae <- imae_mensual(indicador)
#' }
imae_mensual <- function(indicador) {
  ano <- NULL
  mes <- NULL
  file_path <- downloader(indicador)
  imae <- readxl::read_excel(
    file_path,
    skip = 5)
  unlink(file_path)
  # Serie original
  imaeso <- imae[,1:6]
  imaeso <- imaeso[-1,]
  imaeso[1,1] <- 'ano'
  imaeso[1,2] <- 'mes'
  imaeso[1,3] <- 'Indice'
  names(imaeso) <- imaeso[1,]
  imaeso <- imaeso[-1,]
  names(imaeso)[names(imaeso) == 'Interanual'] <- 'Var_interanual'
  names(imaeso)[names(imaeso) == 'Acumulada'] <- 'Var_acumulada'
  names(imaeso)[names(imaeso) == 'Promedio 12 meses'] <- 'Var_promedio_12_meses'
  imaeso <- imaeso[!is.na(imaeso$Indice),]
  imaeso$ano <- stringr::str_remove(imaeso$ano, 'Promedio ')
  imaeso <- tidyr::fill(imaeso, ano, .direction = 'up')
  imaeso <- tidyr::fill(imaeso, ano)
  imaeso <- dplyr::filter(imaeso, !is.na(mes))
  imaeso <- Dmisc::vars_to_date(imaeso, year = 1, month = 2)
  imaeso$serie <- 'Serie original'

  # Serie desestacionalizada
  imaesd <- imae[,c(1:2, 7:11)]
  imaesd <- imaesd[-1,]
  imaesd[1,1] <- 'ano'
  imaesd[1,2] <- 'mes'
  imaesd[1,3] <- 'Indice'
  names(imaesd) <- imaesd[1,]
  imaesd <- imaesd[-1,]
  names(imaesd)[names(imaesd) == 'Respecto al per\u00EDodo anterior'] <- 'Var_periodo_anterior'
  names(imaesd)[names(imaesd) == 'Interanual'] <- 'Var_interanual'
  names(imaesd)[names(imaesd) == 'Acumulada'] <- 'Var_acumulada'
  names(imaesd)[names(imaesd) == 'Promedio 12 meses'] <- 'Var_promedio_12_meses'
  imaesd <- dplyr::mutate(imaesd,
                          ano = stringr::str_remove(ano, 'Promedio '))
  imaesd <- imaesd[!is.na(imaesd$Indice),]
  imaesd <- tidyr::fill(imaesd, ano, .direction = 'up')
  imaesd <- tidyr::fill(imaesd, ano)
  imaesd <- imaesd[!is.na(imaesd$mes),]
  imaesd <- Dmisc::vars_to_date(imaesd, year = 1, month = 2)
  imaesd$serie <- 'Serie desestacionalizada'

  # Serie tendencia-ciclo
  imaest <- imae[,c(1:2, 12:16)]
  imaest <- imaest[-1,]
  imaest[1,1] <- 'ano'
  imaest[1,2] <- 'mes'
  imaest[1,3] <- 'Indice'
  names(imaest) <- imaest[1,]
  imaest <- imaest[-1,]
  names(imaest)[names(imaest) == 'Respecto al per\u00EDodo anterior'] <- 'Var_periodo_anterior'
  names(imaest)[names(imaest) == 'Interanual'] <- 'Var_interanual'
  names(imaest)[names(imaest) == 'Acumulada'] <- 'Var_acumulada'
  names(imaest)[names(imaest) == 'Promedio 12 meses'] <- 'Var_promedio_12_meses'
  imaest <- imaest[!is.na(imaest$Indice),]
  imaest <- dplyr::mutate(imaest,
                          ano = stringr::str_remove(ano, 'Promedio '))
  imaest <- tidyr::fill(imaest, ano, .direction = 'up')
  imaest <- tidyr::fill(imaest, ano)
  imaest <- imaest[!is.na(imaest$mes),]
  imaest <- Dmisc::vars_to_date(imaest, year = 1, month = 2)
  imaest$serie <- 'Serie Tendencia-Ciclo'

  #
  dplyr::bind_rows(imaeso, imaesd, imaest)
}

#' Deflactor del PIB
#'
#'  \lifecycle{experimental}
#'
#' @param indicador Vea \code{\link{downloader}}
#'
#' @return [data.frame]: los datos como se descargan del banco con muy pocas
#'   modificaciones. Luego esos datos son tomados por pib_deflactor_trimestral y
#'   pib_deflactor_anual
#'
#' @examples
#' \dontrun{
#'   def <- pib_deflactor()
#' }
pib_deflactor <- function(indicador) {
  `...1` <- NULL
  ano <- NULL
  `...2` <- NULL
  file <- "/mnt/d/Descargas/pib_deflactor_2007.xls"
  #file <- downloader(indicador)
  def <- readxl::read_excel(file, skip = 4, col_names = F)
  return(def)
  def %>%
    dplyr::mutate(
      ano = dplyr::if_else(stringr::str_detect(...1, "[0-9]"), ...1, NA_character_),
      ano = stringr::str_remove_all(ano, "[^0-9]")
    ) %>%
    tidyr::fill(ano, .direction = "up") %>%
    tidyr::drop_na(...2)
}

#' Deflactor del PIB Trimestral
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
#'   dpt <- pib_deflactor_trimestral()
#' }
pib_deflactor_trimestral <- function(indicador = c(original_url = "https://cdn.bancentral.gov.do/documents/estadisticas/sector-real/documents/pib_deflactor_2007.xls", file_ext = "xls")){
  `...1` <- NULL
  ano <- NULL
  serie <- NULL
  fecha <- NULL
  def <- pib_deflactor(indicador) %>%
    tidyr::fill(
      ...1
    ) %>%
    dplyr::filter(
      !stringr::str_detect(...1, "[0-9]")
    ) %>%
    dplyr::mutate(
      ...1 = dplyr::case_when(
        ...1 == "IV" ~ "Q4",
        ...1 == "III" ~ "Q3",
        ...1 == "II" ~ "Q2",
        ...1 == "I" ~ "Q1"
      ),
      fecha = paste(ano, ...1)
    ) %>%
    dplyr::select(
      -c(ano, ...1)
    )%>%
    dplyr::relocate(fecha) %>%
    t() %>%
    as.data.frame()
  def[1,1] <- "serie"
  def[1,2] <- "indicador"
  def <- def %>%
    janitor::row_to_names(1) %>%
    tidyr::fill(serie) %>%
    tidyr::pivot_longer(-c(serie, indicador), values_drop_na = T)
}

#' Deflactor del PIB Anual
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
#'   dpa <- pib_deflactor_anual()
#' }
pib_deflactor_anual <- function(indicador = c(original_url = "https://cdn.bancentral.gov.do/documents/estadisticas/sector-real/documents/pib_deflactor_2007.xls", file_ext = "xls")){
  `...1` <- NULL
  ano <- NULL
  serie <- NULL
  def <- pib_deflactor(indicador) %>%
    tidyr::fill(
      ...1
    ) %>%
    dplyr::filter(
      !startsWith(...1, "I")
    ) %>%
    dplyr::select(
      -c(...1)
    )%>%
    dplyr::relocate(ano) %>%
    t() %>%
    as.data.frame()
  def[1,1] <- "serie"
  def[1,2] <- "indicador"
  def <- def %>%
    janitor::row_to_names(1) %>%
    tidyr::fill(serie) %>%
    tidyr::pivot_longer(-c(serie, indicador), values_drop_na = T)
}



#' Índice de Precios al Consumidor (octubre 2019 - septiembre 2020)
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
#'   ipc20 <- ipc_mensual_2020()
#' }
ipc_mensual_2020 <- function(indicador = NULL){
  if(is.null(indicador)){
    indicador = c(
      original_url = "https://cdn.bancentral.gov.do/documents/estadisticas/precios/documents/ipc_base_2019-2020.xls",
      file_ext = "xls"
    )
  }
  `...2` <- NULL
  `...1` <- NULL
  file <- downloader(indicador)
  datos <- readxl::read_excel(file, col_names = F)
  datos <- tidyr::drop_na(datos, ...2)
  datos <- tidyr::fill(datos, ...1)
  datos <- datos[,1:7]
  names(datos) <- c('ano',
                    'mes',
                    'indice',
                    'variacion_mensual',
                    'variacion_con_diciembre',
                    'variacion_anual',
                    'variacion_promedio_12_meses')
  datos <- Dmisc::vars_to_date(datos, year = 1, month = 2)
  datos
}
