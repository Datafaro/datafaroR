# Mercado cambiario ----


#' Tipo de cambio dólar diario
#'
#'  \lifecycle{experimental}
#'
#' @param indicador Vea \code{\link{downloader}}
#' @param metadata indica si se retornan los datos o la metadata del indicador
#'
#' @return [data.frame]: los datos del indicador en forma tabular
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   tipo_cambio_dolar_diario()
#' }
tipo_cambio_dolar_diario <- function(indicador = NULL, metadata = FALSE){
  if(metadata){
    return(
      tibble::tribble(
        ~col, ~name, ~unit, ~dtype,
        "date", "Fecha", "Diaria", "date",
        "compra", "Precio de compra", "RD$/US$", "f2",
        "venta", "Precio de venta", "RD$/US$", "f2"
      )
    )
  }
  if(is.null(indicador)){
    indicador <- c(
      original_url = "https://cdn.bancentral.gov.do/documents/estadisticas/mercado-cambiario/documents/TASA_DOLAR_REFERENCIA_MC.xls",
      file_ext = "xls"
    )
  }
  file <- "/mnt/c/Users/drdsd/Downloads/TASA_DOLAR_REFERENCIA_MC.xls"
  if (!file.exists(file)) {
    file <- downloader(indicador)
  }
  readxl::read_excel(file, sheet = "Diaria", skip = 2) %>%
    Dmisc::vars_to_date(year = 1, month = 2, day = 3) %>%
    setNames(c("date", "compra", "venta")) %>%
    type.convert(as.is = T)
}


#' @rdname tipo_cambio_dolar_diario
#' @export
tipo_cambio_usd_dop_diario <- function(...) tipo_cambio_dolar_diario(...)


#' Tipo de cambio dólar mensual
#'
#'  \lifecycle{experimental}
#'
#' @param indicador Vea \code{\link{downloader}}
#' @param metadata indica si se retornan los datos o la metadata del indicador
#'
#' @return [data.frame]: los datos del indicador en forma tabular
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   tipo_cambio_dolar_mensual()
#' }
tipo_cambio_dolar_mensual <- function(indicador = NULL, metadata = FALSE){
  if(metadata){
    return(
      tibble::tribble(
        ~col, ~name, ~unit, ~dtype, ~key,
        "date", "Fecha", "Mensual", "mdate", 1,
        "compra", "Precio de compra", "RD$/US$", "f2", 0,
        "venta", "Precio de venta", "RD$/US$", "f2", 0,
        "tipo", "Tipo de indicador", "", "text", 1
      )
    )
  }
  if(is.null(indicador)){
    indicador <- c(
      original_url = "https://cdn.bancentral.gov.do/documents/estadisticas/mercado-cambiario/documents/TASA_DOLAR_REFERENCIA_MC.xls",
      file_ext = "xls"
    )
  }
  file <- "/mnt/c/Users/drdsd/Downloads/TASA_DOLAR_REFERENCIA_MC.xls"
  if (!file.exists(file)) {
    file <- downloader(indicador)
  }
  readxl::read_excel(file, sheet = "PromMensual", skip = 2) %>%
    dplyr::mutate(tipo = "Promedio mensual") %>%
    dplyr::bind_rows(
      readxl::read_excel(file, sheet = "FPMensual", skip = 2) %>%
        dplyr::mutate(tipo = "Final de período mensual")
    ) %>%
    Dmisc::vars_to_date(year = 1, month = 2) %>%
    setNames(c("date", "compra", "venta", "tipo")) %>%
    type.convert(as.is = T)
}


#' @rdname tipo_cambio_dolar_mensual
#' @export
tipo_cambio_usd_dop_mensual <- function(...) tipo_cambio_dolar_mensual(...)


#' Tipo de cambio dólar trimestral
#'
#'  \lifecycle{experimental}
#'
#' @param indicador Vea \code{\link{downloader}}
#' @param metadata indica si se retornan los datos o la metadata del indicador
#'
#' @return [data.frame]: los datos del indicador en forma tabular
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   tipo_cambio_dolar_trim()
#' }
tipo_cambio_dolar_trim <- function(indicador = NULL, metadata = FALSE){
  if(is.null(indicador)){
    indicador <- c(
      original_url = "https://cdn.bancentral.gov.do/documents/estadisticas/mercado-cambiario/documents/TASA_DOLAR_REFERENCIA_MC.xls",
      file_ext = "xls",
      max_changes = 4
    )
  }
  if(metadata){
    return(
      list(
        "kvars" = c("date", "tipo"),
        "max_changes" = indicador$max_changes,
        "var_info" =
          tibble::tribble(
            ~col, ~name, ~unit, ~dtype,
            "date", "Fecha", "Trimestral", "qdate",
            "compra", "Precio de compra", "RD$/US$", "f2",
            "venta", "Precio de venta", "RD$/US$", "f2",
            "tipo", "Tipo de indicador", "", "text"
          )
      )
    )
  }
  file <- "/mnt/c/Users/drdsd/Downloads/TASA_DOLAR_REFERENCIA_MC.xls"
  if (!file.exists(file)) {
    file <- downloader(indicador)
  }
  tbl <- readxl::read_excel(file, sheet = "PromTrimestral", skip = 2) %>%
    dplyr::mutate(tipo = "Promedio trimestral") %>%
    dplyr::bind_rows(
      readxl::read_excel(file, sheet = "FPTrimestral", skip = 2) %>%
        dplyr::mutate(tipo = "Final de período trimestral")
    )

  tbl %>%
    Dmisc::vars_to_date(year = 1, quarter = 2) %>%
    setNames(c("date", "compra", "venta", "tipo")) %>%
    type.convert(as.is = T)
}


#' @rdname tipo_cambio_dolar_trim
#' @export
tipo_cambio_usd_dop_trim <- function(...) tipo_cambio_dolar_trim(...)


#' Tipo de cambio dólar anual
#'
#'  \lifecycle{experimental}
#'
#' @param indicador Vea \code{\link{downloader}}
#' @param metadata indica si se retornan los datos o la metadata del indicador
#'
#' @return [data.frame]: los datos del indicador en forma tabular
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   tipo_cambio_dolar_anual()
#' }
tipo_cambio_dolar_anual <- function(indicador = NULL, metadata = FALSE){
  if(metadata){
    return(
      tibble::tribble(
        ~col, ~name, ~unit, ~dtype,
        "ano", "Año", "", "ydate",
        "compra", "Precio de compra", "RD$/US$", "f2",
        "venta", "Precio de venta", "RD$/US$", "f2",
        "tipo", "Tipo de indicador", "", "text"
      )
    )
  }
  if(is.null(indicador)){
    indicador <- c(
      original_url = "https://cdn.bancentral.gov.do/documents/estadisticas/mercado-cambiario/documents/TASA_DOLAR_REFERENCIA_MC.xls",
      file_ext = "xls"
    )
  }
  file <- "/mnt/c/Users/drdsd/Downloads/TASA_DOLAR_REFERENCIA_MC.xls"
  if (!file.exists(file)) {
    file <- downloader(indicador)
  }
  readxl::read_excel(file, sheet = "PromAnual", skip = 2) %>%
    dplyr::mutate(tipo = "Promedio anual") %>%
    dplyr::bind_rows(
      readxl::read_excel(file, sheet = "FPAnual", skip = 2) %>%
        dplyr::mutate(tipo = "Final de período anual")
    ) %>%
    setNames(c("ano", "compra", "venta", "tipo")) %>%
    type.convert(as.is = T)
}


#' @rdname tipo_cambio_dolar_anual
#' @export
tipo_cambio_usd_dop_anual <- function(...) tipo_cambio_dolar_anual(...)



# Precios ----


#' Índice de Precios al Consumidor (octubre 2019 - septiembre 2020)
#'
#'  \lifecycle{experimental}
#'
#' @param indicador Vea \code{\link{downloader}}
#' @param metadata indica si se retornan los datos o la metadata del indicador
#'
#' @return [data.frame]: los datos del indicador en forma tabular
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   ipc_mensual_2020()
#' }
ipc_mensual_2020 <- function(indicador = NULL, metadata = FALSE){
  if(metadata){
    return(
      tibble::tribble(
        ~col, ~name, ~unit, ~dtype, ~key,
        "date", "Fecha", "Mensual", "mdate",1,
        "indice", "IPC", "Índice", "f1",0,
        "indice__tc", "Tasa de crecimiento mensual", "Porcentaje (%)", "f1",0,
        "indice__tcd", "Tasa de crecimiento con diciembre", "Porcentaje (%)", "f1",0,
        "indice__tci", "Tasa de crecimiento interanual", "Porcentaje (%)", "f1",0,
        "indice__tc12meses", "Tasa de crecimiento promedio 12 meses", "Porcentaje (%)", "f1",0
      )
    )
  }
  if(is.null(indicador)){
    indicador = c(
      original_url = "https://cdn.bancentral.gov.do/documents/estadisticas/precios/documents/ipc_base_2019-2020.xls",
      file_ext = "xls"
    )
  }
  `...2` <- NULL
  `...1` <- NULL
  file <- "/mnt/c/Users/drdsd/Downloads/ipc_base_2019-2020.xls"
  if (!file.exists(file)) {
    file <- downloader(indicador)
  }
  datos <- readxl::read_excel(file, col_names = F)
  datos <- tidyr::drop_na(datos, ...2)
  datos <- tidyr::fill(datos, ...1)
  datos <- datos[,1:7]
  names(datos) <- c('ano',
                    'mes',
                    'indice',
                    'indice__tc',
                    'indice__tcd',
                    'indice__tci',
                    'indice__tc12meses')
  datos <- Dmisc::vars_to_date(datos, year = 1, month = 2)
  datos %>%
    type.convert(as.is = T)
}


#' Índice de Precios al Consumidor (IPC) anualizado
#'
#'  \lifecycle{experimental}
#'
#' @param indicador Vea \code{\link{downloader}}
#' @param metadata indica si se retornan los datos o la metadata del indicador
#'
#' @return [data.frame]: los datos del indicador en forma tabular
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   ipc_anualizado()
#' }
ipc_anualizado <- function(indicador = NULL, metadata = FALSE){
  if(metadata){
    return(
      tibble::tribble(
        ~col, ~name, ~unit, ~dtype,
        "ano", "Año", "", "ydate",
        "ipc_2010", "IPC (2010 = 100)", "Índice", "f1",
        "ipc_2020", "IPC (Oct. 2019 - Sep. 2020)", "Índice", "f1",
        "inflacion_anualizada", "Tasa de inflación anualizada", "Porcentaje (%)", "f1",
        "inflacion_promedio_12_meses", "Tasa de inflación promedio 12 meses", "Porcentaje (%)", "f1"
      )
    )
  }
  if(is.null(indicador)){
    indicador <- c(
      original_url = "https://cdn.bancentral.gov.do/documents/estadisticas/precios/documents/ipc_anual_base_2019-2020.xls",
      file_ext = "xls"
    )
  }
  file <- "/mnt/c/Users/drdsd/Downloads/ipc_anual_base_2019-2020.xls"
  if (!file.exists(file)) {
    file <- downloader(indicador)
  }
  readxl::read_excel(file, skip = 7, col_names = F) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), .fns = as.numeric)) %>%
    tidyr::drop_na(...1) %>%
    setNames(c("ano", "ipc_2010", "ipc_2020", "inflacion_anualizada", "inflacion_promedio_12_meses")) %>%
    type.convert(as.is = T)
}


#' Inflación promedio 12 meses
#'
#'  \lifecycle{experimental}
#'
#' @param indicador Vea \code{\link{downloader}}
#' @param metadata indica si se retornan los datos o la metadata del indicador
#'
#' @return [data.frame]: los datos del indicador en forma tabular
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   inflacion_promedio_12_meses()
#' }
inflacion_promedio_12_meses <- function(indicador = NULL, metadata = FALSE){
  if(metadata){
    return(
      tibble::tribble(
        ~col, ~name, ~unit, ~dtype,
        "ano", "Año", "", "ydate",
        "ipc_2010", "IPC (2010 = 100)", "Índice", "f1",
        "ipc_2020", "IPC (Oct. 2019 - Sep. 2020)", "Índice", "f1",
        "inflacion_promedio_12_meses", "Tasa de inflación promedio 12 meses", "Porcentaje (%)", "f1"
      )
    )
  }
  if(is.null(indicador)){
    indicador <- c(
      original_url = "https://cdn.bancentral.gov.do/documents/estadisticas/precios/documents/ipc_anual_1947_2020.xls",
      file_ext = "xls"
    )
  }
  file <- "/mnt/c/Users/drdsd/Downloads/ipc_anual_1947_2020.xls"
  if (!file.exists(file)) {
    file <- downloader(indicador)
  }
  readxl::read_excel(file, skip = 6, col_names = F)[,1:4] %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(), .fns = as.numeric
      )
    ) %>%
    tidyr::drop_na(...1) %>%
    setNames(c("ano", "ipc_2010", "ipc_2020", "inflacion_promedio_12_meses")) %>%
    type.convert(as.is = T)
}


#' IPC Subyacente
#'
#'  \lifecycle{experimental}
#'
#' @param indicador Vea \code{\link{downloader}}
#' @param metadata indica si se retornan los datos o la metadata del indicador
#'
#' @return [data.frame]: los datos del indicador en forma tabular
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   ipc_subyacente()
#' }
ipc_subyacente <- function(indicador = NULL, metadata = FALSE){
  if(metadata){
    return(
      tibble::tribble(
        ~col, ~name, ~unit, ~dtype,
        "date", "Fecha", "Mensual", "mdate",
        "ipc", "IPC Subyacente", "Índice", "f1",
        "inflacion_mensual", "Tasa de inflación mensual", "Porcentaje (%)", "f1",
        "inflacion_acumulada", "Tasa de inflación acumulada", "Porcentaje (%)", "f1",
        "inflacion_anualizada", "Tasa de inflación anualizada", "Porcentaje (%)", "f1"
      )
    )
  }
  if(is.null(indicador)){
    indicador <- c(
      original_url = "https://cdn.bancentral.gov.do/documents/estadisticas/precios/documents/ipc_subyacente_base_2019-2020.xlsx",
      file_ext = "xlsx"
    )
  }
  file <- "/mnt/c/Users/drdsd/Downloads/ipc_subyacente_base_2019-2020.xlsx"
  if (!file.exists(file)) {
    file <- downloader(indicador)
  }
  readxl::read_excel(file, skip = 5, col_names = F)[,1:6] %>%
    tidyr::drop_na(...2) %>%
    tidyr::fill(...1) %>%
    setNames(c("ano", "mes", "ipc", "inflacion_mensual", "inflacion_acumulada", "inflacion_anualizada")) %>%
    Dmisc::vars_to_date(year = 1, month = 2) %>%
    type.convert(as.is = T)
}


#' IPC transables y no transables
#'
#'  \lifecycle{experimental}
#'
#' @param indicador Vea \code{\link{downloader}}
#' @param metadata indica si se retornan los datos o la metadata del indicador
#'
#' @return [data.frame]: los datos del indicador en forma tabular
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   ipc_transables_no_transables()
#' }
ipc_transables_no_transables <- function(indicador = NULL, metadata = FALSE){
  if(metadata){
    return(
      tibble::tribble(
        ~col, ~name, ~unit, ~dtype,
        "date", "Fecha", "Mensual", "mdate",
        "ipc", "IPC", "Índice", "f1",
        "variacion_mensual", "Variación porcentual mensual", "Porcentaje (%)", "f1",
        "variacion_con_diciembre", "Variación porcentual con diciembre", "Porcentaje (%)", "f1",
        "grupo", "Grupo de bienes", "", "text"
      )
    )
  }
  if(is.null(indicador)){
    indicador <- c(
      original_url = "https://cdn.bancentral.gov.do/documents/estadisticas/precios/documents/ipc_tnt_base_2019-2020.xls",
      file_ext = "xls"
    )
  }
  file <- "/mnt/c/Users/drdsd/Downloads/ipc_tnt_base_2019-2020.xls"
  if (!file.exists(file)) {
    file <- downloader(indicador)
  }
  ipc <- readxl::read_excel(file, skip = 5, col_names = F) %>%
    tidyr::drop_na(...2) %>%
    dplyr::filter(!stringr::str_detect(...2, "[0-9]"))

  ipc[,1:5] %>%
    dplyr::mutate(
      grupo = "IPC General"
    ) %>%
    dplyr::bind_rows(
      ipc[,c(1,2,6:8)] %>%
        dplyr::mutate(
          grupo = "Bienes transables"
        )
    ) %>%
    dplyr::bind_rows(
      ipc[,c(1,2,9:11)] %>%
        dplyr::mutate(
          grupo = "Bienes no transables"
        )
    ) %>%
    tidyr::fill(...1) %>%
    Dmisc::vars_to_date(year = 1, month = 2) %>%
    setNames(c("date", "ipc", "variacion_mensual", "variacion_con_diciembre", "grupo")) %>%
    dplyr::mutate(
      dplyr::across(2:4, .fns = as.numeric)
    )
}


ipc_mensual_2010 <- function(indicador = NULL, metadata = FALSE){
  if(metadata){
    return(
      tibble::tribble(
        ~col, ~name, ~unit, ~dtype,
        "date", "Fecha", "Mensual", "mdate",
        "indice", "IPC", "Índice", "f1",
        "variacion_mensual", "Variación porcentual mensual", "Porcentaje (%)", "f1",
        "variacion_con_diciembre", "Variación porcentual con diciembre", "Porcentaje (%)", "f1",
        "variacion_anual", "Variación porcentual anual", "Porcentaje (%)", "f1",
        "variacion_promedio_12_meses", "Variación promedio 12 meses", "Porcentaje (%)", "f1"
      )
    )
  }
  if(is.null(indicador)){
    indicador = c(
      original_url = "https://cdn.bancentral.gov.do/documents/estadisticas/precios/documents/ipc_base_2019-2020.xls",
      file_ext = "xls"
    )
  }
  `...2` <- NULL
  `...1` <- NULL
  file <- "/mnt/c/Users/drdsd/Downloads/ipc_base_2019-2020.xls"
  if (!file.exists(file)) {
    file <- downloader(indicador)
  }
}




# Sector externo ----


#' Balanza de pagos anual
#'
#'  \lifecycle{experimental}
#'
#' @param indicador Vea \code{\link{downloader}}
#' @param metadata indica si se retornan los datos o la metadata del indicador
#'
#' @return [data.frame]: los datos del indicador en forma tabular
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   balanza_pagos_anual()
#' }
balanza_pagos_anual <- function(indicador = NULL, metadata = FALSE){
  if(metadata){
    return(
      tibble::tribble(
        ~col, ~name, ~unit, ~dtype, ~key,
        "orden", "Orden", "", "int", 1,
        "nivel", "Nivel", "", "int", 1,
        "conceptos", "Conceptos", "", "text", 1,
        "ano", "Año", "", "ydate", 1,
        "valor", "Valor", "Millones de US$", "f1", 0
      )
    )
  }
  if(is.null(indicador)){
    indicador <- c(
      original_url = "https://cdn.bancentral.gov.do/documents/estadisticas/sector-externo/documents/bpagos_6.xls",
      file_ext = "xls"
    )
  }
  file <- "/mnt/c/Users/drdsd/Downloads/bpagos_6.xls"
  if (!file.exists(file)) {
    file <- downloader(indicador)
  }
  readxl::read_excel(file, skip = 4) %>%
    tidyr::drop_na(2) %>%
    dplyr::bind_cols(nvl_balanza_pagos) %>%
    dplyr::relocate(orden, nivel, conceptos) %>%
    dplyr::select(-conceptos) %>%
    tidyr::pivot_longer(-c(1:3), names_to = "ano", values_to = "valor") %>%
    setNames(., tolower(names(.))) %>%
    type.convert(as.is = T)

  download_domar("pib-gasto-anual")
}


#' Balanza de pagos trimestral
#'
#'  \lifecycle{experimental}
#'
#' @param indicador Vea \code{\link{downloader}}
#' @param metadata indica si se retornan los datos o la metadata del indicador
#'
#' @return [data.frame]: los datos del indicador en forma tabular
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   balanza_pagos_trim()
#' }
balanza_pagos_trim <- function(indicador = NULL, metadata = FALSE){
  if(metadata){
    return(
      tibble::tribble(
        ~col, ~name, ~unit, ~dtype, ~key,
        "orden", "Orden", "", "int", 1,
        "nivel", "Nivel", "", "int", 1,
        "conceptos", "Conceptos", "", "text", 1,
        "date", "Fecha", "Trimestral", "qdate", 1,
        "valor", "Valor trimestral", "Millones de US$", "f1", 0,
        "valor_acumulado", "Valor acumulado", "Millones de US$", "f1", 0
      )
    )
  }
  if(is.null(indicador)){
    indicador <- c(
      original_url = "https://cdn.bancentral.gov.do/documents/estadisticas/sector-externo/documents/bpagos__trim_6.xls",
      file_ext = "xls"
    )
  }
  file <- "/mnt/c/Users/drdsd/Downloads/bpagos__trim_6.xls"
  if (!file.exists(file)) {
    print("Downloading...")
    file <- downloader(indicador)
  }
  bpa <- readxl::read_excel(file, col_names = F) %>%
    tidyr::drop_na(...2) %>%
    t() %>%
    as.data.frame() %>%
    dplyr::mutate(
      V1 = dplyr::if_else(V1 == "Conceptos", "1900", V1),
      V2 = dplyr::if_else(is.na(V2), "Ene-Mar", V2)
    )

  bpan <- bpa %>%
    dplyr::filter(V2 == "Ene-Mar" | !stringr::str_detect(V2, "Ene-")) %>%
    Dmisc::vars_to_date(year = 1, quarter = 2) %>%
    t() %>%
    as.data.frame() %>%
    janitor::row_to_names(1) %>%
    dplyr::bind_cols(nvl_balanza_pagos) %>%
    dplyr::relocate(orden, nivel, conceptos) %>%
    dplyr::select(-conceptos) %>%
    tidyr::pivot_longer(-c(1:3), names_to = "date", values_to = "valor")

  bpaa <- bpa %>%
    dplyr::filter(stringr::str_detect(V2, "Ene-")) %>%
    Dmisc::vars_to_date(year = 1, quarter = 2) %>%
    t() %>%
    as.data.frame() %>%
    janitor::row_to_names(1) %>%
    dplyr::bind_cols(nvl_balanza_pagos) %>%
    dplyr::relocate(orden, nivel, conceptos) %>%
    dplyr::select(-conceptos) %>%
    tidyr::pivot_longer(-c(1:3), names_to = "date", values_to = "valor_acumulado")

  bpan %>%
    dplyr::left_join(bpaa) %>%
    setNames(c("orden", "nivel", "conceptos", "date", "valor", "valor_acumulado")) %>%
    type.convert(as.is = T) %>%
    dplyr::left_join(
      readr::read_csv(url("http://159.203.111.74/app/datos/pib-gasto-trim/d?out=csv&t=24d6e719-e8fc-421e-942c-95b59f170879")) %>%
        type.convert(as.is = T) %>%
        dplyr::filter(componente == "Producto Interno Bruto") %>%
        dplyr::select(date, pib_usd, pib_acumulado_usd)
    ) %>%
    dplyr::mutate(
      valor__ppib = valor/pib*100,
      valor_acumulado__ppib = valor_acumulado/pib_acumulado*100
    ) %>%
    dplyr::select(-pib, -pib_acumulado) %>%
    dplyr::group_by(conceptos) %>%
    dplyr::arrange(conceptos, date) %>%
    dplyr::mutate(
      valor__tc = (valor/dplyr::lag(valor)-1)*100,
      valor__tca = (valor/dplyr::lag(valor, 4)-1)*100,
      valor_acumulado__tca = (valor_acumulado/dplyr::lag(valor_acumulado, 4)-1)*100
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(orden, nivel)
}


#' Exportaciones trimestral
#'
#'  \lifecycle{experimental}
#'
#' @param indicador Vea \code{\link{downloader}}
#' @param metadata indica si se retornan los datos o la metadata del indicador
#'
#' @return [data.frame]: los datos del indicador en forma tabular
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   exportaciones_trim()
#' }
exportaciones_trim <- function(indicador = NULL, metadata = FALSE){
  if(metadata){
    return(
      tibble::tribble(
        ~col, ~name, ~unit, ~dtype,
        "orden", "Orden", "", "int",
        "nivel", "Nivel", "", "int",
        "codigo", "Código del producto", "", "text",
        "detalle", "Detalle", "", "text",
        "date", "Fecha", "Trimestral", "qdate",
        "valor", "Valor", "Millones de US$", "f1"
      )
    )
  }
  if(is.null(indicador)){
    indicador <- c(
      original_url = "https://cdn.bancentral.gov.do/documents/estadisticas/sector-externo/documents/Exportaciones_Trimestrales_6.xls",
      file_ext = "xls",
      max_changes = 64*4
    )
  }
  file <- "/mnt/c/Users/drdsd/Downloads/Exportaciones_Trimestrales_6.xls"
  if (!file.exists(file)) {
    file <- downloader(indicador)
  }
  export <- readxl::read_excel(file, skip = 3, col_names = F) %>%
    tidyr::drop_na(...3) %>%
    t() %>%
    as.data.frame() %>%
    dplyr::filter(is.na(V2) | V2 != "Total") %>%
    tidyr::fill(V1)
  export$V1[1:2] <- c("1900", "1900")
  export$V2[1:2] <- c("E-M", "A-J")
  export %>%
    Dmisc::vars_to_date(year = 1, quarter = 2) %>%
    t() %>%
    as.data.frame() %>%
    janitor::row_to_names(1) %>%
    dplyr::bind_cols(nvl_exportaciones) %>%
    dplyr::relocate(orden, nivel, detalle) %>%
    tidyr::pivot_longer(-c(1:5), names_to = "date", values_to = "valor") %>%
    dplyr::select(-detalle) %>%
    setNames(c("orden", "nivel", "codigo", "detalle", "date", "valor")) %>%
    type.convert(as.is = T)
}


#' Exportaciones anual
#'
#'  \lifecycle{experimental}
#'
#' @param indicador Vea \code{\link{downloader}}
#' @param metadata indica si se retornan los datos o la metadata del indicador
#'
#' @return [data.frame]: los datos del indicador en forma tabular
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   exportaciones_anual()
#' }
exportaciones_anual <- function(indicador = NULL, metadata = FALSE){
  if(metadata){
    return(
      tibble::tribble(
        ~col, ~name, ~unit, ~dtype,
        "orden", "Orden", "", "int",
        "nivel", "Nivel", "", "int",
        "codigo", "Código del producto", "", "text",
        "detalle", "Detalle", "", "text",
        "ano", "Año", "", "ydate",
        "valor", "Valor", "Millones de US$", "f1"
      )
    )
  }
  if(is.null(indicador)){
    indicador <- c(
      original_url = "https://cdn.bancentral.gov.do/documents/estadisticas/sector-externo/documents/Exportaciones_Anuales_6.xls",
      file_ext = "xls"
    )
  }
  file <- "/mnt/c/Users/drdsd/Downloads/Exportaciones_Anuales_6.xls"
  if (!file.exists(file)) {
    file <- downloader(indicador)
  }
  readxl::read_excel(file, skip = 6) %>%
    tidyr::drop_na(`2010`) %>%
    dplyr::bind_cols(nvl_exportaciones) %>%
    dplyr::relocate(orden, nivel, detalle) %>%
    dplyr::select(-detalle) %>%
    tidyr::pivot_longer(-c(1:4)) %>%
    setNames(c("orden", "nivel", "codigo", "detalle", "ano", "valor")) %>%
    type.convert(as.is = T)
}


#' Importaciones trimestral
#'
#'  \lifecycle{experimental}
#'
#' @param indicador Vea \code{\link{downloader}}
#' @param metadata indica si se retornan los datos o la metadata del indicador
#'
#' @return [data.frame]: los datos del indicador en forma tabular
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   importaciones_trim()
#' }
importaciones_trim <- function(indicador = NULL, metadata = FALSE){
  if(metadata){
    return(
      tibble::tribble(
        ~col, ~name, ~unit, ~dtype,
        "orden", "Orden", "", "int",
        "nivel", "Nivel", "", "int",
        "codigo", "Código del producto", "", "text",
        "detalle", "Detalle", "", "text",
        "date", "Fecha", "Trimestral", "qdate",
        "valor", "Valor", "Millones de US$", "f1"
      )
    )
  }
  if(is.null(indicador)){
    indicador <- c(
      original_url = "https://cdn.bancentral.gov.do/documents/estadisticas/sector-externo/documents/Importaciones_Trimestrales_6.xls",
      file_ext = "xls",
      max_changes = 54*5
    )
  }
  file <- "/mnt/c/Users/drdsd/Downloads/Importaciones_Trimestrales_6.xls"
  if (!file.exists(file)) {
    file <- downloader(indicador)
  }
  imports <- readxl::read_excel(file, skip = 5, col_names = F) %>%
    tidyr::drop_na(...3) %>%
    t() %>%
    as.data.frame()
  imports$V1[1:2] <- c("1900", "1900")
  imports$V2[1:2] <- c("E-M", "A-J")

  imports %>%
    dplyr::filter(V2 != "Total") %>%
    tidyr::fill(V1) %>%
    Dmisc::vars_to_date(year = 1, quarter = 2) %>%
    t() %>%
    as.data.frame() %>%
    janitor::row_to_names(1) %>%
    dplyr::bind_cols(nvl_importaciones) %>%
    dplyr::relocate(orden, nivel, detalle) %>%
    dplyr::select(-detalle) %>%
    tidyr::pivot_longer(-c(1:4)) %>%
    setNames(c("orden", "nivel", "codigo", "detalle", "date", "valor")) %>%
    type.convert(as.is = T)
}


#' Importaciones anual
#'
#'  \lifecycle{experimental}
#'
#' @param indicador Vea \code{\link{downloader}}
#' @param metadata indica si se retornan los datos o la metadata del indicador
#'
#' @return [data.frame]: los datos del indicador en forma tabular
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   importaciones_anual()
#' }
importaciones_anual <- function(indicador = NULL, metadata = FALSE){
  if(metadata){
    return(
      tibble::tribble(
        ~col, ~name, ~unit, ~dtype,
        "orden", "Orden", "", "int",
        "nivel", "Nivel", "", "int",
        "codigo", "Código del producto", "", "text",
        "detalle", "Detalle", "", "text",
        "ano", "Año", "", "ydate",
        "valor", "Valor", "Millones de US$", "f1"
      )
    )
  }
  if(is.null(indicador)){
    indicador <- c(
      original_url = "https://cdn.bancentral.gov.do/documents/estadisticas/sector-externo/documents/Importaciones_Anuales_6.xls",
      file_ext = "xls"
    )
  }
  file <- "/mnt/c/Users/drdsd/Downloads/Importaciones_Anuales_6.xls"
  if (!file.exists(file)) {
    file <- downloader(indicador)
  }
  readxl::read_excel(file, skip = 7) %>%
    tidyr::drop_na(`2010`) %>%
    dplyr::bind_cols(nvl_importaciones) %>%
    dplyr::relocate(orden, nivel, detalle) %>%
    dplyr::select(-detalle) %>%
    tidyr::pivot_longer(-c(1:4)) %>%
    setNames(c("orden", "nivel", "codigo", "detalle", "ano", "valor")) %>%
    type.convert(as.is = T)
}




# Sector Fiscal ----


#' Estado de Operaciones del sector público no financiero (en millones de RD$)
#'
#'  \lifecycle{experimental}
#'
#' @param indicador Vea \code{\link{downloader}}
#' @param metadata indica si se retornan los datos o la metadata del indicador
#'
#' @return [data.frame]: los datos del indicador en forma tabular
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   estado_operaciones_spnf()
#' }
estado_operaciones_spnf <- function(indicador = NULL, metadata = FALSE){
  if(is.null(indicador)){
    indicador <- c(
      original_url = "https://cdn.bancentral.gov.do/documents/estadisticas/documents/Operaciones_Mensual.xlsx",
      file_ext = "xlsx",
      max_changes = 108*55
    )
  }
  if(metadata){
    return(
      tibble::tribble(
        ~col, ~name, ~unit, ~dtype, ~key,
        "orden", "Orden de las operaciones", "", "int", 1,
        "nivel", "Nivel de las operaciones", "", "int", 1,
        "operacion", "Operación", "", "text", 1,
        "date", "Fecha", "Meses", "mdate", 1,
        "valor", "Valor", "Millones de RD$", "f1", 0
      )
    )
  }
  file <- "/mnt/c/Users/drdsd/Downloads/Operaciones_Mensual.xlsx"
  if (!file.exists(file)) {
    file <- downloader(indicador)
  } else {
    print("Local file...")
  }
  datos <- readxl::read_excel(file, col_names = F) %>%
    dplyr::mutate(
      ...2 = dplyr::case_when(
        stringr::str_detect(...1, "Transacciones") ~ ...1,
        TRUE ~ ...2
      )
    ) %>%
    dplyr::select(-...1) %>%
    tidyr::drop_na(...2)

  datos[1:2,1] <- c("1900", "Enero")
  datos <- datos %>%
    t() %>%
    as.data.frame() %>%
    tidyr::fill(V1) %>%
    dplyr::mutate(
      V1 = stringr::str_remove_all(V1, "[^0-9]")
    ) %>%
    dplyr::filter(
      !stringr::str_detect(V2, "-")
    )%>%
    Dmisc::vars_to_date(year = 1, month = 2) %>%
    t() %>%
    as.data.frame()

  datos[1,1] <- "operacion"

  datos %>%
    janitor::row_to_names(1) %>%
    dplyr::bind_cols(nvl_estado_operaciones_spnf %>% dplyr::select(-operacion))%>%
    dplyr::relocate(orden, nivel) %>%
    tidyr::pivot_longer(-c(orden, nivel, operacion), names_to = "date", values_to = "valor") %>%
    type.convert(as.is = TRUE)
}




# Sector monetario y financiero ----


## Base Monetaria y Agregados Monetarios ----







## Balances sectoriales ----



#' Balance sectorial de las OSD: Resumen por instrumentos
#'
#'  \lifecycle{experimental}
#'
#' @param indicador Vea \code{\link{downloader}}
#' @param metadata indica si se retornan los datos o la metadata del indicador
#'
#' @return [data.frame]: los datos del indicador en forma tabular
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   balance_osd_resumen()
#' }
balance_osd_resumen <- function(indicador = NULL, metadata = FALSE){
  if(metadata){
    return(
      tibble::tribble(
        ~col, ~name, ~unit, ~dtype,
        "date", "Fecha", "Mensual", "mdate",
        "activos_billetes_monedas", "Activos - Billetes y monedas", "", "f1",
        "activos_depositos", "Activos - Depósitos", "", "f1",
        "activos_vda", "Activos - Valores distintos de acciones (tenencia)", "", "f1",
        "activos_prestamos", "Activos - Préstamos", "", "f1",
        "activos_acciones_opc", "Activos - Acciones y otras participaciones de capital", "", "f1",
        "activos_otros_financieros", "Activos - Otros activos financieros 2/", "", "f1",
        "activos_no_financieros", "Activos - Activos no financieros", "", "f1",
        "activos_total", "Total activos", "", "f1",
        "pyc_depositos_idsa", "Pasivos y Capital - Depósitos - Incluidos en dinero en sentido amplio", "", "f1",
        "pyc_depositos_edsa", "Pasivos y Capital - Depósitos - Excluidos de dinero en sentido amplio", "", "f1",
        "pyc_vda", "Pasivos y Capital - Valores distintos de acciones", "", "f1",
        "pyc_prestamos", "Pasivos y Capital - Préstamos", "", "f1",
        "pyc_otros_pasivos", "Pasivos y Capital - Otros pasivos 2/", "", "f1",
        "pyc_acciones_opc", "Pasivos y Capital - Acciones y otras participaciones de capital", "", "f1",
        "pyc_total", "Total pasivos y capital", "", "f1"
      )
    )
  }
  if(is.null(indicador)){
    indicador <- c(
      original_url = "https://cdn.bancentral.gov.do/documents/estadisticas/sector-monetario-y-financiero/documents/balance_osd_resumen.xls",
      file_ext = "xls",
      max_changes = 2
    )
  }
  file <- "/mnt/c/Users/drdsd/Downloads/balance_osd_resumen.xls"
  if (!file.exists(file)) {
    file <- downloader(indicador)
  }
  readxl::read_excel(file) %>%
    tidyr::drop_na(...2) %>%
    Dmisc::vars_to_date(year = 1, month = 2) %>%
    setNames(c("date", "activos_billetes_monedas", "activos_depositos", "activos_vda",
               "activos_prestamos", "activos_acciones_opc",
               "activos_otros_financieros", "activos_no_financieros", "activos_total",
               "pyc_depositos_idsa", "pyc_depositos_edsa", "pyc_vda", "pyc_prestamos",
               "pyc_otros_pasivos", "pyc_acciones_opc", "pyc_total")) %>%
    type.convert(as.is=TRUE)
}



#'  Balance sectorial de las OSD: Instrumentos y sectores institucionales (Activos)
#'
#'  \lifecycle{experimental}
#'
#' @param indicador Vea \code{\link{downloader}}
#' @param metadata indica si se retornan los datos o la metadata del indicador
#'
#' @return [data.frame]: los datos del indicador en forma tabular
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   balance_isi_activos()
#' }
balance_isi_activos <- function(indicador = NULL, metadata = FALSE){
  if(metadata){
    return(
      tibble::tribble(
        ~col, ~name, ~unit, ~dtype,
        "date", "Fecha", "Mensual", "mdate",
        "bmd_no_residentes", "Billetes y monedas y depósitos - No residentes", "", "f1",
        "bmd_banco_central", "Billetes y monedas y depósitos - Banco Central", "", "f1",
        "bmd_osd", "Billetes y monedas y depósitos - Otras sociedades de depósito", "", "f1",
        "bmd_otros_sectores", "Billetes y monedas y depósitos - Otros sectores", "", "f1",
        "vda_no_residentes", "Valores distintos de acciones - No residentes", "", "f1",
        "vda_banco_central", "Valores distintos de acciones - Banco Central", "", "f1",
        "vda_gobierno_central", "Valores distintos de acciones - Gobierno Central", "", "f1",
        "vda_osnf", "Valores distintos de acciones - Otras sociedades no financieras", "", "f1",
        "vda_otros_sectores", "Valores distintos de acciones - Otros sectores 1/", "", "f1",
        "prestamos_no_residentes", "Préstamos - No residentes", "", "f1",
        "prestamos_osd", "Préstamos - Otras sociedades de depósito", "", "f1",
        "prestamos_osf", "Préstamos - Otras sociedades financieras", "", "f1",
        "prestamos_gobierno_central", "Préstamos - Gobierno central", "", "f1",
        "prestamos_gel", "Préstamos - Gobierno estatal y local", "", "f1",
        "prestamos_spnf", "Préstamos - Sociedades públicas no financieras", "", "f1",
        "prestamos_osnf", "Préstamos - Otras sociedades no financieras", "", "f1",
        "prestamos_hogares_isflsh", "Préstamos - Hogares e ISFLSH", "", "f1",
        "otros_activos", "Otros activos", "", "f1",
        "total_activos", "Total activos", "", "f1"
      )
    )
  }
  if(is.null(indicador)){
    indicador <- c(
      original_url = "https://cdn.bancentral.gov.do/documents/estadisticas/sector-monetario-y-financiero/documents/balance_osd_activos.xls",
      file_ext = "xls",
      max_changes = 2
    )
  }
  file <- "/mnt/c/Users/drdsd/Downloads/balance_osd_activos.xls"
  if (!file.exists(file)) {
    file <- downloader(indicador)
  }
  readxl::read_excel(file) %>%
    tidyr::drop_na(...2) %>%
    Dmisc::vars_to_date(year = 1, month = 2) %>%
    setNames(c("date", "bmd_no_residentes", "bmd_banco_central", "bmd_osd",
               "bmd_otros_sectores", "vda_no_residentes", "vda_banco_central",
               "vda_gobierno_central", "vda_osnf", "vda_otros_sectores",
               "prestamos_no_residentes", "prestamos_osd", "prestamos_osf",
               "prestamos_gobierno_central", "prestamos_gel", "prestamos_spnf",
               "prestamos_osnf", "prestamos_hogares_isflsh", "otros_activos",
               "total_activos")) %>%
    type.convert(as.is=TRUE)
}



#' Balance sectorial de las OSD: Instrumentos y sectores institucionales (Activos en MN)
#'
#'  \lifecycle{experimental}
#'
#' @param indicador Vea \code{\link{downloader}}
#' @param metadata indica si se retornan los datos o la metadata del indicador
#'
#' @return [data.frame]: los datos del indicador en forma tabular
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   balance_isi_activos_mn()
#' }
balance_isi_activos_mn <- function(indicador = NULL, metadata = FALSE){
  if(metadata){
    return(
      tibble::tribble(
        ~col, ~name, ~unit, ~dtype,
        "date", "Fecha", "Mensual", "mdate",
        "bmd_no_residentes", "Billetes y monedas y depósitos - No residentes", "", "f1",
        "bmd_banco_central", "Billetes y monedas y depósitos - Banco Central", "", "f1",
        "bmd_osd", "Billetes y monedas y depósitos - Otras sociedades de depósito", "", "f1",
        "bmd_otros_sectores", "Billetes y monedas y depósitos - Otros sectores", "", "f1",
        "vda_no_residentes", "Valores distintos de acciones - No residentes", "", "f1",
        "vda_banco_central", "Valores distintos de acciones - Banco Central", "", "f1",
        "vda_gobierno_central", "Valores distintos de acciones - Gobierno Central", "", "f1",
        "vda_osnf", "Valores distintos de acciones - Otras sociedades no financieras", "", "f1",
        "vda_otros_sectores", "Valores distintos de acciones - Otros sectores 1/", "", "f1",
        "prestamos_no_residentes", "Préstamos - No residentes", "", "f1",
        "prestamos_osd", "Préstamos - Otras sociedades de depósito", "", "f1",
        "prestamos_osf", "Préstamos - Otras sociedades financieras", "", "f1",
        "prestamos_gobierno_central", "Préstamos - Gobierno central", "", "f1",
        "prestamos_gel", "Préstamos - Gobierno estatal y local", "", "f1",
        "prestamos_spnf", "Préstamos - Sociedades públicas no financieras", "", "f1",
        "prestamos_osnf", "Préstamos - Otras sociedades no financieras", "", "f1",
        "prestamos_hogares_isflsh", "Préstamos - Hogares e ISFLSH", "", "f1",
        "otros_activos", "Otros activos", "", "f1",
        "total_activos", "Total activos", "", "f1"
      )
    )
  }
  if(is.null(indicador)){
    indicador <- c(
      original_url = "https://cdn.bancentral.gov.do/documents/estadisticas/sector-monetario-y-financiero/documents/balance_osd_activos_mn.xls",
      file_ext = "xls",
      max_changes = 2
    )
  }
  file <- "/mnt/c/Users/drdsd/Downloads/balance_osd_activos_mn.xls"
  if (!file.exists(file)) {
    file <- downloader(indicador)
  }
  readxl::read_excel(file) %>%
    tidyr::drop_na(...2) %>%
    Dmisc::vars_to_date(year = 1, month = 2) %>%
    setNames(c("date", "bmd_no_residentes", "bmd_banco_central", "bmd_osd",
               "bmd_otros_sectores", "vda_no_residentes", "vda_banco_central",
               "vda_gobierno_central", "vda_osnf", "vda_otros_sectores",
               "prestamos_no_residentes", "prestamos_osd", "prestamos_osf",
               "prestamos_gobierno_central", "prestamos_gel", "prestamos_spnf",
               "prestamos_osnf", "prestamos_hogares_isflsh", "otros_activos",
               "total_activos")) %>%
    type.convert(as.is=TRUE)
}



#' Balance sectorial de las OSD: Instrumentos y sectores institucionales (Activos en ME)
#'
#'  \lifecycle{experimental}
#'
#' @param indicador Vea \code{\link{downloader}}
#' @param metadata indica si se retornan los datos o la metadata del indicador
#'
#' @return [data.frame]: los datos del indicador en forma tabular
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   balance_isi_activos_me()
#' }
balance_isi_activos_me <- function(indicador = NULL, metadata = FALSE){
  if(metadata){
    return(
      tibble::tribble(
        ~col, ~name, ~unit, ~dtype,
        "date", "Fecha", "Mensual", "mdate",
        "bmd_no_residentes", "Billetes y monedas y depósitos - No residentes", "", "f1",
        "bmd_banco_central", "Billetes y monedas y depósitos - Banco Central", "", "f1",
        "bmd_osd", "Billetes y monedas y depósitos - Otras sociedades de depósito", "", "f1",
        "bmd_otros_sectores", "Billetes y monedas y depósitos - Otros sectores", "", "f1",
        "vda_no_residentes", "Valores distintos de acciones - No residentes", "", "f1",
        "vda_banco_central", "Valores distintos de acciones - Banco Central", "", "f1",
        "vda_gobierno_central", "Valores distintos de acciones - Gobierno Central", "", "f1",
        "vda_osnf", "Valores distintos de acciones - Otras sociedades no financieras", "", "f1",
        "vda_otros_sectores", "Valores distintos de acciones - Otros sectores 1/", "", "f1",
        "prestamos_no_residentes", "Préstamos - No residentes", "", "f1",
        "prestamos_osd", "Préstamos - Otras sociedades de depósito", "", "f1",
        "prestamos_osf", "Préstamos - Otras sociedades financieras", "", "f1",
        "prestamos_gobierno_central", "Préstamos - Gobierno central", "", "f1",
        "prestamos_gel", "Préstamos - Gobierno estatal y local", "", "f1",
        "prestamos_spnf", "Préstamos - Sociedades públicas no financieras", "", "f1",
        "prestamos_osnf", "Préstamos - Otras sociedades no financieras", "", "f1",
        "prestamos_hogares_isflsh", "Préstamos - Hogares e ISFLSH", "", "f1",
        "otros_activos", "Otros activos", "", "f1",
        "total_activos", "Total activos", "", "f1"
      )
    )
  }
  if(is.null(indicador)){
    indicador <- c(
      original_url = "https://cdn.bancentral.gov.do/documents/estadisticas/sector-monetario-y-financiero/documents/balance_osd_activos_me.xls",
      file_ext = "xls",
      max_changes = 2
    )
  }
  file <- "/mnt/c/Users/drdsd/Downloads/balance_osd_activos_me.xls"
  if (!file.exists(file)) {
    file <- downloader(indicador)
  }
  readxl::read_excel(file) %>%
    tidyr::drop_na(...2) %>%
    Dmisc::vars_to_date(year = 1, month = 2) %>%
    setNames(c("date", "bmd_no_residentes", "bmd_banco_central", "bmd_osd",
               "bmd_otros_sectores", "vda_no_residentes", "vda_banco_central",
               "vda_gobierno_central", "vda_osnf", "vda_otros_sectores",
               "prestamos_no_residentes", "prestamos_osd", "prestamos_osf",
               "prestamos_gobierno_central", "prestamos_gel", "prestamos_spnf",
               "prestamos_osnf", "prestamos_hogares_isflsh", "otros_activos",
               "total_activos")) %>%
    dplyr::select(c("date", "bmd_no_residentes", "bmd_banco_central", "bmd_osd",
                    "bmd_otros_sectores", "vda_no_residentes", "vda_banco_central",
                    "vda_gobierno_central", "vda_osnf", "vda_otros_sectores",
                    "prestamos_no_residentes", "prestamos_osd", "prestamos_osf",
                    "prestamos_gobierno_central", "prestamos_gel", "prestamos_spnf",
                    "prestamos_osnf", "prestamos_hogares_isflsh", "otros_activos",
                    "total_activos"))%>%
    type.convert(as.is=TRUE)
}



#' Balance sectorial de las OSD: Instrumentos y sectores institucionales (Pasivos)
#'
#'  \lifecycle{experimental}
#'
#' @param indicador Vea \code{\link{downloader}}
#' @param metadata indica si se retornan los datos o la metadata del indicador
#'
#' @return [data.frame]: los datos del indicador en forma tabular
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   balance_isi_pasivos()
#' }
balance_isi_pasivos <- function(indicador = NULL, metadata = FALSE){
  if(metadata){
    return(
      tibble::tribble(
        ~col, ~name, ~unit, ~dtype,
        "date", "Fecha", "Mensual", "mdate",
        "depositos_no_residentes", "Depósitos - No residentes", "", "f1",
        "depositos_osd", "Depósitos - Otras sociedades de depósito", "", "f1",
        "depositos_osf", "Depósitos - Otras sociedades financieras", "", "f1",
        "depositos_gobierno_central", "Depósitos - Gobierno central", "", "f1",
        "depositos_gel", "Depósitos - Gobiernos estatales y locales", "", "f1",
        "depositos_spnf",  "Depósitos - Sociedades públicas no financieras", "", "f1",
        "depositos_osnf", "Depósitos - Otras sociedades no financieras", "", "f1",
        "depositos_hogares_isflsh", "Depósitos - Hogares e ISFLSH", "", "f1",
        "prestamos_no_residentes", "Préstamos - No residentes", "", "f1",
        "prestamos_banco_central", "Préstamos - Banco Central", "", "f1",
        "prestamos_osd", "Préstamos - Otras sociedades de depósito", "", "f1",
        "prestamos_osf",  "Préstamos - Otras sociedades financieras", "", "f1",
        "prestamos_otros_sectores", "Préstamos - Otros sectores 1/", "", "f1",
        "valores", "Valores", "", "f1",
        "otros_pasivos_capital", "Otros pasivos y capital", "", "f1",
        "total_pasivos_capital", "Total pasivos y capital", "", "f1"
      )
    )
  }
  if(is.null(indicador)){
    indicador <- c(
      original_url = "https://cdn.bancentral.gov.do/documents/estadisticas/sector-monetario-y-financiero/documents/balance_osd_pasivos.xls",
      file_ext = "xls",
      max_changes = 2
    )
  }
  file <- "/mnt/c/Users/drdsd/Downloads/balance_osd_pasivos.xls"
  if (!file.exists(file)) {
    file <- downloader(indicador)
  }
  readxl::read_excel(file) %>%
    tidyr::drop_na(...2) %>%
    Dmisc::vars_to_date(year = 1, month = 2) %>%
    setNames(c("date", "depositos_no_residentes", "depositos_osd", "depositos_osf",
               "depositos_gobierno_central", "depositos_gel", "depositos_spnf",
               "depositos_osnf", "depositos_hogares_isflsh", "prestamos_no_residentes",
               "prestamos_banco_central", "prestamos_osd", "prestamos_osf",
               "prestamos_otros_sectores", "valores", "otros_pasivos_capital",
               "total_pasivos_capital")) %>%
    type.convert(as.is=TRUE)
}



#'  Balance sectorial de las OSD: Instrumentos y sectores institucionales (Pasivos en MN)
#'
#'  \lifecycle{experimental}
#'
#' @param indicador Vea \code{\link{downloader}}
#' @param metadata indica si se retornan los datos o la metadata del indicador
#'
#' @return [data.frame]: los datos del indicador en forma tabular
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   balance_isi_pasivos_mn()
#' }
balance_isi_pasivos_mn <- function(indicador = NULL, metadata = FALSE){
  if(metadata){
    return(
      tibble::tribble(
        ~col, ~name, ~unit, ~dtype,
        "date", "Fecha", "Mensual", "mdate",
        "depositos_no_residentes", "Depósitos - No residentes", "", "f1",
        "depositos_osd", "Depósitos - Otras sociedades de depósito", "", "f1",
        "depositos_osf", "Depósitos - Otras sociedades financieras", "", "f1",
        "depositos_gobierno_central", "Depósitos - Gobierno central", "", "f1",
        "depositos_gel", "Depósitos - Gobiernos estatales y locales", "", "f1",
        "depositos_spnf",  "Depósitos - Sociedades públicas no financieras", "", "f1",
        "depositos_osnf", "Depósitos - Otras sociedades no financieras", "", "f1",
        "depositos_hogares_isflsh", "Depósitos - Hogares e ISFLSH", "", "f1",
        "prestamos_no_residentes", "Préstamos - No residentes", "", "f1",
        "prestamos_banco_central", "Préstamos - Banco Central", "", "f1",
        "prestamos_osd", "Préstamos - Otras sociedades de depósito", "", "f1",
        "prestamos_osf",  "Préstamos - Otras sociedades financieras", "", "f1",
        "prestamos_otros_sectores", "Préstamos - Otros sectores 1/", "", "f1",
        "valores", "Valores", "", "f1",
        "otros_pasivos", "Otros pasivos", "", "f1",
        "total_pasivos", "Total pasivos", "", "f1"
      )
    )
  }
  if(is.null(indicador)){
    indicador <- c(
      original_url = "https://cdn.bancentral.gov.do/documents/estadisticas/sector-monetario-y-financiero/documents/balance_osd_pasivos_mn.xls",
      file_ext = "xls",
      max_changes = 2
    )
  }
  file <- "/mnt/c/Users/drdsd/Downloads/balance_osd_pasivos_mn.xls"
  if (!file.exists(file)) {
    file <- downloader(indicador)
  }
  readxl::read_excel(file) %>%
    tidyr::drop_na(...2) %>%
    Dmisc::vars_to_date(year = 1, month = 2) %>%
    setNames(c("date", "depositos_no_residentes", "depositos_osd", "depositos_osf",
               "depositos_gobierno_central", "depositos_gel", "depositos_spnf",
               "depositos_osnf", "depositos_hogares_isflsh", "prestamos_no_residentes",
               "prestamos_banco_central", "prestamos_osd", "prestamos_osf",
               "prestamos_otros_sectores", "valores", "otros_pasivos",
               "total_pasivos")) %>%
    type.convert(as.is=TRUE)
}



#'  Balance sectorial de las OSD: Instrumentos y sectores institucionales (Activos en ME)
#'
#'  \lifecycle{experimental}
#'
#' @param indicador Vea \code{\link{downloader}}
#' @param metadata indica si se retornan los datos o la metadata del indicador
#'
#' @return [data.frame]: los datos del indicador en forma tabular
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   balance_isi_pasivos_me()
#' }
balance_isi_pasivos_me<- function(indicador = NULL, metadata = FALSE){
  if(metadata){
    return(
      tibble::tribble(
        ~col, ~name, ~unit, ~dtype,
        "date", "Fecha", "Mensual", "mdate",
        "depositos_no_residentes", "Depósitos - No residentes", "", "f1",
        "depositos_osd", "Depósitos - Otras sociedades de depósito", "", "f1",
        "depositos_osf", "Depósitos - Otras sociedades financieras", "", "f1",
        "depositos_gobierno_central", "Depósitos - Gobierno central", "", "f1",
        "depositos_gel", "Depósitos - Gobiernos estatales y locales", "", "f1",
        "depositos_spnf",  "Depósitos - Sociedades públicas no financieras", "", "f1",
        "depositos_osnf", "Depósitos - Otras sociedades no financieras", "", "f1",
        "depositos_hogares_isflsh", "Depósitos - Hogares e ISFLSH", "", "f1",
        "prestamos_no_residentes", "Préstamos - No residentes", "", "f1",
        "prestamos_banco_central", "Préstamos - Banco Central", "", "f1",
        "prestamos_osd", "Préstamos - Otras sociedades de depósito", "", "f1",
        "prestamos_osf",  "Préstamos - Otras sociedades financieras", "", "f1",
        "prestamos_otros_sectores", "Préstamos - Otros sectores 1/", "", "f1",
        "valores", "Valores", "", "f1",
        "otros_pasivos", "Otros pasivos", "", "f1",
        "total_pasivos", "Total pasivos", "", "f1"
      )
    )
  }
  if(is.null(indicador)){
    indicador <- c(
      original_url = "https://cdn.bancentral.gov.do/documents/estadisticas/sector-monetario-y-financiero/documents/balance_osd_pasivos_me.xls",
      file_ext = "xls",
      max_changes = 2
    )
  }
  file <- "/mnt/c/Users/drdsd/Downloads/balance_osd_pasivos_me.xls"
  if (!file.exists(file)) {
    file <- downloader(indicador)
  }
  readxl::read_excel(file) %>%
    tidyr::drop_na(...2) %>%
    Dmisc::vars_to_date(year = 1, month = 2) %>%
    setNames(c("date", "depositos_no_residentes", "depositos_osd", "depositos_osf",
               "depositos_gobierno_central", "depositos_gel", "depositos_spnf",
               "depositos_osnf", "depositos_hogares_isflsh", "prestamos_no_residentes",
               "prestamos_banco_central", "prestamos_osd", "prestamos_osf",
               "prestamos_otros_sectores", "valores", "otros_pasivos",
               "total_pasivos")) %>%
    type.convert(as.is=TRUE)
}



## Indicadores monetarios y financieros del Banco Central ----


#'  Indicadores monetarios y armonizados del Banco Central
#'
#'  \lifecycle{experimental}
#'
#' @param indicador Vea \code{\link{downloader}}
#' @param metadata indica si se retornan los datos o la metadata del indicador
#'
#' @return [data.frame]: los datos del indicador en forma tabular
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   indicadores_bcrd()
#' }
indicadores_bcrd <- function(indicador = NULL, metadata = FALSE){
  if(is.null(indicador)){
    indicador <- c(
      original_url = "https://cdn.bancentral.gov.do/documents/estadisticas/sector-monetario-y-financiero/documents/serie_indicadores_bcrd.xlsx",
      file_ext = "xlsx",
      max_changes = 108*55
    )
  }
  if(metadata){
    return(
      tibble::tribble(
        ~col, ~name, ~unit, ~dtype, ~key,
        "orden", "Orden del indicador", "", "int", 1,
        "nivel", "Nivel del indicador", "", "int", 1,
        "indicador", "Indicador", "", "text", 1,
        "date", "Fecha", "Meses", "mdate", 1,
        "valor", "Valor", "Millones de RD$", "f1", 0
      )
    )
  }
  file <- "/mnt/c/Users/drdsd/Downloads/serie_indicadores_bcrd.xlsx"
  if (!file.exists(file)) {
    file <- downloader(indicador)
  } else {
    print("Local file...")
  }
  datos <- readxl::read_excel(file, skip = 4, col_names = F)
  datos <- datos %>%
    tidyr::drop_na(...1)
  datos <- datos[1:(match("En millones RD$", datos$...1)-1),]
  datos[1,1] <- "0"
  t(datos) %>%
    as.data.frame() %>%
    Dmisc::vars_to_date(date = "V1") %>%
    t() %>%
    as.data.frame() -> datos
  datos[1,1] <- "indicador"
  datos %>%
    janitor::row_to_names(1) %>%
    dplyr::bind_cols(nvl_indicadores_bcrd %>% dplyr::select(-indicador)) %>%
    dplyr::relocate(orden, nivel) %>%
    tidyr::pivot_longer(-c(orden, nivel, indicador), names_to = "date", values_to = "valor", values_transform = list(valor = as.numeric)) %>%
    type.convert(as.is = TRUE) %>%
    dplyr::group_by(indicador) %>%
    dplyr::mutate(
      valor__tci = (valor/dplyr::lag(valor, 12)-1)*100,
      valor__tci = dplyr::case_when(orden < 40 ~ valor__tci)
    ) %>%
    dplyr::ungroup()
}




## Panoramas ----


#' Panorama Banco Central
#'
#'  \lifecycle{experimental}
#'
#' @param indicador Vea \code{\link{downloader}}
#' @param metadata indica si se retornan los datos o la metadata del indicador
#'
#' @return [data.frame]: los datos del indicador en forma tabular
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   panorama_bc()
#' }
panorama_bc <- function(indicador = NULL, metadata = FALSE){
  if(metadata){
    return(
      tibble::tribble(
        ~col, ~name, ~unit, ~dtype,
        "date", "Fecha", "Mensual", "mdate",
        "aen_pfnr_cp", "Activos externos netos (AEN) 2/ - Activos frente a no residentes - Activos de reserva oficial - (1)", "", "f1",
        "aen_afnr_otros", "Activos externos netos (AEN) 2/ - Activos frente a no residentes - Otros - (2)", "", "f1",
        "aen_pfnr_cp", "Activos externos netos (AEN) 2/ - Pasivos frente a no residentes - C.P. - (3)", "", "f1",
        "aen_afnr_lp", "Activos externos netos (AEN) 2/ - Pasivos frente a no residentes - L.P. - (4)", "", "f1",
        "aen_rin", "Activos externos netos (AEN) 2/ - Reservas internacionales netas - ('(4A)=1-3)", "", "f1",
        "aen_total", "Activos externos netos (AEN) 2/ - Total - ('(5)=(1+2)- (3+4))", "", "f1",
        "activos_internos_gc", "Activos internos (AI) - Gobierno central (neto) - (6)", "", "f1",
        "activos_internos_spf", "Activos internos (AI) - Sociedades públicas no financieras - (7)", "", "f1",
        "activos_internos_osd", "Activos internos (AI) - Otras sociedades de depósito - (8)", "", "f1",
        "activos_internos_os", "Activos internos (AI) - Otros sectores - (9)", "", "f1",
        "activos_internos_op", "Activos internos (AI) - Otras partidas (neto) 3/ - (10)", "", "f1",
        "activos_internos_totales", "Activos internos (AI) - Total - (11)", "", "f1",
        "base_monetaria", "Base Monetaria - (AEN+AI=BM)  - ((12) = (5)+(11)= (13+14))", "", "f1",
        "bm_ampliada_bmc", "Base monetaria amplia 4/- Billetes y monedas en circulación - (13)", "", "f1",
        "bm_ampliada_dvop", "Base monetaria amplia 4/- Depósitos, valores y otros pasivos - (14)", "", "f1",
        "tipo_de_cambio_me", "Tipo de cambio para conversión de ME en el balance - (15)", "", "f1"
      )
    )
  }
  if(is.null(indicador)){
    indicador <- c(
      original_url = "https://cdn.bancentral.gov.do/documents/estadisticas/sector-monetario-y-financiero/documents/panorama_pbc.xls",
      file_ext = "xls"
    )
  }
  file <- "/mnt/c/Users/drdsd/Downloads/panorama_pbc.xls"
  if (!file.exists(file)) {
    file <- downloader(indicador)
  }
  pan <- readxl::read_excel(file, skip = 12, col_names = F) %>%
    tidyr::drop_na(...6) %>%
    Dmisc::vars_to_date(year = 1, month = 2) %>%
    t()
  rsms <- rowSums(type.convert(pan[-1,], as.is = T), na.rm = T) != 0
  pan[c(TRUE, rsms),] %>%
    t() %>%
    as.data.frame() %>%
    setNames(c("date", "aen_afnr_aro", "aen_afnr_otros", "aen_pfnr_cp", "aen_afnr_lp",
               "aen_rin", "aen_total", "activos_internos_gc", "activos_internos_spf",
               "activos_internos_osd", "activos_internos_os", "activos_internos_op",
               "activos_internos_totales", "base_monetaria", "bm_ampliada_bmc",
               "bm_ampliada_dvop", "tipo_de_cambio_me")) %>%
    dplyr::select(1:17) %>%
    type.convert(as.is=TRUE)
}


#' Panorama Otras Sociedades de Depósitos
#'
#'  \lifecycle{experimental}
#'
#' @param indicador Vea \code{\link{downloader}}
#' @param metadata indica si se retornan los datos o la metadata del indicador
#'
#' @return [data.frame]: los datos del indicador en forma tabular
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   panorama_posd()
#' }
panorama_posd <- function(indicador = NULL, metadata = FALSE){
  if(metadata){
    return(
      tibble::tribble(
        ~col, ~name, ~unit, ~dtype,
        "date", "Fecha", "Mensual", "mdate",
        "aen_afnr", "Activos externos netos (AEN) - Activos frente a no residentes (1)", "", "f1",
        "aen_pfnr", "Activos externos netos (AEN) - Pasivos frente a no residentes (2)", "", "f1",
        "aen_total", "Activos externos netos (AEN) - Total (3=1+2)", "", "f1",
        "activos_internos_gc", "Activos internos  (AI) - Gobierno central (neto) (4)", "", "f1",
        "activos_internos_gel", "Activos internos  (AI) - Gobierno estatal y local (5)", "", "f1",
        "activos_internos_spnf", "Activos internos  (AI) - Sociedades públicas no financieras (6)", "", "f1",
        "activos_internos_bc_bym", "Activos internos  (AI) - Banco central - Billetes y monedas (7)", "", "f1",
        "activos_internos_bc_dep", "Activos internos  (AI) - Banco central - Depósitos (8)", "", "f1",
        "activos_internos_bc_valores", "Activos internos  (AI) - Banco central - Valores (9)", "", "f1",
        "activos_internos_osf", "Activos internos  (AI) - Otras sociedades financieras (10)", "", "f1",
        "activos_internos_osnf", "Activos internos  (AI) - Otras sociedades no financieras (11)", "", "f1",
        "activos_internos_hogares_isflsh", "Activos internos  (AI) - Hogares e ISFLSH (12)", "", "f1",
        "activos_internos_op", "Activos internos  (AI) - Otras partidas (neto) 1/ (13)", "", "f1",
        "activos_internos_total", "Activos internos  (AI) - Total (AI) (14= 4 a 13)", "", "f1",
        "pdsa_total", "Pasivos incluidos en la definición de dinero en sentido amplio (PDSA) (15=3+14=16+17)", "", "f1",
        "pdsa_depositos_transferibles", "PDSA - Depósitos transferibles (16)", "", "f1",
        "pdsa_odv", "PDSA - Otros depósitos y valores (17)", "", "f1"
      )
    )
  }
  if(is.null(indicador)){
    indicador <- c(
      original_url = "https://cdn.bancentral.gov.do/documents/estadisticas/sector-monetario-y-financiero/documents/panorama_posd.xls",
      file_ext = "xls"
    )
  }
  file <- "/mnt/c/Users/drdsd/Downloads/panorama_posd.xls"
  if (!file.exists(file)) {
    file <- downloader(indicador)
  }
  readxl::read_excel(file, skip = 12, col_names = F) %>%
    tidyr::drop_na(...6) %>%
    Dmisc::vars_to_date(year = 1, month = 2) %>%
    setNames(c("date", "aen_afnr", "aen_pfnr", "aen_total", "activos_internos_gc",
               "activos_internos_gel", "activos_internos_spnf", "activos_internos_bc_bym",
               "activos_internos_bc_dep", "activos_internos_bc_valores",
               "activos_internos_osf", "activos_internos_osnf", "activos_internos_hogares_isflsh",
               "activos_internos_op", "activos_internos_total", "pdsa_total",
               "pdsa_depositos_transferibles", "pdsa_odv")) %>%
    type.convert(as.is=T)
}


#' Panorama banco múltiples
#'
#'  \lifecycle{experimental}
#'
#' @param indicador Vea \code{\link{downloader}}
#' @param metadata indica si se retornan los datos o la metadata del indicador
#'
#' @return [data.frame]: los datos del indicador en forma tabular
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   panorama_bm()
#' }
panorama_bm <- function(indicador = NULL, metadata = FALSE){
  if(metadata){
    return(
      tibble::tribble(
        ~col, ~name, ~unit, ~dtype,
        "date", "Fecha", "Mensual", "mdate",
        "aen_afnr", "Activos externos netos (AEN) - Activos frente a no residentes (1)", "", "f1",
        "aen_pfnr", "Activos externos netos (AEN) - Pasivos frente a no residentes (2)", "", "f1",
        "aen_total", "Activos externos netos (AEN) - Total (3=1+2)", "", "f1",
        "activos_internos_gc", "Activos internos  (AI) - Gobierno central (neto) (4)", "", "f1",
        "activos_internos_gel", "Activos internos  (AI) - Gobierno estatal y local (5)", "", "f1",
        "activos_internos_spnf", "Activos internos  (AI) - Sociedades públicas no financieras (6)", "", "f1",
        "activos_internos_bc_bym", "Activos internos  (AI) - Banco central - Billetes y monedas (7)", "", "f1",
        "activos_internos_bc_dep", "Activos internos  (AI) - Banco central - Depósitos (8)", "", "f1",
        "activos_internos_bc_valores", "Activos internos  (AI) - Banco central - Valores (9)", "", "f1",
        "activos_internos_osf", "Activos internos  (AI) - Otras sociedades financieras (10)", "", "f1",
        "activos_internos_osnf", "Activos internos  (AI) - Otras sociedades no financieras (11)", "", "f1",
        "activos_internos_hogares_isflsh", "Activos internos  (AI) - Hogares e ISFLSH (12)", "", "f1",
        "activos_internos_op", "Activos internos  (AI) - Otras partidas (neto) 1/ (13)", "", "f1",
        "activos_internos_total", "Activos internos  (AI) - Total (AI) (14= 4 a 13)", "", "f1",
        "pdsa_total", "Pasivos incluidos en la definición de dinero en sentido amplio (PDSA) (15=3+14=16+17)", "", "f1",
        "pdsa_depositos_transferibles", "PDSA - Depósitos transferibles (16)", "", "f1",
        "pdsa_odv", "PDSA - Otros depósitos y valores (17)", "", "f1"
      )
    )
  }
  if(is.null(indicador)){
    indicador <- c(
      original_url = "https://cdn.bancentral.gov.do/documents/estadisticas/sector-monetario-y-financiero/documents/panorama_pbm.xls",
      file_ext = "xls"
    )
  }
  file <- "/mnt/c/Users/drdsd/Downloads/panorama_pbm.xls"
  if (!file.exists(file)) {
    file <- downloader(indicador)
  }
  readxl::read_excel(file, skip = 12, col_names = F) %>%
    tidyr::drop_na(...6) %>%
    Dmisc::vars_to_date(year = 1, month = 2) %>%
    setNames(c("date", "aen_afnr", "aen_pfnr", "aen_total", "activos_internos_gc",
               "activos_internos_gel", "activos_internos_spnf", "activos_internos_bc_bym",
               "activos_internos_bc_dep", "activos_internos_bc_valores",
               "activos_internos_osf", "activos_internos_osnf", "activos_internos_hogares_isflsh",
               "activos_internos_op", "activos_internos_total", "pdsa_total",
               "pdsa_depositos_transferibles", "pdsa_odv")) %>%
    type.convert(as.is=T)
}


#' Panorama Sociedades de Depósitos
#'
#'  \lifecycle{experimental}
#'
#' @param indicador Vea \code{\link{downloader}}
#' @param metadata indica si se retornan los datos o la metadata del indicador
#'
#' @return [data.frame]: los datos del indicador en forma tabular
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   panorama_psd()
#' }
panorama_psd <- function(indicador = NULL, metadata = FALSE){
  if(metadata){
    return(
      tibble::tribble(
        ~col, ~name, ~unit, ~dtype,
        "date", "Fecha", "Mensual", "mdate",
        "aen_afnr", "Activos externos netos (AEN) - Activos frente a no residentes (1)", "", "f1",
        "aen_pfnr", "Activos externos netos (AEN) - Pasivos frente a no residentes (2)", "", "f1",
        "aen_total", "Activos externos netos (AEN) - Total (3=1+2)", "", "f1",
        "activos_internos_gc", "Activos internos  (AI) - Gobierno central (neto) (4)", "", "f1",
        "activos_internos_spnf", "Activos internos  (AI) - Sociedades públicas no financieras (5)", "", "f1",
        "activos_internos_gel", "Activos internos  (AI) - Gobierno estatal y local (6)", "", "f1",
        "activos_internos_osf", "Activos internos  (AI) - Otras sociedades financieras (7)", "", "f1",
        "activos_internos_osnf", "Activos internos  (AI) - Otras sociedades no financieras (8)", "", "f1",
        "activos_internos_hogares_isflsh", "Activos internos  (AI) - Hogares e ISFLSH (9)", "", "f1",
        "activos_internos_op", "Activos internos  (AI) - Otras partidas (neto) 1/ (10)", "", "f1",
        "activos_internos_total", "Activos internos  (AI) - Total (AI) (11 = 4 a 10)", "", "f1",
        "dsa_total", "Dinero en sentido amplio (DSA) (12 = 3 + 11 + 13 al 18)", "", "f1",
        "dsa_bmpp", "Dinero en sentido amplio (DSA) - Billetes y monedas en poder del público (13)", "", "f1",
        "dsa_dt_mn", "Dinero en sentido amplio (DSA) - Depósitos transferibles - Moneda Nacional (14)", "", "f1",
        "dsa_dt_me", "Dinero en sentido amplio (DSA) - Depósitos transferibles - Moneda Extranjera (15)", "", "f1",
        "dsa_od_mn", "Dinero en sentido amplio (DSA) - Otros depósitos - Moneda Nacional (16)", "", "f1",
        "dsa_od_me", "Dinero en sentido amplio (DSA) - Otros depósitos - Moneda Extranjera (17)", "", "f1",
        "dsa_valores_mn", "Dinero en sentido amplio (DSA) - Valores - Moneda Nacional (18)", "", "f1",
        "dsa_valores_me", "Dinero en sentido amplio (DSA) - Valores - Moneda Extranjera (19)", "", "f1"
      )
    )
  }
  if(is.null(indicador)){
    indicador <- c(
      original_url = "https://cdn.bancentral.gov.do/documents/estadisticas/sector-monetario-y-financiero/documents/panorama_psd.xls",
      file_ext = "xls"
    )
  }
  file <- "/mnt/c/Users/drdsd/Downloads/panorama_psd.xls"
  if (!file.exists(file)) {
    file <- downloader(indicador)
  }
  readxl::read_excel(file, skip = 12, col_names = F) %>%
    tidyr::drop_na(...6) %>%
    Dmisc::vars_to_date(year = 1, month = 2) %>%
    setNames(c("date", "aen_afnr", "aen_pfnr", "aen_total", "activos_internos_gc",
               "activos_internos_spnf", "activos_internos_gel", "activos_internos_osf",
               "activos_internos_osnf", "activos_internos_hogares_isflsh",
               "activos_internos_op", "activos_internos_total", "dsa_total",
               "dsa_bmpp", "dsa_dt_mn", "dsa_dt_me", "dsa_od_mn", "dsa_od_me",
               "dsa_valores_mn", "dsa_valores_me")) %>%
    type.convert(as.is=TRUE)
}


#' Panorama otras sociedades financieras
#'
#'  \lifecycle{experimental}
#'
#' @param indicador Vea \code{\link{downloader}}
#' @param metadata indica si se retornan los datos o la metadata del indicador
#'
#' @return [data.frame]: los datos del indicador en forma tabular
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   panorama_osf()
#' }
panorama_osf <- function(indicador = NULL, metadata = FALSE){
  if(metadata){
    return(
      tibble::tribble(
        ~col, ~name, ~unit, ~dtype,
        "date", "Fecha", "Mensual", "mdate",
        "aen_afnr", "Activos externos netos (AEN) - Activos frente a no residentes (1)", "", "f1",
        "aen_pfnr", "Activos externos netos (AEN) - Pasivos frente a no residentes (2)", "", "f1",
        "aen_total", "Activos externos netos (AEN) - Total (3=1+2)", "", "f1",
        "activos_internos_gc", "Activos internos  (AI) - Gobierno central (neto) (4)", "", "f1",
        "activos_internos_gel", "Activos internos  (AI) - Gobierno estatal y local (5)", "", "f1",
        "activos_internos_spnf", "Activos internos  (AI) - Sociedades públicas no financieras (6)", "", "f1",
        "activos_internos_bc_bym", "Activos internos  (AI) - Banco central - Billetes y monedas (7)", "", "f1",
        "activos_internos_bc_dep", "Activos internos  (AI) - Banco central - Depósitos y valores de encaje (8)", "", "f1",
        "activos_internos_bc_otros", "Activos internos  (AI) - Banco central - Otros activos (9)", "", "f1",
        "activos_internos_osd", "Activos internos  (AI) - Otras sociedades de depósitos (10)", "", "f1",
        "activos_internos_snf", "Activos internos  (AI) - Sociedades no financieras (11)", "", "f1",
        "activos_internos_hogares_isflsh", "Activos internos  (AI) - Hogares e ISFLSH (12)", "", "f1",
        "activos_internos_op", "Activos internos  (AI) - Otras partidas (neto) 1/ (13)", "", "f1",
        "activos_internos_total", "Activos internos  (AI) - Total (AI) (14= 4 a 13)", "", "f1",
        "pasivos_total", "Pasivos (15=3+14=16+17)", "", "f1",
        "pasivos_rts", "Pasivos - Reservas técnicas de seguros (16)", "", "f1",
        "pasivos_oopr", "Pasivos - Otras obligaciones con el público residente (17)", "", "f1"
      )
    )
  }
  if(is.null(indicador)){
    indicador <- c(
      original_url = "https://cdn.bancentral.gov.do/documents/estadisticas/sector-monetario-y-financiero/documents/panorama_osf.xls",
      file_ext = "xls"
    )
  }
  file <- "/mnt/c/Users/drdsd/Downloads/panorama_osf.xls"
  if (!file.exists(file)) {
    file <- downloader(indicador)
  }
  readxl::read_excel(file, skip = 12, col_names = F) %>%
    tidyr::drop_na(...6) %>%
    Dmisc::vars_to_date(year = 1, month = 2) %>%
    setNames(c("date", "aen_afnr", "aen_pfnr", "aen_total", "activos_internos_gc",
               "activos_internos_gel", "activos_internos_spnf", "activos_internos_bc_bym",
               "activos_internos_bc_dep", "activos_internos_bc_otros",
               "activos_internos_osd", "activos_internos_snf", "activos_internos_hogares_isflsh",
               "activos_internos_op", "activos_internos_total", "pasivos_total",
               "pasivos_rts", "pasivos_oopr")) %>%
    type.convert(as.is=T)
}


#' Panorama sociedades financieras
#'
#'  \lifecycle{experimental}
#'
#' @param indicador Vea \code{\link{downloader}}
#' @param metadata indica si se retornan los datos o la metadata del indicador
#'
#' @return [data.frame]: los datos del indicador en forma tabular
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   panorama_sf()
#' }
panorama_sf <- function(indicador = NULL, metadata = FALSE){
  if(metadata){
    return(
      tibble::tribble(
        ~col, ~name, ~unit, ~dtype,
        "date", "Fecha", "Mensual", "mdate",
        "aen_afnr", "Activos externos netos (AEN) - Activos frente a no residentes (1)", "", "f1",
        "aen_pfnr", "Activos externos netos (AEN) - Pasivos frente a no residentes (2)", "", "f1",
        "aen_total", "Activos externos netos (AEN) - Total (3=1-2)", "", "f1",
        "activos_internos_gc", "Activos internos  (AI) - Gobierno central (neto) (4)", "", "f1",
        "activos_internos_spnf", "Activos internos  (AI) - Sociedades públicas no financieras (5)", "", "f1",
        "activos_internos_gel", "Activos internos  (AI) - Gobierno estatal y local (6)", "", "f1",
        "activos_internos_snf", "Activos internos  (AI) - Sociedades no financieras (7)", "", "f1",
        "activos_internos_hogares_isflsh", "Activos internos  (AI) - Hogares e ISFLSH (8)", "", "f1",
        "activos_internos_op", "Activos internos  (AI) - Otras partidas (neto) 1/ (9)", "", "f1",
        "activos_internos_total", "Activos internos  (AI) - Total (AI) (10 = 4 a 9)", "", "f1",
        "pasivos_total", "Pasivos (11 = 3 + 10 = 12 al 18)", "", "f1",
        "pasivos_bmpp", "Pasivos - Billetes y monedas en poder del público (12)", "", "f1",
        "pasivos_dep_mn", "Pasivos - Depósitos - Moneda Nacional (13)", "", "f1",
        "pasivos_dep_me", "Pasivos - Depósitos - Moneda Extranjera (14)", "", "f1",
        "pasivos_valores_mn", "Pasivos - Valores - Moneda Nacional (15)", "", "f1",
        "pasivos_valores_me", "Pasivos - Valores - Moneda Extranjera (16)", "", "f1",
        "pasivos_rts", "Pasivos - Reservas técnicas de seguros (17)", "", "f1",
        "pasivos_otros", "Pasivos - Otros Pasivos (18)", "", "f1"
      )
    )
  }
  if(is.null(indicador)){
    indicador <- c(
      original_url = "https://cdn.bancentral.gov.do/documents/estadisticas/sector-monetario-y-financiero/documents/panorama_sf.xls",
      file_ext = "xls"
    )
  }
  file <- "/mnt/c/Users/drdsd/Downloads/panorama_sf.xls"
  if (!file.exists(file)) {
    file <- downloader(indicador)
  }
  readxl::read_excel(file, skip = 12, col_names = F) %>%
    tidyr::drop_na(...6) %>%
    Dmisc::vars_to_date(year = 1, month = 2) %>%
    setNames(c("date", "aen_afnr", "aen_pfnr", "aen_total", "activos_internos_gc",
               "activos_internos_spnf", "activos_internos_gel", "activos_internos_snf",
               "activos_internos_hogares_isflsh", "activos_internos_op",
               "activos_internos_total", "pasivos_total", "pasivos_bmpp",
               "pasivos_dep_mn", "pasivos_dep_me", "pasivos_valores_mn",
               "pasivos_valores_me", "pasivos_rts", "pasivos_otros")) %>%
    type.convert(as.is=TRUE)
}



## Tasas de interés activas y pasivas anual promedio ponderada de las entidades de intermediación financiera ----



#' Tasas de interés activas nominales mensuales 2017-current - Banco Múltiples
#'
#'  \lifecycle{experimental}
#'
#' @param indicador Vea \code{\link{downloader}}
#' @param metadata indica si se retornan los datos o la metadata del indicador
#'
#' @return [data.frame]: los datos del indicador en forma tabular
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   tasas_interes_activas_bm_2017()
#' }
tasas_interes_activas_bm_2017 <- function(indicador = NULL, metadata = FALSE){
  if(is.null(indicador)){
    indicador <- c(
      original_url = "https://cdn.bancentral.gov.do/documents/estadisticas/sector-monetario-y-financiero/documents/tbm_activad.xlsx",
      file_ext = "xlsx",
      max_changes = 15
    )
  }
  if(metadata){
    return(
      tibble::tribble(
        ~col, ~name, ~unit, ~dtype, ~key,
        "grupo", "Grupo", "", "text", 1,
        "categoria", "Categoría", "", "text", 1,
        "date", "Fecha", "Meses", "mdate", 1,
        "valor", "Valor", "", "f1", 0
      )
    )
  }
  file <- "/mnt/c/Users/drdsd/Downloads/tbm_activad.xlsx"
  if (!file.exists(file)) {
    file <- downloader(indicador)
  } else {
    print("Local file...")
  }
  datos <- readxl::read_excel(file, skip = 4, col_names = F)
  datos %>%
    tidyr::fill(...1) %>%
    dplyr::filter(nchar(...1) >= 4) %>%
    tidyr::drop_na(...2) %>%
    dplyr::mutate(
      year = stringr::str_remove_all(...1, stringr::regex("[^0-9]")),
      year = dplyr::case_when(
        nchar(year) > 4 ~ stringr::str_sub(year, 1, 4),
        nchar(year) < 4 ~ NA_character_,
        TRUE ~ year
      ),
      ...1 = stringr::str_remove_all(...1, stringr::regex("[^a-z]", ignore_case = TRUE))
    ) %>%
    tidyr::fill(year) %>%
    dplyr::relocate(year) %>%
    dplyr::filter(...1 != "") -> datos

  datos[1:2, 1] <- "1900"
  datos[1:2, 2] <- "Enero"

  datos <- datos %>%
    Dmisc::vars_to_date(year = 1, month = 2) %>%
    t %>%
    as.data.frame()

  datos[1, 1:2] <- c("grupo", "categoria")

  datos %>%
    janitor::row_to_names(1) %>%
    tidyr::fill(grupo) %>%
    tidyr::pivot_longer(-c(grupo, categoria), names_to = "date", values_to = "valor") %>%
    type.convert(as.is = TRUE)
}



# Sector real----



#' PIB por enfoque del gasto trimestral
#'
#'   \lifecycle{experimental}
#'
#'
#' @param indicador Vea \code{\link{downloader}}
#' @param metadata indica si se retornan los datos o la metadata del indicador
#'
#' @return [data.frame]: los datos del indicador en forma tabular
#'
#' @export
#'
#' @examples
#' \dontrun{
#' pib_gasto_trim()
#' }
pib_gasto_trim <- function(indicador = NULL, metadata = FALSE){
  if(is.null(indicador)){
    indicador <- c(
      original_url = "https://cdn.bancentral.gov.do/documents/estadisticas/sector-real/documents/pib_gasto_2007.xls",
      file_ext = "xls",
      max_changes = 18
    )
  }
  if(metadata){
    return(
          tibble::tribble(
            ~col, ~name, ~unit, ~dtype, ~key,
            "orden", "Orden de los componentes", "", "int", 1,
            "nivel", "Nivel de los componentes", "", "int", 1,
            "componente", "Componente", "", "text", 1,
            "date", "Fecha", "Trimestres", "qdate", 1,
            "pib", "PIB", "Millones de RD$", "f1", 0,
            "pib__ponderacion", "Ponderación por componente", "Porcentaje (%)", "f2", 0,
            "pib_acumulado", "PIB acumulado", "Millones de RD$", "f1", 0,
            "pib_acumulado__ponderacion", "Ponderación por componente (PIB acumulado)", "Porcentaje (%)", "f2", 0,
            "pib__ive", "Índice de Valores Encadenados (IVE) del PIB", "Índice (2007=100)", "f1", 0,
            "pib__tci", "Tasa de crecimiento PIB", "Porcentaje (%)", "f1", 0,
            "pib__incidencia", "Incidencia por componente del PIB", "", "f1", 0,
            "pib_acumulado__ive", "Índice de Valores Encadenados (IVE) - PIB Acumulado", "Índice (2007=100)", "f1", 0,
            "pib_acumulado__tci", "Tasa de crecimiento - PIB Acumulado", "Porcentaje (%)", "f1", 0,
            "pib_acumulado__incidencia", "Incidencia por componente del PIB Acumulado", "", "f1", 0
          )
      )
  }
  `...2` <- NULL
  V1 <- NULL
  V2 <- NULL
  pibFile <- "/mnt/c/Users/drdsd/Downloads/pib_gasto_2007.xls"
  if (!file.exists(pibFile)) {
    pibFile <- downloader(indicador)
  }
  pib <- readxl::read_excel(pibFile, sheet = 'PIB$_Trim', skip = 5, col_names = F)
  pib <- tidyr::drop_na(pib, ...2)
  pib <- pib[1:11,]
  pib <- t(pib)
  pib[,1] <- stringr::str_remove_all(pib[,1], "[^0-9]")
  pib <- as.data.frame(pib)
  pib <- tidyr::fill(pib, V1)
  pib <- dplyr::mutate(pib,
                       V2 = dplyr::case_when(
                         V2 == 'E-M' ~ 'Q1',
                         V2 == 'A-J' ~ 'Q2',
                         V2 == 'J-S' ~ 'Q3',
                         V2 == 'O-D' ~ 'Q4'
                       ),
                       V1 = paste(trimws(V1), trimws(V2)),
                       V2 = NULL)
  pib <- t(pib)
  pib[1,1] <- 'componente'
  pib <- as.data.frame(pib)
  names(pib) <- pib[1,]
  pib <- pib[-1,]
  pib <- tidyr::pivot_longer(pib, -componente, names_to = "date", values_to = "pib")

  # Ponderacion por componente
  pib2 <- readxl::read_excel(pibFile, sheet = 'PIB$_Trim', skip = 25, col_names = F)
  pib2 <- tidyr::drop_na(pib2, ...2)
  pib2 <- pib2[1:11,]
  pib2 <- t(pib2)
  pib2[,1] <- stringr::str_remove_all(pib2[,1], '[^0-9]')
  pib2[1,1] <- NA
  pib2 <- as.data.frame(pib2)
  pib2 <- tidyr::fill(pib2, V1)
  pib2 <- dplyr::mutate(pib2,
                        V2 = dplyr::case_when(
                          V2 == 'E-M' ~ 'Q1',
                          V2 == 'A-J' ~ 'Q2',
                          V2 == 'J-S' ~ 'Q3',
                          V2 == 'O-D' ~ 'Q4'
                        ),
                        V1 = paste(trimws(V1), trimws(V2)),
                        V2 = NULL)
  pib2 <- t(pib2)
  pib2[1,1] <- 'componente'
  pib2 <- as.data.frame(pib2)
  names(pib2) <- pib2[1,]
  pib2 <- pib2[-1,]
  pib2 <- tidyr::pivot_longer(pib2, -componente, names_to = "date", values_to = "pib__ponderacion")

  pib <- dplyr::left_join(pib, pib2)


  # PIB Acumulado
  pib2 <- readxl::read_excel(pibFile, sheet = 'PIB$_Trim_Acum', skip = 6, col_names = F)
  pib2 <- tidyr::drop_na(pib2, ...2)
  pib2 <- pib2[1:11,]
  pib2 <- t(pib2)
  pib2[,1] <- stringr::str_remove(pib2[,1], '\\(p\\)')
  pib2[1,1] <- NA
  pib2 <- as.data.frame(pib2)
  pib2 <- tidyr::fill(pib2, V1)
  pib2 <- dplyr::mutate(pib2,
                        V2 = dplyr::case_when(
                          V2 == 'E-M' ~ 'Q1',
                          V2 == 'E-J' ~ 'Q2',
                          V2 == 'E-S' ~ 'Q3',
                          V2 == 'E-D' ~ 'Q4'
                        ),
                        V1 = paste(trimws(V1), trimws(V2)),
                        V2 = NULL)
  pib2 <- t(pib2)
  pib2[1,1] <- 'componente'
  pib2 <- as.data.frame(pib2)
  names(pib2) <- pib2[1,]
  pib2 <- pib2[-1,]
  pib2 <- tidyr::pivot_longer(pib2, -componente, names_to = "date", values_to = "pib_acumulado")

  pib <- dplyr::left_join(pib, pib2)

  # Ponderación por componente PIB acumulado
  pib2 <- readxl::read_excel(pibFile, sheet = 'PIB$_Trim_Acum', skip = 25, col_names = F)
  pib2 <- tidyr::drop_na(pib2, ...2)
  pib2 <- pib2[1:11,]
  pib2 <- t(pib2)
  pib2[,1] <- stringr::str_remove(pib2[,1], '\\(p\\)')
  pib2[1,1] <- NA
  pib2 <- as.data.frame(pib2)
  pib2 <- tidyr::fill(pib2, V1)
  pib2 <- dplyr::mutate(pib2,
                        V2 = dplyr::case_when(
                          V2 == 'E-M' ~ 'Q1',
                          V2 == 'E-J' ~ 'Q2',
                          V2 == 'E-S' ~ 'Q3',
                          V2 == 'E-D' ~ 'Q4'
                        ),
                        V1 = paste(trimws(V1), trimws(V2)),
                        V2 = NULL)
  pib2 <- t(pib2)
  pib2[1,1] <- 'componente'
  pib2 <- as.data.frame(pib2)
  names(pib2) <- pib2[1,]
  pib2 <- pib2[-1,]
  pib2 <- tidyr::pivot_longer(pib2, -componente, names_to = "date", values_to = "pib_acumulado__ponderacion")

  pib <- dplyr::left_join(pib, pib2)

  ## INDICE PIB

  pib2 <- readxl::read_excel(pibFile, sheet = 'PIBK_Trim', skip = 6, col_names = F)
  pib2 <- pib2[!is.na(pib2$...1) | !is.na(pib2$...2),]
  pib2 <- pib2[1:11,]
  pib2 <- t(pib2)
  pib2[,1] <- stringr::str_remove(pib2[,1], '\\(p\\)')
  pib2[1,1] <- NA
  pib2 <- as.data.frame(pib2)
  pib2 <- tidyr::fill(pib2, V1)
  pib2 <- dplyr::mutate(pib2,
                        V2 = dplyr::case_when(
                          V2 == 'E-M' ~ 'Q1',
                          V2 == 'A-J' ~ 'Q2',
                          V2 == 'J-S' ~ 'Q3',
                          V2 == 'O-D' ~ 'Q4'
                        ),
                        V1 = paste(trimws(V1), trimws(V2)),
                        V2 = NULL)
  pib2 <- t(pib2)
  pib2[,1] <- stringr::str_remove(pib2[,1], '\\(1\\)')
  pib2[1,1] <- 'componente'
  pib2 <- as.data.frame(pib2)
  names(pib2) <- pib2[1,]
  pib2 <- pib2[-1,]
  pib2 <- tidyr::pivot_longer(pib2, -componente, names_to = "date", values_to = "pib__ive")

  pib <- dplyr::left_join(pib, pib2)

  # Tasa de crecimiento

  pib2 <- readxl::read_excel(pibFile, sheet = 'PIBK_Trim', skip = 25, col_names = F)
  pib2 <- pib2[!is.na(pib2$...1) | !is.na(pib2$...2),]
  pib2 <- pib2[1:11,]
  pib2 <- t(pib2)
  pib2[,1] <- stringr::str_remove(pib2[,1], '\\(p\\)')
  pib2[1,1] <- NA
  pib2 <- as.data.frame(pib2)
  pib2 <- tidyr::fill(pib2, V1)
  pib2 <- dplyr::mutate(pib2,
                        V2 = dplyr::case_when(
                          V2 == 'E-M' ~ 'Q1',
                          V2 == 'A-J' ~ 'Q2',
                          V2 == 'J-S' ~ 'Q3',
                          V2 == 'O-D' ~ 'Q4'
                        ),
                        V1 = paste(trimws(V1), trimws(V2)),
                        V2 = NULL)
  pib2 <- t(pib2)
  pib2[,1] <- stringr::str_remove(pib2[,1], '\\(1\\)')
  pib2[1,1] <- 'componente'
  pib2 <- as.data.frame(pib2)
  names(pib2) <- pib2[1,]
  pib2 <- pib2[-1,]
  pib2 <- tidyr::pivot_longer(pib2, -componente, names_to = "date", values_to = "pib__tci")

  pib <- dplyr::left_join(pib, pib2)

  # Incidencia por componente
  pib2 <- readxl::read_excel(pibFile, sheet = 'PIBK_Trim', skip = 44, col_names = F)
  pib2 <- pib2[!is.na(pib2$...1) | !is.na(pib2$...2),]
  pib2 <- pib2[1:11,]
  pib2 <- t(pib2)
  pib2[,1] <- stringr::str_remove(pib2[,1], '\\(p\\)')
  pib2[1,1] <- NA
  pib2 <- as.data.frame(pib2)
  pib2 <- tidyr::fill(pib2, V1)
  pib2 <- dplyr::mutate(pib2,
                        V2 = dplyr::case_when(
                          V2 == 'E-M' ~ 'Q1',
                          V2 == 'A-J' ~ 'Q2',
                          V2 == 'J-S' ~ 'Q3',
                          V2 == 'O-D' ~ 'Q4'
                        ),
                        V1 = paste(trimws(V1), trimws(V2)),
                        V2 = NULL)
  pib2 <- t(pib2)
  pib2[,1] <- stringr::str_remove(pib2[,1], '\\(1\\)')
  pib2[1,1] <- 'componente'
  pib2 <- as.data.frame(pib2)
  names(pib2) <- pib2[1,]
  pib2 <- pib2[-1,]
  pib2 <- tidyr::pivot_longer(pib2, -componente, names_to = "date", values_to = "pib__incidencia")

  pib <- dplyr::left_join(pib, pib2)


  ## INDICE PIB ACUMULADO

  pib2 <- readxl::read_excel(pibFile, sheet = 'PIBK_Trim_Acum', skip = 6, col_names = F)
  pib2 <- pib2[!is.na(pib2$...1) | !is.na(pib2$...2),]
  pib2 <- pib2[1:11,]
  pib2 <- t(pib2)
  pib2[,1] <- stringr::str_remove(pib2[,1], '\\(p\\)')
  pib2[1,1] <- NA
  pib2 <- as.data.frame(pib2)
  pib2 <- tidyr::fill(pib2, V1)
  pib2 <- dplyr::mutate(pib2,
                        V2 = dplyr::case_when(
                          V2 == 'E-M' ~ 'Q1',
                          V2 == 'E-J' ~ 'Q2',
                          V2 == 'E-S' ~ 'Q3',
                          V2 == 'E-D' ~ 'Q4'
                        ),
                        V1 = paste(trimws(V1), trimws(V2)),
                        V2 = NULL)
  pib2 <- t(pib2)
  pib2[,1] <- stringr::str_remove(pib2[,1], '\\(1\\)')
  pib2[1,1] <- 'componente'
  pib2 <- as.data.frame(pib2)
  names(pib2) <- pib2[1,]
  pib2 <- pib2[-1,]
  pib2 <- tidyr::pivot_longer(pib2, -componente, names_to = "date", values_to = "pib_acumulado__ive")

  pib <- dplyr::left_join(pib, pib2)

  # Tasa de crecimiento

  pib2 <- readxl::read_excel(pibFile, sheet = 'PIBK_Trim_Acum', skip = 25, col_names = F)
  pib2 <- pib2[!is.na(pib2$...1) | !is.na(pib2$...2),]
  pib2 <- pib2[1:11,]
  pib2 <- t(pib2)
  pib2[,1] <- stringr::str_remove(pib2[,1], '\\(p\\)')
  pib2[1,1] <- NA
  pib2 <- as.data.frame(pib2)
  pib2 <- tidyr::fill(pib2, V1)
  pib2 <- dplyr::mutate(pib2,
                        V2 = dplyr::case_when(
                          V2 == 'E-M' ~ 'Q1',
                          V2 == 'E-J' ~ 'Q2',
                          V2 == 'E-S' ~ 'Q3',
                          V2 == 'E-D' ~ 'Q4'
                        ),
                        V1 = paste(trimws(V1), trimws(V2)),
                        V2 = NULL)
  pib2 <- t(pib2)
  pib2[,1] <- stringr::str_remove(pib2[,1], '\\(1\\)')
  pib2[1,1] <- 'componente'
  pib2 <- as.data.frame(pib2)
  names(pib2) <- pib2[1,]
  pib2 <- pib2[-1,]
  pib2 <- tidyr::pivot_longer(pib2, -componente, names_to = "date", values_to = "pib_acumulado__tci")

  pib <- dplyr::left_join(pib, pib2)

  # Incidencia por componente
  pib2 <- readxl::read_excel(pibFile, sheet = 'PIBK_Trim_Acum', skip = 44, col_names = F)
  pib2 <- pib2[!is.na(pib2$...1) | !is.na(pib2$...2),]
  pib2 <- pib2[1:11,]
  pib2 <- t(pib2)
  pib2[,1] <- stringr::str_remove(pib2[,1], '\\(p\\)')
  pib2[1,1] <- NA
  pib2 <- as.data.frame(pib2)
  pib2 <- tidyr::fill(pib2, V1)
  pib2 <- dplyr::mutate(pib2,
                        V2 = dplyr::case_when(
                          V2 == 'E-M' ~ 'Q1',
                          V2 == 'E-J' ~ 'Q2',
                          V2 == 'E-S' ~ 'Q3',
                          V2 == 'E-D' ~ 'Q4'
                        ),
                        V1 = paste(trimws(V1), trimws(V2)),
                        V2 = NULL)
  pib2 <- t(pib2)
  pib2[,1] <- stringr::str_remove(pib2[,1], '\\(1\\)')
  pib2[1,1] <- 'componente'
  pib2 <- as.data.frame(pib2)
  names(pib2) <- pib2[1,]
  pib2 <- pib2[-1,]
  pib2 <- tidyr::pivot_longer(pib2, -componente, names_to = "date", values_to = "pib_acumulado__incidencia")

  pib <- dplyr::left_join(pib, pib2)

  pib$date <- lubridate::ceiling_date(as.Date(tsibble::yearquarter(pib$date)), unit = "quarter")
  pib$date <- lubridate::add_with_rollback(pib$date, lubridate::days(-1))


  pib %>%
    dplyr::left_join(domar::nvl_pib_gasto) %>%
    dplyr::relocate(c(orden, nivel)) %>%
    type.convert(as.is = T) %>%
    dplyr::left_join(
      download_domar("tipo-cambio-usd-dop-trim") |>
        dplyr::filter(stringr::str_detect(tipo, "Promedio")) |>
        dplyr::select(date, tipo_cambio = compra)
    ) %>%
    dplyr::mutate(
      pib__usd = pib/tipo_cambio,
      year = lubridate::year(date)
    ) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(
      tipo_cambio_acum = dplyr::cummean(tipo_cambio),
      pib_acumulado__usd = pib_acumulado/tipo_cambio_acum
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-c(year, dplyr::starts_with("tipo_camb")))
}




#' PIB por enfoque del gasto anual
#'
#'   \lifecycle{experimental}
#'
#' @param indicador Vea \code{\link{downloader}}
#' @param metadata indica si se retornan los datos o la metadata del indicador
#'
#' @return [data.frame]: los datos del indicador en forma tabular

#' @export
#'
#' @examples
#' \dontrun{
#' pib_gasto_anual()
#' }
pib_gasto_anual <- function(data = NULL, metadata = FALSE){
  if(metadata){
    return(
      tibble::tribble(
        ~col, ~name, ~unit, ~dtype, ~key,
        "orden", "Orden de los componentes", "", "int", 1,
        "nivel", "Nivel de los componentes", "", "int", 1,
        "componente", "Componente", "", "text", 1,
        "ano", "Año", "", "ydate", 1,
        "pib", "Valor del PIB", "Millones de RD$", "f1", 0,
        "pib__ponderacion", "Ponderación por componente", "Porcentaje (%)", "f2", 0,
        "pib__ive", "Índice de Valores Encadenados (IVE) del PIB", "Índice (2007=100)", "f1", 0,
        "pib__tci", "Tasa de crecimiento PIB", "Porcentaje (%)", "f1", 0,
        "pib__incidencia", "Incidencia por componente del PIB", "", "f1", 0
      )
    )
  } else if(is.null(data)){
    datos <- pib_gasto_trim()
  } else {
    datos <- data
  }
  datos %>%
    dplyr::select(1:4, dplyr::contains("acum")) %>%
    dplyr::filter(lubridate::month(date) == 12) %>%
    setNames(c("orden", "nivel", "componente", "ano", "pib", "pib__ponderacion", "pib__ive", "pib__tci", "pib__incidencia")) %>%
    dplyr::mutate(ano = lubridate::year(ano)) %>%
    type.convert(as.is = T)
}




#' PIB por enfoque de origen trimestral
#'
#'   \lifecycle{experimental}
#'
#' @param indicador Vea \code{\link{downloader}}
#' @param metadata indica si se retornan los datos o la metadata del indicador
#'
#' @return [data.frame]: los datos del indicador en forma tabular

#' @export
#'
#' @examples
#' \dontrun{
#' pib_origen_trim()
#' }
pib_origen_trim <- function(indicador = NULL, metadata = FALSE){
  if(metadata){
    return(
      tibble::tribble(
        ~col, ~name, ~unit, ~dtype, ~key,
        "orden", "Orden de las ramas", "", "int", 1,
        "nivel", "Nivel de las ramas", "", "int", 1,
        "rae", "Rama de Actividad Económica", "", "text", 1,
        "date", "Fecha", "Trimestral", "qdate", 1,
        "valor_agregado", "Valor agregado de la rama", "Millones de RD$", "f1", 0,
        "ponderacion", "Ponderación por rama", "Razón", "f3", 0,
        "va_acumulado", "Valor agregado acumulado", "Millones de RD$", "f1", 0,
        "ponderacion_acum", "Ponderación acumulada", "Razón", "f3", 0,
        "ive", "Índice de Volumen Encadenados (IVE)", "Índice", "f1", 0,
        "tasa_crecimiento", "Tasa de crecimiento", "Porcentaje (%)", "f1", 0,
        "incidencia", "Incidencia", "", "f1", 0,
        "ive_acum", "Índice de Valores Encadenados (IVE) acumulado", "Índice", "f1", 0,
        "tasa_crecimiento_acum", "Tasa de crecimiento acumulada", "Porcentaje (%)", "f1", 0,
        "incidencia_acum", "Incidencia acumulada", "", "f1", 0
      )
    )
  }
  if(is.null(indicador)){
    indicador <- c(
      original_url = "https://cdn.bancentral.gov.do/documents/estadisticas/sector-real/documents/pib_origen_2007.xlsx",
      file_ext = "xlsx",
      max_changes = 62
    )
  }
  file <- "/mnt/c/Users/drdsd/Downloads/pib_origen_2007.xlsx"
  if (!file.exists(file)) {
    file <- downloader(indicador)
  }

  # Valor agregado
  va <- readxl::read_excel(file, sheet = "PIB$_Trim", skip = 6, col_names = F) %>%
    tidyr::drop_na(...2) %>%
    .[1:33,] %>%
    t() %>%
    as.data.frame() %>%
    dplyr::mutate(
      V1 = stringr::str_remove_all(V1, stringr::regex("[^0-9]"))
    ) %>%
    tidyr::fill(V1) %>%
    janitor::row_to_names(1)

  names(va)[1:2] <- c("year", "quarter")

  va <- va%>%
    Dmisc::vars_to_date(year = 1, quarter = 2) %>%
    tidyr::pivot_longer(-date, names_to = "rae", values_to = "pib")

  # Ponderacion
  pva <- readxl::read_excel(file, sheet = "PIB$_Trim", skip = 42, col_names = F) %>%
    tidyr::drop_na(...2) %>%
    .[1:33,] %>%
    t() %>%
    as.data.frame() %>%
    dplyr::mutate(
      V1 = stringr::str_remove_all(V1, stringr::regex("[^0-9]"))
    ) %>%
    tidyr::fill(V1) %>%
    janitor::row_to_names(1)

  names(pva)[1:2] <- c("year", "quarter")

  pva <- pva %>%
    Dmisc::vars_to_date(year = 1, quarter = 2) %>%
    tidyr::pivot_longer(-date, names_to = "rae", values_to = "pib__ponderacion")

  # Valor agregado acumulado
  vaa <- readxl::read_excel(file, sheet = "PIB$_Trim_Acum", skip = 6, col_names = F) %>%
    tidyr::drop_na(...2) %>%
    .[1:33,] %>%
    t() %>%
    as.data.frame() %>%
    dplyr::mutate(
      V1 = stringr::str_remove_all(V1, stringr::regex("[^0-9]"))
    ) %>%
    tidyr::fill(V1) %>%
    janitor::row_to_names(1)

  names(vaa)[1:2] <- c("year", "quarter")

  vaa <- vaa  %>%
    Dmisc::vars_to_date(year = 1, quarter = 2) %>%
    tidyr::pivot_longer(-date, names_to = "rae", values_to = "pib_acumulado")

  # Valor agregado acumulado
  pvaa <- readxl::read_excel(file, sheet = "PIB$_Trim_Acum", skip = 42, col_names = F) %>%
    tidyr::drop_na(...2) %>%
    .[1:33,] %>%
    t() %>%
    as.data.frame() %>%
    dplyr::mutate(
      V1 = stringr::str_remove_all(V1, stringr::regex("[^0-9]"))
    ) %>%
    tidyr::fill(V1) %>%
    janitor::row_to_names(1)

  names(pvaa)[1:2] <- c("year", "quarter")

  pvaa <- pvaa   %>%
    Dmisc::vars_to_date(year = 1, quarter = 2) %>%
    tidyr::pivot_longer(-date, names_to = "rae", values_to = "pib_acumulado__ponderacion")

  # Indice de valores encadenados (IVE)
  ive <- readxl::read_excel(file, sheet = "PIBK_Trim", skip = 6, col_names = F) %>%
    tidyr::drop_na(...2) %>%
    .[1:33,] %>%
    t() %>%
    as.data.frame() %>%
    dplyr::mutate(
      V1 = stringr::str_remove_all(V1, stringr::regex("[^0-9]"))
    ) %>%
    tidyr::fill(V1) %>%
    janitor::row_to_names(1)

  names(ive)[1:2] <- c("year", "quarter")

  ive <- ive   %>%
    Dmisc::vars_to_date(year = 1, quarter = 2) %>%
    tidyr::pivot_longer(-date, names_to = "rae", values_to = "pib__ive")

  # Tasa de crecimiento
  tc <- readxl::read_excel(file, sheet = "PIBK_Trim", skip = 42, col_names = F) %>%
    tidyr::drop_na(...6) %>%
    .[1:33,] %>%
    t() %>%
    as.data.frame() %>%
    dplyr::mutate(
      V1 = stringr::str_remove_all(V1, stringr::regex("[^0-9]"))
    ) %>%
    tidyr::fill(V1) %>%
    janitor::row_to_names(1)

  names(tc)[1:2] <- c("year", "quarter")

  tc <- tc   %>%
    Dmisc::vars_to_date(year = 1, quarter = 2) %>%
    tidyr::pivot_longer(-date, names_to = "rae", values_to = "pib__tci")

  # Incidencia
  iva <- readxl::read_excel(file, sheet = "PIBK_Trim", skip = 78, col_names = F) %>%
    tidyr::drop_na(...6) %>%
    .[1:33,] %>%
    t() %>%
    as.data.frame() %>%
    dplyr::mutate(
      V1 = stringr::str_remove_all(V1, stringr::regex("[^0-9]"))
    ) %>%
    tidyr::fill(V1) %>%
    janitor::row_to_names(1)

  names(iva)[1:2] <- c("year", "quarter")

  iva <- iva   %>%
    Dmisc::vars_to_date(year = 1, quarter = 2) %>%
    tidyr::pivot_longer(-date, names_to = "rae", values_to = "pib__incidencia")

  # Indice de valores encadenados (IVE) acumulado
  ivea <- readxl::read_excel(file, sheet = "PIBK_Trim_Acum", skip = 6, col_names = F) %>%
    tidyr::drop_na(...2) %>%
    .[1:33,] %>%
    t() %>%
    as.data.frame() %>%
    dplyr::mutate(
      V1 = stringr::str_remove_all(V1, stringr::regex("[^0-9]"))
    ) %>%
    tidyr::fill(V1) %>%
    janitor::row_to_names(1)

  names(ivea)[1:2] <- c("year", "quarter")

  ivea <- ivea   %>%
    Dmisc::vars_to_date(year = 1, quarter = 2) %>%
    tidyr::pivot_longer(-date, names_to = "rae", values_to = "pib_acumulado__ive")

  # Tasa de crecimiento
  tca <- readxl::read_excel(file, sheet = "PIBK_Trim_Acum", skip = 42, col_names = F) %>%
    tidyr::drop_na(...6) %>%
    .[1:33,] %>%
    t() %>%
    as.data.frame() %>%
    dplyr::mutate(
      V1 = stringr::str_remove_all(V1, stringr::regex("[^0-9]"))
    ) %>%
    tidyr::fill(V1) %>%
    janitor::row_to_names(1)

  names(tca)[1:2] <- c("year", "quarter")

  tca <- tca   %>%
    Dmisc::vars_to_date(year = 1, quarter = 2) %>%
    tidyr::pivot_longer(-date, names_to = "rae", values_to = "pib_acumulado__tci")

  # Incidencia
  ivaa <- readxl::read_excel(file, sheet = "PIBK_Trim_Acum", skip = 78, col_names = F) %>%
    tidyr::drop_na(...6) %>%
    .[1:33,] %>%
    t() %>%
    as.data.frame() %>%
    dplyr::mutate(
      V1 = stringr::str_remove_all(V1, stringr::regex("[^0-9]"))
    ) %>%
    tidyr::fill(V1) %>%
    janitor::row_to_names(1)

  names(ivaa)[1:2] <- c("year", "quarter")

  ivaa <- ivaa   %>%
    Dmisc::vars_to_date(year = 1, quarter = 2) %>%
    tidyr::pivot_longer(-date, names_to = "rae", values_to = "pib_acumulado__incidencia")

  #unlink(file)

  va %>%
    dplyr::left_join(pva) %>%
    dplyr::left_join(vaa) %>%
    dplyr::left_join(pvaa) %>%
    dplyr::left_join(ive) %>%
    dplyr::left_join(tc) %>%
    dplyr::left_join(iva) %>%
    dplyr::left_join(ivea) %>%
    dplyr::left_join(tca) %>%
    dplyr::left_join(ivaa) %>%
    dplyr::left_join(domar::nvl_pib_origen) %>%
    dplyr::relocate(orden, nivel) %>%
    type.convert(as.is = T)
}



#' PIB por enfoque de origen anual
#'
#'   \lifecycle{experimental}
#'
#' @param indicador Vea \code{\link{downloader}}
#' @param metadata indica si se retornan los datos o la metadata del indicador
#'
#' @return [data.frame]: los datos del indicador en forma tabular

#' @export
#'
#' @examples
#' \dontrun{
#' pib_origen_anual()
#' }
pib_origen_anual <- function(data = NULL, metadata = FALSE){
  if(metadata){
    return(
      tibble::tribble(
        ~col, ~name, ~unit, ~dtype,
        "orden", "Orden de las ramas", "", "int",
        "nivel", "Nivel de las ramas", "", "int",
        "rae", "Rama de Actividad Económica", "", "text",
        "ano", "Año", "", "ydate",
        "valor_agregado", "Valor agregado de la rama", "Millones de RD$", "f1",
        "ponderacion", "Ponderación por rama", "Razón", "f3",
        "ive", "Índice de Volumen Encadenados (IVE)", "Índice", "f1",
        "tasa_crecimiento", "Tasa de crecimiento", "Porcentaje (%)", "f1",
        "incidencia", "Incidencia", "", "f1",
      )
    )
  } else if(is.null(data)){
    datos <- pib_origen_trim()
  } else {
    datos <- data
  }

  datos %>%
    dplyr::select(1:4, dplyr::contains("acum")) %>%
    dplyr::filter(lubridate::month(date) == 12) %>%
    setNames(c("orden", "nivel", "ano", "rae", "valor_agregado", "ponderacion", "ive", "tasa_crecimiento", "incidencia")) %>%
    dplyr::mutate(ano = lubridate::year(ano)) %>%
    type.convert(as.is = T)
}



#' Índices de Valores Encadenados (IVE)
#'
#'   \lifecycle{experimental}
#'
#' @param indicador Vea \code{\link{downloader}}
#'
pib_ive <- function(indicador = NULL){
  if(is.null(indicador)){
    indicador <- c(
      original_url = "https://cdn.bancentral.gov.do/documents/estadisticas/sector-real/documents/pib_2007.xlsx",
      file_ext = "xlsx",
      max_changes = 2
    )
  }
  file <- "/mnt/c/Users/drdsd/Downloads/pib_2007.xlsx"
  if (!file.exists(file)) {
    file <- downloader(indicador)
  }
  ive <- readxl::read_excel(file, skip = 8, col_names = F) %>%
    dplyr::filter(!stringr::str_detect(...1, "Preliminar")) %>%
    dplyr::mutate(
      ano = dplyr::case_when(
        stringr::str_detect(...1, "[0-9]") ~ stringr::str_remove_all(...1, "[^0-9]")
      ),
      ...1 = dplyr::case_when(
        !stringr::str_detect(...1, "[0-9]") ~ ...1
      )
    ) %>%
    dplyr::relocate(ano) %>%
    tidyr::fill(ano, .direction = "up")

  ive[, 1:4] %>%
    setNames(c("ano", "trim", "ive", "variacion_interanual")) %>%
    dplyr::mutate(serie = "Serie original") %>%
    dplyr::bind_rows(
      ive[, c(1, 2, 5, 6)] %>%
        setNames(c("ano", "trim", "ive", "variacion_interanual")) %>%
        dplyr::mutate(serie = "Serie desestacionalizada")
    )
}


#' Índice de Volumen Encadenados (IVE) trimestral
#'
#'   \lifecycle{experimental}
#'
#' @param indicador Vea \code{\link{downloader}}
#' @param metadata indica si se retornan los datos o la metadata del indicador
#'
#' @return [data.frame]: los datos del indicador en forma tabular

#' @export
#'
#' @examples
#' \dontrun{
#' pib_ive_trim()
#' }
pib_ive_trim <- function(indicador = NULL, metadata = FALSE){
  if(metadata){
    return(
      tibble::tribble(
        ~col, ~name, ~unit, ~dtype,
        "date", "Fecha", "Trimestres", "qdate",
        "ive", "Índice de Volumen Encadenados (IVE)", "Índice", "f1",
        "variacion_interanual", "Variación (%) interanual", "Porcentaje (%)", "f1",
        "serie", "Serie", "", "text"
      )
    )
  }
  pib_ive(indicador) %>%
    tidyr::drop_na(trim) %>%
    Dmisc::vars_to_date(year = 1, quarter = 2) %>%
    type.convert(as.is = T)
}


#' Índice de Volumen Encadenados (IVE) anual
#'
#'   \lifecycle{experimental}
#'
#' @param indicador Vea \code{\link{downloader}}
#' @param metadata indica si se retornan los datos o la metadata del indicador
#'
#' @return [data.frame]: los datos del indicador en forma tabular

#' @export
#'
#' @examples
#' \dontrun{
#' pib_ive_anual()
#' }
pib_ive_anual <- function(indicador = NULL, metadata = FALSE){
  if(metadata){
    return(
      tibble::tribble(
        ~col, ~name, ~unit, ~dtype,
        "ano", "Año", "", "ydate",
        "ive", "Índice de Volumen Encadenados (IVE)", "Índice", "f1",
        "variacion_interanual", "Variación (%) interanual", "Porcentaje (%)", "f1"
      )
    )
  }
  pib_ive(indicador) %>%
    dplyr::filter(is.na(trim)) %>%
    dplyr::select(-trim, -serie) %>%
    tidyr::drop_na(ive) %>%
    type.convert(as.is = T)
}



#' Deflactor del Producto Interno Bruto (PIB)
#'
#'  \lifecycle{deprecated}
#'
#' @param indicador Vea \code{\link{downloader}}
#'
pib_deflactor <- function(indicador = NULL) {
  if(is.null(indicador)){
    indicador = c(
      original_url = "https://cdn.bancentral.gov.do/documents/estadisticas/sector-real/documents/pib_deflactor_2007.xls",
      file_ext = "xls"
    )
  }
  `...1` <- NULL
  ano <- NULL
  `...2` <- NULL
  file <- "/mnt/c/Users/drdsd/Downloads/pib_deflactor_2007.xls"
  if (!file.exists(file)) {
    file <- downloader(indicador)
  }
  def <- readxl::read_excel(file, skip = 4, col_names = F)
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
#' @param metadata indica si se retornan los datos o la metadata del indicador
#'
#' @return [data.frame]: los datos del indicador en forma tabular
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   pib_deflactor_trimestral()
#' }
pib_deflactor_trim <- function(indicador = NULL){
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
    tidyr::pivot_longer(-c(serie, indicador), values_drop_na = T) %>%
    type.convert(as.is = T)
}


#' Deflactor del PIB Anual
#'
#'  \lifecycle{experimental}
#'
#' @param indicador Vea \code{\link{downloader}}
#' @param metadata indica si se retornan los datos o la metadata del indicador
#'
#' @return [data.frame]: los datos del indicador en forma tabular
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   pib_deflactor_anual()
#' }
pib_deflactor_anual <- function(indicador = NULL){
  `...1` <- NULL
  ano <- NULL
  serie <- NULL
  def <- pib_deflactor(indicador) %>%
    tidyr::fill(...1) %>%
    dplyr::filter(!startsWith(...1, "I")) %>%
    dplyr::select(-c(...1)) %>%
    dplyr::relocate(ano) %>%
    t() %>%
    as.data.frame()
  def[1,1] <- "serie"
  def[1,2] <- "indicador"
  def %>%
    janitor::row_to_names(1) %>%
    tidyr::fill(serie) %>%
    tidyr::pivot_longer(-c(serie, indicador), values_drop_na = T) %>%
    type.convert(as.is = T)
}



#' Indicador Mensual de Actividad Económica (IMAE) Mensual
#'
#'  \lifecycle{experimental}
#'
#' @param indicador Vea \code{\link{downloader}}
#' @param metadata indica si se retornan los datos o la metadata del indicador
#'
#' @return [data.frame]: los datos del indicador en forma tabular
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   imae_mensual(indicador)
#' }
imae_mensual <- function(indicador = NULL, metadata = FALSE) {
  if(is.null(indicador)){
    indicador = c(
      original_url = "https://cdn.bancentral.gov.do/documents/estadisticas/sector-real/documents/imae.xlsx",
      file_ext = "xlsx",
      max_changes = 9
    )
  }
  if(metadata){
    return(
      tibble::tribble(
        ~col, ~name, ~unit, ~dtype, ~key,
        "date", "Fecha", "Mensual", "mdate", 1,
        "indice", "Índice Mensual de Actividad Económica (IMAE)", "Índice", "f1", 0,
        "serie", "Serie", "", "text", 1,
        "variacion_interanual", "Variación (%) interanual", "Porcentaje (%)", "f1", 0,
        "variacion_acumulada", "Variación (%) acumulada", "Porcentaje (%)", "f1", 0,
        "variacion_promedio_12_meses", "Variación (%) promedio 12 meses", "Porcentaje (%)", "f1", 0,
        "variacion_periodo_anterior", "Variación (%) periodo anterior", "Porcentaje (%)", "f1", 0
      )
    )
  }
  ano <- NULL
  mes <- NULL
  file <- "/mnt/c/Users/drdsd/Downloads/imae.xlsx"
  if (!file.exists(file)) {
    file <- downloader(indicador)
  }
  imae <- readxl::read_excel(file, skip = 5)
  #unlink(file_path)
  # Serie original
  imaeso <- imae[,1:6]
  imaeso <- imaeso[-1,]
  imaeso[1,1] <- 'ano'
  imaeso[1,2] <- 'mes'
  imaeso[1,3] <- 'indice'
  names(imaeso) <- imaeso[1,]
  imaeso <- imaeso[-1,]
  names(imaeso)[names(imaeso) == 'Interanual'] <- 'variacion_interanual'
  names(imaeso)[names(imaeso) == 'Acumulada'] <- 'variacion_acumulada'
  names(imaeso)[names(imaeso) == 'Promedio 12 meses'] <- 'variacion_promedio_12_meses'
  imaeso <- imaeso[!is.na(imaeso$indice),]
  imaeso$ano <- stringr::str_remove_all(imaeso$ano, '[^0-9]')
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
  imaesd[1,3] <- 'indice'
  names(imaesd) <- imaesd[1,]
  imaesd <- imaesd[-1,]
  names(imaesd)[names(imaesd) == 'Respecto al per\u00EDodo anterior'] <- 'variacion_periodo_anterior'
  names(imaesd)[names(imaesd) == 'Interanual'] <- 'variacion_interanual'
  names(imaesd)[names(imaesd) == 'Acumulada'] <- 'variacion_acumulada'
  names(imaesd)[names(imaesd) == 'Promedio 12 meses'] <- 'variacion_promedio_12_meses'
  imaesd <- dplyr::mutate(imaesd,
                          ano = stringr::str_remove_all(ano, '[^0-9]'))
  imaesd <- imaesd[!is.na(imaesd$indice),]
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
  imaest[1,3] <- 'indice'
  names(imaest) <- imaest[1,]
  imaest <- imaest[-1,]
  names(imaest)[names(imaest) == 'Respecto al per\u00EDodo anterior'] <- 'variacion_periodo_anterior'
  names(imaest)[names(imaest) == 'Interanual'] <- 'variacion_interanual'
  names(imaest)[names(imaest) == 'Acumulada'] <- 'variacion_acumulada'
  names(imaest)[names(imaest) == 'Promedio 12 meses'] <- 'variacion_promedio_12_meses'
  imaest <- imaest[!is.na(imaest$indice),]
  imaest <- dplyr::mutate(imaest,
                          ano = stringr::str_remove_all(ano, '[^0-9]'))
  imaest <- tidyr::fill(imaest, ano, .direction = 'up')
  imaest <- tidyr::fill(imaest, ano)
  imaest <- imaest[!is.na(imaest$mes),]
  imaest <- Dmisc::vars_to_date(imaest, year = 1, month = 2)
  imaest$serie <- 'Serie Tendencia-Ciclo'

  #
  dplyr::bind_rows(imaeso, imaesd, imaest) %>%
    type.convert(as.is = T)
}


#' Producto Interno Bruto (PIB) per cápita
#'
#'  \lifecycle{experimental}
#'
#' @param indicador Vea \code{\link{downloader}}
#' @param metadata indica si se retornan los datos o la metadata del indicador
#'
#' @return [data.frame]: los datos del indicador en forma tabular
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   pib_per_capita()
#' }
pib_per_capita <- function(indicador = NULL, metadata = FALSE){
  if(metadata){
    return(
      tibble::tribble(
        ~col, ~name, ~unit, ~dtype,
        "ano", "Año", "", "ydate",
        "poblacion", "Población", "Miles", "int",
        "pib", "PIB Corriente RD$", "Millones de RD$", "f1",
        "pib_pc", "PIB Corriente per cápita RD$", "RD$", "f1",
        "pib_usd", "PIB Corriente US$", "Millones US$", "f1",
        "pib_pc_usd", "PIB Corriente per cápita US$", "", "f1",
        "ive", "Índice de Volumen Encadenados (IVE)", "Índice", "f1"
      )
    )
  }
  if(is.null(indicador)){
    indicador = c(
      original_url = "https://cdn.bancentral.gov.do/documents/estadisticas/sector-real/documents/pib_dolares.xls",
      file_ext = "xls"
    )
  }
  file <- "/mnt/c/lUsers/drdsd/Downloads/pib_dolares.xls"
  if (!file.exists(file)) {
    file <- downloader(indicador)
  }
  readxl::read_excel(file, skip = 7, col_names = F) %>%
    dplyr::mutate(
      ...1 = stringr::str_remove_all(...1, "[^0-9]"),
      ...1 = as.numeric(...1)
    )%>%
    tidyr::drop_na(...1) %>%
    dplyr::filter(!duplicated(...1)) %>%
    setNames(c("ano", "poblacion", "pib", "pib_pc", "pib_usd", "pib_pc_usd", "ive")) %>%
    type.convert(as.is = T)
}




























"
BANCO CENTRAL
FISCAL
Estado de Operaciones del sector público no financiero (% del PIB)
https://cdn.bancentral.gov.do/documents/estadisticas/documents/Operaciones_PIB_Anual.xlsx

MONETARIO

III. Base Monetaria y Agregados Monetarios
1. Base monetaria

2. Agregados monetarios

3. Agregados monetarios por sectores

4. Encaje bancario requerido y efectivo por monedas

IV. Agregados de Crédito
1. Crédito al sector público no financiero del BC

2. Crédito al sector público no financiero de las OSD

V. Tasas de Interés
1. Valores subastados del Banco Central por plazos de colocación en MN

2. Tasas de interés bancarias activas en MN

3. Tasas de interés bancarias activas en ME

4. Tasas de interés bancarias pasivas en MN

5. Tasas de interés bancarias pasivas en ME


TURISMO.
Llegada vía aérea
Total (según residencia y aeropuertos)
Tasa de ocupación
"
