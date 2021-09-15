#'  Deuda del Sector Público No Financiero por Acreedor
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
#'   saldo_deuda_spnf_xacre()
#' }
saldo_deuda_spnf_xacre <- function(indicador = NULL, metadata = FALSE) {
  if (metadata) {
    return(
      tibble::tribble(
        ~col, ~name, ~unit, ~dtype, ~key,
        "orden", "Orden", "", "int", 1,
        "nivel", "Nivel", "", "int", 1,
        "ano", "Año", "", "ydate", 1,
        "fuente", "Fuente de deuda/Acreedor", "", "text", 1,
        "valor", "Valor", "US$ millones", "f1", 0
      )
    )
  }
  if (is.null(indicador)) {
    indicador <- c(
      original_url = "https://www.creditopublico.gob.do/inicio/estadisticas",
      file_ext = "html",
      max_changes = 34*2
    )
  }

  file <- "/mnt/c/Users/drdsd/Downloads/20210817_salxacree.xlsx"
  if (!file.exists(file)) {
    html <- downloader(indicador)

    html %>%
      rvest::html_elements("a") %>%
      rvest::html_attr("href") %>%
      stringr::str_split(' ') %>%
      dplyr::bind_cols() %>%
      tidyr::pivot_longer(dplyr::everything()) %>%
      dplyr::filter(stringr::str_detect(value, "_salxacre")) %>%
      dplyr::mutate(
        value2 = value,
        value2 = stringr::str_remove_all(value2, "[^0-9]"),
        value2 = stringr::str_sub(value2, start = -8L),
        value2 = as.numeric(value2)
      ) %>%
      dplyr::filter(value2 == max(value2)) %>%
      dplyr::pull(value) %>%
      .[[1]] %>%
      paste0("https://www.creditopublico.gob.do", .) -> indicador$original_url

    indicador$file_ext <- "xlsx"

    file <- downloader(indicador)
  } else {
    print("Local file...")
  }

  datos <- readxl::read_excel(file, skip = 12, col_names = FALSE)

  datos <- datos[1:(match("RESUMEN", datos[[1]]) - 1), ]

  datos %>%
    t() %>%
    as.data.frame() %>%
    tidyr::drop_na(V1) -> datos

  datos[2:nrow(datos), 1] <- stringr::str_remove_all(datos[2:nrow(datos), 1], stringr::regex("[^0-9]"))

  datos %>%
    dplyr::mutate(
      V1 = dplyr::case_when(
        nchar(V1) < 4 ~ paste0("20", V1),
        TRUE ~ V1
      )
    ) %>%
    dplyr::select(-V2) %>%
    t() %>%
    as.data.frame() %>%
    tidyr::drop_na(...1) %>%
    janitor::row_to_names(1)  %>%
    dplyr::bind_cols(nvl_saldo_deuda_spnf_xacre %>% dplyr::select(-fuente)) %>%
    dplyr::relocate(orden, nivel) -> datos

  names(datos)[3] <- "fuente"

  datos %>%
    tidyr::pivot_longer(-c(1:3), names_to = "ano", values_to = "valor") %>%
    type.convert(as.is = TRUE)
}
