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
        "date", "Fecha", "Anual", "ydate", 1,
        "fuente", "Fuente de deuda/Acreedor", "", "text", 1,
        "valor", "Valor", "US$ millones", "f1", 0
      )
    )
  }
  if (is.null(indicador)) {
    indicador <- c(
      original_url = "https://www.creditopublico.gob.do/Content/estadisticas/2021/julio/20210817_salxacree.xlsx",
      file_ext = "xlsx",
      max_changes = 34*2
    )
  }
  year <- lubridate::year(Sys.Date())
  month <- lubridate::month(Sys.Date() - months(1))
  file <- "/mnt/c/Users/drdsd/Downloads/20210817_salxacree.xlsx"
  file2 <- FALSE

  meses <- c("diciembre" = 12, "noviembre" = 11, "octubre" = 10, "septiembre" = 9, "agosto" = 8, "julio" = 7, "junio" = 6, "mayo" = 5, "abril" = 4, "marzo" = 3, "febrero" = 2, "enero" = 1)
  pos <- match(lubridate::month(Sys.Date() - months(1)), meses)
  meses <- c(meses[pos:length(meses)], meses[1:(pos - 1)])

  stop <- FALSE

  if (file.exists(file)) {
    print("Local file...")
    datos <- readxl::read_excel(file, skip = 12, col_names = FALSE)
  } else {
    for (y in year:(year - 1)) {
      for (m in names(meses)) {
        for (y2 in year:(year - 1)) {
          for (m2 in meses) {
            for (d in 31:1) {
              url <- glue::glue("https://www.creditopublico.gob.do/Content/estadisticas/{y}/{m}/{y2}{stringr::str_pad(m2, 2, pad = 0)}{stringr::str_pad(d, 2, pad = 0)}_salxacree.xlsx")
              indicador[["original_url"]] <- url
              res <- httr::GET(url)
              if (!stringr::str_detect(res[["headers"]][["content-type"]], "text/html")) {
                file2 <- downloader(indicador)
                stop <- TRUE
              }
              if (stop) {
                break
              }
            }
            if (stop) {
              break
            }
          }
          if (stop) {
            break
          }
        }
        if (stop) {
          break
        }
      }
      if (stop) {
        break
      }
    }
    datos <- readxl::read_excel(file2, skip = 12, col_names = FALSE)
  }

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
    tidyr::pivot_longer(-c(1:3), names_to = "date", values_to = "valor") %>%
    type.convert(as.is = TRUE) %>%
    dplyr::relocate(c(orden, nivel, date))
}
