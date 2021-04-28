pib_per_capita <- function(indicador = c(original_url = "https://cdn.bancentral.gov.do/documents/estadisticas/sector-real/documents/pib_dolares.xls", file_ext = "xls")){
  V1 <- NULL
  V2 <- NULL
  `...1` <- NULL
  `...2` <- NULL
  ano <- NULL
  file <- downloader(indicador)
  pib <- readxl::read_excel(file, skip = 4, col_names = F) %>%
    t() %>%
    as.data.frame() %>%
    dplyr::mutate(
      V1 = paste(V1, V2)
    ) %>%
    t() %>%
    as.data.frame() %>%
    dplyr::mutate(
      ...1 = stringr::str_remove_all(...1, "[^0-9]")
    ) %>%
    tidyr::drop_na(...1, ...2) %>%
    dplyr::mutate(
      indicador = dplyr::if_else(duplicated(...1), "Tasa de crecimiento", "Valor")
    ) %>%
    janitor::row_to_names(1)
  names(pib)[[1]] <- "ano"
  names(pib) <- janitor::make_clean_names(names(pib))
  dplyr::filter(pib, ano != "") %>%
    dplyr::rename("indicador" = "valor")
}
