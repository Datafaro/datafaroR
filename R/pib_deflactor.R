pib_deflactor <- function(indicador) {
  `...1` <- NULL
  ano <- NULL
  `...2` <- NULL
  file <- downloader(indicador)
  def <- readxl::read_excel(file, skip = 4, col_names = F) %>%
    dplyr::mutate(
      ano = dplyr::if_else(stringr::str_detect(...1, "[0-9]"), ...1, NA_character_),
      ano = stringr::str_remove_all(ano, "[^0-9]")
    ) %>%
    tidyr::fill(ano, .direction = "up") %>%
    tidyr::drop_na(...2)
  def
}

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
