pib_ive <- function(indicador){
  `...1` <- NULL
  `...2` <- NULL
  ano <- NULL
  file <- downloader(indicador)
  ive <- readxl::read_excel(file, skip = 4, col_names = F)
  ive <- tidyr::fill(ive, ...1)
  ive <- dplyr::mutate(
    ive,
    ano = dplyr::if_else(stringr::str_detect(...1, "[a-z]"), ...1, NA_character_),
    ano = stringr::str_remove_all(ano, "[^0-9]")
  ) %>%
    tidyr::fill(ano, .direction = "up") %>%
    tidyr::drop_na(...2)
}

pib_ive_trimestral <- function(indicador = c(original_url ="https://cdn.bancentral.gov.do/documents/estadisticas/sector-real/documents/pib_2007.xlsx", file_ext = "xlsx")){
  `...1` <- NULL
  ano <- NULL
  fecha <- NULL
  serie <- NULL
  ive <- pib_ive(indicador)
  ive <- ive %>%
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
    )
  ive <- ive %>%
    dplyr::select(
      -ano,
      -...1
    )
  ive <- t(dplyr::relocate(ive, fecha))
  ive[1,1] <- "serie"
  ive[1,2] <- "indicador"
  ive <- janitor::row_to_names(ive, 1)
  ive <- as.data.frame(ive)
  ive <- tidyr::fill(ive, serie)
  ive <- tidyr::pivot_longer(ive, -c("serie", "indicador"), names_to = "fecha", values_drop_na = T)
  ive
}

pib_ive_anual <- function(indicador = c(original_url ="https://cdn.bancentral.gov.do/documents/estadisticas/sector-real/documents/pib_2007.xlsx", file_ext = "xlsx")){
  `...1` <- NULL
  ano <- NULL
  serie <- NULL
  ive <- pib_ive(indicador)
  ive <- ive %>%
    dplyr::filter(
      !startsWith(...1, "I")
    )
  ive <- ive %>%
    dplyr::select(
      -...1
    )
  ive <- t(dplyr::relocate(ive, ano))
  ive[1,1] <- "serie"
  ive[1,2] <- "indicador"
  ive <- janitor::row_to_names(ive, 1)
  ive <- as.data.frame(ive)
  ive <- tidyr::fill(ive, serie)
  ive <- tidyr::pivot_longer(ive, -c("serie", "indicador"), names_to = "ano", values_drop_na = T)
  ive
}
