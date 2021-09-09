# Informe de desempeño ----


#'  Informe de desempeño del sector energético: variables relevantes
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
#'   informe_desempeno_sector_energetico_variables_relevantes()
#' }
informe_desempeno_sector_energetico_variables_relevantes <- function(indicador = NULL, metadata = FALSE) {
  if (metadata) {
    return(
      tibble::tribble(
        ~col, ~name, ~unit, ~dtype, ~key,
        "orden", "Orden", "", "int", 1,
        "nivel", "Nivel", "", "int", 1,
        "date", "Fecha", "Mensual", "mdate", 1,
        "variable", "Variable", "", "text", 1,
        "valor", "Valor", "", "f1", 0
      )
    )
  }
  if (is.null(indicador)) {
    indicador <- c(
      original_url = "https://cdeee.gob.do/transparencia/informes-del-sector-energetico/",
      file_ext = "html",
      max_changes = 38 * 2
    )
  }

  file <- "/mnt/c/Users/drdsd/Downloads/Informe-de-Desempeno-Junio-2021-Anexos._.xlsx"
  if (!file.exists(file)) {
    remDr <- RSelenium::remoteDriver(
      remoteServerAddr = "localhost",
      port = 4445L,
      browserName = "firefox"
    )

    remDr$open()

    remDr$navigate("https://cdeee.gob.do/transparencia/informes-del-sector-energetico/")

    year <- remDr$findElement("xpath", '//*[@id="wpfb-cat-1740"]/span/a')

    year$clickElement()


    month <- remDr$findElement("xpath", '//*[@id="wpfb-cat-1869"]/span/a')

    month$clickElement()

    remDr$getPageSource() %>%
      .[[1]] %>%
      stringr::str_split("\n") %>%
      dplyr::bind_cols() %>%
      dplyr::filter(stringr::str_detect(...1, ".xlsx")) %>%
      .[[1]] %>%
      stringr::str_split(" ") %>%
      dplyr::bind_cols() %>%
      dplyr::filter(stringr::str_detect(...1, ".xlsx")) %>%
      tidyr::separate(...1, c("V1", "V2", "V3"), "\"") %>%
      dplyr::pull(V2) %>%
      .[[1]] -> indicador$original_url
    indicador$file_ext <- "xlsx"

    file <- downloader(indicador)
  } else {
    print("Local file...")
  }

  datos <- readxl::read_excel(file, skip = 6, col_names = FALSE)

  datos[1, 2] <- "9999"

  datos %>%
    tidyr::drop_na(...2) %>%
    t() %>%
    as.data.frame() %>%
    dplyr::mutate(V1 = as.numeric(V1)) %>%
    tidyr::drop_na(V1) %>%
    dplyr::filter(V1 > 3000) %>%
    dplyr::mutate(V1 = as.Date(V1, origin="1900-01-01")) %>%
    t() %>%
    as.data.frame() -> datos

  datos[1,1] <- "variable"

  datos %>%
    janitor::row_to_names(1) %>%
    dplyr::bind_cols(nvl_idsevr %>% dplyr::select(-variable)) %>%
    dplyr::relocate(orden, nivel) %>%
    tidyr::pivot_longer(-c(1:3), names_to = "date", values_to = "valor") %>%
    type.convert(as.is = TRUE)
}



#'  Informe de desempeño del sector energético: EDE's
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
#'   informe_desempeno_sector_energetico_edes()
#' }
informe_desempeno_sector_energetico_edes <- function(indicador = NULL, metadata = FALSE) {
  if (metadata) {
    return(
      tibble::tribble(
        ~col, ~name, ~unit, ~dtype, ~key,
        "indicador", "Indicador", "", "text", 1,
        "ede", "EDE", "", "text", 1,
        "date", "Fecha", "Mensual", "mdate", 1,
        "valor", "Valor", "", "f1", 0
      )
    )
  }
  if (is.null(indicador)) {
    indicador <- c(
      original_url = "https://cdeee.gob.do/transparencia/informes-del-sector-energetico/",
      file_ext = "html",
      max_changes = 168 * 2
    )
  }

  file <- "/mnt/c/Users/drdsd/Downloads/Informe-de-Desempeno-Junio-2021-Anexos._.xlsx"
  if (!file.exists(file)) {
    remDr <- RSelenium::remoteDriver(
      remoteServerAddr = "localhost",
      port = 4445L,
      browserName = "firefox"
    )

    remDr$open()

    remDr$navigate("https://cdeee.gob.do/transparencia/informes-del-sector-energetico/")

    year <- remDr$findElement("xpath", '//*[@id="wpfb-cat-1740"]/span/a')

    year$clickElement()


    month <- remDr$findElement("xpath", '//*[@id="wpfb-cat-1869"]/span/a')

    month$clickElement()

    remDr$getPageSource() %>%
      .[[1]] %>%
      stringr::str_split("\n") %>%
      dplyr::bind_cols() %>%
      dplyr::filter(stringr::str_detect(...1, ".xlsx")) %>%
      .[[1]] %>%
      stringr::str_split(" ") %>%
      dplyr::bind_cols() %>%
      dplyr::filter(stringr::str_detect(...1, ".xlsx")) %>%
      tidyr::separate(...1, c("V1", "V2", "V3"), "\"") %>%
      dplyr::pull(V2) %>%
      .[[1]] -> indicador$original_url
    indicador$file_ext <- "xlsx"

    file <- downloader(indicador)
  } else {
    print("Local file...")
  }

  datos <- readxl::read_excel(file, sheet = 2, skip = 6, col_names = FALSE)

  datos[1,1] <- "9999"

  datos %>%
    tidyr::drop_na(...1) %>%
    t() %>%
    as.data.frame() %>%
    dplyr::mutate(
      V1 = as.numeric(V1)
    ) %>%
    tidyr::drop_na(V1) %>%
    dplyr::filter(V1 > 3000) %>%
    dplyr::mutate(V1 = as.Date(V1, origin = "1900-01-01")) %>%
    t() %>%
    as.data.frame() -> datos

  datos[1,1] <- "ede"

  datos %>%
    janitor::row_to_names(1) %>%
    dplyr::mutate(
      indicador = dplyr::case_when(!(ede %in% c("Edenorte", "Edesur", "Edeeste")) ~ ede),
      ede = dplyr::case_when(ede %in% c("Edenorte", "Edesur", "Edeeste") ~ ede, TRUE ~ "Total"),
      .before = "ede"
    ) %>%
    tidyr::fill(indicador) %>%
    tidyr::pivot_longer(-c(1:2), names_to = "date", values_to = "valor") %>%
    type.convert(as.is = TRUE)
}
