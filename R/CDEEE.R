# Informe de desempe\\u00F1o ----


#'  Informe de desempe\\u00F1o del sector energ\\u00E9tico
#'
#'  \lifecycle{experimental}
#'
#' @param indicador Vea \code{\link{downloader}}
#'
#' @return [data.frame]: los datos del indicador en forma tabular
#'
#' @examples
#' \dontrun{
#'   informe_desempeno_sector_energetico()
#' }
informe_desempeno_sector_energetico <- function(indicador = NULL) {
  . <- NULL
  if (is.null(indicador)) {
    indicador <- c(
      original_url = "https://cdeee.gob.do/transparencia/informes-del-sector-energetico/",
      file_ext = "html",
      max_changes = 38 * 2
    )
  }

  file <- "/mnt/c/Users/drdsd/Downloads/Informe-de-Desempeno-Junio-2021-Anexos._.xlsx"
  if (!file.exists(file)) {

    did <- docker_start()

    remDr <- RSelenium::remoteDriver(
      remoteServerAddr = "localhost",
      port = 4445L,
      browserName = "firefox"
    )

    remDr$open()

    remDr$navigate("https://cdeee.gob.do/transparencia/informes-del-sector-energetico/")

    page <- remDr$getPageSource()

    page[[1]] %>%
      rvest::read_html() %>%
      rvest::html_element(xpath = '//*[@id="collapseOne"]') %>%
      xml2::xml_child(., 1) %>%
      xml2::xml_child(., 1) %>%
      xml2::xml_child(., 1) %>%
      xml2::xml_attrs(.) %>%
      .[["id"]] -> year_id

    year <- remDr$findElement(using = "xpath", glue::glue('//*[@id="{year_id}"]/span/a'))

    year$clickElement()

    Sys.sleep(5)

    page <- remDr$getPageSource()

    page[[1]] %>%
      rvest::read_html() %>%
      rvest::html_element(xpath = '//*[@id="collapseOne"]') %>%
      xml2::xml_child(., 1) %>%
      xml2::xml_child(., 1) %>%
      xml2::xml_child(., 1) %>%
      xml2::xml_child(., 3) %>%
      xml2::xml_child(., 1) %>%
      xml2::xml_child(., 2) %>%
      xml2::xml_child(., 2) %>%
      xml2::xml_attrs(.) %>%
      .[["href"]] %>%
      stringr::str_split("#") %>%
      .[[1]] %>%
      .[[2]] -> month_id

    month <- remDr$findElement(using = "xpath", glue::glue('//*[@id="{month_id}"]/span/a'))

    month$clickElement()

    Sys.sleep(5)

    page <- remDr$getPageSource()

    page[[1]] %>%
      rvest::read_html() %>%
      rvest::html_elements("a") %>%
      rvest::html_attr("href") %>%
      as.data.frame() %>%
      dplyr::filter(stringr::str_detect(`.`, "xls")) %>%
      .[[1]] -> indicador[["original_url"]]

    if(stringr::str_detect(indicador[["original_url"]], "xlsx")){
      indicador[["file_ext"]] <- "xlsx"
    } else if(stringr::str_detect(indicador[["original_url"]], "xls")){
      indicador[["file_ext"]] <- "xls"
    }

    remDr$close()

    docker_stop(did)

    file <- downloader(indicador)

  } else {
    print("Local file...")
  }

  file
}


#'  Informe de desempe\\u00F1o del sector energ\\u00E9tico: variables relevantes
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
  ...2 <- NULL
  V1 <- NULL
  variable <- NULL
  orden <- NULL
  nivel <- NULL
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

  datos <- readxl::read_excel(informe_desempeno_sector_energetico(indicador), skip = 6, col_names = FALSE)

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
    dplyr::bind_cols(domar::nvl_idsevr %>% dplyr::select(-variable)) %>%
    dplyr::relocate(orden, nivel) %>%
    tidyr::pivot_longer(-c(1:3), names_to = "date", values_to = "valor") %>%
    utils::type.convert(as.is = TRUE)
}



#'  Informe de desempe\\u00F1o del sector energ\\u00E9tico: EDE's
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
  ...1 <- NULL
  V1 <- NULL
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

  datos <- readxl::read_excel(informe_desempeno_sector_energetico(indicador), sheet = 2, skip = 6, col_names = FALSE)

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
    utils::type.convert(as.is = TRUE)
}
