#' Obtener la última actualización de un conjunto de datos de Datafaro
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param .dataset [character] Código del conjunto de datos
#' @param .token [character] Token de acceso a la API. Por defecto se utiliza el
#' token guardado en la variable de entorno \code{DATAFARO_TOKEN}. Si no se ha
#' guardado un token, se debe especificar el token manualmente.
#'
#' @return Un data.frame con la última actualización del conjunto de datos.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   get_data("imae")
#' }
get_data <- function(.dataset, .token = Sys.getenv('DATAFARO_TOKEN')) {

  # TODO: Add a tc argument to download the dataset withouth change rates variables
  # Other for prop and so on
  if (inherits(.dataset, 'data.frame')){
    id <- .dataset$id
  } else {
    id <- .dataset
  }

  url <- glue::glue("{API_URL}/v1/update/?fmt=csv&id={id}")

  .res <- url %>%
    httr2::request() %>%
    httr2::req_headers(Authorization = paste0("Token ", .token)) %>%
    httr2::req_perform() %>%
    httr2::resp_body_raw() %>%
    readr::read_csv(show_col_types = F)

  attr(.res, 'metadata') <- get_metadata(id, .token)
  .res
}
