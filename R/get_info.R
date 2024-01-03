#' Obtener una lista de los indicadores disponibles de DataFaro
#'
#'   `r lifecycle::badge("experimental")`
#'
#' @param indicator [character] Código del indicador (opcional)
#' @param area [character] Código del área (opcional)
#' @param institution [character] Código de la institución (opcional)
#' @param freq [character] Código de la frecuencia
#' \itemize{
#' \item \code{H} Horaria
#' \item \code{D} Diaria
#' \item \code{W} Semanal
#' \item \code{M} Mensual
#' \item \code{Y} Anual
#' }
#' @param .token [character] Token de acceso a la API. Por defecto se utiliza el
#' token guardado en la variable de entorno \code{DATAFARO_TOKEN}. Si no se ha
#' guardado un token, se debe especificar el token manualmente.
#'
#' @return Un data.frame con la lista de indicadores. El data.frame tiene
#' las siguientes columnas:
#' \itemize{
#' \item \code{id} Código del indicador
#' \item \code{name} Nombre del indicador
#' \item \code{description} Descripción del indicador
#' \item \code{area} Código del área
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' list_indicators()
#'
#' list_indicators(indicator = "imae")
#'
#' list_indicators(area = "ECONREAL")
#' }
list_indicators <- function(
    indicator = NULL,
    area = NULL,
    institution = NULL,
    freq = NULL,
    .token = Sys.getenv('DATAFARO_TOKEN')
    ) {
  . <- NULL
  url <- glue::glue("{API_URL}/v1/")
  params <- character()
  path <- "indicator"

  if (!is.null(indicator)) {
    params <- c(params, paste0("indicator=", indicator))
  }

  if (!is.null(area)) {
    params <- c(params, paste0("area=", area))
  }

  if (!is.null(institution)) {
    params <- c(params, paste0("institution=", institution))
  }

  if (!is.null(freq)) {
    params <- c(params, paste0("freq=", freq))
  }

  tryCatch(
    {
      .res <- httr2::request(paste0(url, path, "/?", paste0(params, collapse = "&"))) %>%
        httr2::req_headers(Authorization = paste0("Token ", .token)) %>%
        httr2::req_perform() %>%
        httr2::resp_body_json()

      if ("id" %in% names(.res)) {
        .res <- .res %>%
          as.data.frame()
      } else {
        .res <- .res %>%
          do.call(rbind, .) %>%
          as.data.frame()
      }

      return(.res)
    },
    error = function(e) {
      httr2::last_response() %>%
        httr2::resp_body_json() %>%
        stop(call. = FALSE)
    }
  )
}


#' Obtener una lista de los conjuntos de datos disponibles de DataFaro
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param dataset [character] Código del conjunto de datos (opcional)
#' @param indicator [character] Código del indicador (opcional)
#' @param area [character] Código del área (opcional)
#' @param institution [character] Código de la institución (opcional)
#' @param freq [character] Código de la frecuencia
#' \itemize{
#' \item \code{H} Horaria
#' \item \code{D} Diaria
#' \item \code{W} Semanal
#' \item \code{M} Mensual
#' \item \code{Y} Anual
#' }
#' @param .token [character] Token de acceso a la API. Por defecto se utiliza el
#' token guardado en la variable de entorno \code{DATAFARO_TOKEN}. Si no se ha
#' guardado un token, se debe especificar el token manualmente.
#'
#' @return Un data.frame con la lista de conjuntos de datos. El data.frame tiene
#' las siguientes columnas:
#' \itemize{
#' \item \code{id} Código del conjunto de datos
#' \item \code{indicator} Código del indicador
#' \item \code{name} Nombre del conjunto de datos
#' \item \code{institution} Código de la institución
#' \item \code{description} Descripción del conjunto de datos
#' \item \code{status} Estado del conjunto de datos
#' \itemize{
#' \item \code{A} Activo
#' \item \code{I} Inactivo
#' \item \code{D} Descontinuado
#' \item \code{R} Reemplazado
#' }
#' \item \code{freq} Código de la frecuencia
#' \itemize{
#' \item \code{H} Horaria
#' \item \code{D} Diaria
#' \item \code{W} Semanal
#' \item \code{M} Mensual
#' \item \code{Y} Anual
#' }
#' \item \code{interval} El número de veces por frecuencia que el conjunto de
#' datos es actualizado
#' \item \code{last_update} Fecha de la última actualización
#' \item \code{next_update} Fecha de la próxima actualización
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' list_datasets()
#'
#' list_datasets(dataset = "imae")
#'
#' list_datasets(area = "ECONREAL")
#' }
list_datasets <- function(
    dataset = NULL,
    indicator = NULL,
    area = NULL,
    institution = NULL,
    freq = NULL,
    .token = Sys.getenv('DATAFARO_TOKEN')
) {
  . <- NULL
  url <- glue::glue("{API_URL}/v1/")
  params <- character()
  path <- "dataset"

  if (!is.null(dataset)) {
    params <- c(params, paste0("dataset=", dataset))
  }

  if (!is.null(indicator)) {
    params <- c(params, paste0("indicator=", indicator))
  }

  if (!is.null(area)) {
    params <- c(params, paste0("area=", area))
  }

  if (!is.null(institution)) {
    params <- c(params, paste0("institution=", institution))
  }

  if (!is.null(freq)) {
    params <- c(params, paste0("freq=", freq))
  }

  tryCatch(
    {
      .res <- httr2::request(paste0(url, path, "/?", paste0(params, collapse = "&"))) %>%
        httr2::req_headers(Authorization = paste0("Token ", .token)) %>%
        httr2::req_perform() %>%
        httr2::resp_body_json()

      if ("id" %in% names(.res)) {
        .res <- .res %>%
          as.data.frame()
      } else {
        .res <- .res %>%
          do.call(rbind, .) %>%
          as.data.frame()
      }

      return(.res)
    },
    error = function(e) {
      httr2::last_response() %>%
        httr2::resp_body_json() %>%
        stop(call. = FALSE)
    }
  )
}






#' Obtener una lista de las áreas disponibles de DataFaro
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param .token [character] Token de acceso a la API. Por defecto se utiliza el
#' token guardado en la variable de entorno \code{DATAFARO_TOKEN}. Si no se ha
#' guardado un token, se debe especificar el token manualmente.
#'
#' @return Un data.frame con la lista de áreas. El data.frame tiene
#' las siguientes columnas:
#' \itemize{
#' \item \code{code} Código del área
#' \item \code{name} Nombre del área
#' \item \code{description} Descripción del área
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' list_areas()
#' }
list_areas <- function(.token = Sys.getenv('DATAFARO_TOKEN')) {
  . <- NULL
  url <- glue::glue("{API_URL}/v1/area")

  httr2::request(url) %>%
    httr2::req_headers(Authorization = paste0("Token ", .token)) %>%
    httr2::req_perform() %>%
    httr2::resp_body_json() %>%
    do.call(rbind, .) %>%
    as.data.frame()
}





#' Obtener una lista de las instituciones disponibles de DataFaro
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param .token [character] Token de acceso a la API. Por defecto se utiliza el
#' token guardado en la variable de entorno \code{DATAFARO_TOKEN}. Si no se ha
#' guardado un token, se debe especificar el token manualmente.
#'
#' @return Un data.frame con la lista de instituciones. El data.frame tiene
#' las siguientes columnas:
#' \itemize{
#' \item \code{code} Código de la institución
#' \item \code{name} Nombre de la institución
#' \item \code{description} Descripción de la institución
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' list_institutions()
#' }
list_institutions <- function(.token = Sys.getenv('DATAFARO_TOKEN')) {
  . <- NULL
  url <- glue::glue("{API_URL}/v1/institution")

  httr2::request(url) %>%
    httr2::req_headers(Authorization = paste0("Token ", .token)) %>%
    httr2::req_perform() %>%
    httr2::resp_body_json() %>%
    do.call(rbind, .) %>%
    as.data.frame()
}


#' Obtener los metadatos de un conjunto de datos
#'
#' `r lifecycle::badge("experimental")`
#'
#' Esta función se utiliza junto con \code{\link{get_data}} para obtener
#' los metadatos como un atributo del marco de datos. Por lo tanto, raramente necesitarás
#' usar esta función directamente.
#'
#' @param id [character] Código del conjunto de datos
#' @param .token [character] Token de acceso a la API. Por defecto se utiliza el
#' token guardado en la variable de entorno \code{DATAFARO_TOKEN}. Si no se ha
#' guardado un token, se debe especificar el token manualmente.
#'
#' @return
#' Una lista con los metadatos del conjunto de datos. La lista tiene los siguientes
#' elementos:
#' \itemize{
#' \item \code{metadata} Una sublista con los metadatos de cada columna. El nombre
#' del elemento es el nombre de la columna. La sublista tiene los siguientes elementos:
#' \itemize{
#' \item \code{name} Nombre de la columna bien escrito
#' \item \code{dtype} Tipo de dato. Esto también se utiliza para el formato de datos:
#' \itemize{
#' \item \code{f2} Número flotante con 2 decimales
#' }
#' }
#' \item \code{notes} Un vector de caracteres con las notas del conjunto de datos.
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_metadata("imae")
#' }
get_metadata <- function(id, .token = Sys.getenv('DATAFARO_TOKEN')) {
  url <- glue::glue("{API_URL}/v1/metadata?id={id}")

  httr2::request(url) %>%
    httr2::req_headers(Authorization = paste0("Token ", .token)) %>%
    httr2::req_perform() %>%
    httr2::resp_body_json()
}
