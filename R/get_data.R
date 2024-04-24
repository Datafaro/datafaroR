#' Obtener la última actualización de un conjunto de datos de Datafaro
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param code [character] Código del conjunto de datos
#' @param .token [character] Token de acceso a la API. Si no se ha
#' guardado un token, se debe especificar el token manualmente.
#' @param .cached [logical] Si se debe utilizar la caché. Por defecto es \code{TRUE}.
#'
#' @return Un datalight con la última actualización del conjunto de datos.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_data("NC_XDC")
#' }
get_data <- function(code, .token = auth(), .cached = TRUE) {
  .token # Esto asegura que el token se valide antes de realizar cualquier solicitud.
  metadata <- .fetch_metadata(code, .token)

  # Safe check if metadata is not NULL
  last_update <- if (!is.null(metadata)) metadata$last_update else NA

  # Now call fetch_data even if last_update is NA
  .fetch_data(code, last_update, .token, .cached)
}



.fetch_metadata <- function(code, .token) {
  tryCatch(
    {
      metadata_url <- glue::glue("{API_URL}/v1/data/dataset?src=datafaroR&slug={code}")
      metadata <- metadata_url %>%
        .make_request(.token) %>%
        httr2::resp_body_json()
      return(metadata$datasets[[1]])
    },
    error = function(e) {
      message("Failed to fetch metadata: ", e$message)
      return(invisible(NULL))
    }
  )
}


.fetch_data <- function(code, last_update, .token, .cached) {
  url_path <- glue::glue("v1/data/datalight/?src=datafaroR&slug={code}")
  pins_id <- stringr::str_replace_all(url_path, "[^[:alnum:]]", "-")
  url <- glue::glue("{API_URL}/{url_path}")

  if (is.null(last_update)) {
    tryCatch(
      {
        return(.data_request(url, pins_id, .token))
      },
      error = function(e) {
      }
    )
  }

  if (.cached && pins::pin_exists(.DatafaroBoard, pins_id)) {
    cached_metadata <- pins::pin_read(.DatafaroBoard, pins_id)$metadata
    if (is.null(last_update) || last_update == cached_metadata$last_update) {
      data_url <- glue::glue("{url}&cached={stringr::str_replace(cached_metadata$last_update, ' ', 'T')}")
      return(.cache_fetch(data_url, pins_id, .token))
    }
  }

  return(.data_request(url, pins_id, .token))
}


.cache_fetch <- function(data_url, pins_id, .token) {
  tryCatch(
    {
      data_url %>%
        .make_request(.token)
      as_datalight(pins::pin_read(.DatafaroBoard, pins_id))
    },
    error = function(e) {}
  )
}


.data_request <- function(url, pins_id, .token) {
  tryCatch(
    {
      .res <- url %>%
        .make_request(.token) %>%
        httr2::resp_body_json()

      dl <- .res$datalight
      if (!is.null(dl)) {
        dl$Cached <- Sys.Date()
        . <- suppressMessages(
          utils::capture.output(
            pins::pin_write(.DatafaroBoard, dl, pins_id, type = "json")
          )
        )
        dl$Cached <- NULL
        as_datalight(dl)
      }
    },
    error = function(e) {
      .handle_error(pins_id)
    }
  )
}

.handle_error <- function(pins_id) {
  if (pins::pin_exists(.DatafaroBoard, pins_id)) {
    message("Ocurri\u00f3 un error al intentar obtener los datos. Se utilizar\u00e1 la versi\u00f3n en cach\u00e9.")
    as_datalight(pins::pin_read(.DatafaroBoard, pins_id))
  } else {
    message("Ocurri\u00f3 un error al intentar obtener los datos. No se encontr\u00f3 una versi\u00f3n en cach\u00e9.")
    NULL
  }
}


.make_request <- function(url, .token) {
  paste0(url, "&sessionID=", .getid()) %>%
    httr2::request() %>%
    httr2::req_headers(Authorization = paste0("Token ", .token)) %>%
    httr2::req_perform()
}
