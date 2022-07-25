#' Descarga de archivos
#'
#' @param indicador el vector de datos correspondiente al indicador en cuestion.
#'     Vea la secci\\u00F3n \code{details} para m\\u00E1s detalles.
#'
#' @return la ruta de un archivo temporal que contiene el archivo descargado
#'
#' @details El indicador pasado a la funci\\u00F3n es un vector de datos que debe
#'   contener por lo menos:
#'
#'   \itemize{
#'     \item{Extensi\\u00F3n del archivo \code{(file_ext)}: uno de 'xlsx', 'xls',
#'      'csv', 'pdf', 'dir', 'sql'}
#'      \item{Enlace directo a la descarga \code{(original_url)}}
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#'    downloader(pib_gasto)
#' }
downloader <- function(indicador) {
  file_ext <- as.character(indicador[["file_ext"]])
  original_url <- as.character(indicador[["original_url"]])
  # return(class(original_url))
  # return(length(file_ext))
  if (file_ext == "html") {
    xml2::read_html(original_url)
  } else {
    httr::GET(
      original_url,
      httr::write_disk(tf <- tempfile(fileext = paste0(".", file_ext)))
    )
    tf
  }
}



nivelador <- function(tbl) {
  orden <- NULL
  nivel <- NULL
  nivel0 <- NULL
  tmax <- trunc(max(as.numeric(tbl$orden), na.rm = T))
  tbl <- tbl %>%
    dplyr::mutate(
      nivel0 = stringr::str_detect(orden, "\\-"),
      nivel = dplyr::if_else(
        tmax %in% c(99, 999, 9999) & tidyr::replace_na(as.numeric(orden), 0) >= tmax, tmax,
        stringr::str_count(orden, stringr::regex("\\.")) + 1
      ),
      nivel = dplyr::if_else(nivel0, -1 * nivel, nivel),
      nivel0 = NULL
    )
  tbl$orden <- c(1:nrow(tbl))
  tbl
}

#' Ordenes y niveles de los indicadores
#'
#'
#' @format [data.frame]
"nvl_balanza_pagos"


#' Ordenes y niveles de los indicadores
#'
#'
#' @format [data.frame]
"nvl_pib_gasto"


#' Ordenes y niveles de los indicadores
#'
#'
#' @format [data.frame]
"info"


#' Ordenes y niveles de los indicadores
#'
#'
#' @format [data.frame]
"nvl_balanza_pagos"


#' Ordenes y niveles de los indicadores
#'
#'
#' @format [data.frame]
"nvl_estado_operaciones_spnf"


#' Ordenes y niveles de los indicadores
#'
#'
#' @format [data.frame]
"nvl_exportaciones"


#' Ordenes y niveles de los indicadores
#'
#'
#' @format [data.frame]
"nvl_importaciones"


#' Ordenes y niveles de los indicadores
#'
#'
#' @format [data.frame]
"nvl_indicadores_bcrd"


#' Ordenes y niveles de los indicadores
#'
#'
#' @format [data.frame]
"nvl_indicadores_monetarios_bcrd"


#' Ordenes y niveles de los indicadores
#'
#'
#' @format [data.frame]
"nvl_indicadores_osd"


#' Ordenes y niveles de los indicadores
#'
#'
#' @format [data.frame]
"nvl_idsevr"


#' Ordenes y niveles de los indicadores
#'
#'
#' @format [data.frame]
"nvl_pib_gasto"


#' Ordenes y niveles de los indicadores
#'
#'
#' @format [data.frame]
"nvl_pib_origen"


#' Ordenes y niveles de los indicadores
#'
#'
#' @format [data.frame]
"nvl_resumen_indicadores_mercado_laboral"


#' Ordenes y niveles de los indicadores
#'
#'
#' @format [data.frame]
"nvl_saldo_deuda_spnf_xacre"


#' Tipo de cambio monedas convertibles
#'
#'
#' @format [data.frame]
"monedas_3d_codes"


#' Ejecutar funciones desde Python
#'
#'   Recibe los keys y valores de un diccionario python y lo convierte en un
#'   vector con nombres en R, que luego pasa a la funci\\u00F3n del indicador.
#'
#' @param keys [character]: keys del diccionario python que se utilizan como nombres del vector.
#' @param values values del diccionario python que se utilizan como el contenido del vector.
#'
#' @return Un vector con nombres que se pasa a la funci\\u00F3n del indicador.
#'
#' @export
from_python <- function(keys, values, ...) {
  indicador <- values
  names(indicador) <- keys
  res <- get(stringr::str_replace_all(indicador$id, "-", "_"))(indicador, ...)
  attr(res, "meta") <- NULL
  res
}



#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL


download_domar <- function(id) {
  tryCatch(
    {
      id2 <- stringr::str_replace_all(id, "_", "-")
      readr::read_csv(url(glue::glue("{info$domar_url}/app/datos/{id2}/d?out=csv&t={info$token}"))) %>%
        utils::type.convert(as.is = T)
    },
    error = function(e) {
      id2 <- stringr::str_replace_all(id, "-", "_")
      do.call(what = `::`, args = list("domar", id2))()
    }
  )
}


docker_start <- function() {
  docker_id <- system("docker run -d -p 4445:4444 selenium/standalone-firefox:2.53.0", intern = TRUE)[[1]]
  Sys.sleep(5)
  print(paste0("docker server started (", docker_id, ")"))
  docker_id
}

docker_stop <- function(docker_id) {
  system(paste0("docker stop ", docker_id))
  print(paste0("docker server stoped (", docker_id, ")"))
}


make_metadata <- function(indicador, preliminar, col_info, vistas, notas) {
  meta <- list()
  meta$id <- indicador[["id"]]
  #meta <- as.list(indicador)
  meta$preliminar <- preliminar
  meta$col_info <- col_info
  meta$vistas <- vistas
  meta$notas <- notas
  meta
}


set_metadata <- function(datos, meta){
  attr(datos, "metadata") <- meta
  datos
}


write_metadata <- function(indicador, metadata, .token = NULL) {
  id <- indicador["id"]
  if (!is.null(.token)) {
  tryCatch(
    {
      json_body <- jsonlite::toJSON(list(token = .token, meta = metadata), auto_unbox = TRUE)
      print(httr::POST(glue::glue("{domar::info$domar_url}/app/datos/metadata/{id}"), body = json_body, encode = "raw"))
    },
    error = function(e) {
      #print(e)
    }
  )
  }
}
