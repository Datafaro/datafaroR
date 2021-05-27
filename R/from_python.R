#' Ejecutar funciones desde Python
#'
#'   Recibe los keys y valores de un diccionario python y lo convierte en un
#'   vector con nombres en R, que luego pasa a la función del indicador.
#'
#' @param keys [character]: keys del diccionario python que se utilizan como nombres del vector.
#' @param values values del diccionario python que se utilizan como el contenido del vector.
#'
#' @return Un vector con nombres que se pasa a la función del indicador.
#'
#' @export
from_python <- function(keys, values, metadata = FALSE){
  indicador <- values
  names(indicador) <- keys
  #indicador <- type.convert(indicador)
  get(stringr::str_replace_all(indicador$id, "-", "_"))(indicador, metadata = metadata)
}
