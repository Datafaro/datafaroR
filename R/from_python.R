from_python <- function(keys, values){
  indicador <- values
  names(indicador) <- keys
  #indicador <- type.convert(indicador)
  get(stringr::str_replace_all(indicador$id, "-", "_"))(indicador)
}
