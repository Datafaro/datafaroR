#' Actualiza la fecha de la próxima actualización
#'
#' Basado en el resultado de la actualización y la periodicidad del indicador,
#'  actualiza la fecha de la próxima actualización.
#'  Esta función se utiliza dentro de \code{\link{db_storage}}
#'
#' @param db_conn conexión a la base de datos donde se almacena el listado de
#'  indicadores
#' @param indicator el registro de la tabla de indicadores correspondiente,
#'  al ID del indicador en cuestión
#' @param result un valor 0 o 1 que indica si la actualización fué correcta o no
#'
#'
#' @examples
#' \dontrun{
#' nextTry(db_conn, indicator, 1)
#' }
nextTry <- function(db_conn, indicator, result) {
  statement <- function(action, indicator){
    paste0("UPDATE api_domar_list SET next_update ='",action, "' WHERE id='",indicator$id, "';")
  }
  if(result){
    action <- dplyr::case_when(
      indicator$frequency ==  1 ~ Sys.time(),
      indicator$frequency ==  2 ~ Sys.time() +              23*60*60,
      indicator$frequency ==  3 ~ Sys.time() +   1*24*60*60+23*60*60,
      indicator$frequency ==  4 ~ Sys.time() +   6*24*60*60+23*60*60,
      indicator$frequency ==  5 ~ Sys.time() +  14*24*60*60+23*60*60,
      indicator$frequency ==  6 ~ Sys.time() +  29*24*60*60+23*60*60,
      indicator$frequency ==  7 ~ Sys.time() +  59*24*60*60+23*60*60,
      indicator$frequency ==  8 ~ Sys.time() +  89*24*60*60+23*60*60,
      indicator$frequency ==  9 ~ Sys.time() + 119*24*60*60+23*60*60,
      indicator$frequency == 10 ~ Sys.time() + 179*24*60*60+23*60*60,
      indicator$frequency == 11 ~ Sys.time() + 364*24*60*60+23*60*60
    )
    # Próxima actualización
    DBI::dbExecute(db_conn, statement(action, indicator))
  } else if(!result){
    action <- dplyr::case_when(
      indicator$frequency <= 3 ~ Sys.time(),
      indicator$frequency <= 6 ~ Sys.time() + 6*60*60,
      indicator$frequency >= 7 ~ Sys.time() + 23*60*60
    )
    DBI::dbExecute(db_conn, statement(action, indicator))
  }
}

#' Almacena los datos en una base de datos
#'
#' @param db_conn una conexión a la base de datos donde se almacenarán los datos
#'  y donde está disponible la lista de indicadores con toda la información necesaria
#' @param id el ID del indicador, que debe coincidir con:
#'   \itemize{
#'     \item{el ID asignado en el listado de indicadores}
#'     \item{el nombre de la función que genera los datos}
#'   }
#'
#' @return un mensaje que indica el resultado de la operación
#' @export
#'
#' @examples
#' \dontrun{
#'db_storage(db_conn, 'imae_mensual')
#' }
db_storage <- function(db_conn, id){
  message(id)
  indicadores <- DBI::dbReadTable(db_conn, 'api_domar_list')
  indicator <- indicadores[indicadores$id == id,]
  max_chgs <- indicator$max_changes/100
  if(indicator$indicator_state == 2){
    if(Sys.time() > indicator$next_update){
      if(indicator$tipo_indicador == '2'){
        newData <-get(id)(db_conn, indicator)
      } else {
        newData <-get(id)(indicator)
      }
      if(DBI::dbExistsTable(db_conn, id)){
        oldData <- DBI::dbReadTable(db_conn, id)
        if(validate_data(db_conn, newData, oldData, indicator)){
          DBI::dbWriteTable(db_conn, id, newData, overwrite = TRUE)
          nextTry(db_conn, indicator, 1)
          # Último resultados
          DBI::dbExecute(db_conn, paste0("UPDATE api_domar_list SET last_result ='1' WHERE id='",id, "';"))
          # Última actualización
          DBI::dbExecute(db_conn, paste0("UPDATE api_domar_list SET last_update ='",Sys.time(), "' WHERE id='",id, "';"))
          # Posposiciones consecutivas
          DBI::dbExecute(db_conn, paste0("UPDATE api_domar_list SET pos_count=0 WHERE id='",id, "';"))
          # Errores consecutivos
          DBI::dbExecute(db_conn, paste0("UPDATE api_domar_list SET error_count=0 WHERE id='",id, "';"))

          message('Indicador actualizado satisfactoriamente.')
        } else {
          nextTry(db_conn, indicator, 0)

          # Último resultados
          DBI::dbExecute(db_conn, paste0("UPDATE api_domar_list SET last_result ='2' WHERE id='",id, "';"))
          # Posposiciones consecutivas
          DBI::dbExecute(db_conn, paste0("UPDATE api_domar_list SET pos_count=", as.numeric(indicator$pos_count)+1," WHERE id='",id, "';"))
          # Errores consecutivos
          DBI::dbExecute(db_conn, paste0("UPDATE api_domar_list SET error_count=0 WHERE id='",id, "';"))

          message('No datos nuevos disponible.')
        }
      } else {
        DBI::dbWriteTable(db_conn, id, newData, overwrite = TRUE)

        nextTry(db_conn, indicator, 1)

        # Último resultados
        DBI::dbExecute(db_conn, paste0("UPDATE api_domar_list SET last_result ='1' WHERE id='",id, "';"))
        # Última actualización
        DBI::dbExecute(db_conn, paste0("UPDATE api_domar_list SET last_update ='",Sys.time(), "' WHERE id='",id, "';"))
        # Posposiciones consecutivas
        DBI::dbExecute(db_conn, paste0("UPDATE api_domar_list SET pos_count=0 WHERE id='",id, "';"))
        # Errores consecutivos
        DBI::dbExecute(db_conn, paste0("UPDATE api_domar_list SET error_count=0 WHERE id='",id, "';"))

        message('Indicador creado satisfactoriamente.')
      }
    } else {
      message(paste0('Pr\u00F3xima actualizaci\u00F3n: ', indicator$next_update))
    }
  } else {
    paste0('Inactivo (', id, ')')
  }
}
