#' db_storage a prueba de errores
#'
#' Esta función ejecuta \code{\link{db_storage}} en un tryCatch de forma que si
#'  sucede algún error se envía un email a la dirección indicada
#'
#' @param db_conn vea \code{\link{db_storage}}
#' @param id vea \code{\link{db_storage}}
#' @param to_email dirección a la que enviar el email si un error ocurre.
#' @param creds_file_name un archivo con las credenciales necesarias para enviar emails
#'  vea vea \url{https://rstudio.github.io/blastula/reference/create_smtp_creds_file.html}
#'
#' @return un mensaje indicando el resultado de la operación
#' @export
#'
#' @examples
#' \dontrun{
#'  secure_dbs(db_conn, 'imae_mensual', 'domar@@gmail.com', 'creds_file_name')
#' }
secure_dbs <- function(db_conn, id, to_email, creds_file_name){
  tryCatch({
    db_storage(db_conn, id)
  },
  error = function(e){
    indicadores <- DBI::dbReadTable(db_conn, 'api_domar_list')
    indicator <- indicadores[indicadores$id == id,]
    if(interactive()){
      # Último resultados
      DBI::dbExecute(db_conn, paste0("UPDATE api_domar_list SET last_result ='3' WHERE id='",id, "';"))
      # Posposiciones consecutivas
      DBI::dbExecute(db_conn, paste0("UPDATE api_domar_list SET pos_count=0 WHERE id='",id, "';"))
      # Errores consecutivos
      DBI::dbExecute(db_conn, paste0("UPDATE api_domar_list SET error_count=", as.numeric(indicator$error_count)+1," WHERE id='",id, "';"))

      print(e)

    } else {
      # Último resultados
      DBI::dbExecute(db_conn, paste0("UPDATE api_domar_list SET last_result ='3' WHERE id='",id, "';"))
      # Posposiciones consecutivas
      DBI::dbExecute(db_conn, paste0("UPDATE api_domar_list SET pos_count=0 WHERE id='",id, "';"))
      # Errores consecutivos
      DBI::dbExecute(db_conn, paste0("UPDATE api_domar_list SET error_count=", as.numeric(indicator$error_count)+1," WHERE id='",id, "';"))

      if(as.numeric(indicator$error_count) %in% c(0, 3)){
        email <-
          blastula::compose_email(
            body = blastula::md(e),
            footer = blastula::md(
              c(
                "domar"
              )
            )
          )
        blastula::smtp_send(
          email,
          to = to_email,
          from = 'domar.email@gmail.com',
          subject = paste0("domar error tracking: ", id),
          credentials = blastula::creds_file(creds_file_name)
        )
      }
    }
  }
  )
}
