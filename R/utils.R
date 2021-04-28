#' Descarga de archivos
#'
#' @param indicador el vector de datos correspondiente al indicador en cuestion.
#'     Vea la sección \code{details} para más detalles.
#'
#' @return la ruta de un archivo temporal que contiene el archivo descargado
#'
#' @details El indicador pasado a la función es un vector de datos que debe
#'   contener por lo menos:
#'
#'   \itemize{
#'     \item{Extensión del archivo \code{(file_ext)}: uno de 'xlsx', 'xls',
#'      'csv', 'pdf', 'dir', 'sql'}
#'      \item{Enlace directo a la descarga \code{(original_url)}}
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' downloader(pib_gasto)
#' }
downloader <- function(indicador) {
  file_ext = as.character(indicador[["file_ext"]])
  original_url <- as.character(indicador[["original_url"]])
  #return(class(original_url))
  #return(length(file_ext))
  if (file_ext == "dir") {
    xml2::read_html(original_url)
  } else {
    httr::GET(
      original_url,
      httr::write_disk(tf <- tempfile(fileext = paste0(".",file_ext)))
    )
    tf
  }
}




# #' Multiple variables to unique date variable
# #'
# #' @param data data.frame the target dataframe
# #' @param ano character the character name of variable corresponding to year
# #' @param mes character the character name of variable corresponding to month
# #' @param dia character the character name of variable corresponding to day
# #'
# #' @return data.frame a new dataframe with the compute variable
# #'
# #' @export
# #'
# #' @examples
# #' \dontrun{
# #' df <- to_date(df)
# #' }
# to_date <- function(data, ano = NULL, trim = NULL, mes = NULL, tipo_mes = "char", dia = NULL) {
#   names(data)[ano] <- "ano"
#
#   if (!is.null(dia)) {
#
#   } else if (!is.null(mes)) {
#     names(data)[mes] <- "mes"
#     if (tipo_mes == "num") {
#       data <- dplyr::mutate(data,
#         fecha = paste(ano, mes, "01", sep = "-")
#       )
#     } else {
#       data <- dplyr::mutate(data,
#         mes = stringr::str_trim(mes),
#         mes = stringr::str_sub(mes, 1, 3),
#         mes = stringr::str_to_title(mes),
#         mes = dplyr::case_when(
#           mes == "Ene" ~ "Jan",
#           mes == "Abr" ~ "Apr",
#           mes == "Ago" ~ "Aug",
#           mes == "Dic" ~ "Dec",
#           TRUE ~ mes
#         ),
#         fecha = zoo::as.Date(zoo::as.yearmon(paste(mes, ano)))
#       )
#     }
#     data[, mes] <- NULL
#     data[, ano] <- NULL
#     data <- dplyr::relocate(data, fecha)
#     data
#   }
# }





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




#' DOMAR indicators list
#'
#' @return data.frame corresponding the list of indicatos in domar list
#'
#'@details
#'
#'  Don't use if you don't have access to \code{dmr_list} file.
#'  See \url{https://docs.google.com/spreadsheets/d/1svANACeWShYC--wm7wwSKaUn_bF_xXuF81SnV8yMeb8/edit#gid=0}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dmr_list <- domar_list()
#' }
domar_list <- function() {
  googlesheets4::read_sheet('1svANACeWShYC--wm7wwSKaUn_bF_xXuF81SnV8yMeb8', 'Index')
}






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






#' Secure Google Sheet storage version
#'
#' \lifecycle{superseded}
#'
#' This is a wraper function to tryCatch for \code{\link{gsheet_storage}}.
#'   If error, then an email is send to specified email address.
#'
#' @param to_email email for send the error
#' @param creds_file_name  a file name pointer. See \code{\link[blastula]{creds_file}}
#' @param id See \code{\link{gsheet_storage}}
#' @param ... See \code{\link{gsheet_storage}}
#'
#' @examples
#' \dontrun{
#' secure_gss(email,
#'  'domar.email',
#'   'imae_month',
#'    kvars = c('fecha', 'serie', 'Indice'))
#' }
secure_gss <- function(to_email, creds_file_name, id, ...){
  tryCatch({
    gsheet_storage(id, ...)
  },
  error = function(e){
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
    Sys.sleep(100) # TODO. Evaluate if error is related with Google Sheet cuota limits.
  }
  )
}


#' Storage domar data in Google Sheets
#'
#' \lifecycle{superseded}
#'
#' @param id character ID of variable in the domar data list
#' @param kvars key variables for data comparison. See \code{Details}
#' @param bound numeric a number between 0 and 1. Default to 0.95.
#'   Indicate the min fraction of rows where the key variables need to match.
#'
#'@details
#'
#'  Don't use if you don't have access to \code{\link{domar_list}}.
#'
#'  \code{dmr_list} list of variables available in domar.
#'
#'  Through \code{id} is selected the current variable key info.
#'
#'  If \code{\link{Sys.time}} greather than \code{Próxima actualización}
#'  See \code{\link{domar_list}}
#'
#'  Through the \code{id}, the corresponding function is executed, returning the
#'   \code{newData}, besides the \code{oldData} is retrieve from Google Sheets.
#'
#'  If \code{\link{nrow}} of \code{oldData} is 0. Write \code{newData} to
#'   Google Sheet. The needed arguments are presents in indicator information.
#'
#'  Else both \code{newData} and \code{oldData} are subsetted by all \code{kvars}.
#'  Both dataset are merge by the first \code{n-1} \code{kvars}. The last
#'  variable in \code{kvars} is compared in both datasets.
#'
#'  \code{matchRows} The fraction of rows where the above comparison is \code{TRUE}.
#'  \code{matchNames} The fraction of \code{\link{names}} of \code{oldData}
#'  present in \code{\link{names}} of \code{newData}
#'
#'  If \code{matchNames} not iqual to 1 return an error. See \code{\link{stop}}.
#'
#'  Else if \code{matchRows} iqual to 1, that means both datasets are iquals,
#'  the data is not yet updated. Then \code{dmr_list} is updated.
#'  And next try is set to:
#'  \itemize{
#'    \item \code{\link{Sys.time}} plus 0.5 hours for daily frecuency.
#'    \item \code{\link{Sys.time}} plus 4 hours for monthly frecuency.
#'    \item \code{\link{Sys.time}} plus 20 hours for others frecuencies.
#'  }
#'
#'  Else if \code{matchRows} less than \code{bound} return an error.
#'  See \code{\link{stop}}.
#'
#'  Else \code{newData} is writed to Google Sheet. . The needed arguments are
#'   presents in indicator information. Then \code{dmr_list} is updated.
#'  And next try is set to:
#'  \itemize{
#'    \item \code{\link{Sys.time}} plus 23 hours for daily frecuency.
#'    \item \code{\link{Sys.time}} plus 30 days for monthly frecuency.
#'    \item \code{\link{Sys.time}} plus 92 days for quarter frecuency.
#'    \item \code{\link{Sys.time}} plus 365 days for annual frecuency.
#'  }
#'
#' @examples
#' \dontrun{
#' gsheet_storage(id = 'imae_month', kvars = c('fecha', 'serie', 'Indice'))
#' }
gsheet_storage <- function(id, kvars, bound = 0.95) {
  dmr_list <- domar_list()
  indicador <- dmr_list[dmr_list$ID == id,]

  nextTry <- indicador[,"Pr\u00F3xima actualizaci\u00F3n"][[1]]
  frecuency <- indicador[,"Frecuencia"][[1]]

  if(Sys.time() > nextTry){
    newData <- get(id)(indicador$`Enlace original`)
    ss <- indicador$Metadatos
    oldData <- googlesheets4::read_sheet(ss, id)
    if(nrow(oldData) == 0){
      googlesheets4::sheet_write(data = newData, ss = ss, sheet = id)
    } else {
      combData <- dplyr::left_join(
        dplyr::select(newData, tidyselect::all_of(kvars)),
        dplyr::select(oldData, tidyselect::all_of(kvars)),
        by = kvars[1:(length(kvars)-1)]
      )
      combData$matches <- combData[,paste0(kvars[length(kvars)], '.x')] == combData[,paste0(kvars[length(kvars)], '.y')]
      matchRows <- sum(tidyr::replace_na(combData$matches, 0))/nrow(combData)
      matchNames <- sum(names(oldData) %in% names(newData))/ncol(newData)
      if(matchNames != 1){
        stop('colnames not match')
      } else if(matchRows == 1){
        dmr_list[dmr_list$ID == id, "Pr\u00F3xima actualizaci\u00F3n"] <- dplyr::case_when(
          frecuency == 'Diaria'  ~ Sys.time()+1800,
          frecuency == 'Mensual' ~ Sys.time()+14400,
          TRUE ~ Sys.time()+72000
        )
        googlesheets4::sheet_write(dmr_list, '1svANACeWShYC--wm7wwSKaUn_bF_xXuF81SnV8yMeb8', 'Index')
      } else if(matchRows < bound){
        stop('too much changes')
      } else {
        googlesheets4::sheet_write(data = newData, ss = ss, sheet = id)
        dmr_list[dmr_list$ID == id, "Pr\u00F3xima actualizaci\u00F3n"] <- dplyr::case_when(
          frecuency == 'Diaria'  ~ Sys.time()+82800,
          frecuency == 'Mensual' ~ Sys.time()+2.592e+6,
          frecuency == 'Trimestral' ~ Sys.time()+7.949e+6,
          frecuency == 'Anual' ~ Sys.time()+3.145e+7
        )
        googlesheets4::sheet_write(dmr_list, '1svANACeWShYC--wm7wwSKaUn_bF_xXuF81SnV8yMeb8', 'Index')
      }
    }
  }
}









validate_names <- function(newData, oldData){
  newName <- names(oldData) %in% names(newData)
  oldName <- names(newData) %in% names(oldData)
  if(sum(newName) != ncol(oldData)){
    stop(
      paste0(
        paste(names(oldData)[!newName], collapse = ', '),
        ' no encontrado en nueva data.',
        '


        ',
        'newData: ', paste(names(newData), collapse = ', '),
        '
        ',
        'oldData: ', paste(names(oldData), collapse = ', ')
      )
    )
  } else if(sum(oldName) != ncol(newData)){
    stop(
      paste0(
        paste(names(newData)[!oldName], collapse = ', '),
        ' no encontrado en data anterior.',
        '


        ',
        'newData: ', paste(names(newData), collapse = ', '),
        '
        ',
        'oldData: ', paste(names(oldData), collapse = ', ')
      )
    )
  }
}

validate_changes <- function(newData, oldData, indicator, kvars){
  kvar <- kvars[length(kvars)]
  max_chgs <- indicator$max_changes/100
  combData <- dplyr::left_join(
    dplyr::select(newData, tidyselect::all_of(kvars)),
    dplyr::select(oldData, tidyselect::all_of(kvars)),
    by = kvars[1:(length(kvars)-1)]
  )
  if(nrow(combData) < nrow(oldData)){
    stop(
      paste0(
        'La combinaci\u00F3n result\u00F3 en una menor cantidad de filas que la data anterior: ',
        '

        ',
        'combData: ', nrow(combData),
        '
        ',
        ' oldData: ', nrow(oldData)
      )
    )
  }
  x <- 0.000000000000000000000000000000000000123456789
  combData <- utils::type.convert(combData)
  combData[,paste0(kvar, '.y')][is.na(combData[,paste0(kvar, '.y')]) & !is.na(combData[,paste0(kvar, '.x')])] <- x
  combData$changes <- combData[,paste0(kvar, '.x')] != combData[,paste0(kvar, '.y')]
  trues = sum(combData$changes, na.rm = T)
  falses = sum(!combData$changes, na.rm = T)
  total = trues + falses
  if(trues/total > max_chgs){
    # Si eliminan lentamente los registros más viejes lentamente,
    # nosotros trambién los iremos perdiendo. Siempre y cuando inserten más
    # registros que los que eliminen, de lo contrario el indicador no se actualizará
    # o dará error.
    stop(paste0('Demasiado cambios: ', trues, '(', round(trues/total*100, digits = 2), '%) filas diferentes.
                M\u00E1ximo permitido: ', round(max_chgs*100, 1), '%.'))
  }
  trues
}

validate_kvars <- function(oldData, kvars){
  # Validar que las primeras n-1 k vars identifican 1 a 1 las filas,
  # en ese caso seleccionando solo esas variables unique debería
  # resultar en el total de filas de la data.
  matches <- sum(kvars %in% names(oldData))
  if(matches != length(kvars)){
    stop(
      paste0(
        'kvars incorrectas.
        ',
        'Actual: ', paste(kvars, collapse = ', '),
        '
        Disponibles: ', paste(names(oldData), collapse = ', '),'
        Incorrectas: ', paste(kvars[!(kvars %in% names(oldData))])
      )
    )
  }
}

validate_max_chgs <- function(indicator){
  max_chgs <- indicator$max_changes
  if(any(max_chgs < 0, max_chgs > 100)){
    stop(
      paste0('M\u00E1ximo establecido fuera de los l\u00E1mites permitidos: ', max_chgs, '.
             Debe estar entre 0 y 100.')
    )
  }
}

validate_try <- function(db_conn, indicator){
  if(as.numeric(indicator$pos_count) > 29){
    # Estado del indicador
    DBI::dbExecute(db_conn, paste0("UPDATE api_domar_list SET indicator_state ='1' WHERE id='",indicator$id, "';"))
    stop(
      paste0(
        indicator$id, ': marcado como Inactivo tras ', indicator$pos_count, ' posposiciones consecutivas.'
      )
    )
  } else if(as.numeric(indicator$error_count) > 2){
    # Estado del indicador
    DBI::dbExecute(db_conn, paste0("UPDATE api_domar_list SET indicator_state ='1' WHERE id='",indicator$id, "';"))
    stop(
      paste0(
        indicator$id, ': marcado como Inactivo tras ', indicator$error_count + 1, ' errores consecutivos.'
      )
    )
  }
}

#' Valida los datos antes de escribir a la base de datos
#'
#' @param newData la data obtenida con la función correspondiente
#' @param indicator current indicator
#' @param oldData la data que se encuentra actualmente en la base de datos
#' @param db_conn conexion a base de datos
#'
#' @return número de filas distintas
validate_data <- function(db_conn, newData, oldData, indicator) {
  kvars <- strsplit(gsub(" ", "", indicator$kvars, fixed = TRUE), split =',')[[1]]
  validate_try(db_conn, indicator)
  validate_kvars(oldData, kvars)
  validate_max_chgs(indicator)
  validate_names(newData, oldData)
  validate_changes(newData, oldData, indicator, kvars)
}
