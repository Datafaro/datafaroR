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
