#' Storage domar data in Google Sheets
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
#' @export
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
