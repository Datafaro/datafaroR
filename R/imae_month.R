#' Monthly IMAE
#'
#' @param url the url of the original file in \code{dmr_list}. See \code{\link{domar_list}}
#'
#' @return data.frame data of monthly IMAE
#'
#' @export
#'
#' @details
#'   \code{kvars}
#'   \itemize{
#'     \item \code{fecha}
#'     \item \code{serie}
#'     \item \code{Indice}
#'   }
#'
#' @examples
#' \dontrun{
#' imae <- imae_month()
#' }
imae_month <- function(url) {
  imae <- readxl::read_excel(
    downloader(url, 'xlsx'),
    skip = 5)
  # Serie original
  imaeso <- imae[,1:6]
  imaeso <- imaeso[-1,]
  imaeso[1,1] <- 'ano'
  imaeso[1,2] <- 'mes'
  imaeso[1,3] <- 'Indice'
  names(imaeso) <- imaeso[1,]
  imaeso <- imaeso[-1,]
  names(imaeso)[names(imaeso) == 'Interanual'] <- 'Var_interanual'
  names(imaeso)[names(imaeso) == 'Acumulada'] <- 'Var_acumulada'
  names(imaeso)[names(imaeso) == 'Promedio 12 meses'] <- 'Var_promedio_12_meses'
  imaeso <- imaeso[!is.na(imaeso$Indice),]
  imaeso$ano <- stringr::str_remove(imaeso$ano, 'Promedio ')
  imaeso <- tidyr::fill(imaeso, ano, .direction = 'up')
  imaeso <- tidyr::fill(imaeso, ano)
  imaeso <- dplyr::filter(imaeso, !is.na(mes))
  imaeso <- to_date(imaeso)
  imaeso$serie <- 'Serie original'

  # Serie desestacionalizada
  imaesd <- imae[,c(1:2, 7:11)]
  imaesd <- imaesd[-1,]
  imaesd[1,1] <- 'ano'
  imaesd[1,2] <- 'mes'
  imaesd[1,3] <- 'Indice'
  names(imaesd) <- imaesd[1,]
  imaesd <- imaesd[-1,]
  names(imaesd)[names(imaesd) == 'Respecto al per\u00EDodo anterior'] <- 'Var_periodo_anterior'
  names(imaesd)[names(imaesd) == 'Interanual'] <- 'Var_interanual'
  names(imaesd)[names(imaesd) == 'Acumulada'] <- 'Var_acumulada'
  names(imaesd)[names(imaesd) == 'Promedio 12 meses'] <- 'Var_promedio_12_meses'
  imaesd <- dplyr::mutate(imaesd,
                          ano = stringr::str_remove(ano, 'Promedio '))
  imaesd <- imaesd[!is.na(imaesd$Indice),]
  imaesd <- tidyr::fill(imaesd, ano, .direction = 'up')
  imaesd <- tidyr::fill(imaesd, ano)
  imaesd <- imaesd[!is.na(imaesd$mes),]
  imaesd <- to_date(imaesd)
  imaesd$serie <- 'Serie desestacionalizada'

  # Serie tendencia-ciclo
  imaest <- imae[,c(1:2, 12:16)]
  imaest <- imaest[-1,]
  imaest[1,1] <- 'ano'
  imaest[1,2] <- 'mes'
  imaest[1,3] <- 'Indice'
  names(imaest) <- imaest[1,]
  imaest <- imaest[-1,]
  names(imaest)[names(imaest) == 'Respecto al per\u00EDodo anterior'] <- 'Var_periodo_anterior'
  names(imaest)[names(imaest) == 'Interanual'] <- 'Var_interanual'
  names(imaest)[names(imaest) == 'Acumulada'] <- 'Var_acumulada'
  names(imaest)[names(imaest) == 'Promedio 12 meses'] <- 'Var_promedio_12_meses'
  imaest <- imaest[!is.na(imaest$Indice),]
  imaest <- dplyr::mutate(imaest,
                          ano = stringr::str_remove(ano, 'Promedio '))
  imaest <- tidyr::fill(imaest, ano, .direction = 'up')
  imaest <- tidyr::fill(imaest, ano)
  imaest <- imaest[!is.na(imaest$mes),]
  imaest <- to_date(imaest)
  imaest$serie <- 'Serie Tendencia-Ciclo'

  #
  dplyr::select(dplyr::bind_rows(imaeso, imaesd, imaest),
                tidyselect::all_of(c(
                  "fecha",
                  "Indice",
                  "Var_periodo_anterior",
                  "Var_interanual",
                  "Var_acumulada",
                  "Var_promedio_12_meses",
                  "serie"
                )))
}
