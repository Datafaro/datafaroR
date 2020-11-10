#' PIB por enfoque del gasto trimestral
#'
#' @param indicador vector de datos para este indicador en la lista de domar
#'
#' @return data.frame los datos del PIB por el enfoque del gasto trimestral
#' @export
#'
#' @examples
#' \dontrun{
#' pib_gasto_t(indicador)
#' }
pib_gasto_t <- function(indicador){
  pib <- readxl::read_excel(
    downloader(indicador),
    skip = 5,
    col_names = F
  )
  pib <- tidyr::drop_na(pib, ...2)
  pib <- pib[1:11,]
  pib <- t(pib)
  pib[,1] <- stringr::str_remove(pib[,1], '\\(p\\)')
  pib[1,1] <- NA
  pib <- as.data.frame(pib)
  pib <- tidyr::fill(pib, V1)
  pib <- dplyr::mutate(pib,
                       V2 = dplyr::case_when(
                         V2 == 'E-M' ~ 'Q1',
                         V2 == 'A-J' ~ 'Q2',
                         V2 == 'J-S' ~ 'Q3',
                         V2 == 'O-D' ~ 'Q4'
                       ),
                       V1 = paste(V2, V1),
                       V2 = NULL)
  pib <- t(pib)
  pib[1,1] <- 'Componente'
  pib <- as.data.frame(pib)
  names(pib) <- pib[1,]
  pib <- pib[-1,]
  levels <- dplyr::bind_cols(
    data.frame(
      nivel0 = c(0,0,0,0,0,0,0,0,1),
      nivel1 = c(1,0,0,1,0,0,1,1,0),
      nivel2 = c(0,1,1,0,1,1,1,1,0)
    ),
    pib$Componente
  )
  names(levels)[4] <- 'Componente'
  levels <- tidyr::pivot_longer(levels, -'Componente')
  levels <- levels[levels$value == 1,]
  levels$value <- NULL
  levels <- dplyr::rename(levels, 'nivel' = 'name')
  levels$nivel <- stringr::str_remove(levels$nivel, 'nivel')
  pib <- dplyr::left_join(
    levels,
    pib
  )
  rm(levels)
  pib <- tidyr::pivot_longer(pib, -c('Componente', 'nivel'), names_to= 'fecha')
  utils::type.convert(pib)
}
