#' PIB por enfoque del gasto trimestral
#'
#'   \lifecycle{experimental}
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
pib_gasto_trim <- function(indicador = c(original_url = NULL, file_ext = "xls")){
  if(is.null(indicador[["original_url"]])){
    indicador[["original_url"]] <- "https://cdn.bancentral.gov.do/documents/estadisticas/sector-real/documents/pib_gasto_2007.xls"
  }
  `...2` <- NULL
  V1 <- NULL
  V2 <- NULL
  pibFile <- downloader(indicador)
  pib <- readxl::read_excel(pibFile, sheet = 'PIB$_Trim', skip = 5, col_names = F)
  pib <- tidyr::drop_na(pib, ...2)
  pib <- pib[1:11,]
  pib <- t(pib)
  pib[,1] <- stringr::str_remove_all(pib[,1], "[^0-9]")
  pib <- as.data.frame(pib)
  pib <- tidyr::fill(pib, V1)
  pib <- dplyr::mutate(pib,
                       V2 = dplyr::case_when(
                         V2 == 'E-M' ~ 'Q1',
                         V2 == 'A-J' ~ 'Q2',
                         V2 == 'J-S' ~ 'Q3',
                         V2 == 'O-D' ~ 'Q4'
                       ),
                       V1 = paste(V1, V2),
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
  levels <- tidyr::pivot_longer(levels, -'Componente', names_to = "nivel", values_drop_na = T)
  levels <- levels[levels$value == 1,]
  levels$value <- NULL
  levels$nivel <- stringr::str_remove(levels$nivel, 'nivel')
  order <- dplyr::bind_cols(
    data.frame(
      orden = c(1,11,12,2,21,22,3,4,5)
    ),
    pib$Componente
  )
  names(order)[2] <- 'Componente'
  levels <- dplyr::left_join(levels, order)
  rm(order)
  pib <- dplyr::left_join(levels, pib)
  rm(levels)
  pib <- tidyr::pivot_longer(pib,
                             -c('Componente', 'nivel', 'orden'),
                             names_to= 'fecha',
                             values_to = 'pib',
                             values_drop_na = T)
  pib$fecha <- zoo::as.yearqtr(pib$fecha)
  pib$pib <- as.numeric(pib$pib)

  # Ponderacion por componente
  pib2 <- readxl::read_excel(pibFile, sheet = 'PIB$_Trim', skip = 25, col_names = F)
  pib2 <- tidyr::drop_na(pib2, ...2)
  pib2 <- pib2[1:11,]
  pib2 <- t(pib2)
  pib2[,1] <- stringr::str_remove_all(pib2[,1], '[^0-9]')
  pib2[1,1] <- NA
  pib2 <- as.data.frame(pib2)
  pib2 <- tidyr::fill(pib2, V1)
  pib2 <- dplyr::mutate(pib2,
                       V2 = dplyr::case_when(
                         V2 == 'E-M' ~ 'Q1',
                         V2 == 'A-J' ~ 'Q2',
                         V2 == 'J-S' ~ 'Q3',
                         V2 == 'O-D' ~ 'Q4'
                       ),
                       V1 = paste(V1, V2),
                       V2 = NULL)
  pib2 <- t(pib2)
  pib2[1,1] <- 'Componente'
  pib2 <- as.data.frame(pib2)
  names(pib2) <- pib2[1,]
  pib2 <- pib2[-1,]
  levels <- dplyr::bind_cols(
    data.frame(
      nivel0 = c(0,0,0,0,0,0,0,0,1),
      nivel1 = c(1,0,0,1,0,0,1,1,0),
      nivel2 = c(0,1,1,0,1,1,1,1,0)
    ),
    pib2$Componente
  )
  names(levels)[4] <- 'Componente'
  levels <- tidyr::pivot_longer(levels, -'Componente')
  levels <- levels[levels$value == 1,]
  levels$value <- NULL
  levels <- dplyr::rename(levels, 'nivel' = 'name')
  levels$nivel <- stringr::str_remove(levels$nivel, 'nivel')
  order <- dplyr::bind_cols(
    data.frame(
      orden = c(1,11,12,2,21,22,3,4,5)
    ),
    pib2$Componente
  )
  names(order)[2] <- 'Componente'
  levels <- dplyr::left_join(levels, order)
  rm(order)
  pib2 <- dplyr::left_join(levels, pib2)
  rm(levels)
  pib2 <- tidyr::pivot_longer(pib2,
                              -c('Componente', 'nivel', 'orden'),
                              names_to= 'fecha',
                              values_to = 'ponderacion',
                              values_drop_na = T)
  pib2$fecha <- zoo::as.yearqtr(pib2$fecha)
  pib2$ponderacion <- as.numeric(pib2$ponderacion)

  pib <- dplyr::left_join(pib, pib2)


  # PIB Acumulado
  pib2 <- readxl::read_excel(pibFile, sheet = 'PIB$_Trim_Acum', skip = 6, col_names = F)
  pib2 <- tidyr::drop_na(pib2, ...2)
  pib2 <- pib2[1:11,]
  pib2 <- t(pib2)
  pib2[,1] <- stringr::str_remove(pib2[,1], '\\(p\\)')
  pib2[1,1] <- NA
  pib2 <- as.data.frame(pib2)
  pib2 <- tidyr::fill(pib2, V1)
  pib2 <- dplyr::mutate(pib2,
                       V2 = dplyr::case_when(
                         V2 == 'E-M' ~ 'Q1',
                         V2 == 'E-J' ~ 'Q2',
                         V2 == 'E-S' ~ 'Q3',
                         V2 == 'E-D' ~ 'Q4'
                       ),
                       V1 = paste(V1, V2),
                       V2 = NULL)
  pib2 <- t(pib2)
  pib2[1,1] <- 'Componente'
  pib2 <- as.data.frame(pib2)
  names(pib2) <- pib2[1,]
  pib2 <- pib2[-1,]
  levels <- dplyr::bind_cols(
    data.frame(
      nivel0 = c(0,0,0,0,0,0,0,0,1),
      nivel1 = c(1,0,0,1,0,0,1,1,0),
      nivel2 = c(0,1,1,0,1,1,1,1,0)
    ),
    pib2$Componente
  )
  names(levels)[4] <- 'Componente'
  levels <- tidyr::pivot_longer(levels, -'Componente')
  levels <- levels[levels$value == 1,]
  levels$value <- NULL
  levels <- dplyr::rename(levels, 'nivel' = 'name')
  levels$nivel <- stringr::str_remove(levels$nivel, 'nivel')
  order <- dplyr::bind_cols(
    data.frame(
      orden = c(1,11,12,2,21,22,3,4,5)
    ),
    pib2$Componente
  )
  names(order)[2] <- 'Componente'
  levels <- dplyr::left_join(levels, order)
  rm(order)
  pib2 <- dplyr::left_join(levels, pib2)
  rm(levels)
  pib2 <- tidyr::pivot_longer(pib2,
                              -c('Componente', 'nivel', 'orden'),
                              names_to= 'fecha',
                              values_to = 'pib_acumulado',
                              values_drop_na = T)
  pib2$fecha <- zoo::as.yearqtr(pib2$fecha)
  pib2$pib_acumulado <- as.numeric(pib2$pib_acumulado)

  pib <- dplyr::left_join(pib, pib2)

  # Ponderación por componente PIB acumulado
  pib2 <- readxl::read_excel(pibFile, sheet = 'PIB$_Trim_Acum', skip = 25, col_names = F)
  pib2 <- tidyr::drop_na(pib2, ...2)
  pib2 <- pib2[1:11,]
  pib2 <- t(pib2)
  pib2[,1] <- stringr::str_remove(pib2[,1], '\\(p\\)')
  pib2[1,1] <- NA
  pib2 <- as.data.frame(pib2)
  pib2 <- tidyr::fill(pib2, V1)
  pib2 <- dplyr::mutate(pib2,
                        V2 = dplyr::case_when(
                          V2 == 'E-M' ~ 'Q1',
                          V2 == 'E-J' ~ 'Q2',
                          V2 == 'E-S' ~ 'Q3',
                          V2 == 'E-D' ~ 'Q4'
                        ),
                        V1 = paste(V1, V2),
                        V2 = NULL)
  pib2 <- t(pib2)
  pib2[1,1] <- 'Componente'
  pib2 <- as.data.frame(pib2)
  names(pib2) <- pib2[1,]
  pib2 <- pib2[-1,]
  levels <- dplyr::bind_cols(
    data.frame(
      nivel0 = c(0,0,0,0,0,0,0,0,1),
      nivel1 = c(1,0,0,1,0,0,1,1,0),
      nivel2 = c(0,1,1,0,1,1,1,1,0)
    ),
    pib2$Componente
  )
  names(levels)[4] <- 'Componente'
  levels <- tidyr::pivot_longer(levels, -'Componente')
  levels <- levels[levels$value == 1,]
  levels$value <- NULL
  levels <- dplyr::rename(levels, 'nivel' = 'name')
  levels$nivel <- stringr::str_remove(levels$nivel, 'nivel')
  order <- dplyr::bind_cols(
    data.frame(
      orden = c(1,11,12,2,21,22,3,4,5)
    ),
    pib2$Componente
  )
  names(order)[2] <- 'Componente'
  levels <- dplyr::left_join(levels, order)
  pib2 <- dplyr::left_join(levels, pib2)
  rm(levels)
  pib2 <- tidyr::pivot_longer(pib2,
                              -c('Componente', 'nivel', 'orden'),
                              names_to= 'fecha',
                              values_to = 'ponderacion_acumulada',
                              values_drop_na = T)
  pib2$fecha <- zoo::as.yearqtr(pib2$fecha)
  pib2$ponderacion_acumulada <- as.numeric(pib2$ponderacion_acumulada)

  pib <- dplyr::left_join(pib, pib2)

  #############
  # INDICE PIB
  #############

  pib2 <- readxl::read_excel(pibFile, sheet = 'PIBK_Trim', skip = 6, col_names = F)
  pib2 <- pib2[!is.na(pib2$...1) | !is.na(pib2$...2),]
  pib2 <- pib2[1:11,]
  pib2 <- t(pib2)
  pib2[,1] <- stringr::str_remove(pib2[,1], '\\(p\\)')
  pib2[1,1] <- NA
  pib2 <- as.data.frame(pib2)
  pib2 <- tidyr::fill(pib2, V1)
  pib2 <- dplyr::mutate(pib2,
                        V2 = dplyr::case_when(
                          V2 == 'E-M' ~ 'Q1',
                          V2 == 'A-J' ~ 'Q2',
                          V2 == 'J-S' ~ 'Q3',
                          V2 == 'O-D' ~ 'Q4'
                        ),
                        V1 = paste(V1, V2),
                        V2 = NULL)
  pib2 <- t(pib2)
  pib2[,1] <- stringr::str_remove(pib2[,1], '\\(1\\)')
  pib2[1,1] <- 'Componente'
  pib2 <- as.data.frame(pib2)
  names(pib2) <- pib2[1,]
  pib2 <- pib2[-1,]
  levels <- dplyr::bind_cols(
    data.frame(
      nivel0 = c(0,0,0,0,0,0,0,0,1),
      nivel1 = c(1,0,0,1,0,0,1,1,0),
      nivel2 = c(0,1,1,0,1,1,1,1,0)
    ),
    pib2$Componente
  )
  names(levels)[4] <- 'Componente'
  levels <- tidyr::pivot_longer(levels, -'Componente')
  levels <- levels[levels$value == 1,]
  levels$value <- NULL
  levels <- dplyr::rename(levels, 'nivel' = 'name')
  levels$nivel <- stringr::str_remove(levels$nivel, 'nivel')
  order <- dplyr::bind_cols(
    data.frame(
      orden = c(1,11,12,2,21,22,3,4,5)
    ),
    pib2$Componente
  )
  names(order)[2] <- 'Componente'
  levels <- dplyr::left_join(levels, order)
  pib2 <- dplyr::left_join(levels, pib2)
  rm(levels, order)
  pib2 <- tidyr::pivot_longer(pib2,
                              -c('Componente', 'nivel', 'orden'),
                              names_to= 'fecha',
                              values_to = 'indice',
                              values_drop_na = T)
  pib2$fecha <- zoo::as.yearqtr(pib2$fecha)
  pib2$indice <- as.numeric(pib2$indice)

  pib <- dplyr::left_join(pib, pib2)

  # Tasa de crecimiento

  pib2 <- readxl::read_excel(pibFile, sheet = 'PIBK_Trim', skip = 25, col_names = F)
  pib2 <- pib2[!is.na(pib2$...1) | !is.na(pib2$...2),]
  pib2 <- pib2[1:11,]
  pib2 <- t(pib2)
  pib2[,1] <- stringr::str_remove(pib2[,1], '\\(p\\)')
  pib2[1,1] <- NA
  pib2 <- as.data.frame(pib2)
  pib2 <- tidyr::fill(pib2, V1)
  pib2 <- dplyr::mutate(pib2,
                        V2 = dplyr::case_when(
                          V2 == 'E-M' ~ 'Q1',
                          V2 == 'A-J' ~ 'Q2',
                          V2 == 'J-S' ~ 'Q3',
                          V2 == 'O-D' ~ 'Q4'
                        ),
                        V1 = paste(V1, V2),
                        V2 = NULL)
  pib2 <- t(pib2)
  pib2[,1] <- stringr::str_remove(pib2[,1], '\\(1\\)')
  pib2[1,1] <- 'Componente'
  pib2 <- as.data.frame(pib2)
  names(pib2) <- pib2[1,]
  pib2 <- pib2[-1,]
  levels <- dplyr::bind_cols(
    data.frame(
      nivel0 = c(0,0,0,0,0,0,0,0,1),
      nivel1 = c(1,0,0,1,0,0,1,1,0),
      nivel2 = c(0,1,1,0,1,1,1,1,0)
    ),
    pib2$Componente
  )
  names(levels)[4] <- 'Componente'
  levels <- tidyr::pivot_longer(levels, -'Componente')
  levels <- levels[levels$value == 1,]
  levels$value <- NULL
  levels <- dplyr::rename(levels, 'nivel' = 'name')
  levels$nivel <- stringr::str_remove(levels$nivel, 'nivel')
  order <- dplyr::bind_cols(
    data.frame(
      orden = c(1,11,12,2,21,22,3,4,5)
    ),
    pib2$Componente
  )
  names(order)[2] <- 'Componente'
  levels <- dplyr::left_join(levels, order)
  pib2 <- dplyr::left_join(levels, pib2)
  rm(levels, order)
  pib2 <- tidyr::pivot_longer(pib2,
                              -c('Componente', 'nivel', 'orden'),
                              names_to= 'fecha',
                              values_to = 'tasa_crecimiento',
                              values_drop_na = T)
  pib2$fecha <- zoo::as.yearqtr(pib2$fecha)
  pib2$tasa_crecimiento <- as.numeric(pib2$tasa_crecimiento)

  pib <- dplyr::left_join(pib, pib2)

  # Incidencia por componente
  pib2 <- readxl::read_excel(pibFile, sheet = 'PIBK_Trim', skip = 44, col_names = F)
  pib2 <- pib2[!is.na(pib2$...1) | !is.na(pib2$...2),]
  pib2 <- pib2[1:11,]
  pib2 <- t(pib2)
  pib2[,1] <- stringr::str_remove(pib2[,1], '\\(p\\)')
  pib2[1,1] <- NA
  pib2 <- as.data.frame(pib2)
  pib2 <- tidyr::fill(pib2, V1)
  pib2 <- dplyr::mutate(pib2,
                        V2 = dplyr::case_when(
                          V2 == 'E-M' ~ 'Q1',
                          V2 == 'A-J' ~ 'Q2',
                          V2 == 'J-S' ~ 'Q3',
                          V2 == 'O-D' ~ 'Q4'
                        ),
                        V1 = paste(V1, V2),
                        V2 = NULL)
  pib2 <- t(pib2)
  pib2[,1] <- stringr::str_remove(pib2[,1], '\\(1\\)')
  pib2[1,1] <- 'Componente'
  pib2 <- as.data.frame(pib2)
  names(pib2) <- pib2[1,]
  pib2 <- pib2[-1,]
  levels <- dplyr::bind_cols(
    data.frame(
      nivel0 = c(0,0,0,0,0,0,0,0,1),
      nivel1 = c(1,0,0,1,0,0,1,1,0),
      nivel2 = c(0,1,1,0,1,1,1,1,0)
    ),
    pib2$Componente
  )
  names(levels)[4] <- 'Componente'
  levels <- tidyr::pivot_longer(levels, -'Componente')
  levels <- levels[levels$value == 1,]
  levels$value <- NULL
  levels <- dplyr::rename(levels, 'nivel' = 'name')
  levels$nivel <- stringr::str_remove(levels$nivel, 'nivel')
  order <- dplyr::bind_cols(
    data.frame(
      orden = c(1,11,12,2,21,22,3,4,5)
    ),
    pib2$Componente
  )
  names(order)[2] <- 'Componente'
  levels <- dplyr::left_join(levels, order)
  pib2 <- dplyr::left_join(levels, pib2)
  rm(levels, order)
  pib2 <- tidyr::pivot_longer(pib2,
                              -c('Componente', 'nivel', 'orden'),
                              names_to= 'fecha',
                              values_to = 'incidencia',
                              values_drop_na = T)
  pib2$fecha <- zoo::as.yearqtr(pib2$fecha)
  pib2$incidencia <- as.numeric(pib2$incidencia)

  pib <- dplyr::left_join(pib, pib2)


  #########################
  # INDICE PIB ACUMULADO
  #########################

  pib2 <- readxl::read_excel(pibFile, sheet = 'PIBK_Trim_Acum', skip = 6, col_names = F)
  pib2 <- pib2[!is.na(pib2$...1) | !is.na(pib2$...2),]
  pib2 <- pib2[1:11,]
  pib2 <- t(pib2)
  pib2[,1] <- stringr::str_remove(pib2[,1], '\\(p\\)')
  pib2[1,1] <- NA
  pib2 <- as.data.frame(pib2)
  pib2 <- tidyr::fill(pib2, V1)
  pib2 <- dplyr::mutate(pib2,
                        V2 = dplyr::case_when(
                          V2 == 'E-M' ~ 'Q1',
                          V2 == 'E-J' ~ 'Q2',
                          V2 == 'E-S' ~ 'Q3',
                          V2 == 'E-D' ~ 'Q4'
                        ),
                        V1 = paste(V1, V2),
                        V2 = NULL)
  pib2 <- t(pib2)
  pib2[,1] <- stringr::str_remove(pib2[,1], '\\(1\\)')
  pib2[1,1] <- 'Componente'
  pib2 <- as.data.frame(pib2)
  names(pib2) <- pib2[1,]
  pib2 <- pib2[-1,]
  levels <- dplyr::bind_cols(
    data.frame(
      nivel0 = c(0,0,0,0,0,0,0,0,1),
      nivel1 = c(1,0,0,1,0,0,1,1,0),
      nivel2 = c(0,1,1,0,1,1,1,1,0)
    ),
    pib2$Componente
  )
  names(levels)[4] <- 'Componente'
  levels <- tidyr::pivot_longer(levels, -'Componente')
  levels <- levels[levels$value == 1,]
  levels$value <- NULL
  levels <- dplyr::rename(levels, 'nivel' = 'name')
  levels$nivel <- stringr::str_remove(levels$nivel, 'nivel')
  order <- dplyr::bind_cols(
    data.frame(
      orden = c(1,11,12,2,21,22,3,4,5)
    ),
    pib2$Componente
  )
  names(order)[2] <- 'Componente'
  levels <- dplyr::left_join(levels, order)
  pib2 <- dplyr::left_join(levels, pib2)
  rm(levels, order)
  pib2 <- tidyr::pivot_longer(pib2,
                              -c('Componente', 'nivel', 'orden'),
                              names_to= 'fecha',
                              values_to = 'indice_acumulado',
                              values_drop_na = T)
  pib2$fecha <- zoo::as.yearqtr(pib2$fecha)
  pib2$indice_acumulado <- as.numeric(pib2$indice_acumulado)

  pib <- dplyr::left_join(pib, pib2)

  # Tasa de crecimiento

  pib2 <- readxl::read_excel(pibFile, sheet = 'PIBK_Trim_Acum', skip = 25, col_names = F)
  pib2 <- pib2[!is.na(pib2$...1) | !is.na(pib2$...2),]
  pib2 <- pib2[1:11,]
  pib2 <- t(pib2)
  pib2[,1] <- stringr::str_remove(pib2[,1], '\\(p\\)')
  pib2[1,1] <- NA
  pib2 <- as.data.frame(pib2)
  pib2 <- tidyr::fill(pib2, V1)
  pib2 <- dplyr::mutate(pib2,
                        V2 = dplyr::case_when(
                          V2 == 'E-M' ~ 'Q1',
                          V2 == 'E-J' ~ 'Q2',
                          V2 == 'E-S' ~ 'Q3',
                          V2 == 'E-D' ~ 'Q4'
                        ),
                        V1 = paste(V1, V2),
                        V2 = NULL)
  pib2 <- t(pib2)
  pib2[,1] <- stringr::str_remove(pib2[,1], '\\(1\\)')
  pib2[1,1] <- 'Componente'
  pib2 <- as.data.frame(pib2)
  names(pib2) <- pib2[1,]
  pib2 <- pib2[-1,]
  levels <- dplyr::bind_cols(
    data.frame(
      nivel0 = c(0,0,0,0,0,0,0,0,1),
      nivel1 = c(1,0,0,1,0,0,1,1,0),
      nivel2 = c(0,1,1,0,1,1,1,1,0)
    ),
    pib2$Componente
  )
  names(levels)[4] <- 'Componente'
  levels <- tidyr::pivot_longer(levels, -'Componente')
  levels <- levels[levels$value == 1,]
  levels$value <- NULL
  levels <- dplyr::rename(levels, 'nivel' = 'name')
  levels$nivel <- stringr::str_remove(levels$nivel, 'nivel')
  order <- dplyr::bind_cols(
    data.frame(
      orden = c(1,11,12,2,21,22,3,4,5)
    ),
    pib2$Componente
  )
  names(order)[2] <- 'Componente'
  levels <- dplyr::left_join(levels, order)
  pib2 <- dplyr::left_join(levels, pib2)
  rm(levels, order)
  pib2 <- tidyr::pivot_longer(pib2,
                              -c('Componente', 'nivel', 'orden'),
                              names_to= 'fecha',
                              values_to = 'tasa_crecimiento_acumulada',
                              values_drop_na = T)
  pib2$fecha <- zoo::as.yearqtr(pib2$fecha)
  pib2$tasa_crecimiento_acumulada <- as.numeric(pib2$tasa_crecimiento_acumulada)

  pib <- dplyr::left_join(pib, pib2)

  # Incidencia por componente
  pib2 <- readxl::read_excel(pibFile, sheet = 'PIBK_Trim_Acum', skip = 44, col_names = F)
  pib2 <- pib2[!is.na(pib2$...1) | !is.na(pib2$...2),]
  pib2 <- pib2[1:11,]
  pib2 <- t(pib2)
  pib2[,1] <- stringr::str_remove(pib2[,1], '\\(p\\)')
  pib2[1,1] <- NA
  pib2 <- as.data.frame(pib2)
  pib2 <- tidyr::fill(pib2, V1)
  pib2 <- dplyr::mutate(pib2,
                        V2 = dplyr::case_when(
                          V2 == 'E-M' ~ 'Q1',
                          V2 == 'E-J' ~ 'Q2',
                          V2 == 'E-S' ~ 'Q3',
                          V2 == 'E-D' ~ 'Q4'
                        ),
                        V1 = paste(V1, V2),
                        V2 = NULL)
  pib2 <- t(pib2)
  pib2[,1] <- stringr::str_remove(pib2[,1], '\\(1\\)')
  pib2[1,1] <- 'Componente'
  pib2 <- as.data.frame(pib2)
  names(pib2) <- pib2[1,]
  pib2 <- pib2[-1,]
  levels <- dplyr::bind_cols(
    data.frame(
      nivel0 = c(0,0,0,0,0,0,0,0,1),
      nivel1 = c(1,0,0,1,0,0,1,1,0),
      nivel2 = c(0,1,1,0,1,1,1,1,0)
    ),
    pib2$Componente
  )
  names(levels)[4] <- 'Componente'
  levels <- tidyr::pivot_longer(levels, -'Componente')
  levels <- levels[levels$value == 1,]
  levels$value <- NULL
  levels <- dplyr::rename(levels, 'nivel' = 'name')
  levels$nivel <- stringr::str_remove(levels$nivel, 'nivel')
  order <- dplyr::bind_cols(
    data.frame(
      orden = c(1,11,12,2,21,22,3,4,5)
    ),
    pib2$Componente
  )
  names(order)[2] <- 'Componente'
  levels <- dplyr::left_join(levels, order)
  pib2 <- dplyr::left_join(levels, pib2)
  rm(levels, order)
  pib2 <- tidyr::pivot_longer(pib2,
                              -c('Componente', 'nivel', 'orden'),
                              names_to= 'fecha',
                              values_to = 'incidencia_acumulada',
                              values_drop_na = T)
  pib2$fecha <- zoo::as.yearqtr(pib2$fecha)
  pib2$incidencia_acumulada <- as.numeric(pib2$incidencia_acumulada)

  pib <- dplyr::left_join(pib, pib2)

  pib$fecha <- as.character(pib$fecha)
  unlink(pibFile)
  pib
}
