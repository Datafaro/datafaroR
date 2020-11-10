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
pib_gasto_trim <- function(indicador){
  pibFile <- downloader(indicador)
  pib <- readxl::read_excel(pibFile, skip = 5, col_names = F)
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
  levels <- tidyr::pivot_longer(levels, -'Componente')
  levels <- levels[levels$value == 1,]
  levels$value <- NULL
  levels <- dplyr::rename(levels, 'nivel' = 'name')
  levels$nivel <- stringr::str_remove(levels$nivel, 'nivel')
  pib <- dplyr::left_join(levels, pib)
  rm(levels)
  pib <- tidyr::pivot_longer(pib, -c('Componente', 'nivel'), names_to= 'fecha', values_to = 'pib')
  pib$fecha <- zoo::as.yearqtr(pib$fecha)
  pib$pib <- as.numeric(pib$pib)

  # Ponderacion por componente
  pib <- dplyr::group_by(pib, nivel, fecha)
  pib$pib <- ifelse(pib$Componente == 'Importaciones', -1*pib$pib, pib$pib)
  pib <- dplyr::mutate(pib, ponderacion = pib/sum(pib)*100)
  pib$pib <- ifelse(pib$Componente == 'Importaciones', -1*pib$pib, pib$pib)
  pib$ponderacion <- ifelse(pib$Componente == 'Importaciones', -1*pib$ponderacion, pib$ponderacion)
  pib <- dplyr::ungroup(pib)
  # PIB Acumulado
  pib <- dplyr::group_by(pib, Componente, nivel, ano = format(fecha, '%Y'))
  pib <- dplyr::mutate(pib, pib_acumulado = cumsum(pib))
  pib <- dplyr::ungroup(pib)
  pib$ano <- NULL
  # Ponderación por componente PIB acumulado
  pib$pib_acumulado <- ifelse(pib$Componente == 'Importaciones', -1*pib$pib_acumulado, pib$pib_acumulado)
  pib <- dplyr::group_by(pib, nivel, fecha)
  pib <- dplyr::mutate(pib, ponderacion_acumulado = pib_acumulado/sum(pib_acumulado)*100)
  pib$pib_acumulado <- ifelse(pib$Componente == 'Importaciones', -1*pib$pib_acumulado, pib$pib_acumulado)
  pib$ponderacion_acumulado <- ifelse(pib$Componente == 'Importaciones', -1*pib$ponderacion_acumulado, pib$ponderacion_acumulado)

  #############
  # INDICE PIB
  #############

  indice <- readxl::read_excel(pibFile, sheet = 3, skip = 5, col_names = F)
  indice <- tidyr::drop_na(indice, ...2)
  indice <- indice[1:9,]
  indice <- t(indice)
  indice[,1] <- stringr::str_remove(indice[,1], '\\(p\\)')
  indice[1,1] <- NA
  indice <- as.data.frame(indice)
  indice <- tidyr::fill(indice, V1)
  indice <- dplyr::mutate(indice,
                       V2 = dplyr::case_when(
                         V2 == 'E-M' ~ 'Q1',
                         V2 == 'A-J' ~ 'Q2',
                         V2 == 'J-S' ~ 'Q3',
                         V2 == 'O-D' ~ 'Q4'
                       ),
                       V1 = paste(V1, V2),
                       V2 = NULL)
  indice <- t(indice)
  indice[1,1] <- 'Componente'
  indice <- as.data.frame(indice)
  names(indice) <- indice[1,]
  indice <- indice[-1,]
  levels <- dplyr::bind_cols(
    data.frame(
      nivel0 = c(0,0,0,0,0,0,1),
      nivel1 = c(1,0,0,0,1,1,0),
      nivel2 = c(0,1,1,1,1,1,0)
    ),
    indice$Componente
  )
  names(levels)[4] <- 'Componente'
  levels <- tidyr::pivot_longer(levels, -'Componente')
  levels <- levels[levels$value == 1,]
  levels$value <- NULL
  levels <- dplyr::rename(levels, 'nivel' = 'name')
  levels$nivel <- stringr::str_remove(levels$nivel, 'nivel')
  indice <- dplyr::left_join(levels, indice)
  rm(levels)
  indice <- tidyr::pivot_longer(indice, -c('Componente', 'nivel'), names_to= 'fecha', values_to = 'indice')
  indice$fecha <- zoo::as.yearqtr(indice$fecha)
  indice$indice <- as.numeric(indice$indice)

  pib <- dplyr::left_join(pib, indice)

  # Tasa de crecimiento
  tc <- readxl::read_excel(pibFile, sheet = 3, skip = 25, col_names = F)
  tc <- tidyr::drop_na(tc, ...6)
  tc <- tc[1:9,]
  tc <- t(tc)
  tc[,1] <- stringr::str_remove(tc[,1], '\\(p\\)')
  tc[1,1] <- NA
  tc <- as.data.frame(tc)
  tc <- tidyr::fill(tc, V1)
  tc <- dplyr::mutate(tc,
                      V2 = dplyr::case_when(
                        V2 == 'E-M' ~ 'Q1',
                        V2 == 'A-J' ~ 'Q2',
                        V2 == 'J-S' ~ 'Q3',
                        V2 == 'O-D' ~ 'Q4'
                      ),
                      V1 = paste(V1, V2),
                      V2 = NULL)
  tc <- t(tc)
  tc[1,1] <- 'Componente'
  tc <- as.data.frame(tc)
  names(tc) <- tc[1,]
  tc <- tc[-1,]
  levels <- dplyr::bind_cols(
    data.frame(
      nivel0 = c(0,0,0,0,0,0,1),
      nivel1 = c(1,0,0,0,1,1,0),
      nivel2 = c(0,1,1,1,1,1,0)
    ),
    tc$Componente
  )
  names(levels)[4] <- 'Componente'
  levels <- tidyr::pivot_longer(levels, -'Componente')
  levels <- levels[levels$value == 1,]
  levels$value <- NULL
  levels <- dplyr::rename(levels, 'nivel' = 'name')
  levels$nivel <- stringr::str_remove(levels$nivel, 'nivel')
  tc <- dplyr::left_join(levels, tc)
  rm(levels)
  tc <- tidyr::pivot_longer(tc, -c('Componente', 'nivel'), names_to= 'fecha', values_to = 'tasa_crecimiento')
  tc$fecha <- zoo::as.yearqtr(tc$fecha)
  tc$tasa_crecimiento <- as.numeric(tc$tasa_crecimiento)

  pib <- dplyr::left_join(pib, tc)

  # Incidencia por componente
  incidencia <- readxl::read_excel(pibFile, sheet = 3, skip = 44, col_names = F)
  incidencia <- tidyr::drop_na(incidencia, ...6)
  incidencia <- t(incidencia)
  incidencia[,1] <- stringr::str_remove(incidencia[,1], '\\(p\\)')
  incidencia[1,1] <- NA
  incidencia <- as.data.frame(incidencia)
  incidencia <- tidyr::fill(incidencia, V1)
  incidencia <- dplyr::mutate(incidencia,
                      V2 = dplyr::case_when(
                        V2 == 'E-M' ~ 'Q1',
                        V2 == 'A-J' ~ 'Q2',
                        V2 == 'J-S' ~ 'Q3',
                        V2 == 'O-D' ~ 'Q4'
                      ),
                      V1 = paste(V1, V2),
                      V2 = NULL)
  incidencia <- t(incidencia)
  incidencia[1,1] <- 'Componente'
  incidencia <- as.data.frame(incidencia)
  names(incidencia) <- incidencia[1,]
  incidencia <- incidencia[-1,]
  levels <- dplyr::bind_cols(
    data.frame(
      nivel0 = c(0,0,0,0,0,0,0,0,1),
      nivel1 = c(1,0,0,1,0,0,1,1,0),
      nivel2 = c(0,1,1,0,1,1,1,1,0)
    ),
    incidencia$Componente
  )
  names(levels)[4] <- 'Componente'
  levels <- tidyr::pivot_longer(levels, -'Componente')
  levels <- levels[levels$value == 1,]
  levels$value <- NULL
  levels <- dplyr::rename(levels, 'nivel' = 'name')
  levels$nivel <- stringr::str_remove(levels$nivel, 'nivel')
  incidencia <- dplyr::left_join(levels, incidencia)
  rm(levels)
  incidencia <- tidyr::pivot_longer(incidencia, -c('Componente', 'nivel'), names_to= 'fecha', values_to = 'incidencia')
  incidencia$fecha <- zoo::as.yearqtr(incidencia$fecha)
  incidencia$incidencia <- as.numeric(incidencia$incidencia)

  pib <- dplyr::left_join(pib, incidencia)

  #########################
  # INDICE PIB ACUMULADO
  #########################

  indice_acumulado <- readxl::read_excel(pibFile, sheet = 4, skip = 5, col_names = F)
  indice_acumulado <- tidyr::drop_na(indice_acumulado, ...2)
  indice_acumulado <- indice_acumulado[1:9,]
  indice_acumulado <- t(indice_acumulado)
  indice_acumulado[,1] <- stringr::str_remove(indice_acumulado[,1], '\\(p\\)')
  indice_acumulado[1,1] <- NA
  indice_acumulado <- as.data.frame(indice_acumulado)
  indice_acumulado <- tidyr::fill(indice_acumulado, V1)
  indice_acumulado <- dplyr::mutate(indice_acumulado,
                          V2 = dplyr::case_when(
                            V2 == 'E-M' ~ 'Q1',
                            V2 == 'E-J' ~ 'Q2',
                            V2 == 'E-S' ~ 'Q3',
                            V2 == 'E-D' ~ 'Q4'
                          ),
                          V1 = paste(V1, V2),
                          V2 = NULL)
  indice_acumulado <- t(indice_acumulado)
  indice_acumulado[1,1] <- 'Componente'
  indice_acumulado <- as.data.frame(indice_acumulado)
  names(indice_acumulado) <- indice_acumulado[1,]
  indice_acumulado <- indice_acumulado[-1,]
  levels <- dplyr::bind_cols(
    data.frame(
      nivel0 = c(0,0,0,0,0,0,1),
      nivel1 = c(1,0,0,0,1,1,0),
      nivel2 = c(0,1,1,1,1,1,0)
    ),
    indice_acumulado$Componente
  )
  names(levels)[4] <- 'Componente'
  levels <- tidyr::pivot_longer(levels, -'Componente')
  levels <- levels[levels$value == 1,]
  levels$value <- NULL
  levels <- dplyr::rename(levels, 'nivel' = 'name')
  levels$nivel <- stringr::str_remove(levels$nivel, 'nivel')
  indice_acumulado <- dplyr::left_join(levels, indice_acumulado)
  rm(levels)
  indice_acumulado <- tidyr::pivot_longer(indice_acumulado, -c('Componente', 'nivel'), names_to= 'fecha', values_to = 'indice_acumulado')
  indice_acumulado$fecha <- zoo::as.yearqtr(indice_acumulado$fecha)
  indice_acumulado$indice_acumulado <- as.numeric(indice_acumulado$indice_acumulado)

  pib <- dplyr::left_join(pib, indice_acumulado)

  # Tasa de crecimiento
  tc_acumulado <- readxl::read_excel(pibFile, sheet = 4, skip = 25, col_names = F)
  tc_acumulado <- tidyr::drop_na(tc_acumulado, ...6)
  tc_acumulado <- tc_acumulado[1:9,]
  tc_acumulado <- t(tc_acumulado)
  tc_acumulado[,1] <- stringr::str_remove(tc_acumulado[,1], '\\(p\\)')
  tc_acumulado[1,1] <- NA
  tc_acumulado <- as.data.frame(tc_acumulado)
  tc_acumulado <- tidyr::fill(tc_acumulado, V1)
  tc_acumulado <- dplyr::mutate(tc_acumulado,
                      V2 = dplyr::case_when(
                        V2 == 'E-M' ~ 'Q1',
                        V2 == 'E-J' ~ 'Q2',
                        V2 == 'E-S' ~ 'Q3',
                        V2 == 'E-D' ~ 'Q4'
                      ),
                      V1 = paste(V1, V2),
                      V2 = NULL)
  tc_acumulado <- t(tc_acumulado)
  tc_acumulado[1,1] <- 'Componente'
  tc_acumulado <- as.data.frame(tc_acumulado)
  names(tc_acumulado) <- tc_acumulado[1,]
  tc_acumulado <- tc_acumulado[-1,]
  levels <- dplyr::bind_cols(
    data.frame(
      nivel0 = c(0,0,0,0,0,0,1),
      nivel1 = c(1,0,0,0,1,1,0),
      nivel2 = c(0,1,1,1,1,1,0)
    ),
    tc_acumulado$Componente
  )
  names(levels)[4] <- 'Componente'
  levels <- tidyr::pivot_longer(levels, -'Componente')
  levels <- levels[levels$value == 1,]
  levels$value <- NULL
  levels <- dplyr::rename(levels, 'nivel' = 'name')
  levels$nivel <- stringr::str_remove(levels$nivel, 'nivel')
  tc_acumulado <- dplyr::left_join(levels, tc_acumulado)
  rm(levels)
  tc_acumulado <- tidyr::pivot_longer(tc_acumulado, -c('Componente', 'nivel'), names_to= 'fecha', values_to = 'tc_acumulado')
  tc_acumulado$fecha <- zoo::as.yearqtr(tc_acumulado$fecha)
  tc_acumulado$tc_acumulado <- as.numeric(tc_acumulado$tc_acumulado)

  pib <- dplyr::left_join(pib, tc_acumulado)

  # Incidencia por componente acumulado
  incidencia_acumulado <- readxl::read_excel(pibFile, sheet = 4, skip = 44, col_names = F)
  incidencia_acumulado <- tidyr::drop_na(incidencia_acumulado, ...6)
  incidencia_acumulado <- t(incidencia_acumulado)
  incidencia_acumulado[,1] <- stringr::str_remove(incidencia_acumulado[,1], '\\(p\\)')
  incidencia_acumulado[1,1] <- NA
  incidencia_acumulado <- as.data.frame(incidencia_acumulado)
  incidencia_acumulado <- tidyr::fill(incidencia_acumulado, V1)
  incidencia_acumulado <- dplyr::mutate(incidencia_acumulado,
                              V2 = dplyr::case_when(
                                V2 == 'E-M' ~ 'Q1',
                                V2 == 'E-J' ~ 'Q2',
                                V2 == 'E-S' ~ 'Q3',
                                V2 == 'E-D' ~ 'Q4'
                              ),
                              V1 = paste(V1, V2),
                              V2 = NULL)
  incidencia_acumulado <- t(incidencia_acumulado)
  incidencia_acumulado[1,1] <- 'Componente'
  incidencia_acumulado <- as.data.frame(incidencia_acumulado)
  names(incidencia_acumulado) <- incidencia_acumulado[1,]
  incidencia_acumulado <- incidencia_acumulado[-1,]
  levels <- dplyr::bind_cols(
    data.frame(
      nivel0 = c(0,0,0,0,0,0,0,0,1),
      nivel1 = c(1,0,0,1,0,0,1,1,0),
      nivel2 = c(0,1,1,0,1,1,1,1,0)
    ),
    incidencia_acumulado$Componente
  )
  names(levels)[4] <- 'Componente'
  levels <- tidyr::pivot_longer(levels, -'Componente')
  levels <- levels[levels$value == 1,]
  levels$value <- NULL
  levels <- dplyr::rename(levels, 'nivel' = 'name')
  levels$nivel <- stringr::str_remove(levels$nivel, 'nivel')
  incidencia_acumulado <- dplyr::left_join(levels, incidencia_acumulado)
  rm(levels)
  incidencia_acumulado <- tidyr::pivot_longer(incidencia_acumulado, -c('Componente', 'nivel'), names_to= 'fecha', values_to = 'incidencia_acumulado')
  incidencia_acumulado$fecha <- zoo::as.yearqtr(incidencia_acumulado$fecha)
  incidencia_acumulado$incidencia_acumulado <- as.numeric(incidencia_acumulado$incidencia_acumulado)

  pib <- dplyr::left_join(pib, incidencia_acumulado)
  # Esta es una variable que permitirá validar fácil y rápido cambios en cualquiera de las tablas.
  pib$validar <- pib$pib +
    pib$ponderacion +
    pib$pib_acumulado +
    pib$ponderacion_acumulado +
    pib$indice +
    pib$tasa_crecimiento +
    pib$incidencia +
    pib$indice_acumulado +
    pib$tc_acumulado +
    pib$incidencia_acumulado

  pib$fecha <- as.character(pib$fecha)
  pib
}
