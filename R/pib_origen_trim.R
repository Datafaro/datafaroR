#' PIB por el enfoque de origen
#'
#' @param indicador vector con los datos del indicador
#'
#' @return los datos en formato tabular
#' @export
#'
pib_origen_trim <- function(indicador = c(original_url = NULL, file_ext="xlsx")){
  `...2` <- NULL
  V1 <- NULL
  V2 <- NULL
  Rama <- NULL
  value <- NULL
  level <- NULL
  `...6` <- NULL
  file <- downloader(indicador)
  # PIB
  pib <- readxl::read_excel(file, sheet = 'PIB$_Trim', skip = 6, col_names = F)
  pib <- pib[1:34,]
  pib <- tidyr::drop_na(pib, ...2)
  pib <- t(pib)
  pib[1,1] <- NA
  pib[,1] <- stringr::str_remove_all(pib[,1], '[^0-9]')
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
  pib <- as.data.frame(pib)
  pib[1,1] <- 'Rama'
  names(pib) <- pib[1,]
  pib <- pib[-1,]
  levels <- dplyr::bind_cols(
    data.frame(
      nivel0 = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1),
      nivel1 = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0),
      nivel2 = c(1,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0),
      nivel3 = c(1,0,0,0,1,1,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0),
      nivel4 = c(0,1,1,0,1,0,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,0,0,1,0,0,1,1,0,1,0),
      nivel5 = c(0,1,1,0,1,0,1,1,1,1,1,1,0,1,1,1,1,1,1,1,0,1,1,0,1,1,1,1,0,1,0)
    ),
    pib$Rama
  )
  names(levels)[7] <- 'Rama'
  levels <- tidyr::pivot_longer(levels, -Rama, names_to='level')
  levels <- dplyr::filter(levels,
                          value == 1)
  levels$value <- NULL
  levels <- dplyr::mutate(levels,
                          level = stringr::str_remove(level, 'nivel'))
  order <- dplyr::bind_cols(
    data.frame(
      orden = c('01','0101','0102','02','0201','0202','020201','020202',
                '020203','020204','0203','0204','03','0301','0302','0303',
                '0304','0305','0306','0307','0308','030801','030802','0309',
                '030901','030902','0310','0311','04','05','06')
    ),
    pib$Rama
  )
  names(order)[2] <- 'Rama'
  levels <- dplyr::left_join(levels, order)
  pib <- dplyr::left_join(levels, pib)
  pib <- tidyr::pivot_longer(pib, -c('Rama', 'level', 'orden'), names_to = 'fecha', values_to = 'pib', values_drop_na = T)
  pib$fecha <- zoo::as.yearqtr(pib$fecha)
  pib$pib <- as.numeric(pib$pib)

  # Ponderacion actividad economica
  pib2 <- readxl::read_excel(file, skip = 42, col_names = F)
  pib2 <- tidyr::drop_na(pib2, ...2)
  pib2 <- t(pib2)
  pib2[1,1] <- NA
  pib2[,1] <- stringr::str_remove(pib2[,1], '\\(p\\)')
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
  pib2 <- as.data.frame(pib2)
  pib2[1,1] <- 'Rama'
  names(pib2) <- pib2[1,]
  pib2 <- pib2[-1,]
  levels <- dplyr::bind_cols(
    data.frame(
      nivel0 = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1),
      nivel1 = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0),
      nivel2 = c(1,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0),
      nivel3 = c(1,0,0,0,1,1,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0),
      nivel4 = c(0,1,1,0,1,0,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,0,0,1,0,0,1,1,0,1,0),
      nivel5 = c(0,1,1,0,1,0,1,1,1,1,1,1,0,1,1,1,1,1,1,1,0,1,1,0,1,1,1,1,0,1,0)
    ),
    pib2$Rama
  )
  names(levels)[7] <- 'Rama'
  levels <- tidyr::pivot_longer(levels, -Rama, names_to='level')
  levels <- dplyr::filter(levels,
                          value == 1)
  levels$value <- NULL
  levels <- dplyr::mutate(levels,
                          level = stringr::str_remove(level, 'nivel'))
  order <- dplyr::bind_cols(
    data.frame(
      orden = c('01','0101','0102','02','0201','0202','020201','020202',
                '020203','020204','0203','0204','03','0301','0302','0303',
                '0304','0305','0306','0307','0308','030801','030802','0309',
                '030901','030902','0310','0311','04','05','06')
    ),
    pib2$Rama
  )
  names(order)[2] <- 'Rama'
  levels <- dplyr::left_join(levels, order)
  pib2 <- dplyr::left_join(levels, pib2)
  pib2 <- tidyr::pivot_longer(pib2, -c('Rama', 'level', 'orden'), names_to = 'fecha', values_to = 'ponderacion', values_drop_na = T)
  pib2$fecha <- zoo::as.yearqtr(pib2$fecha)
  pib2$ponderacion <- as.numeric(pib2$ponderacion)

  pib <- dplyr::left_join(pib, pib2)
  rm(pib2)

  # PIB Acumulado
  pib2 <- readxl::read_excel(file, sheet = 'PIB$_Trim_Acum', skip = 6, col_names = F)
  pib2 <- pib2[1:34,]
  pib2 <- tidyr::drop_na(pib2, ...2)
  pib2 <- t(pib2)
  pib2[1,1] <- NA
  pib2[,1] <- stringr::str_remove(pib2[,1], '\\(p\\)')
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
  pib2 <- as.data.frame(pib2)
  pib2[1,1] <- 'Rama'
  names(pib2) <- pib2[1,]
  pib2 <- pib2[-1,]
  levels <- dplyr::bind_cols(
    data.frame(
      nivel0 = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1),
      nivel1 = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0),
      nivel2 = c(1,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0),
      nivel3 = c(1,0,0,0,1,1,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0),
      nivel4 = c(0,1,1,0,1,0,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,0,0,1,0,0,1,1,0,1,0),
      nivel5 = c(0,1,1,0,1,0,1,1,1,1,1,1,0,1,1,1,1,1,1,1,0,1,1,0,1,1,1,1,0,1,0)
    ),
    pib2$Rama
  )
  names(levels)[7] <- 'Rama'
  levels <- tidyr::pivot_longer(levels, -Rama, names_to='level')
  levels <- dplyr::filter(levels,
                          value == 1)
  levels$value <- NULL
  levels <- dplyr::mutate(levels,
                          level = stringr::str_remove(level, 'nivel'))
  order <- dplyr::bind_cols(
    data.frame(
      orden = c('01','0101','0102','02','0201','0202','020201','020202',
                '020203','020204','0203','0204','03','0301','0302','0303',
                '0304','0305','0306','0307','0308','030801','030802','0309',
                '030901','030902','0310','0311','04','05','06')
    ),
    pib2$Rama
  )
  names(order)[2] <- 'Rama'
  levels <- dplyr::left_join(levels, order)
  pib2 <- dplyr::left_join(levels, pib2)
  pib2 <- tidyr::pivot_longer(pib2,
                              -c('Rama', 'level', 'orden'),
                              names_to = 'fecha',
                              values_to = 'pib_acumulado', values_drop_na = T)
  pib2$fecha <- zoo::as.yearqtr(pib2$fecha)
  pib2$pib_acumulado <- as.numeric(pib2$pib_acumulado)

  pib <- dplyr::left_join(pib, pib2)
  rm(pib2)

  # Ponderacion actividad economica acumulado
  pib2 <- readxl::read_excel(file, sheet = 'PIB$_Trim_Acum', skip = 42, col_names = F)
  pib2 <- tidyr::drop_na(pib2, ...2)
  pib2 <- t(pib2)
  pib2[1,1] <- NA
  pib2[,1] <- stringr::str_remove(pib2[,1], '\\(p\\)')
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
  pib2 <- as.data.frame(pib2)
  pib2[1,1] <- 'Rama'
  names(pib2) <- pib2[1,]
  pib2 <- pib2[-1,]
  levels <- dplyr::bind_cols(
    data.frame(
      nivel0 = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1),
      nivel1 = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0),
      nivel2 = c(1,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0),
      nivel3 = c(1,0,0,0,1,1,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0),
      nivel4 = c(0,1,1,0,1,0,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,0,0,1,0,0,1,1,0,1,0),
      nivel5 = c(0,1,1,0,1,0,1,1,1,1,1,1,0,1,1,1,1,1,1,1,0,1,1,0,1,1,1,1,0,1,0)
    ),
    pib2$Rama
  )
  names(levels)[7] <- 'Rama'
  levels <- tidyr::pivot_longer(levels, -Rama, names_to='level')
  levels <- dplyr::filter(levels,
                          value == 1)
  levels$value <- NULL
  levels <- dplyr::mutate(levels,
                          level = stringr::str_remove(level, 'nivel'))
  order <- dplyr::bind_cols(
    data.frame(
      orden = c('01','0101','0102','02','0201','0202','020201','020202',
                '020203','020204','0203','0204','03','0301','0302','0303',
                '0304','0305','0306','0307','0308','030801','030802','0309',
                '030901','030902','0310','0311','04','05','06')
    ),
    pib2$Rama
  )
  names(order)[2] <- 'Rama'
  levels <- dplyr::left_join(levels, order)
  pib2 <- dplyr::left_join(levels, pib2)
  pib2 <- tidyr::pivot_longer(pib2, -c('Rama', 'level', 'orden'), names_to = 'fecha', values_to = 'ponderacion_acumulada', values_drop_na = T)
  pib2$fecha <- zoo::as.yearqtr(pib2$fecha)
  pib2$ponderacion_acumulada <- as.numeric(pib2$ponderacion_acumulada)

  pib <- dplyr::left_join(pib, pib2)
  rm(pib2)

  # INDICE
  pib2 <- readxl::read_excel(file, sheet = 'PIBK_Trim', skip = 6, col_names = F)
  pib2 <- pib2[1:34,]
  pib2 <- tidyr::drop_na(pib2, ...2)
  pib2 <- t(pib2)
  pib2[1,1] <- NA
  pib2[,1] <- stringr::str_remove(pib2[,1], '\\(p\\)')
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
  pib2 <- as.data.frame(pib2)
  pib2[1,1] <- 'Rama'
  names(pib2) <- pib2[1,]
  pib2 <- pib2[-1,]
  levels <- dplyr::bind_cols(
    data.frame(
      nivel0 = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1),
      nivel1 = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0),
      nivel2 = c(1,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0),
      nivel3 = c(1,0,0,0,1,1,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0),
      nivel4 = c(0,1,1,0,1,0,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,0,0,1,0,0,1,1,0,1,0),
      nivel5 = c(0,1,1,0,1,0,1,1,1,1,1,1,0,1,1,1,1,1,1,1,0,1,1,0,1,1,1,1,0,1,0)
    ),
    pib2$Rama
  )
  names(levels)[7] <- 'Rama'
  levels <- tidyr::pivot_longer(levels, -Rama, names_to='level')
  levels <- dplyr::filter(levels,
                          value == 1)
  levels$value <- NULL
  levels <- dplyr::mutate(levels,
                          level = stringr::str_remove(level, 'nivel'))
  order <- dplyr::bind_cols(
    data.frame(
      orden = c('01','0101','0102','02','0201','0202','020201','020202',
                '020203','020204','0203','0204','03','0301','0302','0303',
                '0304','0305','0306','0307','0308','030801','030802','0309',
                '030901','030902','0310','0311','04','05','06')
    ),
    pib2$Rama
  )
  names(order)[2] <- 'Rama'
  levels <- dplyr::left_join(levels, order)
  pib2 <- dplyr::left_join(levels, pib2)
  pib2 <- tidyr::pivot_longer(pib2, -c('Rama', 'level', 'orden'), names_to = 'fecha', values_to = 'indice', values_drop_na = T)
  pib2$fecha <- zoo::as.yearqtr(pib2$fecha)
  pib2$indice <- as.numeric(pib2$indice)

  pib <- dplyr::left_join(pib, pib2)
  rm(pib2)

  # Tasas de crecimiento INDICE
  pib2 <- readxl::read_excel(file, sheet = 'PIBK_Trim', skip = 42, col_names = F)
  pib2 <- pib2[1:34,]
  pib2 <- tidyr::drop_na(pib2, ...6)
  pib2 <- t(pib2)
  pib2[1,1] <- NA
  pib2[,1] <- stringr::str_remove(pib2[,1], '\\(p\\)')
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
  pib2 <- as.data.frame(pib2)
  pib2[1,1] <- 'Rama'
  names(pib2) <- pib2[1,]
  pib2 <- pib2[-1,]
  levels <- dplyr::bind_cols(
    data.frame(
      nivel0 = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1),
      nivel1 = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0),
      nivel2 = c(1,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0),
      nivel3 = c(1,0,0,0,1,1,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0),
      nivel4 = c(0,1,1,0,1,0,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,0,0,1,0,0,1,1,0,1,0),
      nivel5 = c(0,1,1,0,1,0,1,1,1,1,1,1,0,1,1,1,1,1,1,1,0,1,1,0,1,1,1,1,0,1,0)
    ),
    pib2$Rama
  )
  names(levels)[7] <- 'Rama'
  levels <- tidyr::pivot_longer(levels, -Rama, names_to='level')
  levels <- dplyr::filter(levels,
                          value == 1)
  levels$value <- NULL
  levels <- dplyr::mutate(levels,
                          level = stringr::str_remove(level, 'nivel'))
  order <- dplyr::bind_cols(
    data.frame(
      orden = c('01','0101','0102','02','0201','0202','020201','020202',
                '020203','020204','0203','0204','03','0301','0302','0303',
                '0304','0305','0306','0307','0308','030801','030802','0309',
                '030901','030902','0310','0311','04','05','06')
    ),
    pib2$Rama
  )
  names(order)[2] <- 'Rama'
  levels <- dplyr::left_join(levels, order)
  pib2 <- dplyr::left_join(levels, pib2)
  pib2 <- tidyr::pivot_longer(pib2, -c('Rama', 'level', 'orden'), names_to = 'fecha', values_to = 'tasas_crecimiento', values_drop_na = T)
  pib2$fecha <- zoo::as.yearqtr(pib2$fecha)
  pib2$tasas_crecimiento <- as.numeric(pib2$tasas_crecimiento)

  pib <- dplyr::left_join(pib, pib2)
  rm(pib2)

  # Incidencia INDICE
  pib2 <- readxl::read_excel(file, sheet = 'PIBK_Trim', skip = 78, col_names = F)
  pib2 <- pib2[1:34,]
  pib2 <- tidyr::drop_na(pib2, ...6)
  pib2 <- t(pib2)
  pib2[1,1] <- NA
  pib2[,1] <- stringr::str_remove(pib2[,1], '\\(p\\)')
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
  pib2 <- as.data.frame(pib2)
  pib2[1,1] <- 'Rama'
  names(pib2) <- pib2[1,]
  pib2 <- pib2[-1,]
  levels <- dplyr::bind_cols(
    data.frame(
      nivel0 = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1),
      nivel1 = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0),
      nivel2 = c(1,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0),
      nivel3 = c(1,0,0,0,1,1,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0),
      nivel4 = c(0,1,1,0,1,0,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,0,0,1,0,0,1,1,0,1,0),
      nivel5 = c(0,1,1,0,1,0,1,1,1,1,1,1,0,1,1,1,1,1,1,1,0,1,1,0,1,1,1,1,0,1,0)
    ),
    pib2$Rama
  )
  names(levels)[7] <- 'Rama'
  levels <- tidyr::pivot_longer(levels, -Rama, names_to='level')
  levels <- dplyr::filter(levels,
                          value == 1)
  levels$value <- NULL
  levels <- dplyr::mutate(levels,
                          level = stringr::str_remove(level, 'nivel'))
  order <- dplyr::bind_cols(
    data.frame(
      orden = c('01','0101','0102','02','0201','0202','020201','020202',
                '020203','020204','0203','0204','03','0301','0302','0303',
                '0304','0305','0306','0307','0308','030801','030802','0309',
                '030901','030902','0310','0311','04','05','06')
    ),
    pib2$Rama
  )
  names(order)[2] <- 'Rama'
  levels <- dplyr::left_join(levels, order)
  pib2 <- dplyr::left_join(levels, pib2)
  pib2 <- tidyr::pivot_longer(pib2, -c('Rama', 'level', 'orden'), names_to = 'fecha', values_to = 'incidencia', values_drop_na = T)
  pib2$fecha <- zoo::as.yearqtr(pib2$fecha)
  pib2$incidencia <- as.numeric(pib2$incidencia)

  pib <- dplyr::left_join(pib, pib2)
  rm(pib2)

  # INDICE ACUMULADO
  pib2 <- readxl::read_excel(file, sheet = 'PIBK_Trim_Acum', skip = 6, col_names = F)
  pib2 <- pib2[1:34,]
  pib2 <- tidyr::drop_na(pib2, ...2)
  pib2 <- t(pib2)
  pib2[1,1] <- NA
  pib2[,1] <- stringr::str_remove(pib2[,1], '\\(p\\)')
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
  pib2 <- as.data.frame(pib2)
  pib2[1,1] <- 'Rama'
  names(pib2) <- pib2[1,]
  pib2 <- pib2[-1,]
  levels <- dplyr::bind_cols(
    data.frame(
      nivel0 = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1),
      nivel1 = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0),
      nivel2 = c(1,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0),
      nivel3 = c(1,0,0,0,1,1,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0),
      nivel4 = c(0,1,1,0,1,0,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,0,0,1,0,0,1,1,0,1,0),
      nivel5 = c(0,1,1,0,1,0,1,1,1,1,1,1,0,1,1,1,1,1,1,1,0,1,1,0,1,1,1,1,0,1,0)
    ),
    pib2$Rama
  )
  names(levels)[7] <- 'Rama'
  levels <- tidyr::pivot_longer(levels, -Rama, names_to='level')
  levels <- dplyr::filter(levels,
                          value == 1)
  levels$value <- NULL
  levels <- dplyr::mutate(levels,
                          level = stringr::str_remove(level, 'nivel'))
  order <- dplyr::bind_cols(
    data.frame(
      orden = c('01','0101','0102','02','0201','0202','020201','020202',
                '020203','020204','0203','0204','03','0301','0302','0303',
                '0304','0305','0306','0307','0308','030801','030802','0309',
                '030901','030902','0310','0311','04','05','06')
    ),
    pib2$Rama
  )
  names(order)[2] <- 'Rama'
  levels <- dplyr::left_join(levels, order)
  pib2 <- dplyr::left_join(levels, pib2)
  pib2 <- tidyr::pivot_longer(pib2, -c('Rama', 'level', 'orden'), names_to = 'fecha', values_to = 'indice_acumulado', values_drop_na = T)
  pib2$fecha <- zoo::as.yearqtr(pib2$fecha)
  pib2$indice_acumulado <- as.numeric(pib2$indice_acumulado)

  pib <- dplyr::left_join(pib, pib2)
  rm(pib2)

  # Tasas de crecimiento INDICE ACUMULADO
  pib2 <- readxl::read_excel(file, sheet = 'PIBK_Trim_Acum', skip = 42, col_names = F)
  pib2 <- pib2[1:34,]
  pib2 <- tidyr::drop_na(pib2, ...6)
  pib2 <- t(pib2)
  pib2[1,1] <- NA
  pib2[,1] <- stringr::str_remove(pib2[,1], '\\(p\\)')
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
  pib2 <- as.data.frame(pib2)
  pib2[1,1] <- 'Rama'
  names(pib2) <- pib2[1,]
  pib2 <- pib2[-1,]
  levels <- dplyr::bind_cols(
    data.frame(
      nivel0 = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1),
      nivel1 = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0),
      nivel2 = c(1,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0),
      nivel3 = c(1,0,0,0,1,1,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0),
      nivel4 = c(0,1,1,0,1,0,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,0,0,1,0,0,1,1,0,1,0),
      nivel5 = c(0,1,1,0,1,0,1,1,1,1,1,1,0,1,1,1,1,1,1,1,0,1,1,0,1,1,1,1,0,1,0)
    ),
    pib2$Rama
  )
  names(levels)[7] <- 'Rama'
  levels <- tidyr::pivot_longer(levels, -Rama, names_to='level')
  levels <- dplyr::filter(levels,
                          value == 1)
  levels$value <- NULL
  levels <- dplyr::mutate(levels,
                          level = stringr::str_remove(level, 'nivel'))
  order <- dplyr::bind_cols(
    data.frame(
      orden = c('01','0101','0102','02','0201','0202','020201','020202',
                '020203','020204','0203','0204','03','0301','0302','0303',
                '0304','0305','0306','0307','0308','030801','030802','0309',
                '030901','030902','0310','0311','04','05','06')
    ),
    pib2$Rama
  )
  names(order)[2] <- 'Rama'
  levels <- dplyr::left_join(levels, order)
  pib2 <- dplyr::left_join(levels, pib2)
  pib2 <- tidyr::pivot_longer(pib2, -c('Rama', 'level', 'orden'), names_to = 'fecha', values_to = 'tasa_crecimiento_acumulada', values_drop_na = T)
  pib2$fecha <- zoo::as.yearqtr(pib2$fecha)
  pib2$tasa_crecimiento_acumulada <- as.numeric(pib2$tasa_crecimiento_acumulada)

  pib <- dplyr::left_join(pib, pib2)
  rm(pib2)

  # Incidencia INDICE ACUMULADO
  pib2 <- readxl::read_excel(file, sheet = 'PIBK_Trim_Acum', skip = 78, col_names = F)
  pib2 <- pib2[1:34,]
  pib2 <- tidyr::drop_na(pib2, ...6)
  pib2 <- t(pib2)
  pib2[1,1] <- NA
  pib2[,1] <- stringr::str_remove(pib2[,1], '\\(p\\)')
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
  pib2 <- as.data.frame(pib2)
  pib2[1,1] <- 'Rama'
  names(pib2) <- pib2[1,]
  pib2 <- pib2[-1,]
  levels <- dplyr::bind_cols(
    data.frame(
      nivel0 = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1),
      nivel1 = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0),
      nivel2 = c(1,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0),
      nivel3 = c(1,0,0,0,1,1,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0),
      nivel4 = c(0,1,1,0,1,0,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,0,0,1,0,0,1,1,0,1,0),
      nivel5 = c(0,1,1,0,1,0,1,1,1,1,1,1,0,1,1,1,1,1,1,1,0,1,1,0,1,1,1,1,0,1,0)
    ),
    pib2$Rama
  )
  names(levels)[7] <- 'Rama'
  levels <- tidyr::pivot_longer(levels, -Rama, names_to='level')
  levels <- dplyr::filter(levels,
                          value == 1)
  levels$value <- NULL
  levels <- dplyr::mutate(levels,
                          level = stringr::str_remove(level, 'nivel'))
  order <- dplyr::bind_cols(
    data.frame(
      orden = c('01','0101','0102','02','0201','0202','020201','020202',
                '020203','020204','0203','0204','03','0301','0302','0303',
                '0304','0305','0306','0307','0308','030801','030802','0309',
                '030901','030902','0310','0311','04','05','06')
    ),
    pib2$Rama
  )
  names(order)[2] <- 'Rama'
  levels <- dplyr::left_join(levels, order)
  pib2 <- dplyr::left_join(levels, pib2)
  pib2 <- tidyr::pivot_longer(pib2, -c('Rama', 'level', 'orden'), names_to = 'fecha', values_to = 'incidencia_acumulada', values_drop_na = T)
  pib2$fecha <- zoo::as.yearqtr(pib2$fecha)
  pib2$incidencia_acumulada <- as.numeric(pib2$incidencia_acumulada)

  pib <- dplyr::left_join(pib, pib2)
  rm(pib2, levels, order)
  unlink(file)
  pib
}
