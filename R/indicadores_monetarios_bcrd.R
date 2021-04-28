indicadores_monetarios_bcrd <- function(indicador) {
  V1 <- NULL
  fecha <- NULL
  valor <- NULL
  file <- downloader(indicador)
  datos <- readxl::read_excel(
    file,
    skip = 4,
    col_names = F
  )
  datos <- datos[!is.na(datos$...1),]
  datos <- datos[1:44,]
  datos <- dplyr::bind_cols(
    c(
      '',
      rep('Indicadores BCRD', 11),
      rep('Base Monetaria', 13),
      rep('Agregados monetarios', 13),
      rep('Multiplicadores monetarios', 5),
      'Tasa de cambio'
    ),
    c('',
      '1',
      '2',
      '3',
      '4',
      '4.1',
      '4.11',
      '4.2',
      '4.3',
      '5',
      '5.1',
      '6',
      '',
      '7',
      '7.1',
      '7.11',
      '7.2',
      '7.3',
      '8',
      '8.1',
      '8.2',
      '8.3',
      '8.4',
      '8.5',
      '8.6',
      '',
      '9',
      '9.1',
      '9.2',
      '10',
      '10.1',
      '10.2',
      '10.3',
      '10.4',
      '11',
      '11.1',
      '11.2',
      '11.3',
      '',
      '',
      '12.1',
      '12.2',
      '12.3',
      '13'
    ),
    datos
  )
  datos[1, 1:3] <- NA
  datos <- as.data.frame(t(datos))
  datos <- dplyr::mutate(datos,
                         V1 = stringr::str_replace(tolower(V1), 'ene', 'Jan'),
                         V1 = stringr::str_replace(tolower(V1), 'abr', 'Apr'),
                         V1 = stringr::str_replace(tolower(V1), 'ago', 'Aug'),
                         V1 = stringr::str_replace(tolower(V1), 'dic', 'Dec'),
                         V1 = stringr::str_replace_all(V1, '-', ' '),
                         V1 = stringr::str_remove_all(V1, '\\*'),
                         V1 = dplyr::case_when(
                           is.na(as.Date(as.numeric(V1), origin = "1899-12-30")) ~ V1,
                           !is.na(as.Date(as.numeric(V1), origin = "1899-12-30")) ~ as.character(as.Date(as.numeric(V1), origin = "1899-12-30")),
                         ),
                         V1 = stringr::str_remove_all(V1, '\\.'),
                         V1 = dplyr::case_when(
                           stringr::str_count(V1, ' ') == 2 ~ as.character(as.Date(V1, '%d %b %y')),
                           stringr::str_count(V1, ' ') == 1 ~ as.character(as.Date(paste('01', V1), '%d %b %y')),
                           TRUE ~ V1
                         )
                         )
  datos <- as.data.frame(t(datos))
  names(datos) <- make.names(datos[1,])
  names(datos)[1:3] <- c('grupo', 'codigo', 'indicador')
  datos <- datos[-1,]
  datos <- datos[datos$codigo != '',]
  datos <- tidyr::pivot_longer(datos, -c(1:3), names_to = 'fecha', values_to = 'valor')
  datos <- dplyr::mutate(datos,
                         fecha = stringr::str_remove(fecha, 'X'),
                         fecha = stringr::str_replace_all(fecha, '\\.', '-'),
                         valor = as.numeric(valor)
                         )
  datos
}
