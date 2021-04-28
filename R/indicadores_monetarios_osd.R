indicadores_monetarios_osd <- function(indicador) {
  V1 <- NULL
  fecha <- NULL
  valor <- NULL
  file <- downloader(indicador)
  datos <- readxl::read_excel(
    file,
    skip = 4,
    col_names = F
  )
  datos <- datos[!is.na(datos$...2),]
  #datos <- datos[1:44,]
  datos <- dplyr::bind_cols(
    c('',
      '1',
      '11',
      '12',
      '2',
      '21',
      '22',
      '3',
      '31',
      '32',
      '4',
      '401',
      '4011',
      '4012',
      '4013',
      '40131',
      '40132',
      '41',
      '411',
      '4111',
      '4112',
      '4113',
      '41131',
      '41132',
      '42',
      '421',
      '4211',
      '4212',
      '4213',
      '42131',
      '42132',
      '43',
      '431',
      '4311',
      '44',
      '441',
      '4411',
      '5',
      '501',
      '5011',
      '5012',
      '5013',
      '50131',
      '50132',
      '51',
      '511',
      '5111',
      '5112',
      '5113',
      '51131',
      '51132',
      '52',
      '521',
      '5211',
      '5212',
      '5213',
      '52131',
      '52132',
      '53',
      '531',
      '5311',
      '54',
      '541',
      '5411',
      '6',
      '61',
      '62',
      '63',
      '7'
    ),
    datos
  )
  datos[1, 1:2] <- NA
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
  names(datos)[1:2] <- c('codigo', 'indicador')
  datos <- datos[-1,]
  datos <- datos[datos$codigo != '',]
  datos <- tidyr::pivot_longer(datos, -c(1:2), names_to = 'fecha', values_to = 'valor')
  datos <- dplyr::mutate(datos,
                         fecha = stringr::str_remove(fecha, 'X'),
                         fecha = stringr::str_replace_all(fecha, '\\.', '-'),
                         valor = as.numeric(valor)
  )
  datos
}
