deuda_publica_2020 <- function(indicador){
  codigo <- NULL
  fecha <- NULL
  file <- downloader(indicador)
  datos <- readxl::read_excel(file, skip = 11, col_names = F)
  datos <- datos[1:56,]
  datos <- dplyr::bind_cols(
    c(
      '',
      '',
      '',
      '',
      '',
      '2111',
      '2112',
      '2113',
      '2114',
      '2115',
      '211',
      '',
      '',
      '2121',
      '2122',
      '2123',
      '2124',
      '21241',
      '2125',
      '2126',
      '21261',
      '2127',
      '212',
      '',
      '21',
      '',
      '',
      '221',
      '222',
      '223',
      '22',
      '',
      '2',
      '',
      '',
      '1.1',
      '1.2',
      '1.3',
      '1.4',
      '1.5',
      '1.6',
      '',
      '1',
      '',
      '',
      '',
      '',
      '',
      '',
      '',
      '',
      '0',
      '',
      '',
      '',
      ''
    ),
    datos
  )
  datos <- as.data.frame(t(datos))
  datos <- datos[!is.na(datos$V1),]
  datos <- as.data.frame(t(datos))
  names(datos) <- datos[1,]
  datos <- datos[-1,]
  names(datos)[1:2] <- c('codigo', 'cuenta')
  datos <- dplyr::filter(datos, codigo != '')
  datos <- tidyr::pivot_longer(datos, -c(1:2), names_to = 'fecha', values_to = 'valor')
  datos <- dplyr::mutate(datos,
                         fecha = stringr::str_remove_all(fecha, '\\*')
                         )
  datos
}
