balanza_pagos_mensual <- function(indicador){
  V1 <- NULL
  file <- downloader(indicador)
  datos <- readxl::read_excel(file, col_names = F)
  datos <- datos[1:119,]
  datos <- as.data.frame(t(datos))
  datos <- datos[!duplicated(datos$V1, incomparables = NA),]
  datos$V1 <- stringr::str_remove(datos$V1, '\\*')
  datos <- tidyr::fill(datos, V1)
  datos[1:2, 1:2] <- NA
  datos <- Dmisc::vars_to_date(datos, ano = 1, mes = 2)
  datos <- as.data.frame(t(datos))
  datos <- dplyr::bind_cols(
    c(rep('GOBIERNO CENTRAL PRESUPUESTARIO', 96),
      rep('RESTO DEL SECTOR P\u00daBLICO NO FINANCIERO', 19),
      rep('SECTOR P\u00daBLICO NO FINANCIERO', 3)),
    c('', '',
      rep('Transacciones que afectan al patrimonio neto', 47),
      '',
      rep('Transacciones en activos no financieros', 15),
      rep('Transacciones en activos y pasivos financieros (financiamiento)', 29),
      rep('', 5),
      rep('Transacciones en activos y pasivos financieros (financiamiento)', 14),
      rep('', 5)),
    c('', '', '', '1', '11', '111', '1111', '1112', '1113', '113', '114', '1141',
      '11411', '11414', '1142', '1144', '1145', '115', '116', '12', '13', '14',
      '2', '21', '211', '212', '22', '24', '241', '242', '242.1', '25', '251',
      '251.1',
      '252',
      '26',
      '261',
      '262',
      '263',
      '2631',
      '2632',
      '27',
      '28',
      '282',
      '2821',
      '2821.1',
      '2822',
      '2822.1',
      'GOB',
      '',
      '',
      '31',
      '311',
      '313',
      '314',
      '',
      '315',
      '',
      '2M',
      '',
      'PB',
      'PB',
      'NLB',
      'NLB',
      '',
      '',
      '32',
      '321',
      '3212',
      '3215',
      '3218',
      '322',
      '3225',
      '33',
      '331',
      '3313',
      '3313.1',
      '3313.11',
      '3313.12',
      '3313.13',
      '3314',
      '3314.1',
      '3314.2',
      '3315',
      '332',
      '3323',
      '3323.1',
      '3323.2',
      '3323.3',
      '3324',
      '3324.1',
      '3324.2',
      '3324.3',
      'NLBz',
      '',
      '',
      '',
      'NLB',
      'NLB',
      '',
      '32',
      '321',
      '3212',
      '3213',
      '3218',
      '322',
      '33',
      '331',
      '3313',
      '3314',
      '3315',
      '332',
      '3324',
      '',
      '',
      '',
      'NLB',
      'NLB'
    ),
    datos
  )
  datos$V1 <- NULL
  datos <- datos[!is.na(datos$V3),]
  datos[1,1:3] <- NA
  names(datos) <- datos[1,]
  datos <- datos[-1,]
  names(datos)[1:4] <- c('sector', 'tipo_transaccion', 'codigo', 'cuenta')
  datos <- tidyr::pivot_longer(datos, -c(1:4), names_to = 'fecha', values_to = 'valor')
  datos
}
