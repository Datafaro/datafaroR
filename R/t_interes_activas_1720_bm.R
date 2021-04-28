t_interes_activas_1720_bm <- function(indicador){
  `...2` <- NULL
  ano <- NULL
  fecha <- NULL
  file <- downloader(indicador)
  datos <- readxl::read_excel(file, col_names = F)
  datos <- tidyr::drop_na(datos, ...2)
  datos$ano <- datos$...1
  datos <- dplyr::relocate(datos, ano)
  datos <- dplyr::mutate(datos,
                         ano = stringr::str_remove(ano, '\\*'),
                         ano = stringr::str_remove(ano, ' 1/'))
  datos$ano <- as.numeric(datos$ano)
  datos <- tidyr::fill(datos, ano)
  datos <- dplyr::filter(datos, is.na(ano) | ano > 2016)
  datos$ano <- as.character(datos$ano)
  datos[2, 1] <- 'ano'
  datos[2, 2] <- 'mes'
  plazos <- datos[-1,1:8]
  names(plazos) <- plazos[1,]
  plazos <- plazos[-1,]
  plazos <- tidyr::pivot_longer(plazos, -c('ano', 'mes'), names_to = 'categoria', values_to = 'valor')
  plazos$variable <- 'plazos'
  promedio <- datos[-1,c(1:2, 9:10)]
  names(promedio) <- promedio[1,]
  promedio <- promedio[-1,]
  promedio <- tidyr::pivot_longer(promedio, -c('ano', 'mes'), names_to = 'categoria', values_to = 'valor')
  promedio$variable <- 'promedio'
  sectores <- datos[-1,c(1:2, 11:13)]
  names(sectores) <- sectores[1,]
  sectores <- sectores[-1,]
  sectores <- tidyr::pivot_longer(sectores, -c('ano', 'mes'), names_to = 'categoria', values_to = 'valor')
  sectores$variable <- 'sectores'
  preferencial <- datos[-1,c(1:2, 14:17)]
  names(preferencial) <- preferencial[1,]
  preferencial <- preferencial[-1,]
  preferencial <- tidyr::pivot_longer(preferencial, -c('ano', 'mes'), names_to = 'categoria', values_to = 'valor')
  preferencial$variable <- 'preferencial'
  datos <- dplyr::bind_rows(
    plazos,
    promedio,
    sectores,
    preferencial
  )
  datos <- Dmisc::vars_to_date(datos, ano = 1, mes = 2)
  datos <- dplyr::filter(datos, !is.na(fecha))
  datos
}
