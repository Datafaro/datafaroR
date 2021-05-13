ipc_mensual_2020 <- function(indicador){
  `...2` <- NULL
  `...1` <- NULL
  file <- downloader(indicador)
  datos <- readxl::read_excel(file, col_names = F)
  datos <- tidyr::drop_na(datos, ...2)
  datos <- tidyr::fill(datos, ...1)
  datos <- datos[,1:7]
  names(datos) <- c('ano',
                    'mes',
                    'indice',
                    'variacion_mensual',
                    'variacion_con_diciembre',
                    'variacion_anual',
                    'variacion_promedio_12_meses')
  datos <- Dmisc::vars_to_date(datos, year = 1, month = 2)
  datos
}
