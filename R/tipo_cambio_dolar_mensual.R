tipo_cambio_dolar_mensual <- function(indicador){
  file <- downloader(indicador)
  datos <- readxl::read_excel(file, 'PromMensual', skip = 2)
  datos <- Dmisc::vars_to_date(datos, ano = 1, mes = 2)
  datos
}
