nvl_pib_gasto <- tibble::tribble(
  ~orden, ~componente,
  '1',	'Consumo Final',
  '1.1',	'Consumo Privado',
  '1.2',	'Consumo P\\u00FAblico',
  '1',	'Formaci\\u00F3n Bruta de Capital',
  '1.1',	'Formaci\\u00F3n Bruta de Capital Fijo',
  '1.2',	'Variaci\\u00F3n de Existencias',
  '1',	'Exportaciones',
  '-1',	'Importaciones',
  '0',	'Producto Interno Bruto'
) %>%
  nivelador()

usethis::use_data(nvl_pib_gasto, overwrite = TRUE)
