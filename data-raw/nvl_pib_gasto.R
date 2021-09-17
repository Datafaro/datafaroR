nvl_pib_gasto <- tibble::tribble(
  ~orden, ~componente,
  '1',	'Consumo Final',
  '1.1',	'Consumo Privado',
  '1.2',	'Consumo Público',
  '1',	'Formación Bruta de Capital',
  '1.1',	'Formación Bruta de Capital Fijo',
  '1.2',	'Variación de Existencias',
  '1',	'Exportaciones',
  '-1',	'Importaciones',
  '0',	'Producto Interno Bruto'
) %>%
  nivelador()

usethis::use_data(nvl_pib_gasto, overwrite = TRUE)
