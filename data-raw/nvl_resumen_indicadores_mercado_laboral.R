nvl_resumen_indicadores_mercado_laboral <- tibble::tribble(
  ~orden, ~indicador,
  "1", "Total Global de Participaci\\u00F3n",
  "2", "Tasa de Ocupaci\\u00F3n",
  "2.1", "Tasa de subocupaci\\u00F3n por horas",
  "3", "Sector Formal",
  "4", "Sector Informal",
  "5", "Informalidad Total",
  "6", "SU1: Tasa de Desocupaci\\u00F3n 2/",
  "6.1", "  Tasa de cesant\\u00EDa",
  "6.2", "Tasa de nuevos",
  "7", "SU2: Desocupaci\\u00F3n y Subocupaci\\u00F3n 3/",
  "8", "  SU3: Desocupaci\\u00F3n y Fuerza de Trabajo Potencial 4/",
  "9", "  SU4: Desocupaci\\u00F3n + Subocupaci\\u00F3n + Fuerza de Trabajo Potencial 5/",
  "10", "  Tasa de Inactividad",
  "11", "Tasa de Desocupaci\\u00F3n (abiertos con iniciadores)"
) %>% nivelador()

usethis::use_data(nvl_resumen_indicadores_mercado_laboral, overwrite = TRUE)
