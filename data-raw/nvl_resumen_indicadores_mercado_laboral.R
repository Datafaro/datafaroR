nvl_resumen_indicadores_mercado_laboral <- tibble::tribble(
  ~orden, ~indicador,
  "1", "Total Global de Participación",
  "2", "Tasa de Ocupación",
  "2.1", "Tasa de subocupación por horas",
  "3", "Sector Formal",
  "4", "Sector Informal",
  "5", "Informalidad Total",
  "6", "SU1: Tasa de Desocupación 2/",
  "6.1", "  Tasa de cesantía",
  "6.2", "Tasa de nuevos",
  "7", "SU2: Desocupación y Subocupación 3/",
  "8", "  SU3: Desocupación y Fuerza de Trabajo Potencial 4/",
  "9", "  SU4: Desocupación + Subocupación + Fuerza de Trabajo Potencial 5/",
  "10", "  Tasa de Inactividad",
  "11", "Tasa de Desocupación (abiertos con iniciadores)"
) %>% nivelador()

usethis::use_data(nvl_resumen_indicadores_mercado_laboral, overwrite = TRUE)
