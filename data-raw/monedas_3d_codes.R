monedas_3d_codes <- tibble::tribble(
  ~moneda, ~code,
  "DOLAR AUSTRALIANO", "AUD",
  "REAL BRASILENO", "BRL",
  "DOLAR CANADIENSE", "CAD",
  "FRANCO SUIZO", "CHF",
  "YUAN CHINO", "CNY",
  "DERECHO ESPECIAL DE GIRO", "XDR",
  "CORONA DANESA", "DKK",
  "EURO", "EUR",
  "LIBRA ESTERLINA", "GBP",
  "YEN JAPONES", "JPY",
  "CORONA NORUEGA", "NOK",
  "LIBRA ESCOCESA", "GBP",
  "CORONA SUECA", "SEK",
  "DOLAR ESTADOUNIDENSE", "USD",
  "BOLIVAR FUERTE VENEZOLANO", "VES"
)

usethis::use_data(monedas_3d_codes, overwrite = TRUE)
