library(domar)
library(googlesheets4)

myEmail <- 'drdsdaniel@gmail.com'

# Auth
gs4_auth(
  cache = '../.secrets',
  email = 'drdsdaniel@gmail.com'
  )

# imae_month
secure_gss(myEmail, 'domar.email', 'imae_month', kvars = c('fecha', 'serie', 'Indice'))
