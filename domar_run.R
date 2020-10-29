library(domar)
library(googlesheets4)

myEmail <- 'drdsdaniel@gmail.com'

# Auth
gs4_auth(
  cache = '/home/daniel/.secrets',
  email = 'drdsdaniel@gmail.com'
  )

# imae_month
secure_gss(myEmail, '/home/daniel/domar/domar.email', 'imae_month', kvars = c('fecha', 'serie', 'Indice'))
