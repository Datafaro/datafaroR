library(domar)
library(googlesheets4)

email <- 'drdsdaniel@gmail.com'

# Auth
gs4_auth('drdsdaniel@gmail.com') # Cualquier email con acceso a la lista domar.

# imae_month
secure_gss(email, 'domar.email', 'imae_month', kvars = c('fecha', 'serie', 'Indice'))
