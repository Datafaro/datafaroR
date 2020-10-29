
print(Sys.time()) # Para el log del cron.

library(domar)
library(googlesheets4)

myEmail <- 'drdsdaniel@gmail.com'

# Esto es porque tengo users distintos en la máquina de prueba y el servidor.
user <- if(file.exists('/home/drdsdaniel/domar/domar_run.R')){
  'drdsdaniel'
}else{
  'daniel'
}

# Auth
gs4_auth(
  cache = paste0('/home/', user, '/.secrets'),
  email = 'drdsdaniel@gmail.com'
  )

# imae_month
secure_gss(myEmail, paste0('/home/', user,'/domar/domar.email'), 'imae_month', kvars = c('fecha', 'serie', 'Indice'))
