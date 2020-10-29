
print(Sys.time()) #util para el log del cron.

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
googlesheets4::gs4_auth(
  cache = paste0('/home/', user, '/.secrets'),
  email = 'drdsdaniel@gmail.com'
)

# Upload last domar_run.log file to Google Drive
googledrive::drive_auth(
  cache = paste0('/home/', user, '/.secrets'),
  email = 'drdsdaniel@gmail.com'
)
googledrive::drive_upload(
  media = paste0('/home/', user,'/domar/domar_run.log'),
  path =  'Databases/domar/cron_logs/',
  name = paste0(user, '-domar_run.log'))

# imae_month
secure_gss(myEmail, paste0('/home/', user,'/domar/domar.email'), 'imae_month', kvars = c('fecha', 'serie', 'Indice'))
