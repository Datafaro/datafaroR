#util para el log del cron.
message(Sys.time())
message('




        ')



library(domar)
library(googlesheets4)

# Conexión a base de datos
source('db_con.R', local = T)


myEmail <- 'drdsdaniel@gmail.com'

# Esto es porque tengo users distintos en la máquina de prueba y el servidor.
user <- if(file.exists('/home/drdsdaniel/domar/domar_run.R')){
  'drdsdaniel'
}else{
  'daniel'
}

creds_file <- if(interactive()){
  'domar.email'
} else {
  paste0('/home/', user,'/domar/domar.email')
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
  name = paste0(user, '-domar_run.log'),
  overwrite = TRUE)

# IMAE Mensual
secure_gss(myEmail, paste0('/home/', user,'/domar/domar.email'), 'imae_month', kvars = c('fecha', 'serie', 'Indice'))


# Database storage
message('






        DATABASE STORAGE')
lista <- DBI::dbReadTable(db_conn, 'api_domar_list')
lista <- lista[lista$indicator_state == 2,]

for (id in lista$id) {
  secure_dbs(db_conn, id, myEmail, creds_file)
}
