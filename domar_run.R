#util para el log del cron.
message(Sys.time())
message('




        ')



library(domar)

# Conexión a base de datos
source('db_con.R', local = T)

myEmail <- 'drdsdaniel@gmail.com'

# Esto es porque tengo users distintos en la máquina de prueba y el servidor.
user <- if(file.exists('/home/drdsdaniel/domar/domar_run.R')){'drdsdaniel'}else{'daniel'}

creds_file <- if(interactive()){'domar.email'} else {paste0('/home/', user,'/domar/domar.email')}

lista <- DBI::dbReadTable(db_conn, 'api_indicator')
indicador <- DBI::dbReadTable(db_conn, 'api_indicator')
indicador <- indicador[indicador$id == 'deuda_publica_2020',]
lista <- lista[lista$indicator_state == 2,]

for (id in lista$id) {
  secure_dbs(db_conn, id, myEmail, creds_file)
}
