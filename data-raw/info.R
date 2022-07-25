lh = TRUE

if (lh){
  info <- list(
    domar_url = "http://localhost:8000",
    token = "e834b8ce-edd7-44bc-ba08-cee955c4a365"
  )
} else {
  info <- list(
    domar_url = "http://142.93.193.53",
    token = "77ae09d6-626f-4d76-b7b1-3a3ca4444275"
  )
}
usethis::use_data(info, overwrite = TRUE)
