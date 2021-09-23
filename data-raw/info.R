lh = FALSE

if (lh){
  info <- list(
    domar_url = "http://localhost:8000",
    token = "1c156b0f-d67c-4752-b859-efe5eab6cb65"
  )
} else {
  info <- list(
    domar_url = "http://142.93.193.53",
    token = "77ae09d6-626f-4d76-b7b1-3a3ca4444275"
  )
}
usethis::use_data(info, overwrite = TRUE)
