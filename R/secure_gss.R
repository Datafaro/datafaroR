#' Secure Google Sheet storage version
#'
#' This is a wraper function to tryCatch for \code{\link{gsheet_storage}}.
#'   If error, then an email is send to specified email address.
#'
#' @param to_email email for send the error
#' @param creds_file_name  a file name pointer. See \code{\link[blastula]{creds_file}}
#' @param id See \code{\link{gsheet_storage}}
#' @param ... See \code{\link{gsheet_storage}}
#'#'
#' @export
#'
#' @examples
#' \dontrun{
#' secure_gss(email,
#'  'domar.email',
#'   'imae_month',
#'    kvars = c('fecha', 'serie', 'Indice'))
#' }
secure_gss <- function(to_email, creds_file_name, id, ...){
  tryCatch({
    gsheet_storage(id, ...)
    },
    error = function(e){
      email <-
        blastula::compose_email(
          body = blastula::md(e),
          footer = blastula::md(
            c(
              "domar"
            )
          )
        )
      blastula::smtp_send(
        email,
        to = to_email,
        from = 'domar.email@gmail.com',
        subject = paste0("domar error tracking: ", id),
        credentials = blastula::creds_file(creds_file_name)
      )
      Sys.sleep(100) # TODO. Evaluate if error is related with Google Sheet cuota limits.
    }
  )
}
