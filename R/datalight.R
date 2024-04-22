
#' Convert a Datafaro's datalight list object to a datalight object
#'
#' @param data A Datafaro's datalight list object
#'
#' @return a datalight object
#' @export
#'
#' @examples
#' \dontrun{
#'   .res <- list(data = mtcars, dictionary = list(), metadata = list(next_update = Sys.Date() + 30), cached = Sys.Date(), slug = "mtcars")
#'   mi_tibble <- as_datalight(.res)
#'   print(mi_tibble)
#'}
as_datalight <- function(data) {
  json_data <- jsonlite::toJSON(data, auto_unbox = TRUE)
  parsed_data <- jsonlite::fromJSON(json_data, simplifyDataFrame = TRUE)
  new_data <- as_tibble(parsed_data$data) %>% type.convert(as.is = TRUE)
  if ('date' %in% names(new_data)) new_data$date <- lubridate::as_date(new_data$date)
  class(new_data) <- c("datalight", class(new_data))
  if (!is.null(data$dictionary)) attr(new_data, "dict") <- data$dictionary
  if (!is.null(data$metadata)) attr(new_data, "metadata") <- data$metadata
  if (!is.null(data$Cached)) attr(new_data, "Cached") <- data$Cached
  new_data
}




#' Print a datalight object
#'
#' @param x A Datafaro's datalight json object
#' @param ... Other arguments passed on to methods
#'
#' @return a print of the object. Invisible x
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   .res <- jsonlite::toJSON(list(data = mtcars, dictionary = list(), metadata = list(next_update = Sys.Date() + 30), cached = Sys.Date(), slug = "mtcars"))
#'   mi_tibble <- as_datalight(.res)
#'   print(mi_tibble)
#'}
print.datalight <- function(x, ...) {
  NextMethod(x, ...)
  cli::cli_text("Datafaro's datalight: ", attr(x, 'metadata')$slug)
  cli::cli_text("Full name: ", attr(x, 'metadata')$name)
  if (!is.null(attr(x, 'Cached'))){
    cli::cli_text("Cached: ", attr(x, 'Cached'))
  }
  cli::cli_text("Next update: ", attr(x, 'metadata')$next_update)

  invisible(x)
}


