#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL




.getid <- function(){

  if (exists(".DatafaroSessionID")) {
    return(.DatafaroSessionID)
  } else {
    tryCatch({
      id <- Sys.getpid()
      return(id)
    }, error = function(e) {
      return(uuid::UUIDgenerate())
    })
  }
}

.get_cache_board <- function(){
  pins::board_folder(path.expand("~/.DatafaroCache"))
}
