#' DOMAR indicators list
#'
#' @return data.frame corresponding the list of variables in domar
#'
#'@details
#'
#'  Don't use if you don't have access to \code{dmr_list} file.
#'  See \url{https://docs.google.com/spreadsheets/d/1svANACeWShYC--wm7wwSKaUn_bF_xXuF81SnV8yMeb8/edit#gid=0}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dmr_list <- domar_list()
#' }
domar_list <- function() {
 googlesheets4::read_sheet('1svANACeWShYC--wm7wwSKaUn_bF_xXuF81SnV8yMeb8', 'Index')
}
