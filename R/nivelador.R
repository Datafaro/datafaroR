nivelador <- function(tbl){
  orden <- NULL
  tmax <- trunc(max(as.numeric(tbl$orden), na.rm = T))
  tbl <- tbl %>%
    dplyr::mutate(
      nivel0 = stringr::str_detect(orden, "\\-"),
      nivel = dplyr::if_else(
        tmax %in% c(99, 999, 9999) & tidyr::replace_na(as.numeric(orden), 0) >= tmax, tmax,
        stringr::str_count(orden, stringr::regex("\\.")) + 1
      ),
      nivel = dplyr::if_else(nivel0, -1*nivel, nivel),
      nivel0 = NULL
    )
  tbl$orden <- c(1:nrow(tbl))
  tbl
}

#' Ordenes y niveles de los indicadores
#'
#'
#' @format [data.frame]
"nvl_balanza_pagos"
"nvl_balanza_pagos_trim"
"nvl_pib_gasto"
