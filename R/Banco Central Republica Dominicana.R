# Sector real----



#' PIB por enfoque del gasto trimestral
#'
#'   \lifecycle{experimental}
#'
#' @param indicador vector de datos para este indicador en la lista de domar
#' @param metadata indica si se retornan los datos o la metadata del indicador
#'
#' @return data.frame con los datos del indicador en forma tabular
#'
#' @export
#'
#' @examples
#' \dontrun{
#' pib_gasto_trim()
#' }
pib_gasto_trim <- function(indicador = NULL, metadata = FALSE){
  if(metadata){
    return(
      tibble::tribble(
        ~col, ~name, ~unit, ~dtype,
        "orden", "Orden de los componentes", "", "int",
        "nivel", "Nivel de los componentes", "", "int",
        "componente", "Componente", "", "text",
        "date", "Fecha", "Trimestres", "qdate",
        "pib", "Valor del PIB", "Millones de RD$", "comma_f1",
        "ponderacion", "Ponderación por componente", "Porcentaje (%)", "f2",
        "pib_acumulado", "PIB acumulado", "Millones de RD$", "comma_f1",
        "ponderacion_acumulada", "Ponderación por componente (PIB acumulado)", "Porcentaje (%)", "f2",
        "ive_pib", "Índice de Valores Encadenados (IVE) del PIB", "Índice (2007=100)", "f1",
        "tc_pib", "Tasa de crecimiento PIB", "Porcentaje (%)", "f1",
        "incidencia", "Incidencia por componente del PIB", "", "f1",
        "ive_acumulado", "Índice de Valores Encadenados (IVE) - PIB Acumulado", "Índice (2007=100)", "f1",
        "tc_pib_acumulado", "Tasa de crecimiento - PIB Acumulado", "Porcentaje (%)", "f1",
        "incidencia_acumulada", "Incidencia por componente del PIB Acumulado", "", "f1"
      )
    )
  }
  if(is.null(indicador)){
    indicador <- c(
      original_url = "https://cdn.bancentral.gov.do/documents/estadisticas/sector-real/documents/pib_gasto_2007.xls",
      file_ext = "xls"
    )
  }
  `...2` <- NULL
  V1 <- NULL
  V2 <- NULL
  pibFile <- "/mnt/c/Users/drdsd/Downloads/pib_gasto_2007.xls"
  #pibFile <- downloader(indicador)
  pib <- readxl::read_excel(pibFile, sheet = 'PIB$_Trim', skip = 5, col_names = F)
  pib <- tidyr::drop_na(pib, ...2)
  pib <- pib[1:11,]
  pib <- t(pib)
  pib[,1] <- stringr::str_remove_all(pib[,1], "[^0-9]")
  pib <- as.data.frame(pib)
  pib <- tidyr::fill(pib, V1)
  pib <- dplyr::mutate(pib,
                       V2 = dplyr::case_when(
                         V2 == 'E-M' ~ 'Q1',
                         V2 == 'A-J' ~ 'Q2',
                         V2 == 'J-S' ~ 'Q3',
                         V2 == 'O-D' ~ 'Q4'
                       ),
                       V1 = paste(trimws(V1), trimws(V2)),
                       V2 = NULL)
  pib <- t(pib)
  pib[1,1] <- 'componente'
  pib <- as.data.frame(pib)
  names(pib) <- pib[1,]
  pib <- pib[-1,]
  pib <- tidyr::pivot_longer(pib, -componente, names_to = "date", values_to = "pib")

  # Ponderacion por componente
  pib2 <- readxl::read_excel(pibFile, sheet = 'PIB$_Trim', skip = 25, col_names = F)
  pib2 <- tidyr::drop_na(pib2, ...2)
  pib2 <- pib2[1:11,]
  pib2 <- t(pib2)
  pib2[,1] <- stringr::str_remove_all(pib2[,1], '[^0-9]')
  pib2[1,1] <- NA
  pib2 <- as.data.frame(pib2)
  pib2 <- tidyr::fill(pib2, V1)
  pib2 <- dplyr::mutate(pib2,
                        V2 = dplyr::case_when(
                          V2 == 'E-M' ~ 'Q1',
                          V2 == 'A-J' ~ 'Q2',
                          V2 == 'J-S' ~ 'Q3',
                          V2 == 'O-D' ~ 'Q4'
                        ),
                        V1 = paste(trimws(V1), trimws(V2)),
                        V2 = NULL)
  pib2 <- t(pib2)
  pib2[1,1] <- 'componente'
  pib2 <- as.data.frame(pib2)
  names(pib2) <- pib2[1,]
  pib2 <- pib2[-1,]
  pib2 <- tidyr::pivot_longer(pib2, -componente, names_to = "date", values_to = "ponderacion")

  pib <- dplyr::left_join(pib, pib2)


  # PIB Acumulado
  pib2 <- readxl::read_excel(pibFile, sheet = 'PIB$_Trim_Acum', skip = 6, col_names = F)
  pib2 <- tidyr::drop_na(pib2, ...2)
  pib2 <- pib2[1:11,]
  pib2 <- t(pib2)
  pib2[,1] <- stringr::str_remove(pib2[,1], '\\(p\\)')
  pib2[1,1] <- NA
  pib2 <- as.data.frame(pib2)
  pib2 <- tidyr::fill(pib2, V1)
  pib2 <- dplyr::mutate(pib2,
                        V2 = dplyr::case_when(
                          V2 == 'E-M' ~ 'Q1',
                          V2 == 'E-J' ~ 'Q2',
                          V2 == 'E-S' ~ 'Q3',
                          V2 == 'E-D' ~ 'Q4'
                        ),
                        V1 = paste(trimws(V1), trimws(V2)),
                        V2 = NULL)
  pib2 <- t(pib2)
  pib2[1,1] <- 'componente'
  pib2 <- as.data.frame(pib2)
  names(pib2) <- pib2[1,]
  pib2 <- pib2[-1,]
  pib2 <- tidyr::pivot_longer(pib2, -componente, names_to = "date", values_to = "pib_acumulado")

  pib <- dplyr::left_join(pib, pib2)

  # Ponderación por componente PIB acumulado
  pib2 <- readxl::read_excel(pibFile, sheet = 'PIB$_Trim_Acum', skip = 25, col_names = F)
  pib2 <- tidyr::drop_na(pib2, ...2)
  pib2 <- pib2[1:11,]
  pib2 <- t(pib2)
  pib2[,1] <- stringr::str_remove(pib2[,1], '\\(p\\)')
  pib2[1,1] <- NA
  pib2 <- as.data.frame(pib2)
  pib2 <- tidyr::fill(pib2, V1)
  pib2 <- dplyr::mutate(pib2,
                        V2 = dplyr::case_when(
                          V2 == 'E-M' ~ 'Q1',
                          V2 == 'E-J' ~ 'Q2',
                          V2 == 'E-S' ~ 'Q3',
                          V2 == 'E-D' ~ 'Q4'
                        ),
                        V1 = paste(trimws(V1), trimws(V2)),
                        V2 = NULL)
  pib2 <- t(pib2)
  pib2[1,1] <- 'componente'
  pib2 <- as.data.frame(pib2)
  names(pib2) <- pib2[1,]
  pib2 <- pib2[-1,]
  pib2 <- tidyr::pivot_longer(pib2, -componente, names_to = "date", values_to = "ponderacion_acumulada")

  pib <- dplyr::left_join(pib, pib2)

  ## INDICE PIB

  pib2 <- readxl::read_excel(pibFile, sheet = 'PIBK_Trim', skip = 6, col_names = F)
  pib2 <- pib2[!is.na(pib2$...1) | !is.na(pib2$...2),]
  pib2 <- pib2[1:11,]
  pib2 <- t(pib2)
  pib2[,1] <- stringr::str_remove(pib2[,1], '\\(p\\)')
  pib2[1,1] <- NA
  pib2 <- as.data.frame(pib2)
  pib2 <- tidyr::fill(pib2, V1)
  pib2 <- dplyr::mutate(pib2,
                        V2 = dplyr::case_when(
                          V2 == 'E-M' ~ 'Q1',
                          V2 == 'A-J' ~ 'Q2',
                          V2 == 'J-S' ~ 'Q3',
                          V2 == 'O-D' ~ 'Q4'
                        ),
                        V1 = paste(trimws(V1), trimws(V2)),
                        V2 = NULL)
  pib2 <- t(pib2)
  pib2[,1] <- stringr::str_remove(pib2[,1], '\\(1\\)')
  pib2[1,1] <- 'componente'
  pib2 <- as.data.frame(pib2)
  names(pib2) <- pib2[1,]
  pib2 <- pib2[-1,]
  pib2 <- tidyr::pivot_longer(pib2, -componente, names_to = "date", values_to = "ive_pib")

  pib <- dplyr::left_join(pib, pib2)

  # Tasa de crecimiento

  pib2 <- readxl::read_excel(pibFile, sheet = 'PIBK_Trim', skip = 25, col_names = F)
  pib2 <- pib2[!is.na(pib2$...1) | !is.na(pib2$...2),]
  pib2 <- pib2[1:11,]
  pib2 <- t(pib2)
  pib2[,1] <- stringr::str_remove(pib2[,1], '\\(p\\)')
  pib2[1,1] <- NA
  pib2 <- as.data.frame(pib2)
  pib2 <- tidyr::fill(pib2, V1)
  pib2 <- dplyr::mutate(pib2,
                        V2 = dplyr::case_when(
                          V2 == 'E-M' ~ 'Q1',
                          V2 == 'A-J' ~ 'Q2',
                          V2 == 'J-S' ~ 'Q3',
                          V2 == 'O-D' ~ 'Q4'
                        ),
                        V1 = paste(trimws(V1), trimws(V2)),
                        V2 = NULL)
  pib2 <- t(pib2)
  pib2[,1] <- stringr::str_remove(pib2[,1], '\\(1\\)')
  pib2[1,1] <- 'componente'
  pib2 <- as.data.frame(pib2)
  names(pib2) <- pib2[1,]
  pib2 <- pib2[-1,]
  pib2 <- tidyr::pivot_longer(pib2, -componente, names_to = "date", values_to = "tc_pib")

  pib <- dplyr::left_join(pib, pib2)

  # Incidencia por componente
  pib2 <- readxl::read_excel(pibFile, sheet = 'PIBK_Trim', skip = 44, col_names = F)
  pib2 <- pib2[!is.na(pib2$...1) | !is.na(pib2$...2),]
  pib2 <- pib2[1:11,]
  pib2 <- t(pib2)
  pib2[,1] <- stringr::str_remove(pib2[,1], '\\(p\\)')
  pib2[1,1] <- NA
  pib2 <- as.data.frame(pib2)
  pib2 <- tidyr::fill(pib2, V1)
  pib2 <- dplyr::mutate(pib2,
                        V2 = dplyr::case_when(
                          V2 == 'E-M' ~ 'Q1',
                          V2 == 'A-J' ~ 'Q2',
                          V2 == 'J-S' ~ 'Q3',
                          V2 == 'O-D' ~ 'Q4'
                        ),
                        V1 = paste(trimws(V1), trimws(V2)),
                        V2 = NULL)
  pib2 <- t(pib2)
  pib2[,1] <- stringr::str_remove(pib2[,1], '\\(1\\)')
  pib2[1,1] <- 'componente'
  pib2 <- as.data.frame(pib2)
  names(pib2) <- pib2[1,]
  pib2 <- pib2[-1,]
  pib2 <- tidyr::pivot_longer(pib2, -componente, names_to = "date", values_to = "incidencia")

  pib <- dplyr::left_join(pib, pib2)


  ## INDICE PIB ACUMULADO

  pib2 <- readxl::read_excel(pibFile, sheet = 'PIBK_Trim_Acum', skip = 6, col_names = F)
  pib2 <- pib2[!is.na(pib2$...1) | !is.na(pib2$...2),]
  pib2 <- pib2[1:11,]
  pib2 <- t(pib2)
  pib2[,1] <- stringr::str_remove(pib2[,1], '\\(p\\)')
  pib2[1,1] <- NA
  pib2 <- as.data.frame(pib2)
  pib2 <- tidyr::fill(pib2, V1)
  pib2 <- dplyr::mutate(pib2,
                        V2 = dplyr::case_when(
                          V2 == 'E-M' ~ 'Q1',
                          V2 == 'E-J' ~ 'Q2',
                          V2 == 'E-S' ~ 'Q3',
                          V2 == 'E-D' ~ 'Q4'
                        ),
                        V1 = paste(trimws(V1), trimws(V2)),
                        V2 = NULL)
  pib2 <- t(pib2)
  pib2[,1] <- stringr::str_remove(pib2[,1], '\\(1\\)')
  pib2[1,1] <- 'componente'
  pib2 <- as.data.frame(pib2)
  names(pib2) <- pib2[1,]
  pib2 <- pib2[-1,]
  pib2 <- tidyr::pivot_longer(pib2, -componente, names_to = "date", values_to = "ive_acumulado")

  pib <- dplyr::left_join(pib, pib2)

  # Tasa de crecimiento

  pib2 <- readxl::read_excel(pibFile, sheet = 'PIBK_Trim_Acum', skip = 25, col_names = F)
  pib2 <- pib2[!is.na(pib2$...1) | !is.na(pib2$...2),]
  pib2 <- pib2[1:11,]
  pib2 <- t(pib2)
  pib2[,1] <- stringr::str_remove(pib2[,1], '\\(p\\)')
  pib2[1,1] <- NA
  pib2 <- as.data.frame(pib2)
  pib2 <- tidyr::fill(pib2, V1)
  pib2 <- dplyr::mutate(pib2,
                        V2 = dplyr::case_when(
                          V2 == 'E-M' ~ 'Q1',
                          V2 == 'E-J' ~ 'Q2',
                          V2 == 'E-S' ~ 'Q3',
                          V2 == 'E-D' ~ 'Q4'
                        ),
                        V1 = paste(trimws(V1), trimws(V2)),
                        V2 = NULL)
  pib2 <- t(pib2)
  pib2[,1] <- stringr::str_remove(pib2[,1], '\\(1\\)')
  pib2[1,1] <- 'componente'
  pib2 <- as.data.frame(pib2)
  names(pib2) <- pib2[1,]
  pib2 <- pib2[-1,]
  pib2 <- tidyr::pivot_longer(pib2, -componente, names_to = "date", values_to = "tc_pib_acumulado")

  pib <- dplyr::left_join(pib, pib2)

  # Incidencia por componente
  pib2 <- readxl::read_excel(pibFile, sheet = 'PIBK_Trim_Acum', skip = 44, col_names = F)
  pib2 <- pib2[!is.na(pib2$...1) | !is.na(pib2$...2),]
  pib2 <- pib2[1:11,]
  pib2 <- t(pib2)
  pib2[,1] <- stringr::str_remove(pib2[,1], '\\(p\\)')
  pib2[1,1] <- NA
  pib2 <- as.data.frame(pib2)
  pib2 <- tidyr::fill(pib2, V1)
  pib2 <- dplyr::mutate(pib2,
                        V2 = dplyr::case_when(
                          V2 == 'E-M' ~ 'Q1',
                          V2 == 'E-J' ~ 'Q2',
                          V2 == 'E-S' ~ 'Q3',
                          V2 == 'E-D' ~ 'Q4'
                        ),
                        V1 = paste(trimws(V1), trimws(V2)),
                        V2 = NULL)
  pib2 <- t(pib2)
  pib2[,1] <- stringr::str_remove(pib2[,1], '\\(1\\)')
  pib2[1,1] <- 'componente'
  pib2 <- as.data.frame(pib2)
  names(pib2) <- pib2[1,]
  pib2 <- pib2[-1,]
  pib2 <- tidyr::pivot_longer(pib2, -componente, names_to = "date", values_to = "incidencia_acumulada")

  pib <- dplyr::left_join(pib, pib2)

  pib$date <- lubridate::ceiling_date(as.Date(tsibble::yearquarter(pib$date)), unit = "quarter")
  pib$date <- lubridate::add_with_rollback(pib$date, lubridate::days(-1))

  #unlink(pibFile)

  pib %>%
    dplyr::left_join(domar::nvl_pib_gasto) %>%
    dplyr::relocate(c(orden, nivel))
}




#' PIB por enfoque del gasto anual
#'
#'   \lifecycle{experimental}
#'
#' @param data datos del indicador principal
#' @param metadata indica si se retornan los datos o la metadata del indicador
#'
#' @return data.frame con los datos del indicador en forma tabular

#' @export
#'
#' @examples
#' \dontrun{
#' pib_gasto_anual()
#' }
pib_gasto_anual <- function(data = NULL, metadata = FALSE){
  if(metadata){
    return(
      tibble::tribble(
        ~col, ~name, ~unit, ~dtype,
        "orden", "Orden de los componentes", "", "int",
        "nivel", "Nivel de los componentes", "", "int",
        "componente", "Componente", "", "text",
        "ano", "Año", "Anual", "int",
        "pib", "Valor del PIB", "Millones de RD$", "comma_f1",
        "ponderacion", "Ponderación por componente", "Porcentaje (%)", "f2",
        "ive", "Índice de Valores Encadenados (IVE) del PIB", "Índice (2007=100)", "f1",
        "tasa_crecimiento", "Tasa de crecimiento PIB", "Porcentaje (%)", "f1",
        "incidencia", "Incidencia por componente del PIB", "", "f1"
      )
    )
  } else if(is.null(data)){
    datos <- pib_gasto_trim()
  } else {
    datos <- data
  }
  datos %>%
    dplyr::select(1:4, dplyr::contains("acum")) %>%
    dplyr::filter(lubridate::month(date) == 12) %>%
    setNames(c("orden", "nivel", "componente", "ano", "pib", "ponderacion", "ive", "tasa_crecimiento", "incidencia")) %>%
    dplyr::mutate(ano = lubridate::year(ano))
}




#' PIB por enfoque de origen trimestral
#'
#'   \lifecycle{experimental}
#'
#' @param indicador vector de datos para este indicador en la lista de domar
#' @param metadata indica si se retornan los datos o la metadata del indicador
#'
#' @return data.frame con los datos del indicador en forma tabular

#' @export
#'
#' @examples
#' \dontrun{
#' pib_origen_trim()
#' }
pib_origen_trim <- function(indicador = NULL, metadata = FALSE){
  if(metadata){
    return(
      tibble::tribble(
        ~col, ~name, ~unit, ~dtype,
        "orden", "Orden de las ramas", "", "int",
        "nivel", "Nivel de las ramas", "", "int",
        "rae", "Rama de Actividad Económica", "", "text",
        "date", "Fecha", "Trimestral", "qdate",
        "valor_agregado", "Valor agregado de la rama", "Millones de RD$", "comma_f1",
        "ponderacion", "Ponderación por rama", "Razón", "f3",
        "va_acumulado", "Valor agregado acumulado", "Millones de RD$", "comma_f1",
        "ponderacion_acum", "Ponderación acumulada", "Razón", "f3",
        "ive", "Índice de Volumen Encadenados (IVE)", "Índice", "f1",
        "tasa_crecimiento", "Tasa de crecimiento", "Porcentaje (%)", "f1",
        "incidencia", "Incidencia", "", "f1",
        "ive_acum", "Índice de Valores Encadenados (IVE) acumulado", "Índice", "f1",
        "tasa_crecimiento_acum", "Tasa de crecimiento acumulada", "Porcentaje (%)", "f1",
        "incidencia_acum", "Incidencia acumulada", "", "f1"
      )
    )
  }
  if(is.null(indicador)){
    indicador <- c(
      original_url = "https://cdn.bancentral.gov.do/documents/estadisticas/sector-real/documents/pib_origen_2007.xlsx",
      file_ext = "xlsx"
    )
  }

  file <- "/mnt/c/Users/drdsd/Downloads/pib_origen_2007.xlsx"
  #file <- downloader(indicador)

  # Valor agregado
  va <- readxl::read_excel(file, sheet = "PIB$_Trim", skip = 6, col_names = F) %>%
    tidyr::drop_na(...2) %>%
    .[1:33,] %>%
    t() %>%
    as.data.frame() %>%
    dplyr::mutate(
      V1 = stringr::str_remove_all(V1, stringr::regex("[^0-9]"))
    ) %>%
    tidyr::fill(V1) %>%
    janitor::row_to_names(1) %>%
    Dmisc::vars_to_date(1, 2) %>%
    tidyr::pivot_longer(-date, names_to = "rae", values_to = "valor_agregado")

  # Ponderacion
  pva <- readxl::read_excel(file, sheet = "PIB$_Trim", skip = 42, col_names = F) %>%
    tidyr::drop_na(...2) %>%
    .[1:33,] %>%
    t() %>%
    as.data.frame() %>%
    dplyr::mutate(
      V1 = stringr::str_remove_all(V1, stringr::regex("[^0-9]"))
    ) %>%
    tidyr::fill(V1) %>%
    janitor::row_to_names(1) %>%
    Dmisc::vars_to_date(year = 1, quarter = 2) %>%
    tidyr::pivot_longer(-date, names_to = "rae", values_to = "ponderacion")

  # Valor agregado acumulado
  vaa <- readxl::read_excel(file, sheet = "PIB$_Trim_Acum", skip = 6, col_names = F) %>%
    tidyr::drop_na(...2) %>%
    .[1:33,] %>%
    t() %>%
    as.data.frame() %>%
    dplyr::mutate(
      V1 = stringr::str_remove_all(V1, stringr::regex("[^0-9]"))
    ) %>%
    tidyr::fill(V1) %>%
    janitor::row_to_names(1) %>%
    Dmisc::vars_to_date(1, 2) %>%
    tidyr::pivot_longer(-date, names_to = "rae", values_to = "va_acumulado")

  # Valor agregado acumulado
  pvaa <- readxl::read_excel(file, sheet = "PIB$_Trim_Acum", skip = 42, col_names = F) %>%
    tidyr::drop_na(...2) %>%
    .[1:33,] %>%
    t() %>%
    as.data.frame() %>%
    dplyr::mutate(
      V1 = stringr::str_remove_all(V1, stringr::regex("[^0-9]"))
    ) %>%
    tidyr::fill(V1) %>%
    janitor::row_to_names(1) %>%
    Dmisc::vars_to_date(1, 2) %>%
    tidyr::pivot_longer(-date, names_to = "rae", values_to = "ponderacion_acum")

  # Indice de valores encadenados (IVE)
  ive <- readxl::read_excel(file, sheet = "PIBK_Trim", skip = 6, col_names = F) %>%
    tidyr::drop_na(...2) %>%
    .[1:33,] %>%
    t() %>%
    as.data.frame() %>%
    dplyr::mutate(
      V1 = stringr::str_remove_all(V1, stringr::regex("[^0-9]"))
    ) %>%
    tidyr::fill(V1) %>%
    janitor::row_to_names(1) %>%
    Dmisc::vars_to_date(1, 2) %>%
    tidyr::pivot_longer(-date, names_to = "rae", values_to = "ive")

  # Tasa de crecimiento
  tc <- readxl::read_excel(file, sheet = "PIBK_Trim", skip = 42, col_names = F) %>%
    tidyr::drop_na(...6) %>%
    .[1:33,] %>%
    t() %>%
    as.data.frame() %>%
    dplyr::mutate(
      V1 = stringr::str_remove_all(V1, stringr::regex("[^0-9]"))
    ) %>%
    tidyr::fill(V1) %>%
    janitor::row_to_names(1) %>%
    Dmisc::vars_to_date(1, 2) %>%
    tidyr::pivot_longer(-date, names_to = "rae", values_to = "tasa_crecimiento")

  # Incidencia
  iva <- readxl::read_excel(file, sheet = "PIBK_Trim", skip = 78, col_names = F) %>%
    tidyr::drop_na(...6) %>%
    .[1:33,] %>%
    t() %>%
    as.data.frame() %>%
    dplyr::mutate(
      V1 = stringr::str_remove_all(V1, stringr::regex("[^0-9]"))
    ) %>%
    tidyr::fill(V1) %>%
    janitor::row_to_names(1) %>%
    Dmisc::vars_to_date(1, 2) %>%
    tidyr::pivot_longer(-date, names_to = "rae", values_to = "incidencia")

  # Indice de valores encadenados (IVE) acumulado
  ivea <- readxl::read_excel(file, sheet = "PIBK_Trim_Acum", skip = 6, col_names = F) %>%
    tidyr::drop_na(...2) %>%
    .[1:33,] %>%
    t() %>%
    as.data.frame() %>%
    dplyr::mutate(
      V1 = stringr::str_remove_all(V1, stringr::regex("[^0-9]"))
    ) %>%
    tidyr::fill(V1) %>%
    janitor::row_to_names(1) %>%
    Dmisc::vars_to_date(1, 2) %>%
    tidyr::pivot_longer(-date, names_to = "rae", values_to = "ive_acum")

  # Tasa de crecimiento
  tca <- readxl::read_excel(file, sheet = "PIBK_Trim_Acum", skip = 42, col_names = F) %>%
    tidyr::drop_na(...6) %>%
    .[1:33,] %>%
    t() %>%
    as.data.frame() %>%
    dplyr::mutate(
      V1 = stringr::str_remove_all(V1, stringr::regex("[^0-9]"))
    ) %>%
    tidyr::fill(V1) %>%
    janitor::row_to_names(1) %>%
    Dmisc::vars_to_date(1, 2) %>%
    tidyr::pivot_longer(-date, names_to = "rae", values_to = "tasa_crecimiento_acum")

  # Incidencia
  ivaa <- readxl::read_excel(file, sheet = "PIBK_Trim_Acum", skip = 78, col_names = F) %>%
    tidyr::drop_na(...6) %>%
    .[1:33,] %>%
    t() %>%
    as.data.frame() %>%
    dplyr::mutate(
      V1 = stringr::str_remove_all(V1, stringr::regex("[^0-9]"))
    ) %>%
    tidyr::fill(V1) %>%
    janitor::row_to_names(1) %>%
    Dmisc::vars_to_date(1, 2) %>%
    tidyr::pivot_longer(-date, names_to = "rae", values_to = "incidencia_acum")

  #unlink(file)

  va %>%
    dplyr::left_join(pva) %>%
    dplyr::left_join(vaa) %>%
    dplyr::left_join(pvaa) %>%
    dplyr::left_join(ive) %>%
    dplyr::left_join(tc) %>%
    dplyr::left_join(iva) %>%
    dplyr::left_join(ivea) %>%
    dplyr::left_join(tca) %>%
    dplyr::left_join(ivaa) %>%
    dplyr::left_join(domar::nvl_pib_origen) %>%
    dplyr::relocate(orden, nivel)
}



#' PIB por enfoque de origen anual
#'
#'   \lifecycle{experimental}
#'
#' @param data datos del indicador principal
#' @param metadata indica si se retornan los datos o la metadata del indicador
#'
#' @return data.frame con los datos del indicador en forma tabular

#' @export
#'
#' @examples
#' \dontrun{
#' pib_origen_anual()
#' }
pib_origen_anual <- function(data = NULL, metadata = FALSE){
  if(metadata){
    return(
      tibble::tribble(
        ~col, ~name, ~unit, ~dtype,
        "orden", "Orden de las ramas", "", "int",
        "nivel", "Nivel de las ramas", "", "int",
        "rae", "Rama de Actividad Económica", "", "text",
        "ano", "Año", "", "ydate",
        "valor_agregado", "Valor agregado de la rama", "Millones de RD$", "comma_f1",
        "ponderacion", "Ponderación por rama", "Razón", "f3",
        "ive", "Índice de Volumen Encadenados (IVE)", "Índice", "f1",
        "tasa_crecimiento", "Tasa de crecimiento", "Porcentaje (%)", "f1",
        "incidencia", "Incidencia", "", "f1",
      )
    )
  } else if(is.null(data)){
    datos <- pib_origen_trim()
  } else {
    datos <- datos
  }

  datos %>%
    dplyr::select(1:4, dplyr::contains("acum")) %>%
    dplyr::filter(lubridate::month(date) == 12) %>%
    setNames(c("orden", "nivel", "ano", "rae", "valor_agregado", "ponderacion", "ive", "tasa_crecimiento", "incidencia")) %>%
    dplyr::mutate(ano = lubridate::year(ano))
}



#' Balanza de pagos Anual
#'
#' \lifecycle{experimental}
#'
#' @param indicador  Vea \code{\link{downloader}}
#'
#' @return [data.frame]: los datos del indicador en forma tabular
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   bpa <- balanza_pagos_anual()
#' }
balanza_pagos_anual <- function(indicador = NULL){
  `2010` <- NULL
  . <- NULL
  if(is.null(indicador)){
    indicador <-  c(
      original_url = "https://cdn.bancentral.gov.do/documents/estadisticas/sector-externo/documents/bpagos_6.xls",
      file_ext = "xls"
    )
  }
  fecha <- NULL
  file <- downloader(indicador)
  #file <- "/mnt/d/Descargas/bpagos_6 (1).xls"
  datos <- readxl::read_excel(file, skip = 5)
  unlink(file)
  datos <- tidyr::drop_na(datos, `2010`)
  datos <- datos %>%
    dplyr::bind_cols(domar::nvl_balanza_pagos, .)
  if(sum(datos[, "conceptos"] == datos[, "Conceptos"]) == 57){
    datos %>%
      dplyr::select(-"Conceptos") %>%
      tidyr::pivot_longer(-c(1:3), names_to = "ano", values_to = "valor")
  }
}

########################################################################################################

#' Balanza de pagos Trimestral
#'
#'  \lifecycle{experimental}
#'
#' @param indicador  Vea \code{\link{downloader}}
#'
#' @return [data.frame]: los datos del indicador en forma tabular
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   bpt <- balanza_pagos_trim()
#' }
balanza_pagos_trim <- function(indicador = NULL){
  V1 <- NULL
  V2 <- NULL
  . <- NULL
  name <- NULL
  if(is.null(indicador)){
    indicador <- c(
      original_url = "https://cdn.bancentral.gov.do/documents/estadisticas/sector-externo/documents/bpagos__trim_6.xls",
      file_ext = "xls"
    )
  }
  file <- downloader(indicador)
  #file <- "/mnt/d/Descargas/bpagos__trim_6 (4).xls"
  datos <- readxl::read_excel(file, skip = 4, col_names = F) %>%
    tidyr::drop_na(3) %>%
    t() %>%
    tibble::as.tibble() %>%
    dplyr::filter(is.na(V2) | !(V2 %in% c("Ene-Jun", "Ene-Sep", "Ene-Dic"))) %>%
    dplyr::mutate(
      V2 = stringr::str_remove(V2, stringr::regex("[a-zA-Z]*-")),
      V1 = paste(V1, V2)
    ) %>%
    dplyr::select(-V2) %>%
    t() %>%
    tibble::as.tibble() %>%
    janitor::row_to_names(1) %>%
    dplyr::bind_cols(domar::nvl_balanza_pagos, .)
  if(sum(datos[, "conceptos"] == datos[, "Conceptos NA"]) == 57){
    datos %>%
      dplyr::select(-"Conceptos NA") %>%
      tidyr::pivot_longer(-c(1:3), values_to = "valor") %>%
      tidyr::separate(name, into = c("ano", "mes")) %>%
      Dmisc::vars_to_date(year = "ano", month = "mes") %>%
      utils::type.convert()
  } else {
    stop("Nombres del indicador no machean.")
  }
}

###################################################################################################################################

#' Indicador Mensual de Actividad Económica (IMAE) Mensual
#'
#'  \lifecycle{experimental}
#'
#' @param indicador Vea \code{\link{downloader}}
#'
#' @return [data.frame]: los datos del IMAE en forma tabular
#'
#' @export
#'
#' @examples
#' \dontrun{
#' imae <- imae_mensual(indicador)
#' }
imae_mensual <- function(indicador) {
  ano <- NULL
  mes <- NULL
  file_path <- downloader(indicador)
  imae <- readxl::read_excel(
    file_path,
    skip = 5)
  unlink(file_path)
  # Serie original
  imaeso <- imae[,1:6]
  imaeso <- imaeso[-1,]
  imaeso[1,1] <- 'ano'
  imaeso[1,2] <- 'mes'
  imaeso[1,3] <- 'Indice'
  names(imaeso) <- imaeso[1,]
  imaeso <- imaeso[-1,]
  names(imaeso)[names(imaeso) == 'Interanual'] <- 'Var_interanual'
  names(imaeso)[names(imaeso) == 'Acumulada'] <- 'Var_acumulada'
  names(imaeso)[names(imaeso) == 'Promedio 12 meses'] <- 'Var_promedio_12_meses'
  imaeso <- imaeso[!is.na(imaeso$Indice),]
  imaeso$ano <- stringr::str_remove(imaeso$ano, 'Promedio ')
  imaeso <- tidyr::fill(imaeso, ano, .direction = 'up')
  imaeso <- tidyr::fill(imaeso, ano)
  imaeso <- dplyr::filter(imaeso, !is.na(mes))
  imaeso <- Dmisc::vars_to_date(imaeso, year = 1, month = 2)
  imaeso$serie <- 'Serie original'

  # Serie desestacionalizada
  imaesd <- imae[,c(1:2, 7:11)]
  imaesd <- imaesd[-1,]
  imaesd[1,1] <- 'ano'
  imaesd[1,2] <- 'mes'
  imaesd[1,3] <- 'Indice'
  names(imaesd) <- imaesd[1,]
  imaesd <- imaesd[-1,]
  names(imaesd)[names(imaesd) == 'Respecto al per\u00EDodo anterior'] <- 'Var_periodo_anterior'
  names(imaesd)[names(imaesd) == 'Interanual'] <- 'Var_interanual'
  names(imaesd)[names(imaesd) == 'Acumulada'] <- 'Var_acumulada'
  names(imaesd)[names(imaesd) == 'Promedio 12 meses'] <- 'Var_promedio_12_meses'
  imaesd <- dplyr::mutate(imaesd,
                          ano = stringr::str_remove(ano, 'Promedio '))
  imaesd <- imaesd[!is.na(imaesd$Indice),]
  imaesd <- tidyr::fill(imaesd, ano, .direction = 'up')
  imaesd <- tidyr::fill(imaesd, ano)
  imaesd <- imaesd[!is.na(imaesd$mes),]
  imaesd <- Dmisc::vars_to_date(imaesd, year = 1, month = 2)
  imaesd$serie <- 'Serie desestacionalizada'

  # Serie tendencia-ciclo
  imaest <- imae[,c(1:2, 12:16)]
  imaest <- imaest[-1,]
  imaest[1,1] <- 'ano'
  imaest[1,2] <- 'mes'
  imaest[1,3] <- 'Indice'
  names(imaest) <- imaest[1,]
  imaest <- imaest[-1,]
  names(imaest)[names(imaest) == 'Respecto al per\u00EDodo anterior'] <- 'Var_periodo_anterior'
  names(imaest)[names(imaest) == 'Interanual'] <- 'Var_interanual'
  names(imaest)[names(imaest) == 'Acumulada'] <- 'Var_acumulada'
  names(imaest)[names(imaest) == 'Promedio 12 meses'] <- 'Var_promedio_12_meses'
  imaest <- imaest[!is.na(imaest$Indice),]
  imaest <- dplyr::mutate(imaest,
                          ano = stringr::str_remove(ano, 'Promedio '))
  imaest <- tidyr::fill(imaest, ano, .direction = 'up')
  imaest <- tidyr::fill(imaest, ano)
  imaest <- imaest[!is.na(imaest$mes),]
  imaest <- Dmisc::vars_to_date(imaest, year = 1, month = 2)
  imaest$serie <- 'Serie Tendencia-Ciclo'

  #
  dplyr::bind_rows(imaeso, imaesd, imaest)
}

#################################################################################################################################

#' Deflactor del Producto Interno Bruto (PIB)
#'
#'  \lifecycle{experimental}
#'
#' @param indicador Vea \code{\link{downloader}}
#'
#' @return [data.frame]: los datos como se descargan del banco con muy pocas
#'   modificaciones. Luego esos datos son tomados por pib_deflactor_trimestral y
#'   pib_deflactor_anual
#'
#' @examples
#' \dontrun{
#'   def <- pib_deflactor()
#' }
pib_deflactor <- function(indicador = NULL) {
  if(is.null(indicador)){
    indicador = c(
      original_url = "https://cdn.bancentral.gov.do/documents/estadisticas/sector-real/documents/pib_deflactor_2007.xls",
      file_ext = "xls"
    )
  }
  `...1` <- NULL
  ano <- NULL
  `...2` <- NULL
  file <- "/mnt/d/Descargas/pib_deflactor_2007 (3).xls"
  #file <- downloader(indicador)
  def <- readxl::read_excel(file, skip = 4, col_names = F)
  return(def)
  def %>%
    dplyr::mutate(
      ano = dplyr::if_else(stringr::str_detect(...1, "[0-9]"), ...1, NA_character_),
      ano = stringr::str_remove_all(ano, "[^0-9]")
    ) %>%
    tidyr::fill(ano, .direction = "up") %>%
    tidyr::drop_na(...2)
}

###############################################################################################################################################

#' Deflactor del PIB Trimestral
#'
#'  \lifecycle{experimental}
#'
#' @param indicador Vea \code{\link{downloader}}
#'
#' @return [data.frame]: los datos del indicador en forma tabular
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   dpt <- pib_deflactor_trimestral()
#' }
pib_deflactor_trimestral <- function(indicador = NULL){
  `...1` <- NULL
  ano <- NULL
  serie <- NULL
  fecha <- NULL
  def <- pib_deflactor(indicador) %>%
    tidyr::fill(
      ...1
    ) %>%
    dplyr::filter(
      !stringr::str_detect(...1, "[0-9]")
    ) %>%
    dplyr::mutate(
      ...1 = dplyr::case_when(
        ...1 == "IV" ~ "Q4",
        ...1 == "III" ~ "Q3",
        ...1 == "II" ~ "Q2",
        ...1 == "I" ~ "Q1"
      ),
      fecha = paste(ano, ...1)
    ) %>%
    dplyr::select(
      -c(ano, ...1)
    )%>%
    dplyr::relocate(fecha) %>%
    t() %>%
    as.data.frame()
  def[1,1] <- "serie"
  def[1,2] <- "indicador"
  def <- def %>%
    janitor::row_to_names(1) %>%
    tidyr::fill(serie) %>%
    tidyr::pivot_longer(-c(serie, indicador), values_drop_na = T)
}

#####################################################################################################################

#' Deflactor del PIB Anual
#'
#'  \lifecycle{experimental}
#'
#' @param indicador Vea \code{\link{downloader}}
#'
#' @return [data.frame]: los datos del indicador en forma tabular
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   dpa <- pib_deflactor_anual()
#' }
pib_deflactor_anual <- function(indicador = NULL){
  `...1` <- NULL
  ano <- NULL
  serie <- NULL
  def <- pib_deflactor(indicador) %>%
    tidyr::fill(
      ...1
    ) %>%
    dplyr::filter(
      !startsWith(...1, "I")
    ) %>%
    dplyr::select(
      -c(...1)
    )%>%
    dplyr::relocate(ano) %>%
    t() %>%
    as.data.frame()
  def[1,1] <- "serie"
  def[1,2] <- "indicador"
  def <- def %>%
    janitor::row_to_names(1) %>%
    tidyr::fill(serie) %>%
    tidyr::pivot_longer(-c(serie, indicador), values_drop_na = T)
}

##############################################################################################################################

#' Índice de Precios al Consumidor (octubre 2019 - septiembre 2020)
#'
#'  \lifecycle{experimental}
#'
#' @param indicador Vea \code{\link{downloader}}
#'
#' @return [data.frame]: los datos del indicador en forma tabular
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   ipc20 <- ipc_mensual_2020()
#' }
ipc_mensual_2020 <- function(indicador = NULL){
  if(is.null(indicador)){
    indicador = c(
      original_url = "https://cdn.bancentral.gov.do/documents/estadisticas/precios/documents/ipc_base_2019-2020.xls",
      file_ext = "xls"
    )
  }
  `...2` <- NULL
  `...1` <- NULL
  file <- downloader(indicador)
  datos <- readxl::read_excel(file, col_names = F)
  datos <- tidyr::drop_na(datos, ...2)
  datos <- tidyr::fill(datos, ...1)
  datos <- datos[,1:7]
  names(datos) <- c('ano',
                    'mes',
                    'indice',
                    'variacion_mensual',
                    'variacion_con_diciembre',
                    'variacion_anual',
                    'variacion_promedio_12_meses')
  datos <- Dmisc::vars_to_date(datos, year = 1, month = 2)
  datos
}

##########################################################################################################################

#' Índices de Valores Encadenados del PIB
#'
#'  \lifecycle{experimental}
#'
#' @param indicador Vea \code{\link{downloader}}
#'
#' @return [data.frame]: los datos del indicador en forma tabular
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   ive <- pib_ive()
#' }
pib_ive <- function(indicador = NULL){
  if(is.null(indicador)){
    indicador = c(
      original_url = "https://cdn.bancentral.gov.do/documents/estadisticas/sector-real/documents/pib_2007.xlsx",
      file_ext = "xlsx"
    )
  }
  `...1` <- NULL
  `...2` <- NULL
  ano <- NULL
  file <- downloader(indicador)
  ive <- readxl::read_excel(file, skip = 4, col_names = F)
  ive <- tidyr::fill(ive, ...1)
  ive <- dplyr::mutate(
    ive,
    ano = dplyr::if_else(stringr::str_detect(...1, "[a-z]"), ...1, NA_character_),
    ano = stringr::str_remove_all(ano, "[^0-9]")
  ) %>%
    tidyr::fill(ano, .direction = "up") %>%
    tidyr::drop_na(...2)
}

#############################################################################################################################

#' Índices de Valores Encadenados del PIB Trimestral
#'
#'  \lifecycle{experimental}
#'
#' @param indicador Vea \code{\link{downloader}}
#'
#' @return [data.frame]: los datos del indicador en forma tabular
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   ivet <- pib_ive_trimestral()
#' }
pib_ive_trimestral <- function(indicador = NULL){
  `...1` <- NULL
  ano <- NULL
  fecha <- NULL
  serie <- NULL
  ive <- pib_ive(indicador)
  ive <- ive %>%
    dplyr::filter(
      !stringr::str_detect(...1, "[0-9]")
    ) %>%
    dplyr::mutate(
      ...1 = dplyr::case_when(
        ...1 == "IV" ~ "Q4",
        ...1 == "III" ~ "Q3",
        ...1 == "II" ~ "Q2",
        ...1 == "I" ~ "Q1"
      ),
      fecha = paste(ano, ...1)
    )
  ive <- ive %>%
    dplyr::select(
      -ano,
      -...1
    )
  ive <- t(dplyr::relocate(ive, fecha))
  ive[1,1] <- "serie"
  ive[1,2] <- "indicador"
  ive <- janitor::row_to_names(ive, 1)
  ive <- as.data.frame(ive)
  ive <- tidyr::fill(ive, serie)
  ive <- tidyr::pivot_longer(ive, -c("serie", "indicador"), names_to = "fecha", values_drop_na = T)
  ive
}

########################################################################################################################################

#' Índices de Valores Encadenados del PIB Anual
#'
#'  \lifecycle{experimental}
#'
#' @param indicador Vea \code{\link{downloader}}
#'
#' @return [data.frame]: los datos del indicador en forma tabular
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   ivea <- pib_ive_anual()
#' }
pib_ive_anual <- function(indicador = NULL){
  `...1` <- NULL
  ano <- NULL
  serie <- NULL
  ive <- pib_ive(indicador)
  ive <- ive %>%
    dplyr::filter(
      !startsWith(...1, "I")
    )
  ive <- ive %>%
    dplyr::select(
      -...1
    )
  ive <- t(dplyr::relocate(ive, ano))
  ive[1,1] <- "serie"
  ive[1,2] <- "indicador"
  ive <- janitor::row_to_names(ive, 1)
  ive <- as.data.frame(ive)
  ive <- tidyr::fill(ive, serie)
  ive <- tidyr::pivot_longer(ive, -c("serie", "indicador"), names_to = "ano", values_drop_na = T)
  ive
}

#################################################################################################################################################

#' Indicadores Monetarios BCRD
#'
#'  \lifecycle{experimental}
#'
#' @param indicador Vea \code{\link{downloader}}
#'
#' @return [data.frame]: los datos del indicador en forma tabular
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   imbcrd <- indicadores_monetarios_bcrd()
#' }
indicadores_monetarios_bcrd <- function(indicador = NULL) {
  if(is.null(indicador)){
    indicador = c(
      original_url = "https://cdn.bancentral.gov.do/documents/estadisticas/sector-monetario-y-financiero/documents/serie_indicadores_bcrd.xlsx",
      file_ext = "xlsx"
    )
  }
  V1 <- NULL
  fecha <- NULL
  valor <- NULL
  #file <- "/mnt/d/Descargas/serie_indicadores_bcrd (4).xlsx"
  file <- downloader(indicador)
  datos <- readxl::read_excel(
    file,
    skip = 4,
    col_names = F
  ) %>%
    dplyr::filter(!is.na(...250))
  if(nrow(datos) == 40){
    datos <- dplyr::left_join(
      datos,
      nvl_indicadores_monetarios_bcrd,
      by = c("...1" = "indicadores")
    )
  } else {
    stop("")
  }
  datos <- datos %>%
    dplyr::relocate(c("orden", "nivel")) %>%
    t() %>%
    tibble::as.tibble() %>%
    Dmisc::vars_to_date(date = 1) %>%
    t() %>%
    tibble::as.tibble() %>%
    janitor::row_to_names(1)
  names(datos)[1:3] <- c("orden", "nivel", "indicador")
  datos %>%
    tidyr::pivot_longer(-c(1:3), names_to = "date", values_to = "valor") %>%
    dplyr::mutate(
      date = as.Date(date),
      valor = as.numeric(valor)
    ) %>%
    tidyr::drop_na(valor)
}

############################################################################################################

#' Indicadores Monetarios OSD
#'
#'  \lifecycle{experimental}
#'
#' @param indicador Vea \code{\link{downloader}}
#'
#' @return [data.frame]: los datos del indicador en forma tabular
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   imosd <- indicadores_monetarios_osd()
#' }
indicadores_monetarios_osd <- function(indicador) {
  V1 <- NULL
  fecha <- NULL
  valor <- NULL
  file <- downloader(indicador)
  datos <- readxl::read_excel(
    file,
    skip = 4,
    col_names = F
  )
  datos <- datos[!is.na(datos$...2),]
  #datos <- datos[1:44,]
  datos <- dplyr::bind_cols(
    c('',
      '1',
      '11',
      '12',
      '2',
      '21',
      '22',
      '3',
      '31',
      '32',
      '4',
      '401',
      '4011',
      '4012',
      '4013',
      '40131',
      '40132',
      '41',
      '411',
      '4111',
      '4112',
      '4113',
      '41131',
      '41132',
      '42',
      '421',
      '4211',
      '4212',
      '4213',
      '42131',
      '42132',
      '43',
      '431',
      '4311',
      '44',
      '441',
      '4411',
      '5',
      '501',
      '5011',
      '5012',
      '5013',
      '50131',
      '50132',
      '51',
      '511',
      '5111',
      '5112',
      '5113',
      '51131',
      '51132',
      '52',
      '521',
      '5211',
      '5212',
      '5213',
      '52131',
      '52132',
      '53',
      '531',
      '5311',
      '54',
      '541',
      '5411',
      '6',
      '61',
      '62',
      '63',
      '7'
    ),
    datos
  )
  datos[1, 1:2] <- NA
  datos <- as.data.frame(t(datos))
  datos <- dplyr::mutate(datos,
                         V1 = stringr::str_replace(tolower(V1), 'ene', 'Jan'),
                         V1 = stringr::str_replace(tolower(V1), 'abr', 'Apr'),
                         V1 = stringr::str_replace(tolower(V1), 'ago', 'Aug'),
                         V1 = stringr::str_replace(tolower(V1), 'dic', 'Dec'),
                         V1 = stringr::str_replace_all(V1, '-', ' '),
                         V1 = stringr::str_remove_all(V1, '\\*'),
                         V1 = dplyr::case_when(
                           is.na(as.Date(as.numeric(V1), origin = "1899-12-30")) ~ V1,
                           !is.na(as.Date(as.numeric(V1), origin = "1899-12-30")) ~ as.character(as.Date(as.numeric(V1), origin = "1899-12-30")),
                         ),
                         V1 = stringr::str_remove_all(V1, '\\.'),
                         V1 = dplyr::case_when(
                           stringr::str_count(V1, ' ') == 2 ~ as.character(as.Date(V1, '%d %b %y')),
                           stringr::str_count(V1, ' ') == 1 ~ as.character(as.Date(paste('01', V1), '%d %b %y')),
                           TRUE ~ V1
                         )
  )
  datos <- as.data.frame(t(datos))
  names(datos) <- make.names(datos[1,])
  names(datos)[1:2] <- c('codigo', 'indicador')
  datos <- datos[-1,]
  datos <- datos[datos$codigo != '',]
  datos <- tidyr::pivot_longer(datos, -c(1:2), names_to = 'fecha', values_to = 'valor')
  datos <- dplyr::mutate(datos,
                         fecha = stringr::str_remove(fecha, 'X'),
                         fecha = stringr::str_replace_all(fecha, '\\.', '-'),
                         valor = as.numeric(valor)
  )
  datos
}






#' Producto Interno Bruto (PIB) per cápita
#'
#'  \lifecycle{experimental}
#'
#' @param indicador Vea \code{\link{downloader}}
#'
#' @return [data.frame]: los datos del indicador en forma tabular
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   pibpc <- pib_per_capita()
#' }
pib_per_capita <- function(indicador = NULL){
  if(is.null(indicador)){
    indicador = c(
      original_url = "https://cdn.bancentral.gov.do/documents/estadisticas/sector-real/documents/pib_dolares.xls",
      file_ext = "xls"
    )
  }
  V1 <- NULL
  V2 <- NULL
  `...1` <- NULL
  `...2` <- NULL
  ano <- NULL
  file <- downloader(indicador)
  pib <- readxl::read_excel(file, skip = 4, col_names = F) %>%
    t() %>%
    as.data.frame() %>%
    dplyr::mutate(
      V1 = paste(V1, V2)
    ) %>%
    t() %>%
    as.data.frame() %>%
    dplyr::mutate(
      ...1 = stringr::str_remove_all(...1, "[^0-9]")
    ) %>%
    tidyr::drop_na(...1, ...2) %>%
    dplyr::mutate(
      indicador = dplyr::if_else(duplicated(...1), "Tasa de crecimiento", "Valor")
    ) %>%
    janitor::row_to_names(1)
  names(pib)[[1]] <- "ano"
  names(pib) <- janitor::make_clean_names(names(pib))
  dplyr::filter(pib, ano != "") %>%
    dplyr::rename("indicador" = "valor")
}




#' Tasas de interés nominales activas bancos múltiples mensual 2017-2020
#'
#'  \lifecycle{experimental}
#'
#' @param indicador Vea \code{\link{downloader}}
#'
#' @return [data.frame]: los datos del indicador en forma tabular
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   tinabmm <- tasa_interes_nominales_activas_bm_mensual()
#' }
tasa_interes_nominales_activas_bm_mensual <- function(indicador = NULL){
  if(is.null(indicador)){
    indicador = c(
      original_url = "https://cdn.bancentral.gov.do/documents/estadisticas/sector-monetario-y-financiero/documents/tbm_activad.xlsx",
      file_ext = "xlsx"
    )
  }
  `...2` <- NULL
  ano <- NULL
  fecha <- NULL
  file <- downloader(indicador)
  datos <- readxl::read_excel(file, col_names = F)
  datos <- tidyr::drop_na(datos, ...2)
  datos$ano <- datos$...1
  datos <- dplyr::relocate(datos, ano)
  datos <- dplyr::mutate(datos,
                         ano = stringr::str_remove(ano, '\\*'),
                         ano = stringr::str_remove(ano, ' 1/'))
  datos$ano <- as.numeric(datos$ano)
  datos <- tidyr::fill(datos, ano)
  datos <- dplyr::filter(datos, is.na(ano) | ano > 2016)
  datos$ano <- as.character(datos$ano)
  datos[2, 1] <- 'ano'
  datos[2, 2] <- 'mes'
  plazos <- datos[-1,1:8]
  names(plazos) <- plazos[1,]
  plazos <- plazos[-1,]
  plazos <- tidyr::pivot_longer(plazos, -c('ano', 'mes'), names_to = 'categoria', values_to = 'valor')
  plazos$variable <- 'plazos'
  promedio <- datos[-1,c(1:2, 9:10)]
  names(promedio) <- promedio[1,]
  promedio <- promedio[-1,]
  promedio <- tidyr::pivot_longer(promedio, -c('ano', 'mes'), names_to = 'categoria', values_to = 'valor')
  promedio$variable <- 'promedio'
  sectores <- datos[-1,c(1:2, 11:13)]
  names(sectores) <- sectores[1,]
  sectores <- sectores[-1,]
  sectores <- tidyr::pivot_longer(sectores, -c('ano', 'mes'), names_to = 'categoria', values_to = 'valor')
  sectores$variable <- 'sectores'
  preferencial <- datos[-1,c(1:2, 14:17)]
  names(preferencial) <- preferencial[1,]
  preferencial <- preferencial[-1,]
  preferencial <- tidyr::pivot_longer(preferencial, -c('ano', 'mes'), names_to = 'categoria', values_to = 'valor')
  preferencial$variable <- 'preferencial'
  datos <- dplyr::bind_rows(
    plazos,
    promedio,
    sectores,
    preferencial
  )
  datos <- Dmisc::vars_to_date(datos, year = 1, month = 2)
  datos <- dplyr::filter(datos, !is.na(fecha))
  datos
}




#' Tipo de cambio dólar mensual
#'
#'  \lifecycle{experimental}
#'
#' @param indicador Vea \code{\link{downloader}}
#'
#' @return [data.frame]: los datos del indicador en forma tabular
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   tcdm <- tipo_cambio_dolar_mensual()
#' }
tipo_cambio_dolar_mensual <- function(indicador = NULL){
  if(is.null(indicador)){
    indicador = c(
      original_url = "https://cdn.bancentral.gov.do/documents/estadisticas/mercado-cambiario/documents/TASA_DOLAR_REFERENCIA_MC.xls",
      file_ext = "xls"
    )
  }
  file <- downloader(indicador)
  datos <- readxl::read_excel(file, 'PromMensual', skip = 2)
  datos <- Dmisc::vars_to_date(datos, year = 1, month = 2)
  datos
}

"
BANCO CENTRAL
REAL
Producto Interno Bruto (PIB) por sectores de origen.
Valores corrientes e índices de volumen encadenados referenciados al año 2007, trimestral
Producto Interno Bruto (PIB).
Índices de volumen encadenados, referenciados al año 2007. Serie Original y Desestacionalizada, trimestral
Producto Interno Bruto (PIB).
Deflactor, trimestral
Indicador Mensual de Actividad Económica (IMAE).
Serie Original, Desestacionalizada y Tendencia-Ciclo, mensual
Producto Interno Bruto (PIB) per Cápita, RD$ y US$

  PRECIOS
Índice de precios al consumidor (IPC)	1984-2021

IPC anualizado	1984-2020

Tasa de Inflación Promedio (12 meses)	1947-2020

IPC subyacente	2000-2021

IPC transables y no transables	1999-2021


CAMBIARIO: TASA DE CAMBIO
Dólar Estadounidense
Hoy
Serie Histórica 1985-2021



FISCAL
Estado de Operaciones del sector público no financiero (% del PIB)


EXTERNO
Balanza de pagos
Anual	2010-2019

Trimestral	2010-2020

Exportaciones
Trimestral	2010-2020

Anual	2010-2019

Importaciones
Trimestral	2010-2020

Anual	2010-2019




MONETARIO
I. Panoramas
1. Panorama Banco Central (PBC)

2. Panorama de las otras sociedades de depósito (POSD)

a. Panorama de los bancos múltiples (excluidos offshore)

3. Panorama de las sociedades de depósito (PSD)

4. Panorama Otras Sociedades Financieras (OSF)

5. Panorama Sociedades Financieras (SF)

II. Balances Sectoriales
1. Balance sectorial de las OSD: Resumen por instrumentos

2. Balance sectorial de las OSD: Instrumentos y sectores institucionales (Activos)

a. Balance sectorial de las OSD: Instrumentos y sectores institucionales (Activos en MN)

b. Balance sectorial de las OSD: Instrumentos y sectores institucionales (Activos en ME)

3. Balance sectorial de las OSD: Instrumentos y sectores institucionales (Pasivos)

a. Balance sectorial de las OSD: Instrumentos y sectores institucionales (Pasivos en MN)

b. Balance sectorial de las OSD: Instrumentos y sectores institucionales (Activos en ME)

III. Base Monetaria y Agregados Monetarios
1. Base monetaria

2. Agregados monetarios

3. Agregados monetarios por sectores

4. Encaje bancario requerido y efectivo por monedas

IV. Agregados de Crédito
1. Crédito al sector público no financiero del BC

2. Crédito al sector público no financiero de las OSD

V. Tasas de Interés
1. Valores subastados del Banco Central por plazos de colocación en MN

2. Tasas de interés bancarias activas en MN

3. Tasas de interés bancarias activas en ME

4. Tasas de interés bancarias pasivas en MN

5. Tasas de interés bancarias pasivas en ME


TURISMO.
Llegada vía aérea
Total (según residencia y aeropuertos)
Tasa de ocupación
"
