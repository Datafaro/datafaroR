#' Autenticar y obtener token de acceso
#'
#' Esta función verifica si existe información de autenticación previa almacenada y válida.
#' Si no encuentra información válida o el token ha expirado, solicita al usuario que se autentique nuevamente.
#' Si la autenticación es exitosa, genera y almacena un nuevo token de acceso para solicitudes futuras.
#' En caso de encontrar un token válido y no expirado, lo retorna para su uso inmediato.
#'
#' @return Retorna el token de acceso si la autenticación es exitosa o el token almacenado sigue siendo válido.
#'         En otros casos, guía al usuario para realizar una nueva autenticación.
#' @export
#'
#' @examples
#' \dontrun{
#' auth() # Ejecuta esta función para autenticarte o validar el token existente.
#' }
auth <- function() {
  auth_info <- renviron::renviron_get("DATAFARO_AUTH")

  if (is.null(auth_info) || auth_info == "") {
    cli::cli_alert_warning("No se ha encontrado información de autenticación.")
    cli::cli_alert_info("Por favor, autentíquese.")
    return(login())
  }

  auth_info_list <- auth_info %>%
    .auth_decode(.seed = Sys.info()[["nodename"]]) %>%
    strsplit("|", fixed = TRUE) %>%
    unlist()

  if (length(auth_info_list) != 7) {
    return(login())
  }

  .token <- auth_info_list[[1]]
  .name <- auth_info_list[[2]]
  .created <- lubridate::ymd_hms(auth_info_list[[3]])
  .expires <- lubridate::ymd_hms(auth_info_list[[4]])
  .days <- as.numeric(auth_info_list[[5]])
  .auto_renew <- auth_info_list[[6]]
  .refresh_days <- as.numeric(auth_info_list[[7]])

  if (.auto_renew == 1) {
    if ((lubridate::now() + lubridate::days(.refresh_days)) > .expires) {
      return(
        .generate_token(
          .days,
          paste(
            stringr::str_remove(.name, "[0-9]{4}-.+"),
            lubridate::now(),
          ),
          "s",
          "s",
          .refresh_days,
          .token
        )
      )
    }
  }

  tryCatch(
    httr2::request(glue::glue("{API_URL}/v1/token/")) %>%
      httr2::req_headers(Authorization = paste0("Token ", .token)) %>%
      httr2::req_perform(),
    error = function(e) {}
  )

  .res <- httr2::last_response()

  if (.res$status_code == 403) {
    # cat("\014")
    cli::cli_alert_danger("El token es inválido o ha expirado.")
    cli::cli_alert_info("Por favor, autentíquese nuevamente.")
    return(login())
  } else if (.res$status_code == 200) {
    return(invisible(.token))
  } else {
    cli::cli_alert_danger("Error al validar el token.")
    cli::cli_alert_info("Intente nuevamente por favor.")
    cli::cli_alert_info("Si el problema persiste, ejecuta `auth()`para autenticarte nuevamente.")
    cli::cli_alert_info("Si esto último no resuelve el problema contacte con soporte.")
  }

  return(invisible(.token))
}


.get_valid_token <- function() {
  token <- renviron::renviron_get("DATAFARO_TOKEN")

  if (is.null(token)) {
    cli::cli_alert_danger("No se ha encontrado un token válido.")
    cli::cli_alert_info("Use `auth()` para autenticarse.")
    stop(call. = FALSE)
  }

  auth <- renviron::renviron_get("DATAFARO_AUTH") %>%
    strsplit("|", fixed = TRUE) %>%
    unlist()

  if (!is.null(auth) && length(auth) == 3) {
    expires <- lubridate::ymd_hms(auth[1])
    if (expires < lubridate::now()) {
      cli::cli_alert_danger("El token ha expirado.")
      cli::cli_alert_info("Use `auth()` para autenticarse nuevamente.")
      stop(call. = FALSE)
    }

    auto_renew <- auth[2]
    refresh_days <- as.numeric(auth[3])
    if (auto_renew == "1" && (lubridate::now() + lubridate::days(refresh_days)) > expires) {
      return(.generate_token(.token = token))
    }
  }

  return(token)
}


.get_auth <- function(.name) {
  .auth_decode(renviron::renviron_get("DATAFARO_AUTH"), Sys.info()[["nodename"]]) %>%
    .auth_encode(.name)
}

.auth_encode <- function(auth_info, .seed) {
  auth_info %>%
    serialize(NULL) %>%
    sodium::simple_encrypt(sodium::pubkey(.get_hashed_key(.seed))) %>%
    base64enc::base64encode()
}


.auth_decode <- function(auth_info, .seed) {
  auth_info %>%
    base64enc::base64decode() %>%
    sodium::simple_decrypt(., .get_hashed_key(.seed = .seed)) %>%
    unserialize()
}


login <- function(
    usuario = readline("Ingrese su nombre de usuario: "),
    pass = getPass::getPass("Ingrese su contraseña: ")) {

  # Realizar la solicitud con autenticación básica
  .res <- httr2::request(glue::glue("{API_URL}/v1/login/")) %>%
    httr2::req_auth_basic(usuario, pass) %>%
    httr2::req_body_json(list()) %>%
    httr2::req_perform()

  .token <- NULL

  # Verificar el estado de la solicitud
  if (httr2::resp_status(.res) == 200 || httr2::resp_status(.res) == 201) {
    tryCatch(
      {
        token0 <- httr2::resp_body_json(.res)$token
        if (usuario == pass) {
          .days0 <- (
            lubridate::ymd_hms(token0$expires) %>%
              lubridate::as_date() -
              lubridate::today()
          ) %>%
            as.numeric()
          .token_name0 <- ifelse(is.null(token0$name), "", token0$token_name)
          cli::cli_alert_info(glue::glue("El token ({.token_name0}) expirará en {.days0} días."))
          .token <- .generate_token(days = .days0, token_name = .token_name0, .token = token0$key)
        } else {
          .token <- .generate_token(.token = token0$key)
        }
        cat("\014")
        cli::cat_rule("¡Autenticación exitosa!")
      },
      error = function(e) {
        cat("Error: ", e$message, "\n")
      }
    )
  } else {
    cat("Error en la solicitud \n")
  }

  return(invisible(.token))
}

.get_hashed_key <- function(.seed = Sys.info()[["nodename"]]) {
  charToRaw(substr(digest::digest(object = .seed, algo = "sha256", serialize = FALSE), 1, 32))
}

.generate_token <- function(
    days = .ask_for_days(),
    token_name = readline(prompt = "Ingrese un nombre para el token (Ej: Proyecto X): "),
    save = .ask_for_save(),
    auto_renew = .ask_for_auto_renew(),
    refresh_days = if (auto_renew == "s") .ask_for_refresh_days(days) else 0,
    .token) {
  .res <- httr2::request(glue::glue("{API_URL}/v1/token/")) %>%
    httr2::req_headers(Authorization = paste0("Token ", .token)) %>%
    httr2::req_body_json(list(
      name = token_name,
      expires = lubridate::now() + lubridate::days(as.numeric(days))
    )) %>%
    httr2::req_perform()

  # Verificar el estado de la solicitud
  if (httr2::resp_status(.res) == 200 || httr2::resp_status(.res) == 201) {
    new_token <- httr2::resp_body_json(.res)
    if (!new_token$ok) {
      if (!is.null(new_token$detail) && new_token$detail == "FREEUSER") {
        cli::cli_alert_danger("El API solo está disponible para usuarios con suscripción.")
        stop(call. = FALSE)
      }
      cli::cli_alert_danger("Error al generar el token.")
    }
    renviron::renviron_add(
      "DATAFARO_AUTH",
      paste(
        new_token$token$key,
        new_token$token$token_name,
        new_token$token$created,
        new_token$token$expires,
        days,
        c("s" = 1, "n" = 0)[auto_renew],
        refresh_days,
        sep = "|"
      ) %>%
        .auth_encode(.seed = Sys.info()[["nodename"]]),
      in_place = save == "s",
      confirm = FALSE
    )
    return(invisible(new_token$token$key))
  } else {
    cat("Error en la solicitud \n")
  }
}

.ask_for_days <- function() {
  days <- readline(prompt = "Ingrese la cantidad de días que desea que el token sea válido: ")
  if (!grepl("^[0-9]+$", days)) {
    cat("Error: Debe ingresar un número entero positivo.\n")
    days <- .ask_for_days()
  }

  days
}


.ask_for_save <- function() {
  save <- readline(prompt = "¿Desea guardar el archivo .Renviron? (s/n): ")
  if (!grepl("^[sn]$", save)) {
    cat("Error: Debe ingresar 's' o 'n'.\n")
    save <- .ask_for_save()
  }

  save
}

.ask_for_auto_renew <- function() {
  auto_renew <- readline(prompt = "¿Desea que el token se renueve automáticamente? (s/n): ")
  if (!grepl("^[sn]$", auto_renew)) {
    cat("Error: Debe ingresar 's' o 'n'.\n")
    auto_renew <- .ask_for_auto_renew()
  }

  auto_renew
}

.ask_for_refresh_days <- function(days) {
  refresh_days <- readline(prompt = "Ingrese la cantidad de días antes de la expiración para renovar el token: ")
  if (!grepl("^[0-9]+$", refresh_days)) {
    cat("Error: Debe ingresar un número entero positivo.\n")
    refresh_days <- .ask_for_refresh_days(days)
  }

  if (as.numeric(refresh_days) >= as.numeric(days)) {
    cat("Error: La cantidad de días para renovar el token debe ser menor a la cantidad de días de validez del token.\n")
    refresh_days <- .ask_for_refresh_days(days)
  }

  refresh_days
}
