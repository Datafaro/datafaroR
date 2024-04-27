#' Autenticar y obtener token de acceso
#'
#' Esta función verifica si existe información de autenticación previa almacenada y válida.
#' Si no encuentra información válida o el token ha expirado, solicita al usuario que se autentique nuevamente.
#' Si la autenticación es exitosa, genera y almacena un nuevo token de acceso para solicitudes futuras.
#' En caso de encontrar un token válido y no expirado, lo retorna para su uso inmediato.
#'
#' @param scope [character] Alcance de la autenticación. Por defecto es \code{"user"}.
#'  Opciones válidas son \code{"user"} y \code{"project"}. \code{"user"} estará
#'  disponible para todas las sesiones de R, mientras que \code{"project"} solo
#'  estará disponible para las sesiones de R en el proyecto actual.
#'
#' @return Retorna el token de acceso si la autenticación es exitosa o el token almacenado sigue siendo válido.
#'         En otros casos, guía al usuario para realizar una nueva autenticación.
#' @export
#'
#' @examples
#' \dontrun{
#' auth() # Ejecuta esta funci\u00f3n para autenticarte o validar el token existente.
#' }
auth <- function(scope = "user") {
  if (!requireNamespace("renviron", quietly = TRUE)) {
    utils::install.packages("renviron", repos = c("https://adatar-do.r-universe.dev", 'https://cloud.r-project.org'))
  }

  auth_info <- renviron::renviron_get("DATAFARO_AUTH", scope = scope, verbosity = 0)

  if (is.null(auth_info) || auth_info == "") {
    cli::cli_alert_warning("No se ha encontrado informaci\u00f3n de autenticaci\u00f3n.")
    cli::cli_alert_info("Por favor, autent\u00edquese.")
    return(login(scope = scope))
  }

  auth_info_list <- auth_info %>%
    .auth_decode(.seed = Sys.info()[["nodename"]]) %>%
    strsplit("|", fixed = TRUE) %>%
    unlist()

  if (length(auth_info_list) != 7) {
    return(login(scope = scope))
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
          .token,
          scope = scope
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
    cli::cli_alert_danger("El token es inv\u00e1lido o ha expirado.")
    cli::cli_alert_info("Por favor, autent\u00edquese nuevamente.")
    return(login(scope = scope))
  } else if (.res$status_code == 200) {
    return(invisible(.token))
  } else {
    cli::cli_alert_danger("Error al validar el token.")
    cli::cli_alert_info("Intente nuevamente por favor.")
    cli::cli_alert_info("Si el problema persiste, ejecuta `auth()`para autenticarte nuevamente.")
    cli::cli_alert_info("Si esto \u00faltimo no resuelve el problema contacte con soporte.")
  }

  return(invisible(.token))
}


#' Cerrar sesión y eliminar el token de acceso
#'
#' Esta función elimina la información de autenticación almacenada para el alcance especificado,
#' efectivamente cerrando la sesión del usuario. Esto es útil para garantizar que la información
#' sensible no permanezca accesible en sesiones no autenticadas posteriormente.
#'
#' @param scope [character] Alcance de la autenticación para la que se desea cerrar sesión.
#'                          Por defecto es \code{"user"}. Las opciones válidas son \code{"user"}
#'                          y \code{"project"}. \code{"user"} afecta todas las sesiones de R,
#'                          mientras que \code{"project"} solo afecta a la sesión de R en el
#'                          proyecto actual.
#'
#' @return No retorna valores. Invoca un mensaje que informa al usuario que la sesión
#'         ha sido cerrada correctamente.
#' @export
#'
#' @examples
#' \dontrun{
#' log_out() # Ejecuta esta funci\u00f3n para cerrar la sesión y eliminar el token de acceso.
#' }
log_out <- function(scope = "user") {
  renviron::renviron_delete("DATAFARO_AUTH", scope = scope)
  cli::cli_alert_info("Se ha cerrado la sesi\u00f3n.")
}

.get_auth <- function(.name, scope = 'user') {
  .auth_decode(renviron::renviron_get("DATAFARO_AUTH", scope = scope), Sys.info()[["nodename"]]) %>%
    .auth_encode(.name)
}

.auth_encode <- function(auth_info, .seed) {
  auth_info %>%
    serialize(NULL) %>%
    sodium::simple_encrypt(sodium::pubkey(.get_hashed_key(.seed))) %>%
    base64enc::base64encode()
}


.auth_decode <- function(auth_info, .seed) {
  . <- NULL
  auth_info %>%
    base64enc::base64decode() %>%
    sodium::simple_decrypt(., .get_hashed_key(.seed = .seed)) %>%
    unserialize()
}


login <- function(
    usuario = readline("Ingrese su nombre de usuario: "),
    pass = .ask_for_pass(),
    scope = "user") {

  tryCatch(
  # Realizar la solicitud con autenticaci\u00f3n b\u00e1sica
  .res <- httr2::request(glue::glue("{API_URL}/v1/login/")) %>%
    httr2::req_auth_basic(usuario, pass) %>%
    httr2::req_body_json(list()) %>%
    httr2::req_perform(),

    error = function (e) {
    }
  )

  .res <- httr2::last_response()

  .token <- NULL
  if (httr2::resp_status(.res) == 403) {
    cli::cli_alert_danger("Usuario o contrase\u00f1a incorrectos.")
    return(login(scope = scope))
#  } else if (httr2::resp_status(.res) == 403) {
#    cli::cli_alert_danger("El usuario no tiene permisos para acceder a este recurso.")
  } else if (httr2::resp_status(.res) == 200 || httr2::resp_status(.res) == 201) {
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
          cli::cli_alert_info(glue::glue("El token ({.token_name0}) expirar\u00e1 en {.days0} d\u00edas."))
          .token <- .generate_token(days = .days0, token_name = .token_name0, .token = token0$key, scope = scope)
        } else {
          .token <- .generate_token(.token = token0$key, scope = scope)
        }
        cat("\014")
        cli::cat_rule("\u00A1Autenticaci\u00f3n exitosa!")
      },
      error = function(e) {
        cat("Error: ", e$message, "\n")
      }
    )
  } else {
    cli::cli_alert_danger("Error al autenticar.")
    cli::cli_alert_info("Intente nuevamente por favor.")
    cli::cli_alert_info("Si el problema persiste, contacte con soporte.")
  }

  return(invisible(.token))
}


.ask_for_pass <- function() {
  tryCatch(
    {
      pass <- getPass::getPass("Ingrese su contrase\u00f1a: ")
    },
    error = function(e) {
      cli::cli_alert_info("No se pudo enmascarar la contrase\u00f1a en este entorno.")
      cli::cli_alert_info("Esta y ser\u00e1 visible y pudiera quedar almacenada en el historial de la sesi\u00f3n R.")
      pass <- readline("Ingrese su contrase\u00f1a (ser\u00e1 visible): ")
    }
  )
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
    .token,
    scope = 'user') {
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
        cli::cli_alert_danger("El API solo est\u00e1 disponible para usuarios con suscripci\u00f3n.")
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
      confirm = FALSE,
      scope = scope,
      verbosity = 0
    )
    return(invisible(new_token$token$key))
  } else {
    cat("Error en la solicitud \n")
  }
}

.ask_for_days <- function() {
  days <- readline(prompt = "Ingrese la cantidad de d\u00edas que desea que el token sea v\u00e1lido: ")
  if (!grepl("^[0-9]+$", days)) {
    cat("Error: Debe ingresar un n\u00famero entero positivo.\n")
    days <- .ask_for_days()
  }

  days
}


.ask_for_save <- function() {
  return("s")
  save <- readline(prompt = "\u00BFDesea guardar el archivo .Renviron? (s/n): ")
  if (!grepl("^[sn]$", save)) {
    cat("Error: Debe ingresar 's' o 'n'.\n")
    save <- .ask_for_save()
  }

  save
}

.ask_for_auto_renew <- function() {
  auto_renew <- readline(prompt = "\u00BFDesea que el token se renueve autom\u00e1ticamente? (s/n): ")
  if (!grepl("^[sn]$", auto_renew)) {
    cat("Error: Debe ingresar 's' o 'n'.\n")
    auto_renew <- .ask_for_auto_renew()
  }

  auto_renew
}

.ask_for_refresh_days <- function(days) {
  refresh_days <- readline(prompt = "Ingrese la cantidad de d\u00edas antes de la expiraci\u00f3n para renovar el token: ")
  if (!grepl("^[0-9]+$", refresh_days)) {
    cat("Error: Debe ingresar un n\u00famero entero positivo.\n")
    refresh_days <- .ask_for_refresh_days(days)
  }

  if (as.numeric(refresh_days) >= as.numeric(days)) {
    cat("Error: La cantidad de d\u00edas para renovar el token debe ser menor a la cantidad de d\u00edas de validez del token.\n")
    refresh_days <- .ask_for_refresh_days(days)
  }

  refresh_days
}
