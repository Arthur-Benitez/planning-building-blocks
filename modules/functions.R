
## Función genérica para renombrar
maybe_extract <- function(x, mapping){
  stopifnot(is.atomic(x))
  stopifnot(!is.null(names(mapping)))
  was_factor <- is.factor(x)
  x <- as.factor(x)
  idx <- levels(x) %in% names(mapping)
  levels(x)[idx] <- unlist(mapping[match(levels(x)[idx], names(mapping))])
  if (was_factor) {
    x
  } else {
    as.character(x)
  }
}

## Inicializar las columnas requeridas
init_col <- function(data, col, init_value = NA) {
  for (cc in col) {
    if (is.null(data[[cc]])) {
      data[[cc]] <- init_value
    }
  }
  data
}

## Renombrar variables para mostrar
get_pretty_names <- function(x) {
  maybe_extract(x, lang)
}

## Estandarizar IDs como strings
pad_id <- function(x) {
  str_pad(x, 2, 'left', '0')
}

## Generar depto-categoría
get_deptcat <- function(dept, cat) {
  paste(dept, cat, sep = '::')
}

## Extraer depto de deptcat
get_dept <- function(deptcat) {
  substr(deptcat, 1, 2)
}

## Extraer categoría de deptcat
get_cat <- function(deptcat) {
  substr(deptcat, 5, 6)
}

## Formato para números
format_number <- function(x, divide = 1e6, digits = 0) {
  round(x / divide, digits)
}

## Porcentajes (package:fg)
fg_percent <- function (x, digits = 0, force_plus_sign = FALSE) 
{
  res <- sprintf(sprintf("%%.%df%%%%", digits), 100 * x)
  if (force_plus_sign[[1]]) {
    res <- paste0(ifelse(x >= 0, "+", ""), res)
  }
  res
}

## Leer BP de una carpeta con muchos archivos
read_bp <- function(years, base_path = 'data/BP', ...) {
  file.path(base_path, years) %>% 
    keep(dir.exists) %>% 
    list.files(
      path = .,
      pattern = 'BP-D.*\\.csv',
      all.files = FALSE,
      full.names = TRUE,
      recursive = FALSE,
      ignore.case = FALSE,
      include.dirs = FALSE
    ) %>% 
    map_df(
      read_csv,
      col_types = cols(
        dept_nbr = col_number(),
        cat_nbr = col_number(),
        formato = col_character(),
        bp = col_number(),
        mes = col_number()
      )
    ) %>%
    set_names(tolower(names(.)))
}

## Obtener nombre de directorio donde se guardarán los datos
get_directory <- function(group, date) {
  paste('data', group, year(date), sep = '/')
}

## Obtener nombre de archivo donde se guardarán los datos 
get_filename <- function(dir, group, date) {
  group <- ifelse(group == 'detail', ' detalle', '')
  paste(dir, '/', format(today(), format = "%Y%m%d"), '_building_blocks_', format(date, '%Y%m'), group, '.csv', sep = '')
}

## Títulos resumidos
title_business_unit <- function(x, list_max = 4) {
  f <- x$filters
  if (exists('formatos_short') && !is.null(formatos_short)) {
    f$formato <- maybe_extract(f$formato, formatos_short)
  }
  f <- map_chr(f[business_unit_filters], function(y){
    if (length(y) == 0) {
      y <- nombre_todos
    }
    if (length(y) > 1) {
      y <- sort(y)
      if (length(y) > list_max) {
        y <- c(head(y, list_max), '...')
      }
      y <- paste(y, collapse = ',')
    }
    y
  })
  pref <- toupper(substr(business_unit_filters, 1, 1))
  strs <- paste0(pref, ':', f)[f != nombre_todos]
  if (length(strs) == 0) {
    strs_joined <- sprintf('(%s)', nombre_todos)
  } else {
    strs_joined <- paste('(', paste(strs, collapse = ' > '), ')')
  }
  strs_joined
}
title_dates <- function(x) {
  f <- x$filters
  f <- map(time_filters, ~ ifelse(is.null(f[[.x]]), '?', f[[.x]])) %>% set_names(time_filters)
  paste0(f$ym, ' @ ', f$fecha_calculo)
}
title_user <- function(x) {
  paste0(x$user, ' @ ', x$timestamp)
}
title_summary <- function(x, list_max = 4) {
  buss <- title_business_unit(x, list_max = list_max)
  dt <- title_dates(x)
  paste0(buss, ' :: ', dt)
}


## Función para hacer cascadas en ggplot2 (package:fg)
waterfall_data <- function(data, x, y, fill = 1, width = 0.6, total = 'TOTAL', decreasing = TRUE) {
  x <- enquo(x)
  y <- enquo(y)
  fill <- enquo(fill)
  data <- data %>% 
    transmute(x = factor(!!x), y = !!y, fill = !!fill) %>% 
    mutate(x = factor(x))
  if (decreasing) {
    lvls <- c(total, levels(data$x))
  } else {
    lvls <- c(levels(data$x), total)
  }
  data$x <- factor(data$x, levels = lvls)
  
  data2 <- data %>% 
    group_by(x) %>% 
    summarise(y = sum(y))
  n <- nrow(data2)
  s <- sum(data2$y)
  xmin <- seq_len(n + 1) - 0.5 * width
  xmax <- seq_len(n + 1) + 0.5 * width
  if (decreasing) {
    x <- factor(c(total, as.character(data2$x)), levels = lvls)
    ymax <- c(s, s - c(0, cumsum(data2$y)[-n]))
    ymin <- c(ymax - c(s, data2$y))
  } else {
    x <- factor(c(as.character(data2$x), total), levels = lvls)
    ymax <- c(cumsum(data2$y), s)
    ymin <- ymax - c(data2$y, s)
  }
  
  tots <- data %>% 
    group_by(fill) %>% 
    summarise(y = sum(y)) %>% 
    ungroup() %>% 
    mutate(x = factor(total, levels = lvls))
  data.frame(
    x = x,
    xmin = xmin,
    xmax = xmax,
    ymin = ymin,
    ymax = ymax
  ) %>% 
    left_join(bind_rows(data, tots), by = 'x') %>% 
    arrange(x, fill) %>% 
    group_by(x) %>% 
    mutate(
      ymin = ymin + c(0, lag(y)[-1]),
      ymax = ymin + y
    ) %>% 
    ungroup()
} 

sum_filters_data <- function(data, sum_variable = NULL) {
  resumen_vars <- c(business_unit_filters, 'ventas', 'forecast_base',
                    'forecast_promocional','no_resurtible', 'fulfillment_ref', 'gap', 'bp', 'ventas_piezas',
                    'forecast_base_piezas', 'forecast_promocional_piezas', 'fulfillment_ref_piezas')
  data %>%
    group_by(ym, fecha_calculo, !!!syms(sum_variable)) %>%
    summarise_at(
      vars(
        ventas,
        forecast_base,
        forecast_promocional, 
        no_resurtible,
        fulfillment_ref = fulfillment,
        bp,
        ventas_ly,
        ventas_piezas,
        forecast_base_piezas,
        forecast_promocional_piezas,
        fulfillment_ref_piezas = fulfillment_piezas,
        ventas_ly_piezas
      ),
      list(~sum(., na.rm = TRUE))
    ) %>% 
    mutate(gap = (bp - (ventas + forecast_base + forecast_promocional + no_resurtible))) %>%
    ungroup() %>%
    select(one_of(intersect(resumen_vars, names(.))), everything())
}
## Identificadores únicos basados en la hora y en el sistema (similar a UUID)
uid <- function() {
  digest::digest(list(Sys.time(), Sys.info(), runif(1)), 'sha256')
}

## Roles de credentials para el log
roles <- function(credentials) {
  paste0('[', paste(credentials$role, collapse = ', '), ']')
}

## Convertir credentials a string para el log
msg_id <- function(credentials) {
  if (is.character(credentials)) {
    msg <- credentials
  } else if (is.list(credentials)) {
    msgs <- c(
      credentials$user,
      roles(credentials),
      credentials$session
    )
    idx <- nchar(msgs) > 0
    msgs <- msgs[idx]
    keys <- c('user', 'role', 'session')[idx]
    msg <- paste(paste0(keys, ': ', msgs), collapse = '; ')
  } else {
    msg <- ''
  }
  sprintf('{%s}', msg)
}

## Seleccionar sólo user, role y session
msg_cred <- function(credentials) {
  idx <- intersect(c('user', 'role', 'session'), names(credentials))
  if (length(idx) == 0) {
    NULL
  } else {
    credentials[idx]
  }
}


# Login & Auth ------------------------------------------------------------

hash <- function(x) {
  digest::digest(x, algo = 'sha256', serialize = FALSE)
}

role_clearance <- function(role, clearance_levels) {
  lvls <- rep(Inf, length(role))
  idx <- role %in% names(clearance_levels)
  lvls[idx] <- clearance_levels[role[idx]]
  min(lvls)
}

user_clearance <- function(user, clearance_levels) {
  if (!is.list(user) || length(user$role) == 0) {
    return(Inf)
  }
  role_clearance(user$role, clearance_levels)
}

assert_clearance <- function(requester, affected, affected_new) {
  ## Add: affected = NULL, affected_new = user to add
  ## Update: affected = current user to change, affected_new = user after changes
  ## Delete: affected = current user to delete, affected_new = NULL
  requester_level <- user_clearance(requester, clearance_levels)
  affected_level <- user_clearance(affected, clearance_levels)
  affected_new_level <- user_clearance(affected_new, clearance_levels)
  if (is.null(requester$user)) {
    ## NULL has the highest clearance by default to override any settings
    verdict <- TRUE
  } else if (
    requester_level < affected_level &&
    requester_level <= affected_new_level
  ) {
    ## Permission to do anything to users with lower clearance level
    verdict <- TRUE
  } else if (
    !is.null(affected$user) &&
    !is.null(affected_new$user) &&
    requester$user == affected$user &&
    requester$user == affected_new$user &&
    requester_level <= affected_new_level
  ) {
    ## Can only edit self amongst users with same clearance, but can't change user name and can only maintain or lower clearance level
    ## WARNING: You could end up with no owners!
    verdict <- TRUE
  } else {
    verdict <- FALSE
  }
  verdict
}

## Funciones para leer y escribir usuarios
initialize_user_database <- function() {
  list()
}

load_users <- function(user_data_path) {
  readr::read_tsv(user_data_path, col_types = readr::cols(.default = readr::col_character()), na = 'NA') %>% 
    dplyr::mutate(role = stringr::str_split(role, ',')) %>% 
    purrr::transpose()
}

save_users <- function(users, user_data_path) {
  users %>% 
    transpose() %>% 
    tibble::as_tibble() %>% 
    dplyr::mutate_at(vars(user, password_hash), unlist) %>% 
    dplyr::mutate(role = purrr::map_chr(role, ~paste(.x, collapse=','))) %>% 
    readr::write_tsv(user_data_path)
  
  futile.logger::flog.info(toJSON(list(
    message = "SAVED USERS",
    details = list(
      status = 'SUCCESS',
      target = user_data_path
    )
  )))
}

delete_user <- function(users, credentials = NULL, user_to_delete) {
  idx <- which(unlist(purrr::map(users, 'user')) == user_to_delete)
  if (length(idx) == 1) {
    if (assert_clearance(credentials, users[[idx]], NULL)) {
      futile.logger::flog.info(toJSON(list(
        session_info = msg_cred(credentials),
        message = "DELETED USER",
        details = list(
          status = 'SUCCESS',
          target = msg_cred(users[[idx]])
        )
      )))
      users <- users[-idx]
      status <- 0
    } else {
      futile.logger::flog.info(toJSON(list(
        session_info = msg_cred(credentials),
        message = "ERROR DELETING USER",
        details = list(
          status = 'ERROR',
          target = msg_cred(users[[idx]]),
          reason = 'Insufficient clearance'
        )
      )))
      status <- 1
    }
  } else {
    futile.logger::flog.info(toJSON(list(
      session_info = msg_cred(credentials),
      message = "ERROR DELETING USER",
      details = list(
        status = 'ERROR',
        target = msg_cred(list(user = user_to_delete)),
        reason = 'User doesn\'t exist'
      )
    )))
    status <- 1
  }
  list(
    users = users,
    status = status
  )
}

add_user <- function(users, credentials = NULL, new_user, new_password, new_role) {
  idx <- which(unlist(purrr::map(users, 'user')) == new_user)
  #### ARREGLAR! sólo admins deben agregar admins, etc
  if (length(idx) == 0) {
    new <- list(
      user = new_user,
      role = new_role
    )
    if (assert_clearance(credentials, NULL, new)) {
      futile.logger::flog.info(toJSON(list(
        session_info = msg_cred(credentials),
        message = "CREATED USER",
        details = list(
          status = 'SUCCESS',
          target = msg_cred(new)
        )
      )))
      users <- users %>% 
        c(list(list(
          user = new_user,
          password_hash = hash(new_password),
          role = new_role
        )))
      status <- 0
    } else {
      futile.logger::flog.info(toJSON(list(
        session_info = msg_cred(credentials),
        message = "ERROR CREATING USER",
        details = list(
          status = 'ERROR',
          target = msg_cred(new),
          reason = 'Clearance insufficient'
        )
      )))
      status <- 1
    }
  } else {
    futile.logger::flog.info(toJSON(list(
      session_info = msg_cred(credentials),
      message = "ERROR CREATING USER",
      details = list(
        status = 'ERROR',
        target = msg_cred(list(user = new_user)),
        reason = 'User already exists'
      )
    )))
    status <- 1
  }
  list(
    users = users,
    status = status
  )
}

update_user <- function(users, credentials = NULL, user_to_update, new_password = NULL, new_role = NULL) {
  password_empty <- is.null(new_password) || length(new_password) == 0 # || nchar(new_password) == 0
  role_empty <- is.null(new_role) || length(new_role) == 0
  if (password_empty && role_empty) {
    futile.logger::flog.info(toJSON(list(
      session_info = msg_cred(credentials),
      message = "ERROR UPDATING USER",
      details = list(
        status = 'ERROR',
        target = msg_cred(list(user = user_to_update)),
        reason = 'No changes submitted'
      )
    )))
    status <- 1
  } else if (!password_empty && !role_empty) {
    futile.logger::flog.info(toJSON(list(
      session_info = msg_cred(credentials),
      message = "ERROR UPDATING USER",
      details = list(
        status = 'ERROR',
        target = msg_cred(list(user = user_to_update)),
        reason = 'Cannot update password and role at the same time'
      )
    )))
    status <- 1
  } else {
    idx <- which(unlist(purrr::map(users, 'user')) == user_to_update)
    if (length(idx) == 1) {
      new <- list(
        user = user_to_update,
        role = new_role
      )
      if (assert_clearance(credentials, users[[idx]], new)) {
        ## A: Esto es excluyente con B
        if (!password_empty) {
          if (length(new_password) == 1) {
            new_password_hash <- hash(new_password)
            futile.logger::flog.info(toJSON(list(
              session_info = msg_cred(credentials),
              message = "UPDATED USER PASSWORD",
              details = list(
                status = 'SUCCESS',
                target = msg_cred(users[[idx]])
              )
            )))
            users[[idx]]$password_hash <- new_password_hash
            status <- 0
          } else {
            futile.logger::flog.info(toJSON(list(
              session_info = msg_cred(credentials),
              message = "ERROR UPDATING PASSWORD",
              details = list(
                status = 'ERROR',
                target = msg_cred(users[[idx]]),
                reason = 'The new password must be a character vector of length = 1'
              )
            )))
            status <- 1
          }
        }
        ## B: Esto es excluyente con A
        if (!role_empty) {
          if (length(new_role) > 0) {
            futile.logger::flog.info(toJSON(list(
              session_info = msg_cred(credentials),
              message = "UPDATED USER ROLE",
              details = list(
                status = 'SUCCESS',
                target = msg_cred(users[[idx]]),
                updated_target = msg_cred(list(user = users[[idx]]$user, role = new$role))
              )
            )))
            users[[idx]]$role <- new_role
            status <- 0
          } else {
            futile.logger::flog.info(toJSON(list(
              session_info = msg_cred(credentials),
              message = "ERROR UPDATING ROLE",
              details = list(
                status = 'ERROR',
                target = msg_cred(users[[idx]]),
                reason = 'New role must be a character vector of length >= 1'
              )
            )))
            status <- 1
          }
        }
      } else {
        futile.logger::flog.info(toJSON(list(
          session_info = msg_cred(credentials),
          message = "ERROR UPDATING USER",
          details = list(
            status = 'ERROR',
            target = msg_cred(new),
            reason = 'Insufficient clearance'
          )
        )))
        status <- 1
      }
    } else {
      futile.logger::flog.info(toJSON(list(
        session_info = msg_cred(credentials),
        message = "ERROR UPDATING USER",
        details = list(
          status = 'ERROR',
          target = msg_cred(list(user = user_to_update)),
          reason = 'User doesn\'t exist'
        )
      )))
      status <- 1
    }
  }
  
  list(
    users = users,
    status = status
  )
}

is_local <- function() {
  !('mlutils' %in% installed.packages()[ , 1])
}

run_query <- function(db, UID = NULL, PWD = NULL, connector = NULL, query, stringsAsFactors = FALSE, ...) {
  if (is_local()) {
    futile.logger::flog.info('Running locally via ODBC.')
    
    if (is.null(UID) || is.null(PWD)) {
      stop('UID and PWD must be provided to establish a connection.')
    }
      
    if (db == 'teradata') {
      ch <- DBI::dbConnect(
        odbc::odbc(),
        Driver = "Teradata",
        DBCName = 'WM3',
        AUTHENTICATION = "ldap",
        UID = UID,
        PWD = PWD
        )
    } else if (db == 'dsn5') {
      ch <- DBI::dbConnect(
        odbc::odbc(),
        dsn = "dsn5",
        uid = UID,
        pwd = PWD
      )
    }
    connector <- NULL
  } else {
    futile.logger::flog.info('Element environment detected. Running using Element connectors.')
    if (is.null(connector)) {
      stop('A connector must be provided to download data in Element.')
    }
    ch <- NULL
  }
  if (is.null(ch)) {
    res <- mlutils::dataset.load(name = connector, query = query, ...)
  } else {
    res <- DBI::dbGetQuery(ch, query)
    DBI::dbDisconnect(ch)
  }
  if (!stringsAsFactors) {
    res <- res %>%
      mutate_if(is.factor, as.character)
  }
  tibble::as_tibble(res)
}

auth_user <- function(input_user, input_password) {
  users <- load_users(user_data_path)
  idx <- which(unlist(purrr::map(users, 'user')) == input_user)
  if (length(idx) == 1) {
    if (hash(input_password) == users[[idx]]$password_hash) {
      return(list(
        user_auth = TRUE,
        user = input_user,
        role = users[[idx]]$role
      ))
    }
  }
  return(list(
    user_auth = FALSE,
    user = NULL,
    role = NULL
  ))
}

print.numeric <- function(x, digits = 2) {
  x %>% 
    formatC(
      format = "f",
      digits = digits, 
      big.mark = ","
    ) %>% 
    replace(which(str_detect(., 'NA|NaN|Inf')), values = '')
}

print_percentage <- scales::label_comma(accuracy = 0.1, scale = 100, suffix = '%')

