
library(tidyverse)
library(jsonlite)
library(digest)

## Hashear simulación
sha <- function(x, ...) {
  UseMethod('sha')
}

sha.default <- function(x) {
  if (is.null(x)) {
    NULL
  } else {
    purrr::safely(sha.list)(x)$result
  }
}

sha.list <- function(x) {
  sha(jsonlite::toJSON(x))
}

sha.character <- function(x) {
  digest::digest(x, algo = 'sha1', serialize = FALSE)
}

sha.json <- sha.character

## Utilities para transformar timestamp
from_ts <- function(x) {
  format(lubridate::ymd_hms(x), '%Y%m%d-%H%M%S')
}

## Generar ruta de archivos de simulación
simulation_path <- function(x, dir) {
  if (is.simulation(x)) {
    filename <- x$path
  } else if (is.character(x) && length(x) == 1) {
    filename <- x
  } else {
    stop('x must be a single file name or a simulation.')
  }
  paste0(dir, '/', filename)
}

## Guardar simulación
write_simulation <- function(x, dir, overwrite = FALSE) {
  file <- simulation_path(x, dir)
  if (is.blob(x)) {
    if (!file.exists(file) || overwrite) {
      jsonlite::write_json(x, path = file)
    } else {
      warning('This simulation already exists: ', file)
    }
  } else if (is.tree(x)) {
    if (!dir.exists(file) || overwrite) {
      dir.create(file, recursive = TRUE)
      jsonlite::write_json(x, path = paste0(file, '/metadata'))
    }
  } else {
    stop('x must be a tree or a blob.')
  }
  return(0)
}

## Cargar simulación
read_simulation <- function(file, dir) {
  path <- simulation_path(file, dir)
  if (file_test('-f', path)) {
    file <- path
  } else if (file_test('-d', path)) {
    file <- paste0(path, '/metadata')
  }
  x <- jsonlite::fromJSON(file)
  class(x) <- c('list', 'simulation', x$type)
  x
}

## Eliminar simulación
delete_simulation <- function(x, dir) {
  stopifnot(simulation_exists(x, dir))
  file <- simulation_path(x, dir)
  if (file_test('-d', file)) {
    unlink(file, recursive = TRUE)
  } else {
    file.remove(file)
  }
}

## Checar si existe el archivo de una simulación
simulation_exists <- function(x, dir) {
  file.exists(simulation_path(x, dir)) # funciona para archivos y directorios
}

## Constructor S3 de una simulación genérica
simulation <- function(user, filters, dir, write = FALSE) {
  x <- list(
    type = 'simulation',
    user = user,
    timestamp = as.character(Sys.time()),
    filters = filters
  )
  x$sha <- substr(sha(x), 1, 7)
  x$path <- paste0(from_ts(x$timestamp), '_', x$sha)
  class(x) <- c(class(x), 'simulation')
  if (write) {
    write_simulation(x, dir)
  }
  x
}

## Constructor de estrategias de sharing
sharing_permissions <- function(visible = TRUE, locked = FALSE, unlock_deadline = Sys.Date() + 365) {
  list(
    visible = visible,
    locked = locked,
    unlock_deadline = unlock_deadline
  )
}

## Cambiar settings de sharing de un tree
modify_sharing <- function(x, dir, visible = TRUE, locked = FALSE, unlock_deadline = NULL, write = TRUE) {
  stopifnot(is.tree(x))
  original <- x
  x$sharing$visible <- visible
  x$sharing$locked <- locked
  x$sharing$unlock_deadline <- unlock_deadline
  if (write) {
    write_simulation(x, dir, overwrite = TRUE)
  }
  x
}

## Checar si un tree está bloqueado
is_unlocked <- function(x) {
  stopifnot(is.tree(x))
  if (!x$sharing$locked && Sys.Date() <= x$sharing$unlock_deadline) {
    return(TRUE)
  } else if (x$sharing$locked) {
    return('locked')
  } else {
    return('unlock_deadline')
  }
}

## Constructor S3 de una simulación con descendientes
tree <- function(user, filters, dir, sharing = sharing_permissions(), write = TRUE) {
  x <- simulation(user, filters, dir, write = FALSE)
  x$sharing <- sharing
  x$type = 'tree'
  class(x) <- c(class(x), 'tree')
  if (write) {
    write_simulation(x, dir)
  }
  x
}

## Constructor S3 de una simulación básica
blob <- function(user, filters, dir, simulation = NULL, write = TRUE) {
  x <- simulation(user, filters, dir, write = FALSE)
  x$type <- 'blob'
  sim_names <- c('sim_forecast_promocional', 'sim_no_resurtible', 'sim_fulfillment')
  simulation <- map(sim_names, ~ifelse(!is.null(simulation[[.x]]) & !is.na(simulation[[.x]]), simulation[[.x]], 0))
  x$simulation <- set_names(simulation, sim_names)
  class(x) <- c(class(x), 'blob')
  if (write) {
    write_simulation(x, dir)
  }
  x
}

## Método para imprimir simulaciones
print.simulation <- function(x) {
  cl <- tail(class(x), 1)
  class(x) <- 'list'
  cat(sprintf('Simulation %s:\n', cl))
  print(x)
}

## Funciones para checar simulaciones
is.simulation <- function(x) {
  inherits(x, 'simulation')
}

is.tree <- function(x) {
  inherits(x, 'tree')
}

is.blob <- function(x) {
  inherits(x, 'blob')
}

## Obtener descendientes de una simulación (depth = 1 corresponde a hijos directos)
descendants <- function(x, dir, depth = 1L, return_names = FALSE) {
  stopifnot(is.simulation(x) && depth >= 0)
  if (depth == 0 || is.blob(x)) {
    if (return_names) {
      return(x$path)
    } else {
      return(list(x))
    }
  } else if (is.tree(x)) {
    children_paths <- list.files(
      path = simulation_path(x, dir),
      pattern = '^[0-9]', # empiezan con la fecha; esto quita el metadata
      all.files = FALSE,
      full.names = FALSE,
      recursive = FALSE,
      ignore.case = FALSE,
      include.dirs = TRUE
    )
    if (length(children_paths) == 0) {
      if (return_names) {
        return(c())
      } else {
        return(list())
      }
    } else if (depth == 1L) {
      if (return_names) {
        return(children_paths)
      } else {
        return(map(children_paths, ~ read_simulation(.x, simulation_path(x, dir))))
      }
    } else {
      sp <- simulation_path(x, dir)
      return(do.call(c, map(
        descendants(x = x, dir = dir, depth = 1, return_names = FALSE),
        ~descendants(x = .x, dir = sp, depth = depth - 1, return_names = return_names)
      )))
    }
  }
}

## Operador "podría contener" (checar si x podría ponerse en los descendientes de y (i.e. la unidad descrita por x es un subconjunto de la descrita por y))
`%<=%` <- function(x, y) UseMethod('%<=%')
`%<=%.simulation` <- function(x, y) {
  stopifnot(is.simulation(y))
  isTRUE(all(map_lgl(
    intersect(names(y$filters), c(time_pred_filters, business_unit_filters)),
    ~ nombre_todos %in% y$filters[[.x]] || ## y tiene todo
      (
        !is.null(x$filters[[.x]]) && ## si es nulo, sería equivalente a que tiene todo
          length(setdiff(x$filters[[.x]], y$filters[[.x]])) == 0 ## y contiene todos los niveles de x en ese filtro
      )
  )))
}

## Operador equivalencia entre dos simulaciones (checar si dos simulaciones son al mismo nivel)
`%~%` <- function(x, y) UseMethod('%~%')
`%~%.simulation` <- function(x, y) {
  stopifnot(is.simulation(y))
  (x %<=% y) && (y %<=% x)
}

## Checar si x es un hijo de y
is_child <- function(x, y, dir) {
  stopifnot((is.simulation(x) || (is.character(x) && length(x) == 1)) && is.simulation(y))
  if (is.simulation(x)) {
    x <- x$path
  }
  if (is.tree(y)) {
    return(x %in% descendants(y, dir, depth = 1L, return_names = TRUE))
  } else {
    return(FALSE)
  }
}

## Checar si es válido agregar una simulación a un directorio
clashes_with_directory <- function(dir, new_child) {
  stopifnot(is.simulation(new_child))
  children_paths <- list.files(
    path = dir,
    pattern = '^[0-9]', # empiezan con la fecha; esto quita el metadata
    all.files = FALSE,
    full.names = FALSE,
    recursive = FALSE,
    ignore.case = FALSE,
    include.dirs = TRUE
  )
  children <- map(children_paths, ~ read_simulation(.x, dir))
  cond <- map_lgl(children, ~ identical(.x, new_child)) # En home/ sí puede haber clashes, así que sólo checamos repetidos con el mismo nombre
  if (any(cond)) {
    return(children[cond])
  } else {
    return(FALSE)
  }
}
## Checar si es válido agregar una simulación a un tree
clashes_with_children <- function(x, new_child, dir) {
  children <- descendants(x, dir, 1)
  cond <- map_lgl(children, ~ new_child %<=% .x || .x %<=% new_child)
  if (any(cond)) {
    return(children[cond])
  } else {
    return(FALSE)
  }
}

## Agregar descendientes
add_child <- function(x, dir, child, child_dir = dir, overwrite_children = FALSE) {
  stopifnot(is.tree(x))
  stopifnot(is.simulation(child) || (is.character(child) && length(child) == 1))
  if (is.character(child)) {
    if (simulation_exists(child, child_dir)) {
      child <- read_simulation(child, child_dir)
    } else {
      stop('simulation not found: ', child)
    }
  }
  if (child %<=% x) {
    clashes <- clashes_with_children(x, child, dir)
    if (!identical(clashes, FALSE) && !overwrite_children) {
      warning('New children filters must not be a subset of any siblings\' filters.')
    } else {
      if (!identical(clashes, FALSE)) {
        clash_paths <- map_chr(clashes, 'path')
        message('Overwriting conflicting children: ', paste(clash_paths, collapse = ', '))
        x <- remove_children(x = x, children = clashes, dir = dir)
      }
      file.copy(
        from = simulation_path(child, child_dir),
        to = simulation_path(x, dir),
        overwrite = FALSE,
        recursive = TRUE
      )
    }
  } else {
    warning('Children filters must be a subset of parent filters.')
  }
  x
}

add_children <- function(x, dir, children, children_dir = dir, overwrite_children = FALSE) {
  x <- reduce(c(list(x), children), ~add_child(x = .x, dir = dir, child = .y, child_dir = children_dir, overwrite_children = overwrite_children))
  x
}

## Quitar descendientes
remove_child <- function(x, child, dir) {
  stopifnot(is.tree(x))
  stopifnot(is.simulation(child) || (is.character(child) && length(child) == 1))
  if (is.simulation(child)) {
    child <- child$path
  }
  if (is_child(child, x, dir)) {
    unlink(simulation_path(child, simulation_path(x, dir)), recursive = TRUE)
  }
  x
}

remove_children <- function(x, children, dir) {
  x <- reduce(.init = x, .x = children, .f = ~remove_child(.x, .y, dir))
  x
}

## Convertir a blob
as.blob <- function(x, dir, write = TRUE) {
  stopifnot(is.list(x) && !is.null(x$user) && !is.null(x$timestamp) && !is.null(x$filters))
  if (!is.simulation(x)) {
    x <- blob(
      user = x$user,
      filters = x$filters,
      dir = dir,
      simulation = x$simulation,
      write = write
    )
  } else if (is.blob(x)) {
    x <- x
  } else if (is.tree(x)) {
    children <- descendants(x = x, dir = dir, depth = Inf)
    if (length(children) > 0) {
      consolidated_simulation <- children %>%
        map('simulation') %>% 
        transpose() %>% 
        map(unlist) %>% 
        map(~sum(.x, na.rm = TRUE))
    } else {
      consolidated_simulation <- NULL
    }
    x <- blob(
      user = x$user,
      filters = x$filters,
      dir = dir,
      simulation = consolidated_simulation,
      write = write
    )
  }
  x
}


## Obtener lista de trees de un usuario
get_sim_list <- function(path, credentials) {
  x <- list.files(path, full.names = FALSE) %>% 
    grep('metadata', ., value = TRUE, invert = TRUE) %>% 
    map(function(file){
      tryCatch({
        read_simulation(file, path)
      },
      error = function(e){
        futile.logger::flog.info(toJSON(list(
          session_info = msg_cred(credentials),
          message = 'ERROR READING SIMULATION DATA',
          details = list(
            file = file
          ))))
        NULL
      })
    }) %>% 
    discard(is.null)
  filters <- map(x, 'filters') %>% map(~.x[c(business_unit_filters, time_filters)])
  nrep <- max(nchar(unlist(filters)))
  order_str <- filters %>% 
    map_chr(function(fs){
      fs <- map_chr(fs, ~paste(.x, collapse = ''))
      first_filter <- which(fs != nombre_todos & !names(fs) %in% time_filters)[1]
      first_filter <- ifelse(is.na(first_filter), 0, first_filter)
      fs[fs == nombre_todos & seq_along(fs) > first_filter] <- strrep(' ', nrep)
      fs <- str_pad(fs, width = nrep, side = 'right', pad = ' ')
      paste(fs, collapse = '')
    })
  x[order(order_str, decreasing = FALSE, na.last = TRUE)]
}

## Formatear lista de simulaciones para selectInputs
title_choice <- function(x, list_max = 4) {
  buss <- title_business_unit(x, list_max = list_max)
  if (is.tree(x)) {
    dash <- ' /'
  } else {
    dash <- ''
  }
  sprintf('(%s) %s%s', x$sha, buss, dash)
}
title_detail <- function(x, list_max = 4) {
  usr <- title_user(x)
  dt <- title_dates(x)
  sprintf('%s :: %s', dt, usr)
}
get_sim_list_choices <- function(sim_list) {
  if (length(sim_list) == 0) {
    choices <- NULL
  } else {
    sim_names <- sim_list %>% 
      map_chr(title_choice)
    choices <- map_chr(sim_list, 'path')
    names(choices) <- sim_names
  }
  choices
}

# Pruebas -----------------------------------------------------------------


if (FALSE) {
  dr <- 'dev/sim/f'
  x <- blob(
    user = 'f',
    filters = list(
      vicepresidencia = 'abarr',
      departamento = 92,
      categoria = 10
    ),
    dir = dr,
    simulation = list(
      no_resurtible = 17
    )
  )
  
  y <- blob(
    user = 'f',
    filters = list(
      vicepresidencia = 'abarr',
      departamento = 92,
      categoria = 11
    ),
    dir = dr,
    simulation = list(
      no_resurtible = 35
    )
  )
  
  z <- blob(
    user = 'f',
    filters = list(
      vicepresidencia = 'abarr',
      departamento = 95
    ),
    dir = dr,
    simulation = list(
      no_resurtible = 35
    )
  )
  
  
  w <- tree(
    user = 'f',
    filters = list(
      vicepresidencia = 'abarr',
      departamento = 92
    ),
    dir = dr
  )
  w <- w %>%
    add_children(dr, list(x, y), dr)
  
  w2 <- tree(
    user = 'f',
    filters = list(
      vicepresidencia = 'abarr',
      departamento = 92
    ),
    dir = dr,
    children = NULL #list.files(dr)[1:2]
  ) %>%
    add_children(c('da34e2ed41f5ba76c8b74dec1f55ad5016421c05', '81d9678cc83699cff9b9d4196fc305e4c1afb26c'), dr)
  
  w3 <- tree(
    user = 'f',
    filters = list(
      vicepresidencia = 'abarr',
      departamento = 92
    ),
    dir = dr,
    children = NULL #list.files(dr)[1:2]
  ) %>%
    add_children(NULL, dr)
  
  v <- tree(
    user = 'f',
    filters = list(
      vicepresidencia = 'abarr'
    ),
    dir = dr
  )
  v %>% 
    add_children(dr, list(w, z))
  
  a <- tree(
    user = 'f',
    filters = list(),
    dir = dr
  )
  a %>% 
    add_children(dr, list(v))
  
  xs <- list.files(dr) %>% 
    map(~read_simulation(.x, dr))
  
  walk(xs, print)
  
  descendants(v, dr, 0)
  descendants(v, dr, 1)
  descendants(v, dr, 2)
  descendants(v, dr, Inf)
}






