
#' REQUIRES: global.R

library(magrittr)
options(readr.default_locale=readr::locale(tz = ''))

## Generar usuarios iniciales
if (FALSE) {
  initialize_user_database() %>% 
    add_user(NULL, 'f0g00bq', '1234', c('basic', 'admin', 'owner')) %>% 
    .$users %>% 
    add_user(NULL, 'mrpania', '', c('basic', 'admin')) %>% 
    .$users %>% 
    add_user(NULL, 'moibar1', '', c('basic')) %>%
    .$users %>% 
    save_users(user_data_path)
}

## Login
loginUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    id = ns('panel'),
    shiny::wellPanel(
      shiny::h2(lang$login),
      shiny::textInput(ns('user'), lang$user),
      shiny::passwordInput(ns('password'), lang$password),
      shiny::actionButton(ns('user_login'), lang$login),
      shiny::tags$br(),
      shiny::tags$br(),
      shiny::tags$p(sprintf('Si no tienes usuario, puedes pedirlo a la liga %s o usar "guest" (no tiene contraseña).', lang$owner), style = 'color: red; font-weight: bold;'),
      shinyjs::hidden(
        shiny::div(
          id = ns("error"),
          shiny::tags$p(
            lang$wrong_user_or_password,
            style = "color: red; font-weight: bold; padding-top: 5px;", class = "text-center"
          )
        )
      )
    )
  )
}

login <- function(input, output, session, log_out) {
  ## Login
  credentials <- shiny::reactiveValues(user_auth = FALSE)
  
  shiny::observeEvent(log_out(), {
    futile.logger::flog.info(toJSON(list(
      session_info = msg_cred(shiny::reactiveValuesToList(credentials)),
      message = "LOGOUT SUCCESSFUL",
      details = list(
        status = 'SUCCESS',
        session = credentials$session
      )
    )))
    credentials$user_auth <- FALSE
    credentials$user <- NULL
    credentials$role <- NULL
    credentials$session <- NULL
  })
  
  ## Hide panel using JS
  # shiny::observeEvent(credentials$user_auth, ignoreInit = TRUE, {
  #   shinyjs::toggle(id = 'panel')
  # })
  
  shiny::observeEvent(input$user_login, {
    ns <- session$ns
    cred <- auth_user(input$user, input$password)
    futile.logger::flog.info(toJSON(list(
      session_info = msg_cred(shiny::reactiveValuesToList(credentials)),
      message = "ATTEMPTING USER LOGIN",
      details = list(
        credentials = cred
      )
    )))
    if (cred$user_auth) {
      credentials$user_auth <- TRUE
      credentials$user <- cred$user
      credentials$role <- cred$role
      credentials$session <- uid()
      futile.logger::flog.info(toJSON(list(
        session_info = msg_cred(shiny::reactiveValuesToList(credentials)),
        message = "LOGIN SUCCESSFUL",
        details = list(
          status = 'SUCCESS',
          session = credentials$session
        )
      )))
    } else {
      futile.logger::flog.info(toJSON(list(
        session_info = msg_cred(shiny::reactiveValuesToList(credentials)),
        message = "LOGIN FAILED",
        details = list(
          status = 'ERROR'
        )
      )))
      shinyjs::toggle(id = 'error', anim = TRUE, time = 1, animType = 'fade')
      shinyjs::delay(5000, shinyjs::toggle(id = "error", anim = TRUE, time = 1, animType = "fade"))
    }
  })
  
  shiny::reactive({
    shiny::reactiveValuesToList(credentials)
  })
}

## Logout
logoutUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::actionButton(ns("button"), lang$logout, class = "btn-danger")
  # shinyjs::hidden(
  #   shiny::actionButton(ns("button"), 'Log out', class = "btn-danger", style = "color: white;")
  # )
}

logout <- function(input, output, session, active) {
  # shiny::observeEvent(active(), ignoreInit = TRUE, {
  #   shinyjs::toggle(id = "button", anim = TRUE, time = 1, animType = "fade")
  # })
  shiny::reactive({input$button})
}


## Cambio de password
passwordUpdateUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::wellPanel(
    id = ns('password_update'),
    shiny::h2('Cambiar contraseña'),
    shiny::passwordInput(ns('old_password'), lang$old_password),
    shiny::passwordInput(ns('new_password_1'), lang$new_password_1),
    shiny::passwordInput(ns('new_password_2'), lang$new_password_2),
    shiny::actionButton(ns('button'), lang$button),
    shinyjs::hidden(
      shiny::div(
        id = ns("msg"),
        shiny::span(
          textOutput(ns('msg_text')),
          ## NO SIRVE ESTO
          style = "color: red; font-weight: bold; padding-top: 5px;", class = "text-center"
        )
      )
    )
  )
}

passwordUpdateServer <- function(input, output, session, credentials) {
  observeEvent(input$button, {
    if (input$new_password_1 != input$new_password_2) {
      futile.logger::flog.info(toJSON(list(
        session_info = msg_cred(credentials()),
        message = "ERROR CHANGING PASSWORD",
        details = list(
          status = 'ERROR',
          reason = 'New password mismatch'
        )
      )))
      output$msg_text <- renderText(lang$passwords_must_match)
      shinyjs::toggle(id = 'msg', anim = TRUE, time = 1, animType = 'fade')
      shinyjs::delay(5000, shinyjs::toggle(id = "msg", anim = TRUE, time = 1, animType = "fade"))
    } else {
      cred <- auth_user(credentials()$user, input$old_password)
      if (cred$user_auth) {
        output$msg_text <- renderText(lang$password_updated_successfully)
        users <- load_users(user_data_path) %>% 
          update_user(
            credentials = cred,
            user_to_update = cred$user,
            new_password = input$new_password_1,
            new_role = NULL
          )
        if (users$status == 0) {
          save_users(users$users, user_data_path)
        }
      } else {
        futile.logger::flog.info(toJSON(list(
          session_info = msg_cred(credentials()),
          message = "ERROR CHANGING PASSWORD",
          details = list(
            status = 'ERROR',
            reason = 'Current password is incorrect'
          )
        )))
        output$msg_text <- renderText(lang$wrong_current_password)
        shinyjs::toggle(id = 'msg', anim = TRUE, time = 1, animType = 'fade')
        shinyjs::delay(5000, shinyjs::toggle(id = "msg", anim = TRUE, time = 1, animType = "fade"))
      }
    }
  })
}


## Alta de usuarios
userManagementUI <- function(id) {
  ns <- shiny::NS(id)
  actions <- c('add', 'update_password', 'update_role', 'delete')
  names(actions) <- c(lang$add, lang$update_password, lang$update_role, lang$delete)
  shiny::wellPanel(
    id = ns('manage_users'),
    shiny::h2(lang$manage_users),
    shiny::uiOutput(ns('user_list_ui')),
    shiny::textInput(ns('new_user'), lang$user),
    shiny::textInput(ns('new_password'), lang$password),
    shiny::selectInput(ns('new_role'), lang$role, choices = rev(names(clearance_levels)), selected = 'basic', multiple = TRUE),
    shiny::radioButtons(ns('action'), lang$action, choices = actions),
    shiny::actionButton(ns('button'), lang$button),
    shinyjs::hidden(
      shiny::div(
        id = ns("msg"),
        shiny::span(
          textOutput(ns('msg_text')),
          ## NO SIRVE ESTO
          style = "color: red; font-weight: bold; padding-top: 5px;", class = "text-center"
        )
      )
    )
  )
}

userManagementServer <- function(input, output, session, credentials) {
  output$user_list_ui <- shiny::renderUI({
    input$button
    ns <- session$ns
    shiny::selectInput(
      inputId = ns('selected_user'),
      label = lang$selected_user,
      choices = sort(load_users(user_data_path) %>% map_chr('user'))
    )
  })
  shiny::observe({
    req(input$selected_user)
    ns <- session$ns
    user <- load_users(user_data_path) %>% keep(~.x$user == input$selected_user) %>% .[[1]]
    shiny::updateTextInput(session, 'new_user', value = user$user)
    shiny::updateTextInput(session, 'new_password', value = '')
    shiny::updateSelectInput(session, 'new_role', selected = user$role)
  })
  
  msg <- reactiveVal()
  output$msg_text <- renderText(msg())
  shiny::observeEvent(input$button, {
    
    users <- load_users(user_data_path)
    
    if (input$action == 'add') {
      users <- users %>% 
        add_user(credentials(), input$new_user, input$new_password, input$new_role)
    } else if (input$action == 'update_password') {
      users <- users %>% 
        update_user(credentials(), input$new_user, input$new_password, NULL)
    } else if (input$action == 'update_role') {
      users <- users %>% 
        update_user(credentials(), input$new_user, NULL, input$new_role)
    } else if (input$action == 'delete') {
      users <- users %>% 
        delete_user(credentials(), input$new_user)
    }
    
    if (users$status == 0) {
      save_users(users$users, user_data_path)
      msg(switch(
        input$action,
        add = lang$add_success,
        update_password = lang$update_password_success,
        update_role = lang$update_role_success,
        delete = lang$delete_success,
        lang$unknown_action
      ))
    } else {
      msg(switch(
        input$action,
        add = lang$add_error,
        update_password = lang$update_password_error,
        update_role = lang$update_role_error,
        delete = lang$delete_error,
        lang$unknown_action
      ))
    }
    
    shinyjs::show(id = 'msg', anim = TRUE, time = 1, animType = 'fade')
    shinyjs::delay(5000, shinyjs::hide(id = "msg", anim = TRUE, time = 1, animType = "fade"))
  })
}