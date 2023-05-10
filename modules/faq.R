
library(shiny)
library(tidyverse)

faqServer <- function(input, output, session) {
  
  bp_template_file <- 'data/template_BP.xlsx'
  
  output$download_bp_template <- downloadHandler(
    filename = function(){
      basename(bp_template_file)
    },
    content = function(file){
      file.copy(bp_template_file, file)
    }
  )
}

# 
faqUI <- function(id) {
  ns <- shiny::NS(id)
  
  fluidPage(
    tags$h2('FAQ - Preguntas frecuentes'),
    tags$br(),
    box(
      width = 12,
      HTML('<p><b>Q:</b> ¿Cómo puedo crear una cuenta?'),
      HTML('<p><b>A:</b> Pídele a cualquier usuario con permisos de administrador que la cree:</p>'),
      tags$ul(
        tags$li('Cualquier gerente de Planeación de la Demanda'),
        tags$li(sprintf('Cualquier miembro del equipo dueño de %s (%s, %s)', lang$app_name, lang$owner, lang$data_engineer))
      )
    ),
    box(
      width = 12,
      HTML('<p><b>Q:</b> ¿Cómo puedo recuperar mi contraseña?</p>'),
      HTML('<p><b>A:</b> Pídele a cualquier usuario con permisos de administrador que la cambie: <strong>Administración de usuarios > <em>seleccionar tu usuario</em> > Actualizar contraseña</strong></p>')
    ),
    box(
      width = 12,
      HTML('<p><b>Q:</b> ¿Por qué mi departamento no tiene el detalle a nivel categoría?</p>'),
      HTML(sprintf('<p><b>A:</b> Probablemente no está disponible el Business Plan a nivel departamento-categoría-formato. Si quieres tener el detalle, baja el formato, llénalo y envíaselo a %s.</p>', lang$owner)),
      HTML('<p><b>Nota:</b> Debes mantener el formato exacto, incluyendo los nombres de los formatos de negocio.</p>'),
      downloadButton(ns('download_bp_template'), lang$download_bp_template)
    )
  )
}