# Basado en https://deanattali.com/blog/building-shiny-apps-tutorial/
# y en https://mastering-shiny.org/

library(kableExtra)
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(dplyr)
library(kableExtra)
library(tidyverse)
library(tidyquant)
library(knitr)
library(dslabs)
library(janitor)
setwd("/home/mato/Documentos/R/Shiny/shinyAPN")
# base <- read_csv2("credito-diario-2020.csv")

# base %>%
#   group_by(programa_desc) %>%
#   summarize("Crédito Vigente" = sum(credito_vigente), "Crédito Comprometido" = sum(credito_comprometido), "Crédito Devengado" = sum(credito_devengado)) %>%
  # adorn_totals(., where = "row", fill = "-", na.rm = TRUE, name = "Total") %>% # Totales
  # mutate_each(funs(prettyNum(., big.mark=".")))  #Formato de miles



ui <- fluidPage(
  shinythemes::themeSelector(),
    headerPanel("Información Presupuestaria - Administración Pública Nacional"),
    h1("Subtítulo", align = "center"),
    fluidRow(
      column(2,
        pickerInput(
          inputId = "jurisdiccion", 
          label = "Jurisdiccion", 
          choices = unique(base$jurisdiccion_desc), 
          options = list(
            `actions-box` = TRUE, 
            size = 10,
            `selected-text-format` = "count > 3"
          ), 
          multiple = TRUE
      )),
      column(2,
        pickerInput(
          inputId = "servicio", 
          label = "Servicio", 
          choices = NULL, 
          options = list(
            `actions-box` = TRUE, 
            size = 10,
            `selected-text-format` = "count > 3"
          ), 
          multiple = TRUE
      )),      
      column(2,
        pickerInput(
          inputId = "programa", 
          label = "Programa", 
          choices = NULL, 
          options = list(
            `actions-box` = TRUE, 
            size = 10,
            `selected-text-format` = "count > 3"
          ), 
          multiple = TRUE
      )),      
      column(2,
        pickerInput(
          inputId = "actividad", 
          label = "Actividad", 
          choices = NULL, 
          options = list(
            `actions-box` = TRUE, 
            size = 10,
            `selected-text-format` = "count > 3"
          ), 
          multiple = TRUE
        )),            
      column(2,
        pickerInput(
          inputId = "inciso", 
          label = "Inciso", 
          choices = NULL, 
          options = list(
            `actions-box` = TRUE, 
            size = 10,
            `selected-text-format` = "count > 3"
          ), 
          multiple = TRUE
      )),      
    ),
    plotOutput("grafico"),
    br(), br(),
    tableOutput("tabla"),
)

server <- function(input, output, session) {
  #
  # Generamos los menues anidados, tal como lo expican en https://mastering-shiny.org/action-dynamic.html#hierarchical-select
  #
    jurisdiccion <- reactive({
        filter(base, jurisdiccion_desc %in% input$jurisdiccion)
    })
    observeEvent(jurisdiccion(), {
        choices <- unique(jurisdiccion()$servicio_desc)
        updatePickerInput(session, "servicio", choices = choices) 
    })
 
    servicio <- reactive({
      req(input$jurisdiccion)
      filter(base, jurisdiccion_desc %in% input$jurisdiccion & servicio_desc %in% input$servicio)
    })
    observeEvent(servicio(), {
      choices <- unique(servicio()$programa_desc)
      updatePickerInput(session, "programa", choices = choices)
    })

    programa <- reactive({
      req(input$servicio)
      filter(base, jurisdiccion_desc %in% input$jurisdiccion & servicio_desc %in% input$servicio & programa_desc %in% input$programa)
    })
    observeEvent(programa(), {
      choices <- unique(programa()$actividad_desc)
      updatePickerInput(session, "actividad", choices = choices)
    })

    actividad <- reactive({
        req(input$programa)
        filter(base, jurisdiccion_desc %in% input$jurisdiccion & servicio_desc %in% input$servicio & programa_desc %in% input$programa & actividad_desc %in% input$actividad)
    })
    observeEvent(actividad(), {
        choices <- unique(actividad()$inciso_desc)
        updatePickerInput(session, "inciso", choices = choices)
    })

    inciso <- reactive({
      req(input$actividad)
      filter(base, jurisdiccion_desc %in% input$jurisdiccion & servicio_desc %in% input$servicio & programa_desc %in% input$programa & actividad_desc %in% input$actividad & inciso_desc %in% input$inciso)
    })


    ################################
    ## TablaInciso
    ################################
        
    output$tabla <- renderTable({
      req(input$jurisdiccion) # Para que tenga datos al renderizar la tabla y no tire un error, que desaparece al seleccionar alguna jurisdiccion
      
      # Verifica que campos se elijen en la UI, en forma de cascada, y los va cargando al vector "tabla", que es el que define los campos por los que se va a agrupar.
      # Si sólo se elije Jurisidccion, "tabla" contiene sólo "jurisdiccion_desc" y la tabla muestra el group_by_at sólo por jurisdiccin
      
      if (length(input$jurisdiccion)) {
        grupo <- c("jurisdiccion_desc")
        table_data <- jurisdiccion()
      }
      if (length(input$servicio)) {
        grupo <- c("jurisdiccion_desc", "servicio_desc")
        table_data <- servicio()
      }
      if (length(input$programa)) {
        grupo <- c("jurisdiccion_desc", "servicio_desc", "programa_desc")
        table_data <- programa()
      }
      if (length(input$actividad)) {
        grupo <-
          c("jurisdiccion_desc",
            "servicio_desc",
            "programa_desc",
            "actividad_desc")
        table_data <- actividad()
      }
      if (length(input$inciso)) {
        grupo <-
          c(
            "jurisdiccion_desc",
            "servicio_desc",
            "programa_desc",
            "actividad_desc",
            "inciso_desc"
          )
        table_data <- inciso()
      }
      
      
      table_data %>%
        group_by_at(grupo) %>%
        summarize(sum(credito_vigente))  %>%
        adorn_totals(
          .,
          where = "row",
          fill = "-",
          na.rm = TRUE,
          name = "Total"
        ) %>% # Totales
        mutate_each(funs(prettyNum(., big.mark = ".")))  #Formato de miles
      
    })
    

    ################################
    ## Grafico
    ################################
    
    output$grafico <- renderPlot({
      actividad() %>%
        filter(programa_desc == input$programa)
      ggplot(actividad(),
             aes(fill = inciso_desc, y = credito_vigente, x = inciso_desc)) + geom_bar(stat =
                                                                                         "identity") +
        labs(title = "Credito Vigente por Inciso\n", x = "Inciso", y = "Credito Vigente") +
        guides(fill = guide_legend(title = "Inciso"))
    })
    

    
}
shinyApp(ui = ui, server = server)
