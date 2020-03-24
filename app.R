# Basado en https://deanattali.com/blog/building-shiny-apps-tutorial/
# y en https://mastering-shiny.org/


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
#setwd("/home/mato/Documentos/R/Shiny/APN")
#base <- read_csv2("credito-diario-2020.csv")

# base %>%
#   group_by(programa_desc) %>%
#   summarize("Crédito Vigente" = sum(credito_vigente), "Crédito Comprometido" = sum(credito_comprometido), "Crédito Devengado" = sum(credito_devengado)) %>%
  # adorn_totals(., where = "row", fill = "-", na.rm = TRUE, name = "Total") %>% # Totales
  # mutate_each(funs(prettyNum(., big.mark=".")))  #Formato de miles



ui <- fluidPage(
    headerPanel("Información Presupuestaria - Administración Pública Nacional"),
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
    tableOutput("tabla")
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
 
    programa <- reactive({
      req(input$servicio) 
      filter(base, servicio_desc %in% input$servicio)
    })
    observeEvent(programa(), {
      choices <- unique(programa()$programa_desc)
      updatePickerInput(session, "programa", choices = choices)
    })

    actividad <- reactive({
        req(input$programa)
        filter(base, programa_desc %in% input$programa & servicio_desc %in% input$servicio)
    })
    observeEvent(actividad(), {
        choices <- unique(actividad()$actividad_desc)
        updatePickerInput(session, "actividad", choices = choices)
    })

    inciso <- reactive({
      req(input$actividad)
      filter(base, actividad_desc %in% input$actividad & programa_desc %in% input$programa & servicio_desc %in% input$servicio)
    })
    observeEvent(inciso(), {
      choices <- unique(inciso()$inciso_desc)
      updatePickerInput(session, "inciso", choices = choices)
    })

    ################################
    ## Tabla
    ################################
        
    output$tabla <- renderTable({
        req(input$inciso)
        inciso() %>% 
          filter(inciso_desc == input$inciso) %>% 
          group_by("PROGRAMA"= programa_desc, "ACTIVIDAD" = actividad_desc,"INCISO" = inciso_desc) %>%
          summarize("CREDITO VIGENTE" = sum(credito_vigente), "CREDITO COMPROMETIDO" = sum(credito_comprometido), "CREDITO DEVENGADO" = sum(credito_devengado)) %>%
          adorn_totals(., where = "row", fill = "-", na.rm = TRUE, name = "Total") %>% # Totales
          mutate_each(funs(prettyNum(., decimal.mark=",", big.mark=".")))  #Formato de miles 
        })

    ################################
    ## Grafico
    ################################
    
    output$grafico <- renderPlot({
        actividad() %>%
            filter(programa_desc == input$programa)
                    ggplot(actividad(), aes(fill=inciso_desc, y=credito_vigente, x=inciso_desc)) + geom_bar(stat="identity") +
                    labs(title = "Credito Vigente por Inciso\n", x = "Inciso", y = "Credito Vigente") +
              guides(fill=guide_legend(title="Inciso"))
    })
        
}

shinyApp(ui = ui, server = server)
