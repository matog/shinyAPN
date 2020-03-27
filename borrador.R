################################
## TablaServicio
################################

output$tabla_servicio <- renderTable({
  req(input$jurisdiccion)
  req(input$servicio)
  # req(input$programa)
  # req(input$inciso)
  jurisdiccion() %>%
    filter(jurisdiccion_desc %in% input$jurisdiccion, servicio_desc %in% input$servicio) %>%
    group_by("JURISDICCION"= jurisdiccion_desc, "SERVICIO" = servicio_desc) %>%
    summarize("CREDITO VIGENTE" = sum(credito_vigente), "CREDITO COMPROMETIDO" = sum(credito_comprometido), "CREDITO DEVENGADO" = sum(credito_devengado)) %>%
    adorn_totals(., where = "row", fill = "-", na.rm = TRUE, name = "Total") %>% # Totales
    mutate_each(funs(prettyNum(., decimal.mark=",", big.mark=".")))  #Formato de miles
})
################################
## TablaPrograma
################################

output$tabla_programa <- renderTable({
  req(input$jurisdiccion)
  req(input$servicio)
  req(input$programa)
  programa() %>%
    filter(jurisdiccion_desc %in% input$jurisdiccion, servicio_desc %in% input$servicio, programa_desc %in% input$programa) %>%
    group_by("JURISDICCION"= jurisdiccion_desc, "SERVICIO" = servicio_desc, "PROGRAMA"= programa_desc) %>%
    summarize("CREDITO VIGENTE" = sum(credito_vigente), "CREDITO COMPROMETIDO" = sum(credito_comprometido), "CREDITO DEVENGADO" = sum(credito_devengado)) %>%
    adorn_totals(., where = "row", fill = "-", na.rm = TRUE, name = "Total") %>% # Totales
    mutate_each(funs(prettyNum(., decimal.mark=",", big.mark=".")))  #Formato de miles
})
# 
# ################################
# ## TablaJurisdicción
# ################################
# 
output$tabla_jurisdiccion <- renderTable({
  req(input$jurisdiccion)
  # req(input$servicio)
  # req(input$programa)
  # req(input$inciso)
  jurisdiccion() %>%
    filter(jurisdiccion_desc == input$jurisdiccion) %>%
    group_by("Jurisdiccion"= jurisdiccion_desc) %>%
    summarize("CREDITO VIGENTE" = sum(credito_vigente), "CREDITO COMPROMETIDO" = sum(credito_comprometido), "CREDITO DEVENGADO" = sum(credito_devengado)) %>%
    adorn_totals(., where = "row", fill = "-", na.rm = TRUE, name = "Total") %>% # Totales
    mutate_each(funs(prettyNum(., decimal.mark=",", big.mark=".")))  #Formato de miles
})
