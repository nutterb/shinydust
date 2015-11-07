library(shiny)
library(pixiedust)
library(shinydust)
library(dplyr)

options(pixiedust_print_method = "html")

server <- shinyServer(function(input, output) {
  
  currentValue <- reactive({
    c(if (is.null(input$item1)) "" else input$item1,
      if (is.null(input$item2)) "" else input$item2,
      if (is.null(input$item3)) "" else input$item3,
      if (is.null(input$item4)) "" else input$item4)
  })
  
  output$table <- 
    renderText({
      data.frame(generic = paste0("Text Item ", 1:4),
                 stringsAsFactors = FALSE) %>%
        mutate(textInputColumn = textInput_html(inputId = paste0("item", 1:4))) %>%
        cbind(currentValue()) %>%
        dust() %>%
        sprinkle_colnames("Item Description", "Further Input", "Current Value") %>%
        sprinkle(border = "all",
                 border_color = "#A9A9A9",
                 bg_pattern = c("#DCDCDC", "#F5F5F5"),
                 pad = 7) %>%
        sprinkle(cols = 1,
                 width = 2,
                 width_units = "in") %>%
        print(asis = FALSE)
        
   })
})

ui <- shinyUI(fluidPage(
  wellPanel(
    uiOutput("table")
  )
))

shinyApp(ui = ui, server = server) 