library(shiny)
library(plotly)
library(gridlayout)
library(bslib)
library(DT)
library(ggplot2)
library(dplyr)

ui <- grid_page(
  layout = c(
    "header  header   header  ",
    "sidebar bluePlot bluePlot",
    "sidebar bluePlot bluePlot",
    "area5   area4    plotly  "
  ),
  row_sizes = c(
    "65px",
    "1.38fr",
    "0.35fr",
    "1.27fr"
  ),
  col_sizes = c(
    "250px",
    "0.95fr",
    "1.05fr"
  ),
  gap_size = "1rem",
  grid_card(
    area = "sidebar",
    card_header("Filters"),
    card_body(
      sliderInput(
        inputId = "numChicks",
        label = "Number of Chicks",
        min = 1,
        max = 50,
        value = 6,
        width = "100%"
      )
    )
  ),
  grid_card_text(
    area = "header",
    content = "Chicks Weight Visual App",
    alignment = "center",
    is_title = FALSE
  ),
  grid_card_plot(area = "bluePlot"),
  grid_card(
    area = "plotly",
    card_header("Interactive Plot"),
    card_body(
      plotlyOutput(
        outputId = "distPlot",
        width = "100%",
        height = "100%"
      )
    )
  ),
  grid_card(
    area = "area4",
    card_header("Interactive Plot"),
    card_body(plotlyOutput(outputId = "plot"))
  ),
  grid_card(
    area = "area5",
    card_header("Note"),
    card_body(
      "Further deep dives to understand the relationship between chick weight and diet. 
            Note: The ChickWeight data frame has 578 rows and 4 columns from an experiment on the effect of diet on early growth of chicks.
            "
    )
  )
)


server <- function(input, output) {
   
  output$distPlot <- renderPlotly({
    # generate bins based on input$bins from ui.R
    df = ChickWeight
    plot_ly(df, x = ~ weight, type = "histogram")|>
      layout(title = "Chick Weight Distribution")
  })
  
  output$bluePlot <- renderPlot({
    # generate bins based on input$bins from ui.R

    df = ChickWeight

    df|>
      filter(as.numeric(Chick)< input$numChicks)|>
      ggplot() +
      aes(x = Time, y = weight, colour = Chick) +
      geom_line() +
      scale_color_hue(direction = 1) +
      labs(title = "Chicks Weight over time", color = "number of chicks") +
      theme_minimal()
    

  })
  
  output$myTable <- renderDT({
    head(faithful, input$numRows)
  })

  output$plot <- renderPlotly({
    df = ChickWeight
    plot_ly(df, y = ~weight, color = ~Diet, type = "box")|>
      layout(title = "Distribution of chick weights by Diet")
    
  })
}

shinyApp(ui, server)
  

