library(shiny)
library(ggplot2)
library(ggridges)
library(viridis)

# Define UI
ui <- fluidPage(
  # titlePanel("Weekday vs. Weekend Alcohol Consumption"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "View weekend alcohol consumption instead:",
                  choices = c("Weekday Alcohol Consumption" = "weekday.alc",
                              "Weekend Alcohol Consumption" = "weekend.alc"),
                  selected = "weekday.alc")
    ),
    mainPanel(
      plotOutput("density_plot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  df <- read.csv("cleaned_data.csv") 
  
  output$density_plot <- renderPlot({
    # Check which variable is selected
    variable <- switch(input$variable,
                       "weekday.alc" = "Weekday Alcohol Consumption",
                       "weekend.alc" = "Weekend Alcohol Consumption")
    
    # Create the plot
    ggplot(df, aes(x = final.grade, y = factor(.data[[input$variable]]), fill = factor(.data[[input$variable]]))) + 
      geom_density_ridges() + 
      scale_fill_viridis_d(5, option = "plasma") +
      theme_minimal() + 
      scale_y_discrete(breaks = 1:5, labels = c("Very low", "Low", "Moderate", "High", "Very high")) +
      labs(y = "Alcohol Consumption Level", x = "Final Grade", title =
           paste("Effect of", variable, "on Final Grade")) + 
      theme(legend.position = "none", 
            plot.title = element_text(hjust=0.5, size=22), 
            axis.title = element_text(size=20), 
            axis.text.x = element_text(size=16), 
            axis.text.y = element_text(size=16))  +
      scale_x_continuous(breaks = seq(0, 100, by = 10)) 
  })
}

# Run the application
shinyApp(ui = ui, server = server)