
library(shiny)
library(ggplot2)
library(viridis)

colors <- c("gold2", "orange2", "darkorange", "salmon3", 
            "orchid4", "purple3", "blue3", "blue4", "midnightblue")

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput("binwidth", "Bin Width:", min = 0.5, max = 2, value = 1, step = 0.1),
      selectInput(inputId = "color",
                  label = "Fill Color",
                  choices = colors,
                  selected = "blue4"),  
      checkboxInput("facet_gender", "Group by Sex", value=FALSE) # Facet by gender
    ),
    mainPanel(
      plotOutput("plot", height="600px"), 
      uiOutput("annotation") # Output annotation when plot renders
    )
  )
)

server <- function(input, output) {
  df <- read.csv("cleaned_data.csv") 
  
  output$plot <- renderPlot({
    p <- ggplot(df, aes(x=drinking, y=health.status)) + 
      stat_bin_2d(binwidth = input$binwidth, color = "black") + # Change the bin width
      scale_fill_gradient(low = "white", high = input$color) +
      theme_minimal() + 
      labs(title = "Alcohol Consumption vs. Health Status", x = "Alcohol Consumption Level", y = "Health Status", fill = "Count") + 
      scale_x_continuous(breaks = 1:5, labels = c("Very low", "Low", "Moderate", "High", "Very high")) +
      scale_y_continuous(breaks = 1:5, labels = c("Very poor", "Poor", "Moderate", "Good", "Very good")) + # Make text bigger 
      theme(plot.title = element_text(hjust = 0.5, size=22), 
            axis.title = element_text(size = 22),
            axis.text.x = element_text(size = 14, angle = 45, vjust = 0.6),
            axis.text.y = element_text(size = 14), 
            plot.margin = margin(100, 100, 0, 100)) # Create margin where annotation will go
  
    if (input$facet_gender) { # Facet by gender 
      p <- p + facet_grid(. ~ sex, labeller = labeller(sex=c("F"="Female", "M"="Male"))) + 
        theme(strip.text = element_text(size = 20))
    }
  
    p
  })


output$annotation <- renderUI({ 
  tagList(
    div(style = "text-align: center; margin-top: -575px; margin-bottom: 20px; font-size: 14px;", 
        "This app visualizes the relationship between high school students' 
        drinking habits and health status. View all the students together, or 
        group by gender!")
   )
 })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)


