library(shiny)
library(ggplot2)
library(viridis)
library(dplyr)

# Define UI
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "View by father's education level instead:",
                  choices = c("Mother's Education Level" = "mom.edu",
                              "Father's Education Level" = "dad.edu"),
                  selected = "mom.edu")
    ),
    mainPanel(
      plotOutput("barPlot")
    )
  )
)


# Define server logic
server <- function(input, output) {
  
  df <- read.csv("cleaned_data.csv") # Load and prep data 
  
  edu_df <- df %>%
    mutate(higher.edu = case_when(
      higher.edu == 0 ~ "No", 
      higher.edu == 1 ~ "Yes", 
      TRUE ~ as.character(higher.edu)
    ))
  
  edu_df <- edu_df %>%
    mutate(mom.edu = case_when(
      mom.edu == "no education" ~ "No education", 
      mom.edu == "elementary school" ~ "Elementary school",
      mom.edu == "middle school" ~ "Middle school",
      mom.edu == "high school" ~ "High school", 
      mom.edu == "university or higher" ~ "University or higher", 
      TRUE ~ as.character(mom.edu)
    ))
  
  edu_df <- edu_df %>%
    mutate(dad.edu = case_when(
      dad.edu == "no education" ~ "No education", 
      dad.edu == "elementary school" ~ "Elementary school",
      dad.edu == "middle school" ~ "Middle school",
      dad.edu == "high school" ~ "High school", 
      dad.edu == "university or higher" ~ "University or higher", 
      TRUE ~ as.character(dad.edu)
    ))
  
  edu_df$mom.edu <- factor(edu_df$mom.edu, levels = c("No education", "Elementary school", "Middle school", "High school", "University or higher"))
  edu_df$dad.edu <- factor(edu_df$dad.edu, levels = c("No education", "Elementary school", "Middle school", "High school", "University or higher"))
  
  
  output$barPlot <- renderPlot({
    
    # Check which variable is selected
    variable <- switch(input$variable,
                       "mom.edu" = "Mother's Education Level",
                       "dad.edu" = "Father's Education Level")
    
    # Create the plot
    ggplot(edu_df, aes(y=.data[[input$variable]], fill=factor(higher.edu))) + 
      geom_bar(position = "dodge") + 
      geom_text(stat = "count", aes(label = after_stat(count)), 
                position = position_dodge(width = 0.9), vjust = 0.6, hjust = -0.3, size = 5) + 
      theme_minimal() + 
      labs(title = paste("Interest in Attending College by", variable), 
           x = "Count", y = variable, fill = "College") + 
      theme(plot.title = element_text(hjust = 0.5, size = 22), 
            axis.title = element_text(size = 20), 
            axis.text = element_text(size = 16), 
            # axis.text.x = element_blank(),
            legend.title = element_text(size = 16), 
            legend.text = element_text(size = 14)) + 
      scale_fill_manual(values = c("grey", "#0D0887FF")) 
      # guides(fill = FALSE)
  })
}


# Run the application
shinyApp(ui = ui, server = server)
