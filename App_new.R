#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(readr)
library(ggplot2)
library(viridis)


data_shiny <- filtered_final

# Define UI for the Shiny app
ui <- fluidPage(
  titlePanel("NY Times Best Seller Books"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "VarX",
        label = "Select the Author:",
        choices = unique(data_shiny$author)
      )
    ),
    mainPanel(
      plotOutput("barChart")
    )
  )
)

server <- function(input, output) {
  # Filter data based on the selected author
  selected_author_data <- reactive({
    subset(data_shiny, author == input$VarX)
  })

  output$barChart <- renderPlot({
    # Count the number of titles for the selected author
    counts <- table(selected_author_data()$title)

    # Create a bar chart using ggplot2 with viridis color palette
    ggplot(data = as.data.frame(counts), aes(x = Var1, y = Freq, fill = Var1)) +
      geom_bar(stat = "identity", color = "white") +
      labs(title = "Bar Plot of Books by Author", x = "Book Name", y = "Rank") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 25, hjust = 1),
            plot.title = element_text(face = "bold", size = 16)) +
      scale_fill_viridis_d() +
      guides(fill = guide_legend(title = "Author"))
  })
}

# Run the application
shinyApp(ui = ui, server = server)

