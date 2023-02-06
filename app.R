#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load packages ------------------------------------------
library(shiny)
library(ggplot2)
library(tools)
library(shinythemes)


# Load data --------------------------------------------------------------------

load("movies.RData")

# Define UI --------------------------------------------------------------------

## Add image with image tag 
# ui <- fluidPage(
#     titlePanel("An image"),
#     tags$img(___),
# )


ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            selectInput(
                inputId = "y",
                label = "Y-axis:",
                choices = c(
                    "IMDB rating" = "imdb_rating",
                    "IMDB number of votes" = "imdb_num_votes",
                    "Critics Score" = "critics_score",
                    "Audience Score" = "audience_score",
                    "Runtime" = "runtime"
                ),
                selected = "audience_score"
            ),
            
            selectInput(
                inputId = "x",
                label = "X-axis:",
                choices = c(
                    "IMDB rating" = "imdb_rating",
                    "IMDB number of votes" = "imdb_num_votes",
                    "Critics Score" = "critics_score",
                    "Audience Score" = "audience_score",
                    "Runtime" = "runtime"
                ),
                selected = "critics_score"
            ),
            
            # Building a reactive widget ------------------------------------
            
            selectInput(
                inputId = "z",
                label = "Color by:",
                choices = c(
                    "Title Type" = "title_type",
                    "Genre" = "genre",
                    "MPAA Rating" = "mpaa_rating",
                    "Critics Rating" = "critics_rating",
                    "Audience Rating" = "audience_rating"
                ),
                selected = "mpaa_rating"
            ),
            
            sliderInput(
                inputId = "alpha",
                label = "Alpha:",
                min = 0, max = 1,
                value = 0.5
            ),
            
            sliderInput(
                inputId = "size",
                label = "Size:",
                min = 0, max = 5,
                value = 2
            ),
            
            textInput(
                inputId = "plot_title",
                label = "Plot title",
                placeholder = "Enter text to be used as plot title"
            ),
            
            checkboxGroupInput(
                inputId = "selected_type",
                label = "Select movie type(s):",
                choices = c("Documentary", "Feature Film", "TV Movie"),
                selected = "Feature Film"
            ),
            
            actionButton(
                inputId = "update_plot_title",
                label = "Update plot title"
            )
        ),
        
        # Add text with HTML tags
        mainPanel(
            tags$br(),
            tags$p(
                "This data was obtained from",
                tags$a("IMBD", href = "http://www.imbd.com/"), "and",
                tags$a("Rotten Tomatoes", href = "https://www.rottentomatoes.com/"), "."
            ),
            tags$p("The data represents", nrow(movies), "randomly sampled movies released between 1972 to 2014 in the United States."),
            
            plotOutput(outputId = "scatterplot"),
            dataTableOutput(outputId = "movestable"),
            
            # Download data button 
            # mainPanel(
            # HTML("Select filetype and variables, then hit 'Download data'."),
            # downloadButton("download_data", "Download data")
        )
    )
)

# Define server ----------------------------------------------------------------

server <- function(input, output, session) {

    # # Download file option 
    # output$download_data <- downloadHandler(
    #     filename = function() {
    #         paste0("movies.", input$filetype)
    #     },
    #     content = function(file) { 
    #         if(input$filetype == "csv"){ 
    #             write_csv(movies %>% select(input$selected_var), file) 
    #         }
    #         if(input$filetype == "tsv"){ 
    #             write_tsv(movies %>% select(input$selected_var), file) 
    #         }
    #     }
    # )
    
    #Create a subset of data filtering for selected title
    movies_subset <- reactive({
        req(input$selected_type)
        filter(movies, title_type %in% input$selected_type)
    })
    
    new_plot_title <- eventReactive(
        eventExpr = input$update_plot_title,
        valueExpr = {
            toTitleCase(input$plot_title)
        }
    )
    
    # Add render plot
    output$scatterplot <- renderPlot({
        ggplot(data = movies, aes_string(x = input$x, y = input$y, color = input$z)) +
            geom_point(alpha = input$alpha, size = input$size) +
            labs(title = new_plot_title())
    })
    
}
# Create a Shiny app object ----------------------------------------------------

shinyApp(ui = ui, server = server)