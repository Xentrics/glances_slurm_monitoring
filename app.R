#== Packages ----
source("functions.R")

library(shiny)  # Required to run any Shiny app
library(ggplot2)  # For creating pretty plots
library(dplyr)  # For filtering and manipulating data
library(stringr)
library(purrr)
library(forcats)
library(ggtext)

#== Description====
# This script should be run within a shiny-verse docker container
# background_job.R should be run routinely (e.g., every minute) in the background


#== UI (client) ====
ui <-
  fillPage(

    # remove shading during update
    tags$style(type="text/css",
      ".recalculating {opacity: 1.0;}"
    ),

    # update title bar look
    tags$style(
      HTML(
        "#big-heading{
        padding-left:15px; padding-top:5px; padding-bottom:15px;
        color: white; background: #3475b4;
        box-shadow: 2px 2px 2px 2px grey;}"
      )
    ),

    # Title
    titlePanel(
      title = h1(id="big-heading", "MBD Servers", align = "center"),
      # Application Icon (for web-browser tab)
      tags$head(tags$link(rel = "icon", type = "image/png", href = "logo_128.png"),
                tags$title("MBD Servers"))
    ),

    textOutput("update_text"),

    # Server Performance Graphs
    plotOutput("usage_graphs", height = "85%")
  )



#== Server ====
#== Persistent data ----

get_current_time <- function() {
  format(Sys.time(), "%b %d %H:%M")
}

# Create reactivate file readers that loads pre-compute ggplot objects
graph_file <- file.path(getwd(), "data/graph.rds")
if (!file.exists(graph_file)) stop("Graph file not found! Did you run background_job.R?")

safe.readRDS <- possibly(readRDS, NULL)
gg.graph <- reactiveFileReader(20000, NULL, filePath = graph_file, readFunc = safe.readRDS)

# keeps the last valid plot
last_plot <- reactiveValues(plot = NULL)


#== Session data ----
server <- function(input, output) {

  # shows the last render update
  update_text <- reactiveValues(uptext = paste("Last Update:", get_current_time()))

  # loading of data may fail (resulting in a white flash) - so we need to capture this
  # here, we safe the last rendered plot
  # if gg.graph is broken, the last rendered plot will be used instead
  # observere is required to make the if-else clause work
  observe({
    if (!is.null(gg.graph())) {
      output$usage_graphs <- renderPlot({
        g <- gg.graph()
        update_text$uptext <- paste("Last Update:", get_current_time())
        last_plot$plot <- g
        g
      })
    } else {
      #message("Incomplete data.") # use only during debug
      output$usage_graphs <- renderPlot(last_plot$plot)
    }
  })

  # use g to trigger text update
  output$update_text <- renderText(update_text$uptext)
}


#== Run the app ----
shinyApp(ui = ui, server = server)
