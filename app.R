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
        "
        #big-heading {
          padding: 0;
          height: 45px;
          line-height: 45px;
          text-align: center;
          color: white;
          background: #3475b4;
          box-shadow: 2px 2px 2px 2px grey;
          margin: 0px;
          margin-top: -20px;
        }

        @keyframes flash {
          0% { opacity: 1; }
          50% { opacity: 0.4; }
          100% { opacity: 1; }
        }

        #warning_bar {
          margin-top: -20px;
          margin-bottom: 10px;
        }

        #flashing-warning {
          animation: flash 5s infinite;
        }
        "
      )
    ),

    # Title
    titlePanel(
      title = h1(id="big-heading", "MBD Servers"),
      # Application Icon (for web-browser tab)
      tags$head(tags$link(rel = "icon", type = "image/png", href = "logo_128.png"),
                tags$title("MBD Servers"))
    ),

    # Warning bar
    uiOutput("warning_bar"),

    # Time Update Text
    textOutput("update_text"),

    # Server Performance Graphs
    plotOutput("usage_graphs", height = "85%")
  )



#== Server ====
#== Persistent data ----

get_current_time <- function() {
  format(lubridate::with_tz(Sys.time(), "CET"), "%b %d %H:%M %Z")
}

# Create reactivate file readers that loads pre-compute ggplot objects
graph_file <- file.path(getwd(), "data/graph.rds")
if (!file.exists(graph_file)) stop("Graph file not found! Did you run background_job.R?")

safe.readRDS <- possibly(readRDS, NULL)
gg.graph <- reactiveFileReader(20000, NULL, filePath = graph_file, readFunc = safe.readRDS)

warning_file <- file.path(getwd(), "data/warnings.rds")
warning_message <- reactiveFileReader(20000, NULL, filePath = warning_file, readFunc = safe.readRDS)

# keeps the last valid plot
last_plot <- reactiveValues(plot = NULL)


#== Session data ----
server <- function(input, output) {

  # show time of the last image update
  update_text <- reactiveValues(uptext = paste("Last Update:", get_current_time()))

  # render UI for warning bar
  format_str <- "height: 32px; line-height: 32px; text-align: center; box-shadow: 2px 2px 2px 2px grey;"
  output$warning_bar <- renderUI({
    if(warning_message() == "No warnings.") {
      tags$div(id = "warning_bar", style = paste(format_str, "background-color: lightgray;"), h3("No warnings."))
    } else {
      tags$div(id = "warning_bar", style = paste(format_str, "background-color: red; color: white;"), h3(id = "flashing-warning", warning_message()))
    }
  })

  # loading of data may fail (resulting in a white flash) - so we need to capture this
  # here, we safe the last rendered plot
  # if gg.graph is broken, the last rendered plot will be used instead
  # observe is required to make the if-else clause work
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
