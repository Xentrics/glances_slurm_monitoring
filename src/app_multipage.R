# Packages ----
library(shiny)  # Required to run any Shiny app
library(ggplot2)  # For creating pretty plots
library(tidyverse)  # For filtering and manipulating data

# You are using shiny modules. The tabPanel is defined in the top-level UI, but
# your are trying to use a lower-level (module) server to update the top-level UI.
# This will not work. So you need to use top-level server to update top-level UI.
# In other words, you need to pass the parent session object to your module.

pageButtonUi <- function(id) {
  actionButton(NS(id, "page_change"),
               label="Change the Page")
}


pageButtonServer <- function(id, parentSession) {
  moduleServer(id, function(input, output, session) {

    observeEvent(input$page_change, {
      updateNavbarPage(session=parentSession,
                       inputId="pages",
                       selected="second_page")
    })
  })
}


ui <- navbarPage(
  title="test",
  id="pages",
  tabPanel(title="first page",
           sidebarLayout(
             sidebarPanel(
               pageButtonUi("page")
             ),
             mainPanel(
             )
           )
  ),
  tabPanel(title="second_page", "second_page")
)


server <- function(input, output, session) {
  pageButtonServer("page", parentSession = session)
}


shinyApp(ui, server)
