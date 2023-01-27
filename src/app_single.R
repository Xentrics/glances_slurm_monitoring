# Packages ----
library(shiny)  # Required to run any Shiny app
library(ggplot2)  # For creating pretty plots
library(tidyverse)  # For filtering and manipulating data


# ui.R ----
ui <-
  fillPage(
    titlePanel("MDB Servers"),
    plotOutput("myplot"),
    textOutput("mytext")
  )



# server.R ----
files <- list.files(path = "/sbidata/glances/", pattern = "glances", full.names = TRUE)
fileReader <- reactiveFileReader(20000, NULL, files[[1]], readFunc = read_csv)
server <- function(input, output, session) {
  # debug:  isolate(output$data())
  fileData <- reactive(fileReader())

  writeout <- reactive({
    if (nrow(fileData()) > 150) {
      message("Trimming glances output file ...")
      write_csv(x = fileData() %>% slice_tail(n=100), file =  files[[1]])
      "trimming"
    } else {
      "nothing"
    }
  })
  output$mytext <- renderText(writeout())

  df <- reactive({
    fileData() %>%
      slice_tail(n = 100) %>%
      mutate(entry = 1:nrow(.), .before = 1) %>%
      select(entry, cpu.total, mem.percent) %>%
      rename("CPU" = "cpu.total", "MEM" = "mem.percent") %>%
      pivot_longer(-entry, names_to = "type", values_to = "percentage")
  })



  # load <- reactive({
  #   fileData() %>%
  #     slice_tail(n=1) %>%
  #     select(starts_with("load.min"))
  # })

  #load_label <- sprintf(" Load min   1: %.1f\n Load min   5: %.1f\n Load min 15: %.1f", load$load.min1, load$load.min5, load$load.min15)
  #load_label <- sprintf(" Load min   1: %.1f\n Load min   5: %.1f\n Load min 15: %.1f", output$load$load.min1, output$load$load.min5, output$load$load.min15)
  output$myplot <- renderPlot(
    ggplot(df(), aes(x = entry)) +
      geom_path(aes(y = percentage, color = type), size = 2) +
      #annotate("text", x = 1, y = 50, label = load_label, vjust = 0.5, hjust = 0) +
      scale_x_continuous(expand = expansion(0)) +
      scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.01))) +
      ggtitle("Agaricus") +
      theme_bw(base_size = 12) +
      theme(panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.y = element_text(color = "black"))
  )
}

# Run the app ----
shinyApp(ui = ui, server = server)
