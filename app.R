# Packages ----
library(shiny)  # Required to run any Shiny app
library(ggplot2)  # For creating pretty plots
library(tidyverse)  # For filtering and manipulating data

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

# ui.R ----
ui <-
  fillPage(
    titlePanel("MDB Servers"),
    # textOutput("mytext"),
    plotOutput("myplot", height = "90%")
  )



# server.R ----
files <- list.files(path = "/sbidata/glances/", pattern = "glances", full.names = TRUE)
fileReaders <- map(files, ~ reactiveFileReader(60000, NULL, .x, readFunc = read_csv)) %>%
   set_names(., files %>% basename() %>% str_replace("glances\\.(.*)\\.csv", "\\1"))
server <- function(input, output, session) {

  df.list <- map(fileReaders, function(fr) {
    reactive({
      tryCatch({
        fr() %>%
          slice_tail(n = 100) %>%
          mutate(entry = nrow(.):1, .before = 1) %>%
          mutate(load.min5  = pmin(100*load.min5/load.cpucore, 105)) %>%
          mutate(load.min15 = pmin(100*load.min15/load.cpucore, 105)) %>%
          select(entry, cpu.total, mem.percent, load.min5, load.min15) %>%
          rename("CPU" = "cpu.total", "MEM" = "mem.percent") %>%
          pivot_longer(-entry, names_to = "type", values_to = "percentage") %>%
          mutate(type = fct_inorder(type) %>% fct_rev())
      }, error = function(e) {
        data.frame()
      })
    })
  })

  plots <- reactive({
    map2(df.list, names(df.list), function(df, name) {
      if (nrow(df()) > 0) {
        ggplot(df(), aes(x = entry)) +
          geom_path(aes(y = percentage, color = type), size = 2) +
          scale_color_manual(values = c("CPU" = "#F8766D", "MEM" = "#C77CFF", "load.min15" = "darkgreen", "load.min5" = "#00BA38")) +
          scale_x_continuous(expand = expansion(0)) +
          scale_y_continuous(limits = c(0, 105), expand = expansion(mult = c(0, 0.01)), breaks = c(0, 50, 75, 100)) +
          ggtitle(name) +
          theme_bw(base_size = 12) +
          theme(panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank(),
                axis.text.y = element_text(color = "black"))
      } else {
        ggplot() + ggtitle(name)
      }
    })
  })

  output$myplot <- renderPlot(patchwork::wrap_plots(plots(), guides = "collect"))
}

# Run the app ----
shinyApp(ui = ui, server = server, options = list("port" = 7777))
