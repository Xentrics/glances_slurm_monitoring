# Packages ----
library(shiny)  # Required to run any Shiny app
library(ggplot2)  # For creating pretty plots
library(tidyverse)  # For filtering and manipulating data
library(patchwork)

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

# ui.R ----
ui <-
  fillPage(
    tags$style(type="text/css",
      ".recalculating {opacity: 1.0;}"
    ),
    titlePanel("MBD Servers"),
    plotOutput("myplot", height = "90%")
  )



# server.R ----
message("Looking for server files")
files <- list.files(path = "/sbidata/glances/", pattern = "glances", full.names = TRUE)
message(sprintf("Found %i, setting up file readers", length(files)))
fileReaders <- map(files, ~ reactiveFileReader(60000, NULL, .x, readFunc = read_csv)) %>%
   set_names(., files %>% basename() %>% str_replace("glances\\.(.*)\\.csv", "\\1"))
message("Servers are: %s", names(fileReaders))

server <- function(input, output) {

  df.list <- map(fileReaders, function(fr) {
    reactive({
      tryCatch({
        fr() %>%
          slice_tail(n = 100) %>%
          mutate(entry = 1:nrow(.), .before = 1) %>%
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

  # x-labels: currently, we access resources every 30 secs, and we use the last 100 of them, so (30*100)/60
  plots <- reactive({
    map2(df.list, names(df.list), function(df, name) {
      if (nrow(df()) > 0) {
        ggplot(df(), aes(x = entry)) +
          geom_path(aes(y = percentage, color = type), size = 2) +
          scale_color_manual(values = c("CPU" = "#F8766D", "MEM" = "#C77CFF", "load.min15" = "darkgreen", "load.min5" = "#00BA38")) +
          scale_x_continuous(expand = expansion(mult = c(0.015, 0)), n.breaks = 5, labels = sprintf("-min %i", 10*(4:0))) +
          scale_y_continuous(limits = c(0, 105), expand = expansion(mult = c(0, 0.01)), breaks = c(0, 50, 75, 100)) +
          ggtitle(name) +
          labs(y = "% usage", x = "") +
          theme_bw(base_size = 20) +
          theme(title = element_text(face="bold"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                axis.text.x = element_text(color = "black", angle = 45, vjust = 1, hjust = 1),
                axis.text.y = element_text(color = "black"))
      } else {
        ggplot() + ggtitle(name)
      }
    })
  })

  output$myplot <- renderPlot(wrap_plots(plots()) + plot_layout(guides = "collect") & theme(legend.position = 'top'))
}

# Run the app ----
shinyApp(ui = ui, server = server)
