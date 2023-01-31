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

    # remove shading during update
    tags$style(type="text/css",
      ".recalculating {opacity: 1.0;}"
    ),

    # update title bar look
    tags$style(
      HTML(
        "#big-heading{
        padding-left:15px; padding-top:15px; padding-bottom:15px;
        color: white; background: #3475b4;
        box-shadow: 2px 2px 2px 2px grey;}"
      )
    ),

    # Title
    titlePanel(
      title = h1(id="big-heading", "MBD Servers"),
      # Application Icon (for web-browser tab)
      tags$head(tags$link(rel = "icon", type = "image/png", href = "logo_128.png"),
                tags$title("MBD Servers"))
    ),

    # Server Performance Graphs
    plotOutput("usage_graphs", height = "90%")
  )



# server.R ----
message("Looking for server files")
files <- list.files(path = "/sbidata/glances/", pattern = "glances", full.names = TRUE)
# ignore SLURM nodes; only the first two additional nodes get exposed
files <- files %>% discard(str_detect(files, "nodeSBI"))
files <- c(files %>% discard(str_detect(files, "nodesbi")),
           files %>% keep(str_detect(files, "nodesbi")))

# Create reactivate file readers for continues updates
message(sprintf("Found %i, setting up file readers", length(files)))
fileReaders <- map(files, ~ reactiveFileReader(60000, NULL, .x, readFunc = read.csv)) %>%
   set_names(., files %>% basename() %>% str_replace("glances\\.(.*)\\.csv", "\\1"))

message(sprintf("Servers are: %s\n", names(fileReaders)))


# Reorder Position of Server Plots
server_order <- c('ikarus', 'daidalos', 'kottos', 'agaricus', 'amantia', 'russula', 'nodesbi01', 'nodesbi02')
if (any(!names(fileReaders) %in% server_order)) {
  warning("One or more servers are not in 'server_order'. Not chaning order ...")
} else {
  # maybe not all servers are being watched
  server_order <- server_order[server_order %in% names(fileReaders)]
  # reorder
  fileReaders <- fileReaders[server_order]
}


# Server Compute
server <- function(input, output) {

  df.list <- map(fileReaders, function(fr) {
    reactive({
      tryCatch({
        fr() %>%
          slice_tail(n = 120) %>%
          mutate(entry = 1:nrow(.), .before = 1) %>%
          mutate(minute = (-30*nrow(.):1)/60, .before = 1) %>%
          mutate(load.min5  = pmin(100*load.min5/load.cpucore, 105)) %>%
          mutate(load.min15 = pmin(100*load.min15/load.cpucore, 105)) %>%
          select(entry, minute, cpu.total, mem.percent, load.min5, load.min15) %>%
          rename("CPU" = "cpu.total", "MEM" = "mem.percent") %>%
          pivot_longer(-c(entry, minute), names_to = "type", values_to = "percentage") %>%
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
        ggplot(df(), aes(x = minute)) +
          geom_path(aes(y = percentage, color = type), size = 2) +
          scale_color_manual(values = c("CPU" = "#F8766D", "MEM" = "#C77CFF", "load.min15" = "darkgreen", "load.min5" = "#00BA38")) +
          scale_x_continuous(expand = expansion(mult = c(0.015, 0.015)), n.breaks = 6) +
          scale_y_continuous(limits = c(0, 105), expand = expansion(mult = c(0, 0.01)), breaks = c(0, 50, 75, 100)) +
          ggtitle(name) +
          labs(y = "% usage", x = "ago [min]") +
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

  output$usage_graphs <- renderPlot(wrap_plots(plots()) + plot_layout(guides = "collect") & theme(legend.position = 'left'))
}

# Run the app ----
shinyApp(ui = ui, server = server)
