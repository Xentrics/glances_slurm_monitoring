#== Packages ----
library(ggplot2)  # For creating pretty plots
library(tidyverse)  # For filtering and manipulating data
library(patchwork)

#== Description====
# This script should be run every 1 minute using cron
# It reads the current measurement data & creates the ggplots from it

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}



#== Find & load glances measurement data ====
message("Looking for server files")
files <- list.files(path = "/sbidata/glances/", pattern = "glances", full.names = TRUE)
# ignore SLURM nodes; only the first two additional nodes get exposed
files <- files %>% discard(str_detect(files, "nodeSBI"))
files <- c(files %>% discard(str_detect(files, "nodesbi")),
           files %>% keep(str_detect(files, "nodesbi")))

# Create reactivate file readers for continues updates
message(sprintf("Found %i, setting up file readers", length(files)))
df.list <- map(files, read.csv)
names(df.list) <- files %>% basename() %>% str_replace("glances\\.(.*)\\.csv", "\\1")
message(sprintf("Servers are: %s\n", names(df.list)))


# Reorder Position of Server Plots
server_order <- c('ikarus', 'daidalos', 'kottos', 'agaricus', 'amantia', 'russula', 'nodesbi01', 'nodesbi02')
if (any(!names(df.list) %in% server_order)) {
  warning("One or more servers are not in 'server_order'. Not chaning order ...")
} else {
  # maybe not all servers are being watched
  server_order <- server_order[server_order %in% names(df.list)]
  # reorder
  df.list <- df.list[server_order]
}


# Grab & add CPU cores
names(df.list) <- map2(df.list, names(df.list), ~ sprintf("%s, %i threads", .y, .x$cpu.cpucore[[1]]))


# get top-server users
# TODO


#== Filter and Reformat data ====
df.list <- map(df.list, function(df) {
  tryCatch({
    df %>%
      slice_tail(n = 120) %>%
      mutate(entry = 1:nrow(.), .before = 1) %>%
      # x-labels: currently, we access resources every 30 secs, and we use the last 100 of them, so (30*100)/60
      mutate(minute = (-30*nrow(.):1)/60, .before = 1) %>%
      #mutate(load.min5  = pmin(100*load.min5/load.cpucore, 105)) %>%
      mutate(load.min15 = pmin(100*load.min15/load.cpucore, 105)) %>%
      select(entry, minute, cpu.total, mem.percent, load.min5, load.min15) %>%
      rename("CPU" = "cpu.total", "MEM" = "mem.percent") %>%
      pivot_longer(-c(entry, minute), names_to = "type", values_to = "percentage") %>%
      mutate(type = fct_inorder(type) %>% fct_rev())
  }, error = function(e) {
    data.frame()
  })
})


#== Create plot ====

#' Function to compute the actual usage graphs
#' @param df.list Named list of data.frame with the measurement data (long-formatted)
#' @use_type Rendering type. Either
#' patchwork: render each plot individually & concatenate using patchwork. Quite slow, but robust against missing data.
#' facet: render all plots as one ggplot using facet over server. Faster, but failed servers may disappear
calc_graph <- function(df.list, use_type) {
  if (use_type == "patchwork") {
    
    plots <- map2(df.list, names(df.list), function(df, name) {
      if (nrow(df) > 0) {
        ggplot(df, aes(x = minute)) +
          geom_path(aes(y = percentage, color = type), size = 1.5) +
          scale_color_manual(values = c("CPU" = "#F8766D", "MEM" = "#C77CFF", "load.min15" = "darkgreen", "load.min5" = "#00BA38")) +
          scale_x_continuous(expand = expansion(mult = c(0.015, 0.015)), n.breaks = 4) +
          scale_y_continuous(limits = c(0, 105), expand = expansion(mult = c(0, 0.01)), breaks = c(0, 50, 75, 100)) +
          ggtitle(name) +
          labs(y = "% usage", x = "ago [min]") +
          theme_bw(base_size = 16) +
          theme(title = element_text(face="bold"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                axis.text.x = element_text(color = "black", angle = 45, vjust = 1, hjust = 1),
                axis.text.y = element_text(color = "black"))
      } else {
        ggplot() + ggtitle(name)
      }
    })
    
    wrap_plots(plots) + plot_layout(guides = "collect") & theme(legend.position = 'left')
  } else if (use_type == "facet") {
    
    # merge all measurements
    df <- bind_rows(df.list, .id = "Server")
    if (nrow(df) > 0) {
      df %>%
        mutate(Server = fct_inorder(Server)) %>%
        ggplot(aes(x = minute)) +
        facet_wrap(~ Server, scales = "free_x") +
        geom_path(aes(y = percentage, color = type), size = 1.5) +
        scale_color_manual(values = c("CPU" = "#F8766D", "MEM" = "#C77CFF", "load.min15" = "darkgreen", "load.min5" = "#00BA38")) +
        scale_x_continuous(expand = expansion(mult = c(0.015, 0.015)), n.breaks = 4) +
        scale_y_continuous(limits = c(0, 105), expand = expansion(mult = c(0, 0.01)), breaks = c(0, 50, 75, 100)) +
        labs(y = "% usage", x = "ago [min]") +
        theme_bw(base_size = 16) +
        theme(title = element_text(face="bold"),
              strip.text = element_text(face = "bold"),
              strip.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_blank(),
              axis.text.x = element_text(color = "black", angle = 45, vjust = 1, hjust = 1),
              axis.text.y = element_text(color = "black"))
    } else {
      ggplot() + ggtitle(name)
    }
  }
}


# Render plot
usage_graphs <- calc_graph(df.list = df.list, use_type = "facet")

# Save plot to drive (for later use with shiny server)
dir.create("data", showWarnings = F)
saveRDS(usage_graphs, "data/graph.rds")


#== Benchmark approaches ====
# library("rbenchmark")
# # the patchwork approach is, as expected, quite slow (~ 23 seconds)
# time.patch <- rbenchmark::benchmark(print(calc_graph(df.list = df.list, use_type = "patchwork")),
#                                     replications = 5,
#                                     columns = c("test", "replications", "elapsed", "relative", "user.self", "sys.self"))
# 
# # The facet approach is around 4x faster (8-9 seconds)
# time.facet <- rbenchmark::benchmark(print(calc_graph(df.list = df.list, use_type = "facet")),
#                                     replications = 5,
#                                     columns = c("test", "replications", "elapsed", "relative", "user.self", "sys.self"))
# 
# 
# # 22.7 seconds
# time.patch <- rbenchmark::benchmark(print(readRDS("data/graph.rds")),
#                                     replications = 5,
#                                     columns = c("test", "replications", "elapsed", "relative", "user.self", "sys.self"))
# 
# # The facet approach is around 4x faster (8-9 seconds)
# # 10 seconds
# time.facet <- rbenchmark::benchmark(print(readRDS("data/graph.rds")),
#                                     replications = 5,
#                                     columns = c("test", "replications", "elapsed", "relative", "user.self", "sys.self"))

