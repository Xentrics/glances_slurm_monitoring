#== Packages ----
library(ggplot2)  # For creating pretty plots
library(dplyr)  # For filtering and manipulating data
library(stringr)
library(purrr)
library(forcats)

#== Description====
# This script should be run every 1 minute using cron
# It reads the current measurement data & creates the ggplots from it


#== Find & load glances measurement data ====
message("Looking for server files")
data_directory <- "/opt/ngs2/glances/"
usage_files <- list.files(path = data_directory, pattern = "glances", full.names = TRUE)

# ignore SLURM nodes; only the first two additional nodes get exposed
usage_files <- usage_files %>% discard(str_detect(usage_files, "nodeSBI"))
usage_files <- c(usage_files %>% discard(str_detect(usage_files, "nodesbi")),
                 usage_files %>% keep(str_detect(usage_files, "nodesbi")))

# Create reactivate file readers for continues updates
message(sprintf("Found %i, setting up file readers", length(usage_files)))
usage.list <- map(usage_files, read.csv)
names(usage.list) <- usage_files %>% basename() %>% str_replace("glances\\.(.*)\\.csv", "\\1")
message(sprintf("Servers are: %s\n", names(usage.list)))


# Reorder Position of Server Plots
server_order <- c('ikarus', 'daidalos', 'kottos', 'agaricus', 'amantia', 'russula', 'nodesbi01', 'nodesbi02')
if (any(!names(usage.list) %in% server_order)) {
  warning("One or more servers are not in 'server_order'. Not chaning order ...")
} else {
  # maybe not all servers are being watched
  server_order <- server_order[server_order %in% names(usage.list)]
  # reorder
  usage.list <- usage.list[server_order]
}


#== Grab & add CPU cores to server titles ====
names(usage.list) <- map2(usage.list, names(usage.list), ~ sprintf("%s, %i threads", .y, .x$cpu.cpucore[[1]]))


# get top-server users
# TODO


#== Filter and Reformat data ====
usage.list <- map(usage.list, function(df) {
  tryCatch({
    df %>%
      slice_tail(n = 120) %>%
      mutate(entry = 1:nrow(.), .before = 1) %>%
      # x-labels: currently, we access resources every 30 secs, and we use the last 100 of them, so (30*100)/60
      mutate(minute = (-30*nrow(.):1)/60, .before = 1) %>%
      #mutate(load.min5  = pmin(100*load.min5/load.cpucore, 105)) %>%
      mutate(load.min15 = pmin(100*load.min15/load.cpucore, 105)) %>%
      select(entry, minute, cpu.total, mem.percent, load.min15) %>%
      rename("CPU" = "cpu.total", "MEM" = "mem.percent") %>%
      tidyr::pivot_longer(-c(entry, minute), names_to = "type", values_to = "percentage") %>%
      mutate(type = fct_inorder(type) %>% fct_rev())
  }, error = function(e) {
    data.frame()
  })
})


#== Create plot ====

# Render plot
usage_graphs <- calc_graph(usage.list = usage.list, use_type = "facet")

# Save plot to drive (for later use with shiny server)
dir.create("data", showWarnings = F)
saveRDS(usage_graphs, "data/graph.rds")

