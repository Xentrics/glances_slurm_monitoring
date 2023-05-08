#== Packages ----
source("functions.R")
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

# Read CSV files & extract server names from files
message(sprintf("Found %i, setting up file readers", length(usage_files)))
usage.list_tmp <- map(usage_files, read.csv)

usage.list <- map(usage.list_tmp, update_column_names)
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


#== Add information about the strongest users ====
top.files <- list.files(path = data_directory, pattern = "top_users", full.names = TRUE)
names(top.files) <- top.files %>% basename() %>% str_replace("top_users\\.(.*)\\.tsv", "\\1")
top_data_list <- list()

# load only the data for the currently displayed servers
shared <- intersect(names(usage.list), names(top.files))
missing <- setdiff(names(usage.list), shared) %>% set_names(.,.)

if (length(shared) > 0) {
  # load data
  top.files <- top.files[shared]
  top_data_list <- map(top.files, parse_top_data, n_max = 3)
}

if (length(missing) > 0) {
  top_data_list <- c(top_data_list, map(missing, ~ "_unknown_"))
}

# sanity check
stopifnot(names(top_data_list) %in% names(usage.list))
top_data_list <- top_data_list[names(usage.list)]



#== Parse SLURM usage ====
# file created using 'sinfo -N'
susage.df <- read.csv.whitespace(path = file.path(data_directory, "susage.tsv")) %>%
  select(HOSTNAMES, STATE, `CPUS(A/I/O/T)`) %>%
  # select only nodes of interest for the current frame
  filter(HOSTNAMES %in% str_to_lower(names(usage.list))) %>%
  arrange(HOSTNAMES)

state.map <- susage.df %>%
  mutate(STATE = annotate_susage(STATE = STATE, CPUS = `CPUS(A/I/O/T)`)) %>%
  select(HOSTNAMES, STATE) %>% tibble::deframe()

# add untracked ones
missing <- names(usage.list)[!(names(usage.list) %in% names(state.map))]
if (length(missing) > 0) {
  l <- map(missing, function(s) c("<span style='color:gray'>untracked</span>") %>% set_names(s)) %>% unlist()
  state.map <- c(state.map, l)[names(usage.list)]
}


#== Parse SLURM job queue ====
# file created using 'squeue'
queue.df <- read.csv.whitespace(file.path(data_directory, "squeue.tsv")) %>%
  filter(ST == "R") %>%
  filter(NODELIST %in% susage.df$HOSTNAMES) %>%
  left_join(susage.df, by = c("NODELIST" = "HOSTNAMES"))

squeue.map <- tryCatch({
  queue.df %>%
    mutate(USAGE = annotate_squeue(STATE, USER, CPUS)) %>%
    select(NODELIST, USER, USAGE) %>%
    group_by(NODELIST) %>%
    summarise(USAGE = paste(USAGE, collapse = ", ")) %>%
    tibble::deframe()
}, error = function(e) {
  NULL
})

# expand state.map with existing entries
if (!is.null(squeue.map)) {
  state.map[names(squeue.map)] <- sprintf("%s<br>%s", state.map[names(squeue.map)], squeue.map)
}



#== Grab & add CPU cores to server titles ====
# first: check that the order of all files is the same!
stopifnot(names(top_data_list) == names(usage.list))
stopifnot(str_to_lower(names(state.map)) == str_to_lower(names(usage.list)))

# apply consistent (nicer) names
names(usage.list)    <- annotate_server_header(df.list = usage.list)
names(top_data_list) <- names(usage.list)
names(state.map)     <- names(usage.list)


#== Filter and Reformat data ====

# use one server to assess file system usage
fs_usage <- NULL
if (any(str_detect(colnames(usage.list[[1]]), "fs.."))) {

  # summarise /sbidata partition information
  sbidata <- tryCatch({
    first <- usage.list[[1]] %>% slice_head(n=1) %>% select(starts_with("fs..sbidata"), -contains("projects")) %>% select(where(is.numeric))
    last  <- usage.list[[1]] %>% slice_tail(n=1) %>% select(starts_with("fs..sbidata"), -contains("projects")) %>% select(where(is.numeric))
    diff <- first - last
    sbidata <- sprintf("<span style='color:blue'>sbidata</span>: [<span style='color:blue'>%i</span> / <span style='color:blue'>%i Tb</span>], %s <span style='color:blue'>%.2f Gb</span>",
                       round(last$fs..sbidata.used / 1000^4), round(last$fs..sbidata.size / 1000^4),
                       "&Delta;",
                       diff$fs..sbidata.used / 1000^3)
    #ggplot() + ggtitle(sbidata) + theme(plot.title = ggtext::element_markdown())
  }, error = function(e) {
    ""
  })

  # parse remaining real-time file system usage
  fs_usage <- tryCatch({
    usage.list[[1]] %>%
      slice_tail(n = 120) %>%
      mutate(entry = 1:nrow(.), .before = 1) %>%
      # x-labels: currently, we access resources every 30 secs, and we use the last 100 of them, so (30*100)/60
      mutate(minute = (-30*nrow(.):1)/60, .before = 1) %>%
      select(entry, minute, matches("^fs\\.\\..*\\.percent")) %>%
      rename_with(~ .x %>% str_remove("fs..") %>% str_remove(".percent"), .cols = starts_with("fs")) %>%
      rename_with(~ "sbiprojects", any_of("sbidata.projects")) %>%
      tidyr::pivot_longer(-c(entry, minute), names_to = "type", values_to = "percentage") %>%
      mutate(type = factor(type, levels = c("sbidata", "sbiprojects", "sbiarchiv", "sbisra")) %>% fct_rev())
  }, except = function(e) {
    NULL
  })
}


# reformat CPU, Memory, and Load information per server
usage.list <- map(usage.list, function(df) {
  fs_scratch_names <- c("fs..sbilocal.percent", "fs..scratch.percent", "fs..scratch.percent")
  cols_of_interest <- c("cpu.total", "mem.percent", fs_scratch_names)
  tryCatch({
    # (cpucore should stay the same across all values and not be crazy high!)
    cpucore_ref <- median(df$cpu.cpucore[df$cpu.cpucore > 0], na.rm = T)
    df %>%
      slice_tail(n = 120) %>%
      mutate(entry = 1:nrow(.), .before = 1) %>%
      # x-labels: currently, we access resources every 30 secs, and we use the last 100 of them, so (30*100)/60
      mutate(minute = (-30*nrow(.):1)/60, .before = 1) %>%
      # compensate parsing/writing errors
      mutate(across(c(entry, minute, any_of(cols_of_interest), cpu.cpucore), as.numeric)) %>%
      mutate(across(any_of(cols_of_interest), ~ if_else(is.na(cpu.cpucore) | cpu.cpucore != cpucore_ref, NA_real_, .x))) %>%
      select(entry, minute, any_of(cols_of_interest)) %>%
      # make nicer variable names
      rename("CPU" = "cpu.total", "MEM" = "mem.percent") %>%
      rename_with(~ "scratch", .cols = any_of(fs_scratch_names)) %>%
      # convert to long format
      tidyr::pivot_longer(-c(entry, minute), names_to = "type", values_to = "percentage") %>%
      mutate(type = fct_inorder(type) %>% fct_rev())
  }, error = function(e) {
    data.frame()
  })
})


# merge HPC and FS information
if (!is.null(fs_usage)) {
  usage.list <- c(usage.list, list("filesystem" = fs_usage))
  fs_overuse <- fs_usage %>% filter(percentage >= 95) %>% pull(type) %>% unique() %>% map_chr(~ sprintf("<span style='color:red'>!!%s!!</span>", .x)) %>% paste(collapse = "; ")
  top_data_list <- c(top_data_list, list("filesystem" = paste0(fs_overuse, sbidata)))
  state.map <- c(state.map, list("filesystem" = ""))
}


#== Create plot ====

# Render plot
usage_graphs <- calc_graph_top(usage.list = usage.list, top_data_list = top_data_list, state.map = state.map, use_type = "facet")

# Save plot to drive (for later use with shiny server)
dir.create("data", showWarnings = F)
saveRDS(usage_graphs, "data/graph.rds")
ggsave(filename = "data/graph.pdf", plot = usage_graphs, width = 1920, height = 900, units = "px", dpi = 96, scale = 1.25)

