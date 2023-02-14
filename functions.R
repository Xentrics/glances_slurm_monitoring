
.libPaths(c("src/R-site-library", .libPaths()))

#' Simulate GGplot color palette
#' @param n Number of colors to generate
#' @return Character(n). Colors
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}


#' Depending on the export settings of glances, the names of columns may differ
#' This function ensures that the data is parsed the same, regardless of the
#' export setting
#' @param df Data.frame
update_column_names <- function(df) {
  df %>%
    rename_with(~ str_replace_all(.x, "_", "."))
}


simplify_mem_number <- function(x, base = 1024, exp = 3, shift = 2) {
  # round number & replace last two digits with zeros
  round(x/(base^exp) / 10^shift) * 10^shift
}


#' Enhance the names of a list containing server usage data
#' @param df.list Names list of data.frames. Expected: usage.list
#' @param beautify If TRUE, html strings are used to add styling (colors, etc.).
#' @return Names character, were names(val) are names(df.list), but entries are
#' the new names
annotate_server_header <- function(df.list, beautify = TRUE) {

  if (!beautify) {
    map2(df.list, names(df.list), ~ sprintf("%s  [%i Core] [%i GiB Mem]", .y, .x$cpu.cpucore[[1]], round(.x$mem.total[[1]]/(1024^3))))
  } else {
    map2(df.list, names(df.list), ~ sprintf("%s  [ <span style='color:#F8766D; font-weight: normal'>%i Core</span> , <span style='color:#C77CFF; font-weight: normal'>%i GiB Mem</span> ]",
                                            .y,
                                            .x$cpu.cpucore[[1]],
                                            simplify_mem_number(.x$mem.total[[1]])
                                            )
         )
  }
}


safe.read.csv <- purrr::possibly(read.csv, otherwise = NULL)


#' Parse top user file and return a string describing top users
#' @param path Path to top user file
#' @param sep Column separator in 'path'
#' @param use_color_css If TRUE, then CSS syntax for coloring will be used in output. Otherwise, plain text will be
#'   returned.
#' @return Character(1).
parse_top_data <- function(path, sep = "\t", n_max = 5, use_color_css = TRUE) {
  # note: Pct is Load/#CPU
  df <- safe.read.csv(file = path, sep = sep, header = F, col.names = c("User", "Load", "Pct"))

  if (is.null(df)) return("_unknown_")

  # order usage & remove users with less than 1 full core
  df.top <- df %>%
    arrange(desc(Load)) %>%
    filter(Load > 80) %>%
    mutate(Cores = round(Load/100)) %>%
    slice_head(n = n_max) %>%
    mutate(Label = if(use_color_css) {
      glue::glue("<span style='color:#F8766D'>{User}</span> [<span style='color:#00BFC4'>{Cores}</span>]")
    } else {
      glue::glue("{User} [{Cores}]")
    })

  # maybe no heavy user is active
  if (nrow(df.top) == 0) {
    if (use_color_css) {
      return("<span style='color:gray; font-weight: normal'>inactive</span>")
    } else {
      return("_inactive_")
    }
  }

  # build usage string
  paste(df.top$Label, collapse = "; ")
}




#' Function to compute the actual usage graphs
#' @param usage.list Named list of data.frame with the measurement data (long-formatted)
#' @param top_data_list Named list of strings. Names must match names(usage.list)!
#' @use_type Rendering type. Either
#' patchwork: render each plot individually & concatenate using patchwork. Quite slow, but robust against missing data.
#' facet: render all plots as one ggplot using facet over server. Faster, but failed servers may disappear
calc_graph_top <- function(usage.list, top_data_list, use_type) {

  if (!all(names(usage.list) == names(top_data_list))) stop("One or more names in top_data_list those not match names in usage.list!")

  if (use_type == "patchwork") {
    require(patchwork)
    plots <- pmap(list(usage.list, names(usage.list), top_data_list), function(df, name, users) {

      # only plot if any data was loaded
      if (nrow(df) > 0) {
        ggplot(df, aes(x = minute)) +
          geom_path(aes(y = percentage, color = type, linetype = type), size = 1.5) +
          scale_color_manual(values = c("CPU" = "#F8766D", "MEM" = "#C77CFF", "load.min15" = "#00BA38", "scratch" = "#1AA7EC",
                                        "sbiarchiv" = "#1AA7EC", "sbisra" = "#CAF0F8", "sbidata" = "#00B4D8", "sbiprojects" = "#0077B6")) +
          # scale_linetype_manual(values = c("CPU" = 1, "MEM" = 1, "load.min15" = 1,
          #                                  "sbiarchiv" = 2, "sbisra" = 2, "sbidata" = 2, "sbiprojects" = 2)) +
          scale_x_continuous(expand = expansion(mult = c(0.015, 0.015)), n.breaks = 4) +
          scale_y_continuous(limits = c(0, 105), expand = expansion(mult = c(0, 0.01)), breaks = c(0, 50, 75, 100)) +
          ggtitle(name, users) +
          labs(y = "% usage", x = "ago [min]") +
          theme_bw(base_size = 14) +
          theme(plot.title = ggtext::element_markdown(face="bold"),
                plot.subtitle	= ggtext::element_markdown(size = 10, face="bold"),
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
    df <- bind_rows(usage.list, .id = "Server") %>%
      left_join(top_data_list %>% map_df(~ tibble(Users = .x), .id = "Server"))

    # only plot if any data was loaded
    if (nrow(df) > 0) {
      df %>%
        mutate(Server = fct_inorder(Server)) %>%
        ggplot(aes(x = minute)) +
        facet_wrap(~ Server + Users, scales = "free_x") +
        geom_path(aes(y = percentage, color = type, size = type, linetype = type)) +
        scale_color_manual(values = c("CPU" = "#F8766D", "MEM" = "#C77CFF", "load.min15" = "#00BA38", "scratch" = "#1AA7EC",
                                      "sbiarchiv" = "#1AA7EC", "sbisra" = "#CAF0F8", "sbidata" = "red", "sbiprojects" = "darkorange")) +
        scale_size_manual(values = c("CPU" = 1.25, "MEM" = 1.25, "load.min15" = 1.25, "scratch" = 0.75,
                                   "sbiarchiv" = 0.75, "sbisra" = 0.75, "sbidata" = 0.75, "sbiprojects" = 0.75)) +
        scale_linetype_manual(values = c("CPU" = 1, "MEM" = 1, "load.min15" = 1, "scratch" = 4,
                                        "sbiarchiv" = 4, "sbisra" = 4, "sbidata" = 4, "sbiprojects" = 4)) +
        scale_x_continuous(expand = expansion(mult = c(0.015, 0.015)), n.breaks = 4) +
        scale_y_continuous(limits = c(0.01, 105), expand = expansion(mult = c(0, 0.01)), breaks = c(0, 50, 75, 100)) +
        labs(y = "% usage", x = "ago [min]") +
        theme_bw(base_size = 20) +
        theme(plot.title = element_text(face="bold"),
              strip.text = ggtext::element_markdown(face="bold"),
              strip.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_blank(),
              axis.text.x = element_text(color = "black", angle = 45, vjust = 1, hjust = 1),
              axis.text.y = element_text(color = "black"))
    } else {
      ggplot()
    }

  } else {
    stop("Unknown use_type: ", shQuote(use_type))
  }

}



#' Function to compute the actual usage graphs
#' @param usage.list Named list of data.frame with the measurement data (long-formatted)
#' @use_type Rendering type. Either
#' patchwork: render each plot individually & concatenate using patchwork. Quite slow, but robust against missing data.
#' facet: render all plots as one ggplot using facet over server. Faster, but failed servers may disappear
calc_graph <- function(usage.list, use_type) {

  if (use_type == "patchwork") {
    require(patchwork)
    plots <- map2(usage.list, names(usage.list), function(df, name) {

      # only plot if any data was loaded
      if (nrow(df) > 0) {
        ggplot(df, aes(x = minute)) +
          geom_path(aes(y = percentage, color = type), size = 1.5) +
          #scale_color_manual(values = c("CPU" = "#F8766D", "MEM" = "#C77CFF", "load.min15" = "darkgreen", "load.min5" = "#00BA38")) +
          scale_color_manual(values = c("CPU" = "#F8766D", "MEM" = "#C77CFF", "load.min15" = "#00BA38")) +
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
    df <- bind_rows(usage.list, .id = "Server")

    # only plot if any data was loaded
    if (nrow(df) > 0) {
      df %>%
        mutate(Server = fct_inorder(Server)) %>%
        ggplot(aes(x = minute)) +
        facet_wrap(~ Server, scales = "free_x") +
        geom_path(aes(y = percentage, color = type), size = 1.5) +
        #scale_color_manual(values = c("CPU" = "#F8766D", "MEM" = "#C77CFF", "load.min15" = "darkgreen", "load.min5" = "#00BA38")) +
        scale_color_manual(values = c("CPU" = "#F8766D", "MEM" = "#C77CFF", "load.min15" = "#00BA38")) +
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
      ggplot()
    }

  } else {
    stop("Unknown use_type: ", shQuote(use_type))
  }

}



#== Benchmark approaches ====
benchmark_loading <- function() {
  require("rbenchmark")

  # the patchwork approach is, as expected, quite slow (~ 23 seconds)
  time.patch <- rbenchmark::benchmark(print(calc_graph(usage.list = usage.list, use_type = "patchwork")),
                                      replications = 5,
                                      columns = c("test", "replications", "elapsed", "relative", "user.self", "sys.self"))

  # The facet approach is around 4x faster (8-9 seconds)
  time.facet <- rbenchmark::benchmark(print(calc_graph(usage.list = usage.list, use_type = "facet")),
                                      replications = 5,
                                      columns = c("test", "replications", "elapsed", "relative", "user.self", "sys.self"))


  # 22.7 seconds
  time.patch <- rbenchmark::benchmark(print(readRDS("data/graph.rds")),
                                      replications = 5,
                                      columns = c("test", "replications", "elapsed", "relative", "user.self", "sys.self"))

  # The facet approach is around 4x faster (8-9 seconds)
  # 10 seconds
  time.facet <- rbenchmark::benchmark(print(readRDS("data/graph.rds")),
                                      replications = 5,
                                      columns = c("test", "replications", "elapsed", "relative", "user.self", "sys.self"))
}

