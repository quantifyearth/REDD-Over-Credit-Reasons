# load necessary libraries
library(tidyverse)
library(arrow)

# ---- DEFORESTATION RATES ----
deforestation_rate = function(parquet_folder,
                              end_years_path = '',
                              pact = FALSE,
                              project = FALSE) {
  parquet_file_paths = list.files(parquet_folder, full.names = TRUE)
  df_list = lapply(parquet_file_paths, function(file_path) {
    tryCatch({
      arrow::read_parquet(file_path) %>% as_tibble()
    }, error = function(e) {
      message("Error reading: ", basename(file_path), " - ", e)
      NULL
    })
  })
  # name list elements by the file basename
  names(df_list) = basename(parquet_file_paths)
  # remove any files that could not be read
  df_list = df_list[!sapply(df_list, is.null)]
  
  # column selection based on pact and project booleans
  df_list = lapply(df_list, function(df_tbl) {
    # for pact files the following
    if (pact) {
      if (project) {
        # for project files, select and rename columns starting with "k_"
        df_tbl = df_tbl %>%
          select(starts_with("k_")) %>%
          rename_with(~ str_remove(., "k_"))
      } else {
        # otherwise, use columns starting with "s_"
        df_tbl = df_tbl %>%
          select(starts_with("s_")) %>%
          rename_with(~ str_remove(., "s_"))
      }
      # after the above, select the luc columns and remove the first 9 (if they exist)
      df_tbl = df_tbl %>% 
        select(starts_with("luc"))
      if(ncol(df_tbl) > 9){
        df_tbl = df_tbl %>% select(-c(1:9))
      }
    } else {
      # if not a pact file (i.e. an acc certified), just select luc columns
      df_tbl = df_tbl %>% select(starts_with("luc"))
    }
    return(df_tbl)
  })
  
  # pivot each dataframe to long format and compute yearly land cover percentages
  df_list = lapply(df_list, function(df_tbl) {
    df_long_tbl = df_tbl %>% 
      pivot_longer(
        cols = everything(),
        names_to = "year",
        values_to = "luc",
        names_pattern = "luc_(\\d+)"
      ) %>% 
      mutate(year = as.integer(year))
    
    yearly_counts_tbl = df_long_tbl %>%
      group_by(year) %>%
      summarise(
        Undisturbed = sum(luc == 1, na.rm = TRUE) / n(),
        Degraded    = sum(luc == 2, na.rm = TRUE) / n(),
        Deforested  = sum(luc == 3, na.rm = TRUE) / n(),
        Reforested  = sum(luc == 4, na.rm = TRUE) / n(),
        Water       = sum(luc == 5, na.rm = TRUE) / n(),
        Other       = sum(luc == 6, na.rm = TRUE) / n(),
        .groups     = "drop"
      ) %>%
      arrange(year)
    
    return(yearly_counts_tbl)
  })
  
  
  # summarise the deforestation rate by project use end years if pact
  if (pact) {
    # read the end years CSV
    end_years_df = read.csv(end_years_path, stringsAsFactors = FALSE)
    
    # use mapply to process each element along with its name
    summarized_list = mapply(function(df_tbl, file_name) {
      # extract project number from the file name
      proj_no = str_extract(file_name, "\\d+")
      if (!proj_no %in% as.character(end_years_df$project_no)) {
        return(NULL)
      } else {
        # look up the project's end year
        project_end_year = end_years_df %>%
          filter(project_no == proj_no) %>%
          pull(end_year) %>%
          as.numeric()
        result = df_tbl %>%
          filter(year <= project_end_year) %>%
          arrange(year) %>%
          summarise(project_no = as.numeric(proj_no),
                    rate = (1 - (last(Undisturbed + Degraded) / first(Undisturbed + Degraded))^(1 / n())) * 100,
                    .groups = "drop")
        return(result)
      }
    }, df_list, names(df_list), SIMPLIFY = FALSE)
  } else {
    summarized_list = mapply(function(df_tbl, file_name) {
      proj_no = str_extract(file_name, "\\d+")
      result = df_tbl %>%
        arrange(year) %>%
        summarise(project_no = as.numeric(proj_no),
                  rate = (1 - (last(Undisturbed + Degraded) / first(Undisturbed + Degraded))^(1 / n())) * 100,
                  .groups = "drop")
      return(result)
    }, df_list, names(df_list), SIMPLIFY = FALSE)
  }
  
  summarized_list = summarized_list[!sapply(summarized_list, is.null)]
  
  # combine all processed dataframes into one tibble
  combined_rate_tbl = bind_rows(summarized_list) %>%
    select(project_no, rate)
  
  return(combined_rate_tbl)
}

# ---- JOIN CERTIFIED AND ACC DEFORESTATION RATES ----

certified_df = deforestation_rate("parquets/acc_certified_control_parquets") %>% 
  rename(certified_rate = rate)

pact_df = deforestation_rate("parquets/acc_pact_matching_parquets",
                             "csvs/evaluation_end_years.csv",
                             TRUE) %>%
  rename(qem_rate = rate)

control_areas_df = left_join(certified_df, pact_df,
                             by = "project_no")

# ---- RESHAPE DATA FOR PLOTTING ----
plot_control_areas_df = control_areas_df %>% 
  pivot_longer(cols = -project_no,
               names_to = "variable",
               values_to = "value")

plot_control_areas_df$variable = factor(plot_control_areas_df$variable,
                                        levels = c("qem_rate", "certified_rate"))

# ---- PLOT RESULTS ----
s6 = ggplot(plot_control_areas_df, aes(x = variable, y = value, colour = variable)) +
  geom_point(data = plot_control_areas_df,
             position = position_jitter(width = 0.1),
             alpha = 0.3, size = 4, shape = 16) +
  stat_summary(data = plot_control_areas_df,
               aes(x = variable, y = value, colour = variable),
               fun.data = function(y) {
                 data.frame(
                   y = median(y, na.rm = TRUE),
                   ymin = quantile(y, 0.25, na.rm = TRUE),
                   ymax = quantile(y, 0.75, na.rm = TRUE)
                 )
               }, geom = "errorbar", width = 0.2, linewidth = 1,
               position = position_dodge(width = 0.5)) + 
  stat_summary(data = plot_control_areas_df,
               aes(x = variable, y = value, colour = variable),
               fun = median, geom = "crossbar", width = 0.2,
               linewidth = 1, position = position_dodge(width = 0.5)) +
  
  scale_x_discrete(labels = c("qem_rate" = "ACC Control\nQuasi-experimental Methods\n(n = 17)",
                              "certified_rate" = "ACC Control\nCertified Methods\n(n = 17)")) +
  scale_color_manual(values = c("qem_rate" = "darkorchid4", "certified_rate" = "darkred")) +
  
  scale_y_continuous(labels = function(x) sprintf("%.1f", x)) +
  
  ylab("Deforestation Rate (%/year)") +
  theme_classic() +
  theme(axis.title.x  = element_blank(),
        plot.tag = element_text(size = 35),
        axis.title.y  = element_text(size = 24),
        axis.text.x   = element_text(size = 20, colour = "black"),
        axis.text.y   = element_text(size = 18),
        axis.line = element_line(linewidth = 1),
        legend.position = "none")

# ---- FACET GRID PLOT ----

# facet the plots above using multiple plot
ggsave(filename = "pngs/s7_raw.png", plot = s6, dpi = 300, width = 8, height = 6)


# ---- EXTRACT STATS ----

# extract wilcoxon between certified and qem
wilcox.test(control_areas_df$certified_rate, control_areas_df$qem_rate, paired = TRUE)
wilcox.test(control_areas_df$certified_rate, control_areas_df$qem_rate, alternative = "greater", paired = TRUE)
# extract median and iqr for certified and qem
control_areas_df %>%
  summarise(median_certified = median(certified_rate, na.rm = TRUE),
            median_qem = median(qem_rate, na.rm = TRUE),
            ratio = median_certified / median_qem)

            