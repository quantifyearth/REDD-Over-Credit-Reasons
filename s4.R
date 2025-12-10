library(tidyverse)
library(sf)
library(arrow)
library(patchwork)
library(ggsignif)


# ---- LOAD AND PROCESS PARQUET FILES ----

# load all parquet files from the acc_project_area_parquets folder
acc_files = list.files("parquets/acc_project_area_parquets", full.names = TRUE)

# compute the percentage of undisturbed forest in each project

undisturbed_list = list()

for (file in acc_files) {
  # read the project area (k) parquet file and convert to tibble
  df = read_parquet(file) %>% as_tibble()
  # compute the percentage of undisturbed forest at project start year
  percent_undisturbed = (mean(df[[20]] == 1, na.rm = TRUE) + mean(df[[20]] == 2, na.rm = TRUE)) * 100
  # extract the project id from the file name
  project_id = as.numeric(gsub("_k.parquet", "", basename(file)))
  # store the project id and undisturbed percentage in a tibble
  undisturbed_list[[basename(file)]] = tibble(
    project_no = project_id,
    undisturbed_percent = percent_undisturbed
  )
}

# unravel the list of tibbles into a single tibble
undisturbed_df = bind_rows(undisturbed_list)

# ---- COMPUTE ACC DEFORESTATION RATES ----

# load end year data for each project
end_years_df = read.csv("csvs/evaluation_end_years.csv")

# compute the acc deforestation rate for each project
deforestation_list = list()

for (file in acc_files) {
  # read the project area (k) parquet file and convert to tibble
  df = read_parquet(file) %>% 
    as_tibble() %>% 
    # select only the columns with land use class data
    select(starts_with("luc"))
  # extract the project id from the file name
  project_id = as.numeric(str_extract(basename(file), "\\d+"))
  # convert data to long format for determining undisturbed forest sums
  df_long = df %>% pivot_longer(
    cols = everything(),
    names_to = "year",
    values_to = "luc",
    names_pattern = "luc_(\\d+)"
  ) %>% mutate(year = as.integer(year))
  # count the number of undisturbed forest pixels per year
  yearly_counts = df_long %>%
    group_by(year) %>%
    summarise(forest = sum(luc == 1, na.rm = TRUE) + sum(luc == 2, na.rm = TRUE), .groups = "drop")
  
  # discard the first 10 rows (years)
  yearly_counts = yearly_counts[-(1:10), ]
  
  # filter deforestation rate data up to the project's evaluation end year
  if (project_id %in% end_years_df$project_no) {
    # extract the project's end year
    project_end_year = end_years_df %>%
      filter(project_no == project_id) %>% pull(end_year)
    # find the average deforestation rate up to the project's evaluation end year
    deforestation_list[[basename(file)]] = yearly_counts %>%
      filter(year <= project_end_year) %>%
      arrange(year) %>%
      summarise(project_no = project_id,
                acc_rate = (1 - (last(forest) / first(forest))^(1 / n())) * 100, .groups = "drop")
  }
}

# unravel the list of tibbles into a single tibble
deforestation_rate_df = bind_rows(deforestation_list)

# ---- LOAD PROJECT GEOJSON FILES AND COMPUTE AREAS ----
geojson_df = list.files("geojsons/project_area_geojsons", full.names = TRUE) %>%
  map_dfr(~ {
    gdf = st_read(.x, quiet = TRUE) %>% st_make_valid()
    # work with hectares
    area_ha = sum(st_area(gdf), na.rm = TRUE) / 10000
    tibble(project_no = as.numeric(gsub(".geojson", "", basename(.x))),
           area_ha = as.numeric(area_ha))
  })

# ---- LOAD CERTIFIED RATES AND CALCULATE CERTIFIED RATES ----

# load the certified rates csv
cert_df = read.csv("csvs/certified_project_amounts.csv") %>%
  # add the hectarage of project areas
  left_join(geojson_df, by = "project_no") %>%
  # add the proportion of undisturbed
  left_join(undisturbed_df, by = "project_no") %>%
  # work out the amount of undisturbed
  mutate(area_undisturbed = area_ha * (undisturbed_percent / 100))

# update undisturbed area over time as deforestation progresses
cert_df = cert_df %>%
  group_by(project_no) %>%
  mutate(area_undisturbed = first(area_undisturbed) -
           cumsum(lag(proj_def, default = 0))) %>%
  ungroup() %>%
  drop_na()

# compute the mean self-reported deforestation rates
cert_df = cert_df %>%
  group_by(project_no) %>%
  summarise(total = sum(proj_def),
            start = max(area_undisturbed), .groups = "drop")
cert_df$end = cert_df$start - cert_df$total

# load eval periods csv
eval_periods = read.csv("csvs/evaluation_periods.csv")
cert_df = cert_df %>%
  left_join(eval_periods, by = "project_no")
cert_df$compound = (1 - (cert_df$end / cert_df$start)^(1 / cert_df$period)) * 100
cert_df = cert_df %>% rename(cert_rate = compound) 
# ---- SCATTER PLOT (FIGURE 3A) ----

# join the deforestation rates and compute the difference
comparison_df = cert_df %>%
  left_join(deforestation_rate_df, by = "project_no") %>%
  select(project_no, acc_rate, cert_rate)


s4a_plot = ggplot(comparison_df, aes(x = cert_rate, y = acc_rate)) +
  annotate("text", x = 0.6, y = 3, label = "ACC Rate is Higher",
           size = 6) +
  annotate("text", x = 2.6, y = 0.1, label = "ACC Rate is Lower",
           size = 6) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed",
              linewidth = 0.75) +
  geom_segment(aes(x = cert_rate, y = acc_rate,
                   xend = cert_rate, yend = cert_rate), 
               color = "firebrick", alpha = 1, linewidth = 0.5) +
  scale_x_continuous(limits = c(0, 3.2),
                     breaks = seq(0, 3.2, by = 0.5), expand = c(0, 0)) +
  geom_point(size = 1.8, alpha = 1, colour = "firebrick") +  
  scale_y_continuous(limits = c(0, 3.2),
                     breaks = seq(0, 3.2, by = 0.5), expand = c(0, 0)) +
  theme_classic() +
  theme(axis.title = element_text(size = 19),
        axis.text = element_text(size = 13)) +
  labs(x = "Certified Rate (%/year)",
       y = "ACC Rate (%/year)")

# ---- FIGURE 3B ----

plot_comparison_df = comparison_df %>% 
  pivot_longer(cols = -project_no,
               names_to = "variable",
               values_to = "value")

plot_comparison_df$variable = factor(plot_comparison_df$variable,
                                     levels = c("acc_rate", "cert_rate"))
s4b_plot = ggplot(plot_comparison_df, aes(x = variable, y = value, colour = variable)) +
  geom_point(position = position_jitter(width = 0.1), 
             alpha = 0.3, size = 4, shape = 16) +
  stat_summary(fun.data = function(y) {
    data.frame(
      y = median(y, na.rm = TRUE),
      ymin = quantile(y, 0.25, na.rm = TRUE),
      ymax = quantile(y, 0.75, na.rm = TRUE)
    )
  }, geom = "errorbar", width = 0.2, linewidth = 1,
  position = position_dodge(width = 0.5)) + 
  stat_summary(fun = median, geom = "crossbar", width = 0.2,
               linewidth = 1, position = position_dodge(width = 0.5)) +
  scale_x_discrete(labels = c("acc_rate" = "ACC Project\nQuasi-Experimental Methods\n(n = 36)",
                              "cert_rate" = "Certified Project\nCertified Methods\n(n = 36)")) +
  scale_color_manual(values = c("acc_rate" = "darkgreen", "cert_rate" = "darkorange")) +
  scale_y_continuous(labels = function(x) sprintf("%.1f", x)) +
  ylab("Deforestation Rate (%/year)") +
  theme_classic() +
  theme(axis.title  = element_text(size = 19),
        axis.title.x = element_blank(),
        axis.text.x  = element_text(size = 18, colour = "black"),
        axis.text.y  = element_text(size = 14),
        legend.position = "none")

# ---- COMBINE PLOTS INTO A PANEL ----

s4a_plot = s4a_plot + labs(tag = "a", size = 20)
s4b_plot = s4b_plot + labs(tag = "b", size = 20)
combined_plot = (s4a_plot + s4b_plot + plot_layout(ncol = 2)) &
  theme(plot.tag = element_text(size = 20))
combined_plot
# ---- SAVE FINAL PLOT ----

ggsave("pngs/s4_raw.png", combined_plot, bg = "white",
       width = 14, height = 7, units = "in", dpi = 500)

#---- EXTRACT STATS ----

median(comparison_df$cert_rate)
median(comparison_df$acc_rate)
shapiro.test(comparison_df$cert_rate)
shapiro.test(comparison_df$acc_rate)
wilcox.test(comparison_df$acc_rate, comparison_df$cert_rate, paired = TRUE)

# pairwise mean difference
median(comparison_df$acc_rate - comparison_df$cert_rate)

# do all over again with regrowth

# ---- DO IT ALL AGAIN REGROWTH AND DEGRADED----
rm(list = ls())
# ---- LOAD AND PROCESS PARQUET FILES ----

# load all parquet files from the acc_project_area_parquets folder
acc_files = list.files("parquets/acc_project_area_parquets", full.names = TRUE)

# compute the percentage of undisturbed forest in each project

undisturbed_list = list()

for (file in acc_files) {
  # read the project area (k) parquet file and convert to tibble
  df = read_parquet(file) %>% as_tibble()
  # compute the percentage of undisturbed forest at project start year
  percent_undisturbed = (mean(df[[20]] == 1, na.rm = TRUE) + mean(df[[20]] == 2, na.rm = TRUE) +
                           mean(df[[20]] == 4, na.rm = TRUE)) * 100
  # extract the project id from the file name
  project_id = as.numeric(gsub("_k.parquet", "", basename(file)))
  # store the project id and undisturbed percentage in a tibble
  undisturbed_list[[basename(file)]] = tibble(
    project_no = project_id,
    undisturbed_percent = percent_undisturbed
  )
}

# unravel the list of tibbles into a single tibble
undisturbed_df = bind_rows(undisturbed_list)

# ---- COMPUTE ACC DEFORESTATION RATES ----

# load end year data for each project
end_years_df = read.csv("csvs/evaluation_end_years.csv")

# compute the acc deforestation rate for each project
deforestation_list = list()

for (file in acc_files) {
  # read the project area (k) parquet file and convert to tibble
  df = read_parquet(file) %>% 
    as_tibble() %>% 
    # select only the columns with land use class data
    select(starts_with("luc"))
  # extract the project id from the file name
  project_id = as.numeric(str_extract(basename(file), "\\d+"))
  # convert data to long format for determining undisturbed forest sums
  df_long = df %>% pivot_longer(
    cols = everything(),
    names_to = "year",
    values_to = "luc",
    names_pattern = "luc_(\\d+)"
  ) %>% mutate(year = as.integer(year))
  # count the number of undisturbed forest pixels per year
  yearly_counts = df_long %>%
    group_by(year) %>%
    summarise(forest = sum(luc == 1, na.rm = TRUE) + sum(luc == 2, na.rm = TRUE) +
                sum(luc == 4, na.rm = TRUE), .groups = "drop")
  
  # discard the first 10 rows (years)
  yearly_counts = yearly_counts[-(1:10), ]
  
  # filter deforestation rate data up to the project's evaluation end year
  if (project_id %in% end_years_df$project_no) {
    # extract the project's end year
    project_end_year = end_years_df %>%
      filter(project_no == project_id) %>% pull(end_year)
    # find the average deforestation rate up to the project's evaluation end year
    deforestation_list[[basename(file)]] = yearly_counts %>%
      filter(year <= project_end_year) %>%
      arrange(year) %>%
      summarise(project_no = project_id,
                acc_rate = (1 - (last(forest) / first(forest))^(1 / n())) * 100, .groups = "drop")
  }
}

# unravel the list of tibbles into a single tibble
deforestation_rate_df = bind_rows(deforestation_list)

# ---- LOAD PROJECT GEOJSON FILES AND COMPUTE AREAS ----
geojson_df = list.files("geojsons/project_area_geojsons", full.names = TRUE) %>%
  map_dfr(~ {
    gdf = st_read(.x, quiet = TRUE) %>% st_make_valid()
    # work with hectares
    area_ha = sum(st_area(gdf), na.rm = TRUE) / 10000
    tibble(project_no = as.numeric(gsub(".geojson", "", basename(.x))),
           area_ha = as.numeric(area_ha))
  })

# ---- LOAD CERTIFIED RATES AND CALCULATE CERTIFIED RATES ----

# load the certified rates csv
cert_df = read.csv("csvs/certified_project_amounts.csv") %>%
  # add the hectarage of project areas
  left_join(geojson_df, by = "project_no") %>%
  # add the proportion of undisturbed
  left_join(undisturbed_df, by = "project_no") %>%
  # work out the amount of undisturbed
  mutate(area_undisturbed = area_ha * (undisturbed_percent / 100))

# update undisturbed area over time as deforestation progresses
cert_df = cert_df %>%
  group_by(project_no) %>%
  mutate(area_undisturbed = first(area_undisturbed) -
           cumsum(lag(proj_def, default = 0))) %>%
  ungroup() %>%
  drop_na()

# compute the mean self-reported deforestation rates
cert_df = cert_df %>%
  group_by(project_no) %>%
  summarise(total = sum(proj_def),
            start = max(area_undisturbed), .groups = "drop")
cert_df$end = cert_df$start - cert_df$total

# load eval periods csv
eval_periods = read.csv("csvs/evaluation_periods.csv")
cert_df = cert_df %>%
  left_join(eval_periods, by = "project_no")
cert_df$compound = (1 - (cert_df$end / cert_df$start)^(1 / cert_df$period)) * 100
cert_df = cert_df %>% rename(cert_rate = compound) 
# ---- SCATTER PLOT (FIGURE 3A) ----

# join the deforestation rates and compute the difference
comparison_df = cert_df %>%
  left_join(deforestation_rate_df, by = "project_no") %>%
  select(project_no, acc_rate, cert_rate)


s4a_plot = ggplot(comparison_df, aes(x = cert_rate, y = acc_rate)) +
  annotate("text", x = 0.6, y = 3, label = "ACC Rate is Higher",
           size = 6) +
  annotate("text", x = 2.6, y = 0.1, label = "ACC Rate is Lower",
           size = 6) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed",
              linewidth = 0.75) +
  geom_segment(aes(x = cert_rate, y = acc_rate,
                   xend = cert_rate, yend = cert_rate), 
               color = "lightgreen", alpha = 1, linewidth = 0.5) +
  scale_x_continuous(limits = c(0, 3.2),
                     breaks = seq(0, 3.2, by = 0.5), expand = c(0, 0)) +
  geom_point(size = 1.8, alpha = 1, colour = "lightgreen") +  
  scale_y_continuous(limits = c(0, 3.2),
                     breaks = seq(0, 3.2, by = 0.5), expand = c(0, 0)) +
  theme_classic() +
  theme(axis.title = element_text(size = 19),
        axis.text = element_text(size = 13)) +
  labs(x = "Certified Rate (%/year)",
       y = "ACC Rate (%/year)")

# ---- FIGURE 3B ----

plot_comparison_df = comparison_df %>% 
  pivot_longer(cols = -project_no,
               names_to = "variable",
               values_to = "value")

plot_comparison_df$variable = factor(plot_comparison_df$variable,
                                     levels = c("acc_rate", "cert_rate"))
s4b_plot = ggplot(plot_comparison_df, aes(x = variable, y = value, colour = variable)) +
  geom_point(position = position_jitter(width = 0.1), 
             alpha = 0.3, size = 4, shape = 16) +
  stat_summary(fun.data = function(y) {
    data.frame(
      y = median(y, na.rm = TRUE),
      ymin = quantile(y, 0.25, na.rm = TRUE),
      ymax = quantile(y, 0.75, na.rm = TRUE)
    )
  }, geom = "errorbar", width = 0.2, linewidth = 1,
  position = position_dodge(width = 0.5)) + 
  stat_summary(fun = median, geom = "crossbar", width = 0.2,
               linewidth = 1, position = position_dodge(width = 0.5)) +
  scale_x_discrete(labels = c("acc_rate" = "ACC Project\nQuasi-Experimental Methods\n(n = 36)",
                              "cert_rate" = "Certified Project\nCertified Methods\n(n = 36)")) +
  scale_color_manual(values = c("acc_rate" = "darkblue", "cert_rate" = "darkred")) +
  scale_y_continuous(labels = function(x) sprintf("%.1f", x)) +
  ylab("Deforestation Rate (%/year)") +
  theme_classic() +
  theme(axis.title  = element_text(size = 19),
        axis.title.x = element_blank(),
        axis.text.x  = element_text(size = 18, colour = "black"),
        axis.text.y  = element_text(size = 14),
        legend.position = "none")
                
p_label = paste("ACC Rate is Lower **")

s4b_plot = s4b_plot + 
  geom_signif(
    comparisons = list(c("acc_rate", "cert_rate")),
    test = "wilcox.test",
    map_signif_level = TRUE,
    y_position = 3.2,
    textsize = 6, 
    colour = "black",
    annotations = p_label
  )
                     
# ---- COMBINE PLOTS INTO A PANEL ----

s4a_plot = s4a_plot + labs(tag = "a", size = 20)
s4b_plot = s4b_plot + labs(tag = "b", size = 20)
combined_plot = (s4a_plot + s4b_plot + plot_layout(ncol = 2)) &
  theme(plot.tag = element_text(size = 20))
combined_plot
# ---- SAVE FINAL PLOT ----

ggsave("pngs/s4_2_raw.png", combined_plot, bg = "white",
       width = 14, height = 7, units = "in", dpi = 500)

#---- EXTRACT STATS ----

median(comparison_df$cert_rate)
median(comparison_df$acc_rate)
shapiro.test(comparison_df$cert_rate)
shapiro.test(comparison_df$acc_rate)
wilcox.test(comparison_df$acc_rate, comparison_df$cert_rate, paired = TRUE)

# pairwise mean difference
median(comparison_df$acc_rate - comparison_df$cert_rate)

# ---- DO IT ALL AGAIN DEGRADED TRANSITION TRACKING ----
rm(list = ls())


# ---- LOAD AND PROCESS PARQUET FILES ----

# load all parquet files from the acc_project_area_parquets folder
acc_files = list.files("parquets/acc_project_area_parquets", full.names = TRUE)

# compute the percentage of undisturbed forest in each project

undisturbed_list = list()

for (file in acc_files) {
  # read the project area (k) parquet file and convert to tibble
  df = read_parquet(file) %>% as_tibble()
  # compute the percentage of undisturbed forest at project start year
  percent_undisturbed = mean(df[[20]] %in% c(1,2), na.rm = TRUE) * 100
  # print col name being used
  print(paste("Processing file:", basename(file), "using column:", names(df)[20]))
  # extract the project id from the file name
  project_id = as.numeric(gsub("_k.parquet", "", basename(file)))
  # store the project id and undisturbed percentage in a tibble
  undisturbed_list[[basename(file)]] = tibble(
    project_no = project_id,
    undisturbed_percent = percent_undisturbed
  )
}

# unravel the list of tibbles into a single tibble
undisturbed_df = bind_rows(undisturbed_list)

# ---- COMPUTE ACC DEFORESTATION RATES ----

# load end year data for each project
end_years_df = read.csv("csvs/evaluation_end_years.csv")

# compute the acc deforestation rate for each project
deforestation_list = list()

# load end year data for each project
end_years_df = read.csv("csvs/evaluation_end_years.csv")

# compute the acc deforestation rate for each project
deforestation_list = list()

for (file in acc_files) {
  # read the project area (k) parquet file and convert to tibble
  df = read_parquet(file) %>% as_tibble()
  
  # extract the project id from the file name
  project_id = as.numeric(str_extract(basename(file), "\\d+"))
  
  # get start year from column 20
  start_year_col = names(df)[20]
  start_year = as.numeric(str_extract(start_year_col, "\\d+"))
  print(paste("Processing project", project_id, "using years from", start_year_col))
  # extract start year
  start_year = as.numeric(str_extract(start_year_col, "\\d+"))
  # get end year for this project
  if (project_id %in% end_years_df$project_no) {
    project_end_year = end_years_df %>%
      filter(project_no == project_id) %>% pull(end_year)
    
    # find end column
    end_col = paste0("luc_", project_end_year)
    
    print(paste("Project", project_id, "end year column:", end_col))
    
    if (end_col %in% names(df)) {
      # calculate start forest pixels (classes 1 and 2)
      start_1_pixels = sum(df[[start_year_col]] == 1, na.rm = TRUE)
      start_2_pixels = sum(df[[start_year_col]] == 2, na.rm = TRUE)
      start_forest_pixels = start_1_pixels + start_2_pixels
      print(paste("Project", project_id, "start forest pixels:", start_forest_pixels))
      
      # count 1s that became 2, 3, or 4
      lost_1_to_other = 0
      if (start_1_pixels > 0) {
        # pixels that were 1 at start
        pixels_start_1 = which(df[[start_year_col]] == 1)
        # how many became 2, 3, or 4
        lost_1_to_other = sum(df[pixels_start_1, end_col, drop = TRUE] %in% c(2, 3, 4), na.rm = TRUE)
        print(paste("Project", project_id, "lost 1 to other pixels:", lost_1_to_other))
      }
      # 2s that became 3s
      lost_2_to_3 = 0
      if (start_2_pixels > 0) {
        # pixels that were 4 at start
        pixels_start_2 = which(df[[start_year_col]] == 2)
        # how many became 3
        lost_2_to_3 = sum(df[pixels_start_2, end_col, drop = TRUE] == 3, na.rm = TRUE)
        print(paste("Project", project_id, "lost 2 to 3 pixels:", lost_2_to_3))
      }
      
      
      total_lost_pixels = lost_1_to_other + lost_2_to_3
      print(paste("Project", project_id, "total lost pixels:", total_lost_pixels))
      end_forest_pixels = start_forest_pixels - total_lost_pixels
      print(paste("Project", project_id, "end forest pixels:", end_forest_pixels))
      
      # proportions
      total_pixels = nrow(df)
      start_proportion = start_forest_pixels / total_pixels
      end_proportion = end_forest_pixels / total_pixels
      
      # period years
      period_years = project_end_year - start_year
      
      # deforestation rate (using proportions)
      if (start_proportion > 0 && end_proportion > 0 && period_years > 0) {
        acc_rate = (1 - (end_forest_pixels / start_forest_pixels)^(1 / (period_years + 1))) * 100
      } else {
        acc_rate = NA_real_
      }
      print(paste("Project", project_id, "ACC deforestation rate:", round(acc_rate, 4), "%/year"))
      
      # results
      deforestation_list[[basename(file)]] = tibble(
        project_no = project_id,
        acc_rate = acc_rate,
        start_year = start_year,
        end_year = project_end_year,
        start_proportion = start_proportion,
        end_proportion = end_proportion,
        period_years = period_years,
        start_forest_pixels = start_forest_pixels,
        end_forest_pixels = end_forest_pixels,
        lost_1_to_other = lost_1_to_other,
        total_2_to_3 = lost_2_to_3,
        total_lost_pixels = total_lost_pixels
      )
    }
  }
}

# unravel the list of tibbles into a single tibble
deforestation_rate_df = bind_rows(deforestation_list)
sum(deforestation_rate_df$total_2_to_3)
# make table of total lost pixels by cause
total_lost_summary = deforestation_rate_df %>%
  summarise(
    total_lost_1_to_other = sum(lost_1_to_other, na.rm = TRUE),
    proportion_lost_1_to_other = total_lost_1_to_other / sum(total_lost_pixels, na.rm = TRUE),
    total_lost_2_to_3 = sum(total_2_to_3, na.rm = TRUE),
    proportion_lost_2_to_3 = total_lost_2_to_3 / sum(total_lost_pixels, na.rm = TRUE),
    total_lost_pixels = sum(total_lost_pixels, na.rm = TRUE),
    loss_rate = median(acc_rate)
  ) %>%
  pivot_longer(cols = everything(),
               names_to = "metric",
               values_to = "value")
total_lost_summary
# ---- LOAD PROJECT GEOJSON FILES AND COMPUTE AREAS ----
geojson_df = list.files("geojsons/project_area_geojsons", full.names = TRUE) %>%
  map_dfr(~ {
    gdf = st_read(.x, quiet = TRUE) %>% st_make_valid()
    # work with hectares
    area_ha = sum(st_area(gdf), na.rm = TRUE) / 10000
    tibble(project_no = as.numeric(gsub(".geojson", "", basename(.x))),
           area_ha = as.numeric(area_ha))
  })

# ---- LOAD CERTIFIED RATES AND CALCULATE CERTIFIED RATES ----

# load the certified rates csv
cert_df = read.csv("csvs/certified_project_amounts.csv") %>%
  # add the hectarage of project areas
  left_join(geojson_df, by = "project_no") %>%
  # add the proportion of undisturbed
  left_join(undisturbed_df, by = "project_no") %>%
  # work out the amount of undisturbed
  mutate(area_undisturbed = area_ha * (undisturbed_percent / 100))

# update undisturbed area over time as deforestation progresses
cert_df = cert_df %>%
  group_by(project_no) %>%
  mutate(area_undisturbed = first(area_undisturbed),
         area_ha = first(area_ha)) %>%
  ungroup() %>%
  drop_na()

# compute the compound self-reported deforestation rates
cert_df = cert_df %>%
  group_by(project_no) %>%
  arrange(year) %>%
  summarise(total = sum(proj_def),
            start = first(area_undisturbed),
            area_ha = first(area_ha),
            undisturbed_percent = first(undisturbed_percent),.groups = "drop")
cert_df$end = cert_df$start - cert_df$total

# load eval periods csv
eval_periods = read.csv("csvs/evaluation_periods.csv")
cert_df = cert_df %>%
  left_join(eval_periods, by = "project_no")
cert_df$compound = (1 - (cert_df$end / cert_df$start)^(1 / cert_df$period)) * 100
cert_df = cert_df %>% rename(cert_rate = compound) 

# ---- SCATTER PLOT (FIGURE 3A) ----

# join the deforestation rates and compute the difference
comparison_df = cert_df %>%
  left_join(deforestation_rate_df, by = "project_no") %>%
  select(project_no, acc_rate, cert_rate)


s4a_plot = ggplot(comparison_df, aes(x = cert_rate, y = acc_rate)) +
  annotate("text", x = 0.7, y = 3.5, label = "ACC Rate is Higher",
           size = 6) +
  annotate("text", x = 3.0, y = 0.2, label = "ACC Rate is Lower",
           size = 6) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed",
              linewidth = 0.75) +
  geom_segment(aes(x = cert_rate, y = acc_rate,
                   xend = cert_rate, yend = cert_rate), 
               color = "firebrick", alpha = 1, linewidth = 0.5) +
  scale_x_continuous(limits = c(0, 3.8),
                     breaks = seq(0, 3.8, by = 0.5), expand = c(0, 0)) +
  geom_point(size = 1.8, alpha = 1, colour = "firebrick") +  
  scale_y_continuous(limits = c(0, 3.8),
                     breaks = seq(0, 3.8, by = 0.5), expand = c(0, 0)) +
  theme_classic() +
  theme(axis.title = element_text(size = 19),
        axis.text = element_text(size = 13)) +
  labs(x = "Certified Rate (%/year)",
       y = "ACC Rate (%/year)")

# ---- FIGURE 3B ----

plot_comparison_df = comparison_df %>% 
  pivot_longer(cols = -project_no,
               names_to = "variable",
               values_to = "value")

plot_comparison_df$variable = factor(plot_comparison_df$variable,
                                     levels = c("acc_rate", "cert_rate"))
s4b_plot = ggplot(plot_comparison_df, aes(x = variable, y = value, colour = variable)) +
  geom_point(position = position_jitter(width = 0.1), 
             alpha = 0.3, size = 4, shape = 16) +
  stat_summary(fun.data = function(y) {
    data.frame(
      y = median(y, na.rm = TRUE),
      ymin = quantile(y, 0.25, na.rm = TRUE),
      ymax = quantile(y, 0.75, na.rm = TRUE)
    )
  }, geom = "errorbar", width = 0.2, linewidth = 1,
  position = position_dodge(width = 0.5)) + 
  stat_summary(fun = median, geom = "crossbar", width = 0.2,
               linewidth = 1, position = position_dodge(width = 0.5)) +
  scale_x_discrete(labels = c("acc_rate" = "ACC Project\nQuasi-Experimental Methods\n(n = 36)",
                              "cert_rate" = "Certified Project\nCertified Methods\n(n = 36)")) +
  scale_color_manual(values = c("acc_rate" = "darkgreen", "cert_rate" = "darkorange")) +
  scale_y_continuous(labels = function(x) sprintf("%.1f", x)) +
  ylab("Deforestation Rate (%/year)") +
  theme_classic() +
  theme(axis.title  = element_text(size = 19),
        axis.title.x = element_blank(),
        axis.text.x  = element_text(size = 18, colour = "black"),
        axis.text.y  = element_text(size = 14),
        legend.position = "none")

p_label = paste("ACC Rate is Higher **")

s4b_plot = s4b_plot + 
  geom_signif(
    comparisons = list(c("acc_rate", "cert_rate")),
    test = "wilcox.test",
    map_signif_level = TRUE,
    y_position = 3.2,
    textsize = 6, 
    colour = "black",
    annotations = p_label
  )

# ---- COMBINE PLOTS INTO A PANEL ----

s4a_plot = s4a_plot + labs(tag = "a", size = 20)
s4b_plot = s4b_plot + labs(tag = "b", size = 20)
combined_plot = (s4a_plot + s4b_plot + plot_layout(ncol = 2)) &
  theme(plot.tag = element_text(size = 20))
combined_plot
# ---- SAVE FINAL PLOT ----

ggsave("pngs/s4_3_raw.png", combined_plot, bg = "white",
       width = 14, height = 7, units = "in", dpi = 500)

#---- EXTRACT STATS ----

median(comparison_df$cert_rate)
median(comparison_df$acc_rate)
shapiro.test(comparison_df$cert_rate)
shapiro.test(comparison_df$acc_rate)
wilcox.test(comparison_df$acc_rate, comparison_df$cert_rate, paired = TRUE)

# pairwise mean difference
median(comparison_df$acc_rate - comparison_df$cert_rate)

# do all over again with regrowth


# ---- DO IT ALL AGAIN REGROWTH TRANSITION TRACKING ----
rm(list = ls())


# ---- LOAD AND PROCESS PARQUET FILES ----

# load all parquet files from the acc_project_area_parquets folder
acc_files = list.files("parquets/acc_project_area_parquets", full.names = TRUE)

# compute the percentage of undisturbed forest in each project

undisturbed_list = list()

for (file in acc_files) {
  # read the project area (k) parquet file and convert to tibble
  df = read_parquet(file) %>% as_tibble()
  # compute the percentage of undisturbed forest at project start year
  percent_undisturbed = mean(df[[20]] %in% c(1,4), na.rm = TRUE) * 100
  # print col name being used
  print(paste("Processing file:", basename(file), "using column:", names(df)[20]))
  # extract the project id from the file name
  project_id = as.numeric(gsub("_k.parquet", "", basename(file)))
  # store the project id and undisturbed percentage in a tibble
  undisturbed_list[[basename(file)]] = tibble(
    project_no = project_id,
    undisturbed_percent = percent_undisturbed
  )
}

# unravel the list of tibbles into a single tibble
undisturbed_df = bind_rows(undisturbed_list)

# ---- COMPUTE ACC DEFORESTATION RATES ----

# load end year data for each project
end_years_df = read.csv("csvs/evaluation_end_years.csv")

# compute the acc deforestation rate for each project
deforestation_list = list()

# load end year data for each project
end_years_df = read.csv("csvs/evaluation_end_years.csv")

# compute the acc deforestation rate for each project
deforestation_list = list()

for (file in acc_files) {
  # read the project area (k) parquet file and convert to tibble
  df = read_parquet(file) %>% as_tibble()
  
  # extract the project id from the file name
  project_id = as.numeric(str_extract(basename(file), "\\d+"))
  
  # get start year from column 20
  start_year_col = names(df)[20]
  start_year = as.numeric(str_extract(start_year_col, "\\d+"))
  print(paste("Processing project", project_id, "using years from", start_year_col))
  # extract start year
  start_year = as.numeric(str_extract(start_year_col, "\\d+"))
  # get end year for this project
  if (project_id %in% end_years_df$project_no) {
    project_end_year = end_years_df %>%
      filter(project_no == project_id) %>% pull(end_year)
    
    # find end column
    end_col = paste0("luc_", project_end_year)
    
    print(paste("Project", project_id, "end year column:", end_col))
    
    if (end_col %in% names(df)) {
      # calculate start forest pixels (classes 1 and 2)
      start_1_pixels = sum(df[[start_year_col]] == 1, na.rm = TRUE)
      start_4_pixels = sum(df[[start_year_col]] == 4, na.rm = TRUE)
      start_forest_pixels = start_1_pixels + start_4_pixels
      print(paste("Project", project_id, "start forest pixels:", start_forest_pixels))
      
      # count 1s that became 2, 3, or 4
      lost_1_to_other = 0
      if (start_1_pixels > 0) {
        # pixels that were 1 at start
        pixels_start_1 = which(df[[start_year_col]] == 1)
        # how many became 2, 3, or 4
        lost_1_to_other = sum(df[pixels_start_1, end_col, drop = TRUE] %in% c(2, 3, 4), na.rm = TRUE)
        print(paste("Project", project_id, "lost 1 to other pixels:", lost_1_to_other))
      }
      # 4s that became 3s
      lost_4_to_3 = 0
      if (start_4_pixels > 0) {
        # pixels that were 4 at start
        pixels_start_4 = which(df[[start_year_col]] == 4)
        # how many became 3
        lost_4_to_3 = sum(df[pixels_start_4, end_col, drop = TRUE] == 3, na.rm = TRUE)
        print(paste("Project", project_id, "lost 4 to 3 pixels:", lost_4_to_3))
      }
      
      
      total_lost_pixels = lost_1_to_other + lost_4_to_3
      end_forest_pixels = start_forest_pixels - total_lost_pixels
      print(paste("Project", project_id, "end forest pixels:", end_forest_pixels))
      
      # proportions
      total_pixels = nrow(df)
      start_proportion = start_forest_pixels / total_pixels
      end_proportion = end_forest_pixels / total_pixels
      
      # period years
      period_years = project_end_year - start_year
      
      # deforestation rate (using proportions)
      if (start_proportion > 0 && end_proportion > 0 && period_years > 0) {
        acc_rate = (1 - (end_forest_pixels / start_forest_pixels)^(1 / (period_years + 1))) * 100
      } else {
        acc_rate = NA_real_
      }
      print(paste("Project", project_id, "ACC deforestation rate:", round(acc_rate, 4), "%/year"))
      
      # results
      deforestation_list[[basename(file)]] = tibble(
        project_no = project_id,
        acc_rate = acc_rate,
        start_year = start_year,
        end_year = project_end_year,
        start_proportion = start_proportion,
        end_proportion = end_proportion,
        period_years = period_years,
        start_forest_pixels = start_forest_pixels,
        end_forest_pixels = end_forest_pixels,
        lost_1_to_other = lost_1_to_other,
        lost_4_to_3 = lost_4_to_3,
        total_lost_pixels = total_lost_pixels
      )
    }
  }
}

# unravel the list of tibbles into a single tibble
deforestation_rate_df = bind_rows(deforestation_list)

# make table of total lost pixels by cause
total_lost_summary = deforestation_rate_df %>%
  summarise(
    total_lost_1_to_other = sum(lost_1_to_other, na.rm = TRUE),
    proportion_lost_1_to_other = total_lost_1_to_other / sum(total_lost_pixels, na.rm = TRUE),
    total_lost_4_to_3 = sum(lost_4_to_3, na.rm = TRUE),
    proportion_lost_4_to_3 = total_lost_4_to_3 / sum(total_lost_pixels, na.rm = TRUE),
    total_lost_pixels = sum(total_lost_pixels, na.rm = TRUE)
  )
total_lost_summary
# ---- LOAD PROJECT GEOJSON FILES AND COMPUTE AREAS ----
geojson_df = list.files("geojsons/project_area_geojsons", full.names = TRUE) %>%
  map_dfr(~ {
    gdf = st_read(.x, quiet = TRUE) %>% st_make_valid()
    # work with hectares
    area_ha = sum(st_area(gdf), na.rm = TRUE) / 10000
    tibble(project_no = as.numeric(gsub(".geojson", "", basename(.x))),
           area_ha = as.numeric(area_ha))
  })

# ---- LOAD CERTIFIED RATES AND CALCULATE CERTIFIED RATES ----

# load the certified rates csv
cert_df = read.csv("csvs/certified_project_amounts.csv") %>%
  # add the hectarage of project areas
  left_join(geojson_df, by = "project_no") %>%
  # add the proportion of undisturbed
  left_join(undisturbed_df, by = "project_no") %>%
  # work out the amount of undisturbed
  mutate(area_undisturbed = area_ha * (undisturbed_percent / 100))

# update undisturbed area over time as deforestation progresses
cert_df = cert_df %>%
  group_by(project_no) %>%
  mutate(area_undisturbed = first(area_undisturbed),
         area_ha = first(area_ha)) %>%
  ungroup() %>%
  drop_na()

# compute the compound self-reported deforestation rates
cert_df = cert_df %>%
  group_by(project_no) %>%
  arrange(year) %>%
  summarise(total = sum(proj_def),
            start = first(area_undisturbed),
            area_ha = first(area_ha),
            undisturbed_percent = first(undisturbed_percent),.groups = "drop")
cert_df$end = cert_df$start - cert_df$total

# load eval periods csv
eval_periods = read.csv("csvs/evaluation_periods.csv")
cert_df = cert_df %>%
  left_join(eval_periods, by = "project_no")
cert_df$compound = (1 - (cert_df$end / cert_df$start)^(1 / cert_df$period)) * 100
cert_df = cert_df %>% rename(cert_rate = compound) 

# ---- SCATTER PLOT (FIGURE 3A) ----

# join the deforestation rates and compute the difference
comparison_df = cert_df %>%
  left_join(deforestation_rate_df, by = "project_no") %>%
  select(project_no, acc_rate, cert_rate)


s4a_plot = ggplot(comparison_df, aes(x = cert_rate, y = acc_rate)) +
  annotate("text", x = 0.6, y = 3, label = "ACC Rate is Higher",
           size = 6) +
  annotate("text", x = 2.6, y = 0.1, label = "ACC Rate is Lower",
           size = 6) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed",
              linewidth = 0.75) +
  geom_segment(aes(x = cert_rate, y = acc_rate,
                   xend = cert_rate, yend = cert_rate), 
               color = "violet", alpha = 1, linewidth = 0.5) +
  scale_x_continuous(limits = c(0, 3.2),
                     breaks = seq(0, 3.2, by = 0.5), expand = c(0, 0)) +
  geom_point(size = 1.8, alpha = 1, colour = "violet") +  
  scale_y_continuous(limits = c(0, 3.2),
                     breaks = seq(0, 3.2, by = 0.5), expand = c(0, 0)) +
  theme_classic() +
  theme(axis.title = element_text(size = 19),
        axis.text = element_text(size = 13)) +
  labs(x = "Certified Rate (%/year)",
       y = "ACC Rate (%/year)")

# ---- FIGURE 3B ----

plot_comparison_df = comparison_df %>% 
  pivot_longer(cols = -project_no,
               names_to = "variable",
               values_to = "value")

plot_comparison_df$variable = factor(plot_comparison_df$variable,
                                     levels = c("acc_rate", "cert_rate"))
s4b_plot = ggplot(plot_comparison_df, aes(x = variable, y = value, colour = variable)) +
  geom_point(position = position_jitter(width = 0.1), 
             alpha = 0.3, size = 4, shape = 16) +
  stat_summary(fun.data = function(y) {
    data.frame(
      y = median(y, na.rm = TRUE),
      ymin = quantile(y, 0.25, na.rm = TRUE),
      ymax = quantile(y, 0.75, na.rm = TRUE)
    )
  }, geom = "errorbar", width = 0.2, linewidth = 1,
  position = position_dodge(width = 0.5)) + 
  stat_summary(fun = median, geom = "crossbar", width = 0.2,
               linewidth = 1, position = position_dodge(width = 0.5)) +
  scale_x_discrete(labels = c("acc_rate" = "ACC Project\nQuasi-Experimental Methods\n(n = 36)",
                              "cert_rate" = "Certified Project\nCertified Methods\n(n = 36)")) +
  scale_color_manual(values = c("acc_rate" = "burlywood4", "cert_rate" = "cadetblue4")) +
  scale_y_continuous(labels = function(x) sprintf("%.1f", x)) +
  ylab("Deforestation Rate (%/year)") +
  theme_classic() +
  theme(axis.title  = element_text(size = 19),
        axis.title.x = element_blank(),
        axis.text.x  = element_text(size = 18, colour = "black"),
        axis.text.y  = element_text(size = 14),
        legend.position = "none")


# ---- COMBINE PLOTS INTO A PANEL ----
p_label = paste("ACC Rate is Higher **")

s4b_plot = s4b_plot + 
  geom_signif(
    comparisons = list(c("acc_rate", "cert_rate")),
    test = "wilcox.test",
    map_signif_level = TRUE,
    y_position = 3.2,
    textsize = 6, 
    colour = "black",
    annotations = p_label
  )

s4a_plot = s4a_plot + labs(tag = "a", size = 20)
s4b_plot = s4b_plot + labs(tag = "b", size = 20)
combined_plot = (s4a_plot + s4b_plot + plot_layout(ncol = 2)) &
  theme(plot.tag = element_text(size = 20))
combined_plot
# ---- SAVE FINAL PLOT ----

ggsave("pngs/s4_4_raw.png", combined_plot, bg = "white",
       width = 14, height = 7, units = "in", dpi = 500)

#---- EXTRACT STATS ----

median(comparison_df$cert_rate)
median(comparison_df$acc_rate)
shapiro.test(comparison_df$cert_rate)
shapiro.test(comparison_df$acc_rate)
wilcox.test(comparison_df$acc_rate, comparison_df$cert_rate, paired = TRUE)

# pairwise mean difference
median(comparison_df$acc_rate - comparison_df$cert_rate)

# do all over again with regrowth


# ---- DO IT ALL AGAIN REGROWTH AND DEGRADED TRANSITION TRACKING ----
rm(list = ls())


# ---- LOAD AND PROCESS PARQUET FILES ----

# load all parquet files from the acc_project_area_parquets folder
acc_files = list.files("parquets/acc_project_area_parquets", full.names = TRUE)

# compute the percentage of undisturbed forest in each project

undisturbed_list = list()

for (file in acc_files) {
  # read the project area (k) parquet file and convert to tibble
  df = read_parquet(file) %>% as_tibble()
  # compute the percentage of undisturbed forest at project start year
  percent_undisturbed = mean(df[[20]] %in% c(1,4,2), na.rm = TRUE) * 100
  # print col name being used
  print(paste("Processing file:", basename(file), "using column:", names(df)[20]))
  # extract the project id from the file name
  project_id = as.numeric(gsub("_k.parquet", "", basename(file)))
  # store the project id and undisturbed percentage in a tibble
  undisturbed_list[[basename(file)]] = tibble(
    project_no = project_id,
    undisturbed_percent = percent_undisturbed
  )
}

# unravel the list of tibbles into a single tibble
undisturbed_df = bind_rows(undisturbed_list)

# ---- COMPUTE ACC DEFORESTATION RATES ----

# load end year data for each project
end_years_df = read.csv("csvs/evaluation_end_years.csv")

# compute the acc deforestation rate for each project
deforestation_list = list()

# load end year data for each project
end_years_df = read.csv("csvs/evaluation_end_years.csv")

# compute the acc deforestation rate for each project
deforestation_list = list()

for (file in acc_files) {
  # read the project area (k) parquet file and convert to tibble
  df = read_parquet(file) %>% as_tibble()
  
  # extract the project id from the file name
  project_id = as.numeric(str_extract(basename(file), "\\d+"))
  
  # get start year from column 20
  start_year_col = names(df)[20]
  start_year = as.numeric(str_extract(start_year_col, "\\d+"))
  print(paste("Processing project", project_id, "using years from", start_year_col))
  # extract start year
  start_year = as.numeric(str_extract(start_year_col, "\\d+"))
  # get end year for this project
  if (project_id %in% end_years_df$project_no) {
    project_end_year = end_years_df %>%
      filter(project_no == project_id) %>% pull(end_year)
    
    # find end column
    end_col = paste0("luc_", project_end_year)
    
    print(paste("Project", project_id, "end year column:", end_col))
    
    if (end_col %in% names(df)) {
      # calculate start forest pixels (classes 1 and 2 and 4)
      start_1_pixels = sum(df[[start_year_col]] == 1, na.rm = TRUE)
      start_4_pixels = sum(df[[start_year_col]] == 4, na.rm = TRUE)
      start_2_pixels = sum(df[[start_year_col]] == 2, na.rm = TRUE)
      start_forest_pixels = start_1_pixels + start_4_pixels + start_2_pixels
      print(paste("Project", project_id, "start forest pixels:", start_forest_pixels))
      
      # count 1s that became 2, 3, or 4
      lost_1_to_other = 0
      if (start_1_pixels > 0) {
        # pixels that were 1 at start
        pixels_start_1 = which(df[[start_year_col]] == 1)
        # how many became 2, 3, or 4
        lost_1_to_other = sum(df[pixels_start_1, end_col, drop = TRUE] %in% c(2, 3, 4), na.rm = TRUE)
        print(paste("Project", project_id, "lost 1 to other pixels:", lost_1_to_other))
      }
      # 4s that became 3s
      lost_4_to_3 = 0
      if (start_4_pixels > 0) {
        # pixels that were 4 at start
        pixels_start_4 = which(df[[start_year_col]] == 4)
        # how many became 3
        lost_4_to_3 = sum(df[pixels_start_4, end_col, drop = TRUE] == 3, na.rm = TRUE)
        print(paste("Project", project_id, "lost 4 to 3 pixels:", lost_4_to_3))
      }
      # 2s that became 3s
      lost_2_to_3 = 0
      if (start_2_pixels > 0) {
        # pixels that were 2 at start
        pixels_start_2 = which(df[[start_year_col]] == 2)
        # how many became 3
        lost_2_to_3 = sum(df[pixels_start_2, end_col, drop = TRUE] == 3, na.rm = TRUE)
        print(paste("Project", project_id, "lost 2 to 3 pixels:",
                    lost_2_to_3))
      }
      
      total_lost_pixels = lost_1_to_other + lost_4_to_3 + lost_2_to_3
      end_forest_pixels = start_forest_pixels - total_lost_pixels
      print(paste("Project", project_id, "end forest pixels:", end_forest_pixels))
      
      # proportions
      total_pixels = nrow(df)
      start_proportion = start_forest_pixels / total_pixels
      end_proportion = end_forest_pixels / total_pixels
      
      # period years
      period_years = project_end_year - start_year
      
      # deforestation rate (using proportions)
      if (start_proportion > 0 && end_proportion > 0 && period_years > 0) {
        acc_rate = (1 - (end_forest_pixels / start_forest_pixels)^(1 / (period_years + 1))) * 100
      } else {
        acc_rate = NA_real_
      }
      print(paste("Project", project_id, "ACC deforestation rate:", round(acc_rate, 4), "%/year"))
      
      # results
      deforestation_list[[basename(file)]] = tibble(
        project_no = project_id,
        acc_rate = acc_rate,
        start_year = start_year,
        end_year = project_end_year,
        start_proportion = start_proportion,
        end_proportion = end_proportion,
        period_years = period_years,
        start_forest_pixels = start_forest_pixels,
        end_forest_pixels = end_forest_pixels,
        lost_1_to_other = lost_1_to_other,
        lost_4_to_3 = lost_4_to_3,
        lost_2_to_3 = lost_2_to_3,
        total_lost_pixels = total_lost_pixels
      )
    }
  }
}

# unravel the list of tibbles into a single tibble
deforestation_rate_df = bind_rows(deforestation_list)

# make table of total lost pixels by cause
total_lost_summary = deforestation_rate_df %>%
  summarise(
    total_lost_1_to_other = sum(lost_1_to_other, na.rm = TRUE),
    proportion_lost_1_to_other = total_lost_1_to_other / sum(total_lost_pixels, na.rm = TRUE),
    total_lost_4_to_3 = sum(lost_4_to_3, na.rm = TRUE),
    proportion_lost_4_to_3 = total_lost_4_to_3 / sum(total_lost_pixels, na.rm = TRUE),
    total_lost_2_to_3 = sum(lost_2_to_3, na.rm = TRUE),
    proportion_lost_2_to_3 = total_lost_2_to_3 / sum(total_lost_pixels, na.rm = TRUE),
    total_lost_pixels = sum(total_lost_pixels, na.rm = TRUE)
  ) %>% as_tibble()
total_lost_summary
# ---- LOAD PROJECT GEOJSON FILES AND COMPUTE AREAS ----
geojson_df = list.files("geojsons/project_area_geojsons", full.names = TRUE) %>%
  map_dfr(~ {
    gdf = st_read(.x, quiet = TRUE) %>% st_make_valid()
    # work with hectares
    area_ha = sum(st_area(gdf), na.rm = TRUE) / 10000
    tibble(project_no = as.numeric(gsub(".geojson", "", basename(.x))),
           area_ha = as.numeric(area_ha))
  })

# ---- LOAD CERTIFIED RATES AND CALCULATE CERTIFIED RATES ----

# load the certified rates csv
cert_df = read.csv("csvs/certified_project_amounts.csv") %>%
  # add the hectarage of project areas
  left_join(geojson_df, by = "project_no") %>%
  # add the proportion of undisturbed
  left_join(undisturbed_df, by = "project_no") %>%
  # work out the amount of undisturbed
  mutate(area_undisturbed = area_ha * (undisturbed_percent / 100))

# update undisturbed area over time as deforestation progresses
cert_df = cert_df %>%
  group_by(project_no) %>%
  mutate(area_undisturbed = first(area_undisturbed),
         area_ha = first(area_ha)) %>%
  ungroup() %>%
  drop_na()

# compute the compound self-reported deforestation rates
cert_df = cert_df %>%
  group_by(project_no) %>%
  arrange(year) %>%
  summarise(total = sum(proj_def),
            start = first(area_undisturbed),
            area_ha = first(area_ha),
            undisturbed_percent = first(undisturbed_percent),.groups = "drop")
cert_df$end = cert_df$start - cert_df$total

# load eval periods csv
eval_periods = read.csv("csvs/evaluation_periods.csv")
cert_df = cert_df %>%
  left_join(eval_periods, by = "project_no")
cert_df$compound = (1 - (cert_df$end / cert_df$start)^(1 / cert_df$period)) * 100
cert_df = cert_df %>% rename(cert_rate = compound) 

# ---- SCATTER PLOT (FIGURE 3A) ----

# join the deforestation rates and compute the difference
comparison_df = cert_df %>%
  left_join(deforestation_rate_df, by = "project_no") %>%
  select(project_no, acc_rate, cert_rate)


s4a_plot = ggplot(comparison_df, aes(x = cert_rate, y = acc_rate)) +
  annotate("text", x = 0.7, y = 3.4, label = "ACC Rate is Higher",
           size = 6) +
  annotate("text", x = 2.6, y = 0.3, label = "ACC Rate is Lower",
           size = 6) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed",
              linewidth = 0.75) +
  geom_segment(aes(x = cert_rate, y = acc_rate,
                   xend = cert_rate, yend = cert_rate), 
               color = "lightgreen", alpha = 1, linewidth = 0.5) +
  scale_x_continuous(limits = c(0, 3.6),
                     breaks = seq(0, 3.6, by = 0.5), expand = c(0, 0)) +
  geom_point(size = 1.8, alpha = 1, colour = "lightgreen") +  
  scale_y_continuous(limits = c(0, 3.6),
                     breaks = seq(0, 3.6, by = 0.5), expand = c(0, 0)) +
  theme_classic() +
  theme(axis.title = element_text(size = 19),
        axis.text = element_text(size = 13)) +
  labs(x = "Certified Rate (%/year)",
       y = "ACC Rate (%/year)")

# ---- FIGURE 3B ----

plot_comparison_df = comparison_df %>% 
  pivot_longer(cols = -project_no,
               names_to = "variable",
               values_to = "value")

plot_comparison_df$variable = factor(plot_comparison_df$variable,
                                     levels = c("acc_rate", "cert_rate"))
s4b_plot = ggplot(plot_comparison_df, aes(x = variable, y = value, colour = variable)) +
  geom_point(position = position_jitter(width = 0.1), 
             alpha = 0.3, size = 4, shape = 16) +
  stat_summary(fun.data = function(y) {
    data.frame(
      y = median(y, na.rm = TRUE),
      ymin = quantile(y, 0.25, na.rm = TRUE),
      ymax = quantile(y, 0.75, na.rm = TRUE)
    )
  }, geom = "errorbar", width = 0.2, linewidth = 1,
  position = position_dodge(width = 0.5)) + 
  stat_summary(fun = median, geom = "crossbar", width = 0.2,
               linewidth = 1, position = position_dodge(width = 0.5)) +
  scale_x_discrete(labels = c("acc_rate" = "ACC Project\nQuasi-Experimental Methods\n(n = 36)",
                              "cert_rate" = "Certified Project\nCertified Methods\n(n = 36)")) +
  scale_color_manual(values = c("acc_rate" = "darkblue", "cert_rate" = "darkred")) +
  scale_y_continuous(labels = function(x) sprintf("%.1f", x)) +
  ylab("Deforestation Rate (%/year)") +
  theme_classic() +
  theme(axis.title  = element_text(size = 19),
        axis.title.x = element_blank(),
        axis.text.x  = element_text(size = 18, colour = "black"),
        axis.text.y  = element_text(size = 14),
        legend.position = "none")

p_label = paste("ACC Rate is Lower **")

# ---- COMBINE PLOTS INTO A PANEL ----
p_label = paste("ACC Rate is Higher **")

s4b_plot = s4b_plot + 
  geom_signif(
    comparisons = list(c("acc_rate", "cert_rate")),
    test = "wilcox.test",
    map_signif_level = TRUE,
    y_position = 3.2,
    textsize = 6, 
    colour = "black",
    annotations = p_label
  )

s4a_plot = s4a_plot + labs(tag = "a", size = 20)
s4b_plot = s4b_plot + labs(tag = "b", size = 20)
combined_plot = (s4a_plot + s4b_plot + plot_layout(ncol = 2)) &
  theme(plot.tag = element_text(size = 20))
combined_plot
# ---- SAVE FINAL PLOT ----

ggsave("pngs/s4_5_raw.png", combined_plot, bg = "white",
       width = 14, height = 7, units = "in", dpi = 500)

#---- EXTRACT STATS ----

median(comparison_df$cert_rate)
median(comparison_df$acc_rate)
shapiro.test(comparison_df$cert_rate)
shapiro.test(comparison_df$acc_rate)
wilcox.test(comparison_df$acc_rate, comparison_df$cert_rate, paired = TRUE)

# pairwise mean difference
median(comparison_df$acc_rate - comparison_df$cert_rate)


