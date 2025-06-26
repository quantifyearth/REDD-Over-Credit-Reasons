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
  percent_undisturbed = mean(df[[20]] == 1, na.rm = TRUE) * 100
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
    summarise(forest = sum(luc == 1, na.rm = TRUE), .groups = "drop")
  
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
deforestation_rate_df = bind_rows(deforestation_list) %>%
  write_csv("csvs/acc_project_area_rates.csv")

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

# compute the mean self-reported deforestation rates
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


fig3a_plot = ggplot(comparison_df, aes(x = cert_rate, y = acc_rate)) +
  annotate("text", x = 0.6, y = 3, label = "ACC Rate is Higher",
           size = 6) +
  annotate("text", x = 2.6, y = 0.1, label = "ACC Rate is Lower",
           size = 6) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed",
              linewidth = 0.75) +
  geom_segment(aes(x = cert_rate, y = acc_rate,
                   xend = cert_rate, yend = cert_rate), 
               color = "maroon4", alpha = 1, linewidth = 0.5) +
  scale_x_continuous(limits = c(0, 3.2),
                     breaks = seq(0, 3.2, by = 0.5), expand = c(0, 0)) +
  geom_point(size = 1.8, alpha = 1, colour = "maroon4") +  
  scale_y_continuous(limits = c(0, 3.2),
                     breaks = seq(0, 3.2, by = 0.5), expand = c(0, 0)) +
  theme_classic() +
  theme(axis.title = element_text(size = 19),
        axis.text = element_text(size = 13)) +
  labs(x = "Certified deforestation (%/year)",
       y = "ACC deforestation (%/year)")

# ---- FIGURE 3B ----

plot_comparison_df = comparison_df %>% 
  pivot_longer(cols = -project_no,
               names_to = "variable",
               values_to = "value")

plot_comparison_df$variable = factor(plot_comparison_df$variable,
                                     levels = c("acc_rate", "cert_rate"))
fig3b_plot = ggplot(plot_comparison_df, aes(x = variable, y = value, colour = variable)) +
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
  scale_x_discrete(labels = c("acc_rate" = "ACC Project (n = 36)",
                              "cert_rate" = "Certified Project (n = 36)")) +
  scale_color_manual(values = c("acc_rate" = "darkorchid4", "cert_rate" = "firebrick")) +
  scale_y_continuous(labels = function(x) sprintf("%.1f", x)) +
  ylab("Deforestation (%/year)") +
  theme_classic() +
  theme(axis.title  = element_text(size = 19),
        axis.title.x = element_blank(),
        axis.text.x  = element_text(size = 18, colour = "black"),
        axis.text.y  = element_text(size = 14),
        legend.position = "none")

p_value = wilcox.test(comparison_df$acc_rate, comparison_df$cert_rate, paired = TRUE)$p.value
p_label = paste("**")

# create the plot with significance annotation
fig3b_plot = fig3b_plot + 
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

fig3a_plot = fig3a_plot + labs(tag = "a", size = 20)
fig3b_plot = fig3b_plot + labs(tag = "b", size = 20)
combined_plot = (fig3a_plot + fig3b_plot + plot_layout(ncol = 2)) &
  theme(plot.tag = element_text(size = 20))
combined_plot
# ---- SAVE FINAL PLOT ----

ggsave("pngs/fig3_raw.png", combined_plot, bg = "white",
       width = 14, height = 7, units = "in", dpi = 500)

#---- EXTRACT STATS ----

median(comparison_df$cert_rate)
median(comparison_df$acc_rate)
shapiro.test(comparison_df$cert_rate)
shapiro.test(comparison_df$acc_rate)
wilcox.test(comparison_df$acc_rate, comparison_df$cert_rate, alternative = "greater", paired = TRUE)

# difference
median(comparison_df$acc_rate - comparison_df$cert_rate)
