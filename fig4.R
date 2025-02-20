# load necessary libraries
library(tidyverse)
library(arrow)
library(sf)
library(rnaturalearth)
library(patchwork)

# ---- LOAD AND PROCESS PACT PARQUET FILES ----
pact_matching_paths = list.files("parquets/acc_pact_matching_parquets", full.names = TRUE)

pact_control_df_list = map(pact_matching_paths, ~ {
  read_parquet(.x) %>%
    select(starts_with("s_")) %>%
    rename_with(~ str_remove(., "^s_"))
})
names(pact_control_df_list) = pact_matching_paths %>%
  basename() %>%
  str_remove("\\.parquet$")

# extract project 958 for example
pact_control_df = pact_control_df_list[["958"]]

# ---- LOAD CERTIFIED CONTROL AREA PARQUET FILES ----
certified_control_paths = list.files("parquets/acc_certified_control_parquets", full.names = TRUE)

certified_control_df_list = map(certified_control_paths, ~ {
  read_parquet(.x)
})
names(certified_control_df_list) = certified_control_paths %>%
  basename() %>%
  str_remove("_k\\.parquet$")

# extract project 958 for example
certified_control_df = certified_control_df_list[["958"]]

# ---- LOAD PROJECT AREA PARQUET FILES ----
project_area_df_list = map(pact_matching_paths, ~ {
  read_parquet(.x) %>%
    select(starts_with("k_")) %>%
    rename_with(~ str_remove(., "^k_")) %>%
    # unique lat lng col combo
    distinct()
    
})
names(project_area_df_list) = pact_matching_paths %>%
  basename() %>%
  str_remove("\\.parquet$")

# extract project 958 for example
project_area_df = project_area_df_list[["958"]]

# ---- LOAD CERTIFIED CONTROL GEOJSON FILES ----
certified_control_geojson_paths = list.files("geojsons/certified_control_area_geojsons", full.names = TRUE)

certified_control_sf_list = map(certified_control_geojson_paths, ~ {
  st_read(.x) %>% st_make_valid()
})
names(certified_control_sf_list) = certified_control_geojson_paths %>%
  basename() %>%
  str_remove("_reference\\.geojson$")

# extract project 958 for example
certified_control_sf = certified_control_sf_list[["958"]]

# ---- LOAD PROJECT AREA GEOJSON FILES ----
project_area_geojson_paths = list.files("geojsons/project_area_geojsons", full.names = TRUE)

project_area_sf_list = map(project_area_geojson_paths, ~ {
  st_read(.x) %>% st_make_valid()
})
names(project_area_sf_list) = project_area_geojson_paths %>%
  basename() %>%
  str_remove("\\.geojson$")

# extract project 958 for example
project_area_sf = project_area_sf_list[["958"]]


# set colours
cert_colour = "#f71735"
pact_colour = "#41ead4"
project_colour = "#011627"

# ---- LOAD MAP OF PERU ----
peru_sf = ne_countries(country = "Peru", scale = "medium", returnclass = "sf")

# ---- PLOT MAP WITH PERU ----
map_peru_plot = ggplot(data = pact_control_df, aes(x = lng, y = lat)) +
  geom_sf(data = peru_sf, inherit.aes = FALSE, fill = "white", colour = "black", linewidth = 1) +
  geom_point(alpha = 0.3, size = 0.3, colour = pact_colour) +
  geom_sf(data = certified_control_sf, inherit.aes = FALSE, alpha = 0.7, colour = cert_colour, fill = cert_colour) +
  # add box around certified control area
  geom_sf(data = st_as_sfc(st_bbox(certified_control_sf)), inherit.aes = FALSE,
          fill = NA, colour = "black", size = 2) +
  geom_sf(data = project_area_sf, inherit.aes = FALSE, alpha = 0.7, colour = project_colour, fill = project_colour) +
  labs(tag = "a") +
  #white background
  theme_void() +
  theme(legend.title = element_blank(),
        plot.tag = element_text(size = 35),
        legend.position = "none",
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "white", fill = NA, linewidth = 2))

  # ---- PLOT MAP ZOOMED IN ----
map_zoomed_plot = ggplot(data = pact_control_df, aes(x = lng, y = lat)) +
  geom_point(alpha = 0.3, size = 0.8, colour = pact_colour) +
  geom_sf(data = certified_control_sf, inherit.aes = FALSE, alpha = 0.7, colour = cert_colour, fill = cert_colour) +
  geom_sf(data = project_area_sf, inherit.aes = FALSE, alpha = 0.7, colour = project_colour, fill = project_colour) +
  coord_sf(xlim = c(st_bbox(certified_control_sf)$xmin, st_bbox(certified_control_sf)$xmax),
           ylim = c(st_bbox(certified_control_sf)$ymin, st_bbox(certified_control_sf)$ymax)) +
  theme_void() +
  theme(legend.title = element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 2))

# ---- PLOT DENSITY PLOTS ----
cols_list = c("elevation", "slope", "access",
              "cpc0_u", "cpc5_u", "cpc10_u",
              "cpc0_d", "cpc5_d", "cpc10_d")
new_names_list = c("Elevation", "Slope", "Inaccessibility",
                   "Forest~cover~t[0]", "Forest~cover~t[-5]", "Forest~cover~t[-10]",
                   "Deforestation~t[0]", "Deforestation~t[-5]", "Deforestation~t[-10]")

certified_control_df = certified_control_df %>% select(all_of(cols_list))
pact_control_df = pact_control_df %>% select(all_of(cols_list))
project_area_df = project_area_df %>% select(all_of(cols_list))

certified_long_df = certified_control_df %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  mutate(type = "Certified")
pact_long_df = pact_control_df %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  mutate(type = "PACT")
project_long_df = project_area_df %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  mutate(type = "Project Area")

density_plot_df = bind_rows(certified_long_df, pact_long_df, project_long_df)
density_plot_df$variable = factor(density_plot_df$variable,
                                  levels = cols_list,
                                  labels = new_names_list)

density_plot = ggplot(data = density_plot_df,
                      aes(x = value, colour = type, linetype = type)) +
  geom_density(adjust = 8, linewidth = 1.5) +
  facet_wrap(~ variable, scales = "free", nrow = 3, labeller = label_parsed) +
  ylab("Density\n") +
  scale_colour_manual(values = c(cert_colour, project_colour, pact_colour),
                      labels = c("Certified", "PACT", "Project"),
                      guide = "none") +
  scale_x_continuous(n.breaks = 3, guide = guide_axis(check.overlap = TRUE)) +
  scale_linetype_manual(values = c("solid", "solid", "solid")) +
  labs(tag = "b") +
  theme_classic() +
  theme(text = element_text(size = 16),
        axis.line = element_line(linewidth = 1),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 24),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.tag = element_text(size = 35),
        legend.position = "none",
        strip.text = element_text(size = 14, face = "bold"),
        strip.background = element_rect(fill = "white", color = "black", linewidth = 2))

# ---- COMPUTE STANDARDISED MEAN DIFFERENCES (SMD) ----
expected_vars = c("elevation", "slope", "access",
                  "cpc0_u", "cpc5_u", "cpc10_u",
                  "cpc0_d", "cpc5_d", "cpc10_d")

fill_missing = function(df, vars) {
  missing_vars = setdiff(vars, names(df))
  if (length(missing_vars) > 0) {
    df[missing_vars] = NA_real_
  }
  df %>% select(all_of(vars))
}

compute_smd = function(df1, df2, project_no) {
  df1 = fill_missing(df1, expected_vars) %>% mutate(across(everything(), as.numeric))
  df2 = fill_missing(df2, expected_vars) %>% mutate(across(everything(), as.numeric))
  
  mean1 = colMeans(df1, na.rm = TRUE)
  mean2 = colMeans(df2, na.rm = TRUE)
  sd1   = apply(df1, 2, sd, na.rm = TRUE)
  sd2   = apply(df2, 2, sd, na.rm = TRUE)
  
  pooled_sd = sqrt(((nrow(df1) - 1) * sd1^2 + (nrow(df2) - 1) * sd2^2) /
                     (nrow(df1) + nrow(df2) - 2))
  
  smd_values = (mean1 - mean2) / pooled_sd
  
  tibble(
    project_no = project_no,
    variable = names(smd_values),
    smd = smd_values
  )
}

# ---- COMPUTE SMDs FOR EACH PROJECT ----
smd_results_list = list()

for (project in names(project_area_df_list)) {
  if (!(project %in% names(certified_control_df_list)) ||
      !(project %in% names(pact_control_df_list))) next
  
  certified_df = certified_control_df_list[[project]] %>% select(any_of(expected_vars))
  pact_df = pact_control_df_list[[project]] %>% select(any_of(expected_vars))
  project_df = project_area_df_list[[project]] %>% select(any_of(expected_vars))
  
  smd_cert_df = compute_smd(certified_df, project_df, project) %>% mutate(type = "Certified")
  smd_quasi_df = compute_smd(pact_df, project_df, project) %>% mutate(type = "Quasi Experimental")
  
  smd_results_list[[project]] = bind_rows(smd_cert_df, smd_quasi_df)
}

smd_df = bind_rows(smd_results_list)

smd_table = smd_df %>%
  group_by(variable, type) %>%
  summarise(
    median = median(smd, na.rm = TRUE),
    iqr = IQR(smd, na.rm = TRUE),
    n = n(),
    t_stat = ifelse(n() > 1, t.test(smd, mu = 0)$statistic, NA),
    p_value = format(ifelse(n() > 1, t.test(smd, mu = 0)$p.value, NA), scientific = FALSE),
  ) %>%
  ungroup()

# ---- RECODE VARIABLE NAMES FOR SMD PLOT ----
var_order = c("cpc10_d", "cpc5_d", "cpc0_d",
              "cpc10_u", "cpc5_u", "cpc0_u",
              "access", "slope", "elevation")
var_labels = c(
  "cpc10_d" = "Deforestation (t-10, %)",
  "cpc5_d"  = "Deforestation (t-5, %)",
  "cpc0_d"  = "Deforestation (t0, %)",
  "cpc10_u" = "Forest Cover (t-10, %)",
  "cpc5_u"  = "Forest Cover (t-5, %)",
  "cpc0_u"  = "Forest Cover (t0, %)",
  "access"  = "Inaccessibility (mins)",
  "slope"   = "Slope (°)",
  "elevation" = "Elevation (m)"
)
smd_df = smd_df %>% mutate(variable = factor(variable, levels = var_order))

# ---- PLOT SMDs ----
control_cols = c("Certified" = cert_colour, "Quasi Experimental" = pact_colour)
smd_plot = ggplot(smd_df, aes(x = smd, y = variable, colour = type)) +
  annotate(geom = "polygon", x = c(-0.25, 0.25, 0.25, -0.25), y = c(0, 0, 9.5, 9.5), fill = "grey", alpha = 0.5) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(alpha = 0.3, position = position_jitterdodge(dodge.width = 0.75, jitter.width = 0.3)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = project_colour) +
  scale_colour_manual(values = control_cols) +
  scale_y_discrete(labels = var_labels) +
  labs(x = "Standardised Mean Difference", y = NULL, tag = "c") +
  xlim(-2, 2) +
  theme_classic() +
  theme(axis.text.y = element_text(size = 15, angle = 25),
        axis.title.x = element_text(size = 24),
        plot.tag = element_text(size = 35),
        axis.line = element_line(linewidth = 1),
        legend.position = "none")

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
                    rate = (1 - (last(Undisturbed) / first(Undisturbed))^(1 / n())) * 100,
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
                  rate = (1 - (last(Undisturbed) / first(Undisturbed))^(1 / n())) * 100,
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
  # save as csv
  write_csv("csvs/acc_certified_control_rates.csv") %>%
  rename(certified_rate = rate)

pact_df = deforestation_rate("parquets/acc_pact_matching_parquets",
                             "csvs/evaluation_end_years.csv",
                             TRUE) %>%
  # save as csv
  write_csv("csvs/acc_pact_control_rates.csv") %>%
  rename(qem_rate = rate)

pact_project_df = deforestation_rate("parquets/acc_pact_matching_parquets",
                                     "csvs/evaluation_end_years.csv",
                                     TRUE,
                                     TRUE) %>%
  # save as csv
  write_csv("csvs/acc_pact_project_rates.csv")

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
def_plot = ggplot(plot_control_areas_df, aes(x = variable, y = value, colour = variable)) +
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
  scale_color_manual(values = c("qem_rate" = pact_colour, "certified_rate" = cert_colour)) +
  
  scale_y_continuous(labels = function(x) sprintf("%.1f", x)) +
  
  ylab("Deforestation Rate (%/year)") +
  theme_classic() +
  labs(tag = "d") +
  theme(axis.title.x  = element_blank(),
        plot.tag = element_text(size = 35),
        axis.title.y  = element_text(size = 24),
        axis.text.x   = element_text(size = 20, colour = "black"),
        axis.text.y   = element_text(size = 18),
        axis.line = element_line(linewidth = 1),
        legend.position = "none")

# ---- FACET GRID PLOT ----

# facet the plots above using multiple plot
mp_plot = map_peru_plot + density_plot + smd_plot + def_plot + plot_layout(ncol = 2,
                                                                      nrow = 2)
ggsave("pngs/fig4_raw.png", plot = mp_plot, dpi = 300, width = 16, height = 15)
# ---- SAVE ALL PLOTS ----
ggsave(filename = "pngs/map_peru_plot.png", plot = map_peru_plot, dpi = 300, width = 8, height = 6)
ggsave(filename = "pngs/map_zoomed_plot.png", plot = map_zoomed_plot, dpi = 300, width = 8, height = 8)
ggsave(filename = "pngs/density_plot.png", plot = density_plot, dpi = 300, width = 10, height = 8)
ggsave(filename = "pngs/smd_plot.png", plot = smd_plot, dpi = 300, width = 8, height = 6)
ggsave(filename = "pngs/def_plot.png", plot = def_plot, dpi = 300, width = 8, height = 6)


# ---- EXTRACT STATS ----

# extract wilcoxon between certified and qem
wilcox.test(control_areas_df$certified_rate, control_areas_df$qem_rate, paired = TRUE)
wilcox.test(control_areas_df$certified_rate, control_areas_df$qem_rate, alternative = "greater", paired = TRUE)

# extract median and iqr for certified and qem
control_areas_df %>%
  summarise(median_certified = median(certified_rate, na.rm = TRUE),
            median_qem = median(qem_rate, na.rm = TRUE),
            ratio = median_certified / median_qem)

            