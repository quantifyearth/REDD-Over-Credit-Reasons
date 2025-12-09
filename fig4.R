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
pact_control_df = pact_control_df_list[["958"]]#

# determine the proportion of rows with 1 in luc_2010
pact_control_df %>%
  summarise(proportion_undisturbed_2010 = mean(luc_2010 == 1, na.rm = TRUE))

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

# determine the proportion of rows with 1 in luc_2001
certified_control_df %>%
  summarise(proportion_undisturbed_2001 = mean(luc_2001 == 1, na.rm = TRUE))
  

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
calculate_transition_rates = function(parquet_folder,
                                      end_years_path = '',
                                      mode = "all") {
  # validate mode parameter
  valid_modes = c("all", "pact_project", "pact_control", "certified")
  if (!mode %in% valid_modes) {
    stop("mode must be one of: 'all', 'pact_project', 'pact_control', 'certified'")
  }
  
  message("starting transition rate calculation...")
  message("mode: ", mode)
  message("parquet folder: ", parquet_folder)
  
  parquet_file_paths = list.files(parquet_folder, full.names = TRUE)
  
  if (length(parquet_file_paths) == 0) {
    warning("no files found in folder: ", parquet_folder)
    return(NULL)
  }
  
  message("found ", length(parquet_file_paths), " files in folder")
  
  df_list = lapply(parquet_file_paths, function(file_path) {
    tryCatch({
      message("reading file: ", basename(file_path))
      arrow::read_parquet(file_path) %>% as_tibble()
    }, error = function(e) {
      message("error reading: ", basename(file_path), " - ", e)
      NULL
    })
  })
  
  # name list elements by the file basename
  names(df_list) = basename(parquet_file_paths)
  # remove any files that could not be read
  df_list = df_list[!sapply(df_list, is.null)]
  
  if (length(df_list) == 0) {
    warning("no files could be read successfully")
    return(NULL)
  }
  
  message("successfully read ", length(df_list), " files")
  
  # function to calculate transition rates for a single project
  calculate_project_transitions = function(df_tbl, file_name, mode,
                                           end_years_df = NULL) {
    # extract project number
    proj_no = str_extract(file_name, "\\d+")
    
    message("\nprocessing project: ", proj_no, " (file: ", file_name, ")")
    
    # determine how to process based on mode
    original_cols = names(df_tbl)
    message("original columns: ", paste(head(original_cols), collapse = ", "), 
            ifelse(length(original_cols) > 6, " ...", ""))
    
    if (mode == "pact_project") {
      message("processing as pact project area...")
      # check if this file has k_ columns
      if (!any(grepl("^k_", names(df_tbl)))) {
        message("file does not have k_ columns, skipping (might be a control file)")
        return(NULL)
      }
      
      # for pact project areas, use k_ columns
      df_tbl = df_tbl %>%
        select(starts_with("k_")) %>%
        rename_with(~ str_remove(., "k_"))
      
      message("after selecting k_ columns: ", ncol(df_tbl), " columns")
      
      # select luc columns and remove first 9 if they exist
      df_tbl = df_tbl %>% 
        select(starts_with("luc"))
      
      message("after selecting luc columns: ", ncol(df_tbl), " columns")
      
      if(ncol(df_tbl) > 9){
        message("removing first 9 columns (dates)...")
        df_tbl = df_tbl %>% select(-c(1:9))
        message("after removing date columns: ", ncol(df_tbl), " columns")
      }
      is_pact = TRUE
      
    } else if (mode == "pact_control") {
      message("processing as pact control area...")
      # check if this file has s_ columns
      if (!any(grepl("^s_", names(df_tbl)))) {
        message("file does not have s_ columns, skipping (might be a project file)")
        return(NULL)
      }
      
      # for pact control areas, use s_ columns
      df_tbl = df_tbl %>%
        select(starts_with("s_")) %>%
        rename_with(~ str_remove(., "s_"))
      
      message("after selecting s_ columns: ", ncol(df_tbl), " columns")
      
      # select luc columns and remove first 9 if they exist
      df_tbl = df_tbl %>% 
        select(starts_with("luc"))
      
      message("after selecting luc columns: ", ncol(df_tbl), " columns")
      
      if(ncol(df_tbl) > 9){
        message("removing first 9 columns (dates)...")
        df_tbl = df_tbl %>% select(-c(1:9))
        message("after removing date columns: ", ncol(df_tbl), " columns")
      }
      is_pact = TRUE
      
    } else if (mode == "certified") {
      message("processing as certified control...")
      # check if this is actually a certified file (should not have k_ or s_ columns)
      if (any(grepl("^k_", names(df_tbl))) || any(grepl("^s_", names(df_tbl)))) {
        message("file has k_ or s_ columns, skipping (might be a pact file)")
        return(NULL)
      }
      
      # for certified control, just select luc columns
      df_tbl = df_tbl %>% select(starts_with("luc"))
      message("after selecting luc columns: ", ncol(df_tbl), " columns")
      is_pact = FALSE
      
    } else {
      # default mode - auto-detect based on column names
      message("auto-detecting file type based on columns...")
      if (any(grepl("^k_", names(df_tbl)))) {
        message("detected as pact project (has k_ columns)")
        df_tbl = df_tbl %>%
          select(starts_with("k_")) %>%
          rename_with(~ str_remove(., "k_"))
        df_tbl = df_tbl %>% 
          select(starts_with("luc"))
        if(ncol(df_tbl) > 9){
          df_tbl = df_tbl %>% select(-c(1:9))
        }
        is_pact = TRUE
        mode_label = "pact_project"
        
      } else if (any(grepl("^s_", names(df_tbl)))) {
        message("detected as pact control (has s_ columns)")
        df_tbl = df_tbl %>%
          select(starts_with("s_")) %>%
          rename_with(~ str_remove(., "s_"))
        df_tbl = df_tbl %>% 
          select(starts_with("luc"))
        if(ncol(df_tbl) > 9){
          df_tbl = df_tbl %>% select(-c(1:9))
        }
        is_pact = TRUE
        mode_label = "pact_control"
        
      } else {
        message("detected as certified control (no k_ or s_ columns)")
        df_tbl = df_tbl %>% select(starts_with("luc"))
        is_pact = FALSE
        mode_label = "certified"
      }
    }
    
    # determine start and end years
    # all year columns
    year_cols = names(df_tbl)
    years = as.numeric(str_extract(year_cols, "\\d+"))
    
    message("years found: ", paste(sort(years), collapse = ", "))
    
    if (is_pact && !is.null(end_years_df)) {
      message("checking for end year in pact projects...")
      if (proj_no %in% as.character(end_years_df$project_no)) {
        end_year = end_years_df %>%
          filter(project_no == proj_no) %>%
          pull(end_year) %>%
          as.numeric()
        message("using specified end year: ", end_year)
        # filter to include only years <= end_year
        keep_cols = year_cols[years <= end_year]
        df_tbl = df_tbl %>% select(all_of(keep_cols))
        years = years[years <= end_year]
        message("filtered years: ", paste(sort(years), collapse = ", "))
      } else {
        message("project ", proj_no, " not found in end_years file")
        return(NULL)
      }
    }
    
    if (length(years) < 2) {
      message("error: need at least 2 years of data, found: ", length(years))
      return(NULL)
    }
    
    start_year = min(years)
    end_year = max(years)
    n_years = end_year - start_year + 1
    
    message("analysis period: ", start_year, " to ", end_year, " (", n_years, " years)")
    
    # get start and end column names
    start_col = paste0("luc_", start_year)
    end_col = paste0("luc_", end_year)
    
    if (!(start_col %in% names(df_tbl)) || !(end_col %in% names(df_tbl))) {
      message("error: start or end column not found: ", start_col, " / ", end_col)
      return(NULL)
    }
    
    # for all files, use simpler approach
    # find pixels that were forest (1) at start
    forest_start_indices = which(df_tbl[[start_col]] %in% c(1))
    forest_start_count = length(forest_start_indices)
    
    message("forest pixels at start (", start_year, "): ", forest_start_count)
    
    # count lost pixels
    lost_pixels = 0
    
    if (forest_start_count > 0) {
      message("checking ", forest_start_count, " forest pixels for loss...")
      for (idx in forest_start_indices) {
        start_class = df_tbl[[start_col]][idx]
        end_class = df_tbl[[end_col]][idx]
        
        if (start_class == 1 && end_class %in% c(2, 3, 4)) {
          lost_pixels = lost_pixels + 1
        }
      }
    }
    
    forest_end_pixels = forest_start_count - lost_pixels
    
    message("forest pixels at end (", end_year, "): ", forest_end_pixels)
    message("lost pixels: ", lost_pixels)
    
    # calculate rate
    if (forest_start_count > 0 && forest_end_pixels > 0 && n_years > 0) {
      acc_rate = (1 - (forest_end_pixels / forest_start_count)^(1 / n_years)) * 100
      message("calculated annual change rate: ", round(acc_rate, 3), "%")
    } else if (forest_start_count > 0 && forest_end_pixels == 0) {
      acc_rate = 100
      message("all forest lost: rate = 100%")
    } else {
      acc_rate = NA_real_
      message("could not calculate rate (insufficient data)")
    }
    
    # add mode to result for identification
    if (mode == "all") {
      # use the mode_label determined during auto-detection
      mode_to_use = mode_label
    } else {
      mode_to_use = mode
    }
    
    result = tibble(
      project = as.numeric(proj_no),
      start_year = start_year,
      end_year = end_year,
      period_years = n_years,
      total_pixels = nrow(df_tbl),
      forest_start_pixels = forest_start_count,
      forest_end_pixels = forest_end_pixels,
      lost_pixels = lost_pixels,
      acc_deforestation_rate = acc_rate,
      mode = mode_to_use
    )
    
    message("result for project ", proj_no, ":")
    message("  - rate: ", round(acc_rate, 3), "%")
    message("  - period: ", start_year, "-", end_year)
    message("  - start forest: ", forest_start_count)
    message("  - end forest: ", forest_end_pixels)
    
    return(result)
  }
  
  # read end years if needed for pact files
  # for pact modes, read end years file
  if (mode %in% c("all", "pact_project", "pact_control")) {
    if (file.exists(end_years_path) && end_years_path != "") {
      message("reading end years file: ", end_years_path)
      end_years_df = read.csv(end_years_path, stringsAsFactors = FALSE)
      message("loaded ", nrow(end_years_df), " project end years")
    } else {
      end_years_df = NULL
      message("no end years file provided for pact analysis")
    }
  } else {
    end_years_df = NULL
  }
  
  # apply function to all projects
  message("\nprocessing ", length(df_list), " projects...")
  transitions_list = lapply(names(df_list), function(file_name) {
    message("\n--- processing file: ", file_name, " ---")
    result = calculate_project_transitions(
      df_list[[file_name]],
      file_name,
      mode = mode,
      end_years_df = end_years_df
    )
    if (!is.null(result)) {
      message("✓ completed successfully")
    } else {
      message("✗ failed to process")
    }
    result
  })
  
  # remove null results
  valid_results = sum(!sapply(transitions_list, is.null))
  transitions_list = transitions_list[!sapply(transitions_list, is.null)]
  
  message("\nsuccessfully processed ", valid_results, " out of ", length(df_list), " files")
  
  if (length(transitions_list) == 0) {
    warning("no projects could be processed successfully")
    return(NULL)
  }
  
  # combine all results
  combined_transitions_tbl = bind_rows(transitions_list)
  
  message("\nfinal summary:")
  message("total projects processed: ", nrow(combined_transitions_tbl))
  message("average rate: ", round(mean(combined_transitions_tbl$acc_deforestation_rate, na.rm = TRUE), 3), "%")
  message("range of rates: ", round(min(combined_transitions_tbl$acc_deforestation_rate, na.rm = TRUE), 3), "% to ", 
          round(max(combined_transitions_tbl$acc_deforestation_rate, na.rm = TRUE), 3), "%")
  
  return(combined_transitions_tbl)
}

# ---- JOIN CERTIFIED AND ACC DEFORESTATION RATES ----

certified_df = calculate_transition_rates("parquets/acc_certified_control_parquets",
                                          "",
                                          "certified") 

certified_df %>%
  select(project, acc_deforestation_rate) %>%
  rename(rate = acc_deforestation_rate) %>%
  write.csv("csvs/acc_certified_control_rates.csv")

pact_control_df = calculate_transition_rates("parquets/acc_pact_matching_parquets",
                             "csvs/evaluation_end_years.csv",
                             "pact_control")

pact_control_df %>%
  select(project, acc_deforestation_rate) %>%
  rename(rate = acc_deforestation_rate) %>%
  write.csv("csvs/acc_pact_control_rates.csv")

pact_project_df = calculate_transition_rates("parquets/acc_pact_matching_parquets",
                             "csvs/evaluation_end_years.csv",
                             "pact_project")
pact_project_df %>%
  select(project, acc_deforestation_rate) %>%
  rename(rate = acc_deforestation_rate) %>%
  write.csv("csvs/acc_pact_project_rates.csv")

control_areas_df = certified_df %>%
  select(project, acc_deforestation_rate) %>%
  rename(certified_rate = acc_deforestation_rate)

control_areas_df = control_areas_df %>%
  left_join(pact_control_df %>% select(project, acc_deforestation_rate) %>%
              rename(qem_rate = acc_deforestation_rate), by = "project")
             
control_areas_df = control_areas_df %>%
  rename(project_no = project)

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

            