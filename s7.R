# load necessary libraries
library(tidyverse)
library(arrow)

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
    # find pixels that were forest (1, 2) at start
    forest_start_indices = which(df_tbl[[start_col]] %in% c(1, 2))
    all_pixels = nrow(df_tbl)
    forest_start_count = length(forest_start_indices)
    
    message("forest pixels at start (", start_year, "): ", forest_start_count)
    
    # count lost pixels
    lost_pixels = 0
    deg_lost_pixels = 0
    if (forest_start_count > 0) {
      message("checking ", forest_start_count, " forest pixels for loss...")
      for (idx in forest_start_indices) {
        start_class = df_tbl[[start_col]][idx]
        end_class = df_tbl[[end_col]][idx]
        
        if (start_class == 1 && end_class %in% c(2, 3, 4)) {
          lost_pixels = lost_pixels + 1
        } else if (start_class == 2 && end_class == 3) {
          # treat NA as loss
          lost_pixels = lost_pixels + 1
          deg_lost_pixels = deg_lost_pixels + 1
        }
      }
    }
    
    forest_end_pixels = forest_start_count - lost_pixels
    
    message("forest pixels at end (", end_year, "): ", forest_end_pixels)
    message("lost pixels: ", lost_pixels)
    message("degraded lost pixels (2 to 3): ", deg_lost_pixels)
    
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
      deg_lost_pixels = deg_lost_pixels,
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

pact_control_df = calculate_transition_rates("parquets/acc_pact_matching_parquets",
                                             "csvs/evaluation_end_years.csv",
                                             "pact_control")
mean(pact_control_df$total_pixels, na.rm = TRUE)
min(pact_control_df$total_pixels, na.rm = TRUE)
max(pact_control_df$total_pixels, na.rm = TRUE)
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
  scale_color_manual(values = c("qem_rate" = "darkgreen", "certified_rate" = "darkorange")) +

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
ggsave(filename = "pngs/s7_1_raw.png", plot = s6, dpi = 300, width = 8, height = 6)


# ---- EXTRACT STATS ----

# extract wilcoxon between certified and qem
wilcox.test(control_areas_df$certified_rate, control_areas_df$qem_rate, paired = TRUE)
wilcox.test(control_areas_df$certified_rate, control_areas_df$qem_rate, alternative = "greater", paired = TRUE)
# extract median and iqr for certified and qem
control_areas_df %>%
  summarise(median_certified = median(certified_rate, na.rm = TRUE),
            median_qem = median(qem_rate, na.rm = TRUE),
            ratio = median_certified / median_qem)

            
# ---- DO AGAIN BUT REFORESTED INSTEAD ----
# clear all vars in environment
rm(list=ls())

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
    # find pixels that were forest (1, 2) at start
    forest_start_indices = which(df_tbl[[start_col]] %in% c(1, 4))
    forest_start_count = length(forest_start_indices)
    
    message("forest pixels at start (", start_year, "): ", forest_start_count)
    
    # count lost pixels
    lost_pixels = 0
    ref_lost_pixels = 0
    if (forest_start_count > 0) {
      message("checking ", forest_start_count, " forest pixels for loss...")
      for (idx in forest_start_indices) {
        start_class = df_tbl[[start_col]][idx]
        end_class = df_tbl[[end_col]][idx]
        
        if (start_class == 1 && end_class %in% c(2, 3, 4)) {
          lost_pixels = lost_pixels + 1
        } else if (start_class == 4 && end_class == 3) {
          # treat NA as loss
          lost_pixels = lost_pixels + 1
          ref_lost_pixels = ref_lost_pixels + 1
        }
      }
    }
    
    forest_end_pixels = forest_start_count - lost_pixels
    
    message("forest pixels at end (", end_year, "): ", forest_end_pixels)
    message("lost pixels: ", lost_pixels)
    message("regrowth lost pixels (2 to 3): ", ref_lost_pixels)
    
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
      ref_lost_pixels = ref_lost_pixels,
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

pact_control_df = calculate_transition_rates("parquets/acc_pact_matching_parquets",
                                             "csvs/evaluation_end_years.csv",
                                             "pact_control")

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
  scale_color_manual(values = c("qem_rate" = "burlywood4", "certified_rate" = "cadetblue4")) +
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
ggsave(filename = "pngs/s7_2_raw.png", plot = s6, dpi = 300, width = 8, height = 6)


# ---- EXTRACT STATS ----

# extract wilcoxon between certified and qem
wilcox.test(control_areas_df$certified_rate, control_areas_df$qem_rate, paired = TRUE)
wilcox.test(control_areas_df$certified_rate, control_areas_df$qem_rate, alternative = "greater", paired = TRUE)
# extract median and iqr for certified and qem
control_areas_df %>%
  summarise(median_certified = median(certified_rate, na.rm = TRUE),
            median_qem = median(qem_rate, na.rm = TRUE),
            ratio = median_certified / median_qem)





# ---- DO AGAIN BUT REFORESTED AND DEGRADED INSTEAD ----
# clear all vars in environment
rm(list=ls())

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
    # find pixels that were forest (1, 2) at start
    forest_start_indices = which(df_tbl[[start_col]] %in% c(1, 2, 4))
    forest_start_count = length(forest_start_indices)
    
    message("forest pixels at start (", start_year, "): ", forest_start_count)
    
    # count lost pixels
    lost_pixels = 0
    ref_lost_pixels = 0
    deg_lost_pixels = 0
    if (forest_start_count > 0) {
      message("checking ", forest_start_count, " forest pixels for loss...")
      for (idx in forest_start_indices) {
        start_class = df_tbl[[start_col]][idx]
        end_class = df_tbl[[end_col]][idx]
        
        if (start_class == 1 && end_class %in% c(2, 3, 4)) {
          lost_pixels = lost_pixels + 1
        } else if (start_class == 4 && end_class == 3) {
          # treat NA as loss
          lost_pixels = lost_pixels + 1
          ref_lost_pixels = ref_lost_pixels + 1
        } else if (start_class == 2 && end_class == 3) {
          # treat NA as loss
          lost_pixels = lost_pixels + 1
          deg_lost_pixels = deg_lost_pixels + 1
        }
      }
    }
    
    forest_end_pixels = forest_start_count - lost_pixels
    
    message("forest pixels at end (", end_year, "): ", forest_end_pixels)
    message("lost pixels: ", lost_pixels)
    message("regrowth lost pixels (2 to 3): ", ref_lost_pixels)
    message("degraded lost pixels (2 to 3): ", deg_lost_pixels)
    
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
      ref_lost_pixels = ref_lost_pixels,
      deg_lost_pixels = deg_lost_pixels,
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

pact_control_df = calculate_transition_rates("parquets/acc_pact_matching_parquets",
                                             "csvs/evaluation_end_years.csv",
                                             "pact_control")

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
  scale_color_manual(values = c("qem_rate" = "darkblue", "certified_rate" = "darkred")) +
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
ggsave(filename = "pngs/s7_3_raw.png", plot = s6, dpi = 300, width = 8, height = 6)


# ---- EXTRACT STATS ----

# extract wilcoxon between certified and qem
wilcox.test(control_areas_df$certified_rate, control_areas_df$qem_rate, paired = TRUE)
wilcox.test(control_areas_df$certified_rate, control_areas_df$qem_rate, alternative = "greater", paired = TRUE)
# extract median and iqr for certified and qem
control_areas_df %>%
  summarise(median_certified = median(certified_rate, na.rm = TRUE),
            median_qem = median(qem_rate, na.rm = TRUE),
            ratio = median_certified / median_qem)


