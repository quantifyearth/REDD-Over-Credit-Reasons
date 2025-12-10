# load necessary libraries
library(tidyverse)

# read the CSV file
data = read.csv("csvs/west_avoided_amounts.csv")

# define relevant QEM and VM columns
qem_sources = c("TW_2020", "AG_2022", "AG_2024", "PACTv2", "TW_2023")
vm_columns = c("VM0006-FSF-1", "VM0006-FSF-2", "VM0007-MLP", "VM0007-SW", 
                "VM0009", "VM0015-MLP", "VM0015-SW", "official")

# process QEM data
qem_data = data %>%
  filter(cf_source %in% qem_sources) %>%
  group_by(ID) %>%
  summarise(
    mean = mean(avd_def_yr, na.rm = TRUE),
    sd = sd(avd_def_yr, na.rm = TRUE),
    n = n(),
    lower_bound = mean - 1.96 * (sd / sqrt(n)),
    upper_bound = mean + 1.96 * (sd / sqrt(n)),
    QEM_Estimates = sprintf("%.1f (CI = %.1f, %.1f)", mean, lower_bound, upper_bound)
  ) %>%
  select(ID, QEM_Estimates)

# process VM data and calculate t-scores and p-values
vm_data = data %>%
  filter(cf_source %in% vm_columns) %>%
  group_by(ID, cf_source) %>%
  summarise(avd_def_yr = mean(avd_def_yr, na.rm = TRUE), .groups = "drop") %>%
  left_join(
    data %>%
      filter(cf_source %in% qem_sources) %>%
      group_by(ID) %>%
      summarise(
        mean = mean(avd_def_yr, na.rm = TRUE),
        sd = sd(avd_def_yr, na.rm = TRUE),
        df = n() - 1
      ),
    by = "ID"
  ) %>%
  mutate(
    t_score = (avd_def_yr - mean) / (sd / sqrt(df)),
    p_value = 1 - pt(t_score, df = df),
    formatted = case_when(
      p_value < 0.001 ~ sprintf("%.1f *** (T = %.2f)", avd_def_yr, t_score),
      p_value < 0.01 ~ sprintf("%.1f ** (T = %.2f)", avd_def_yr, t_score),
      p_value < 0.05 ~ sprintf("%.1f * (T = %.2f)", avd_def_yr, t_score),
      TRUE ~ sprintf("%.1f (T = %.2f)", avd_def_yr, t_score)
    )
  ) %>%
  select(ID, cf_source, formatted) %>%
  pivot_wider(names_from = cf_source, values_from = formatted)

# combine qem and vm data
final_table = qem_data %>%
  left_join(vm_data, by = "ID")

# pivot for the final format
pivoted_table = final_table %>%
  pivot_longer(-ID, names_to = "Metric", values_to = "Value") %>%
  pivot_wider(names_from = ID, values_from = Value)

# view the pivoted table
print(pivoted_table)

