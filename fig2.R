# load packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(cowplot)
library(magrittr)
library(RColorBrewer)
library(weights)
library(GGally)
library(patchwork)

# set theme to classic and define colour palette
theme_set(theme_classic())
my_colors = colorRampPalette(brewer.pal(9, "BuGn"))(10)[c(4,6,8,10)]

# statistical assessments
tw_2020_df = read.csv(file.path("csvs", "tw_2020_avoided_amounts.csv"))
tw_2023_df = read.csv(file.path("csvs", "tw_2023_avoided_amounts.csv"))
tw_2024_df = read.csv(file.path("csvs", "tw_2024_avoided_amounts.csv"))
ag_2022_df = read.csv(file.path("csvs", "ag_2022_avoided_amounts.csv"))
ag_2024_df = read.csv(file.path("csvs", "ag_2024_avoided_amounts.csv")) %>%
  select(ID = vcs_id, Avd_def = DD_ha) %>%
  mutate(ID = as.numeric(str_replace(ID, "PL", ""))) %>%
  mutate(Avd_def = -Avd_def)  # reverse sign for avoided deforestation

# pact assessments
pact_v2_df = read.csv(file.path("csvs", "pact_2025_avoided_amounts.csv")) %>%
  filter(id != "1325") %>%
  rename(
    Start = start_year,
    End = end_year,
    ID = id
  )

# certified assessments
vcs_df = as.data.frame(read.csv(file.path("csvs", "certified_avoided_amounts.csv")))

# align certified assessment data format with statistical data
vcs_df$total.avoided.def[which(vcs_df$total.avoided.def == "#VALUE!" | vcs_df$total.avoided.def == "")] = NA
vcs_df$total.additionality[which(vcs_df$total.additionality == "#VALUE!" | vcs_df$total.additionality == "")] = NA
vcs_df$raw.additionality[which(vcs_df$raw.additionality == "#VALUE!" | vcs_df$raw.additionality == "")] = NA

vcs_df$total.avoided.def = as.numeric(vcs_df$total.avoided.def)
vcs_df$total.additionality = as.numeric(vcs_df$total.additionality)
vcs_df$raw.additionality = as.numeric(vcs_df$raw.additionality)
vcs_df$def.emissions.ONLY = as.numeric(vcs_df$def.emissions.ONLY)
vcs_df$Start = as.numeric(vcs_df$Start)
vcs_df$End = as.numeric(vcs_df$End)

# select and rename columns for certified assessments
vcs_df = vcs_df %>%
  select(c("ID", "Start", "End", "total.avoided.def")) %>%
  rename(Avd_def = total.avoided.def) %>%
  mutate(source = "VERRA", 
         label = "VERRA")

# align statistical assessments for tw_2020
tw_2020_df = tw_2020_df %>% 
  select(c("ID", "start", "end", "avd_def")) %>%
  rename(Start = start, End = end, Avd_def = avd_def) %>%
  mutate(source = "TW_2020",
         label = "west 20")

# align statistical assessments for tw_2023
tw_2023_df = tw_2023_df %>% 
  select(c("ID", "Start", "End", "Avoided_def")) %>%
  rename(Avd_def = Avoided_def) %>%
  mutate(source = "TW_2023",
         label = "west 23")

# align statistical assessments for tw_2024 (filtered for a specific project)
tw_2024_df = tw_2024_df %>% 
  select(ID, Start = year_start, End = year_end, Avd_def = def) %>%
  filter(ID == 934) %>%
  mutate(source = "TW_2023",  # note: combined with tw_2023 for simplicity
         label = "west 23")

# align statistical assessments for ag_2022
ag_2022_df$Start = 0
ag_2022_df$End = 5
ag_2022_df = ag_2022_df %>% 
  select(ID, Start, End, Avoided_def) %>% 
  rename(Avd_def = Avoided_def) %>%
  mutate(source = "AG_2022",
         label = "guizar-coutino 22")

# align statistical assessments for ag_2024
ag_2024_df = ag_2024_df %>%
  mutate(Start = 0, End = 5) %>% 
  select(c("ID", "Start", "End", "Avd_def")) %>%
  mutate(source = "AG_2024",
         label = "guizar-coutino 24")

# align pact assessments
pact_v2_df = pact_v2_df %>% 
  select(c("ID", "Start", "End", "avoided_disturbance_ha")) %>% 
  rename(Avd_def = avoided_disturbance_ha) %>%
  mutate(source = "PACTv2", 
         label = "pactv2")

# combine all datasets into one
combined_df = rbind(tw_2020_df, tw_2023_df, tw_2024_df, ag_2022_df, ag_2024_df, pact_v2_df, vcs_df) %>%
  mutate(ID = as.factor(ID)) %>%
  mutate(avd_def_yr = Avd_def / (End - Start))

# read project metadata and combine with assessments
all_projects_df = read.csv("csvs/project_metadata.csv") %>%
  select(ID, method = Methodology, country = Country.Area)

combined_df = combined_df %>%
  left_join(all_projects_df %>% mutate(ID = as.factor(ID))) %>%
  mutate(
    source = as.factor(source),
    label = as.factor(label),
    method = as.factor(method),
    country = as.factor(country)
  ) %>%
  mutate(counterfactual = ifelse(source == "VERRA", 0, 1))

# filter to only include unplanned deforestation methodologies
combined_df = combined_df %>%
  filter(method %in% c("VM0006", "VM0007", "VM0009", "VM0015"))

# write combined dataset to csv
write.csv(combined_df, file.path("csvs/certified_qem_combined_totals.csv"))

# align the datasets for certified vs. statistical assessments
cf_vcs_comp_def_df = combined_df %>% 
  select(ID, avd_def_yr, source) %>%
  group_by(ID) %>% 
  pivot_wider(names_from = source, values_from = avd_def_yr) %>% 
  filter(!is.na(VERRA)) %>% 
  pivot_longer(cols = c(-ID, -VERRA), names_to = "cf_source", values_to = "cf_avd") %>%
  ungroup() %>%
  filter(!is.na(cf_avd)) %>%
  mutate(Aadj = cf_avd / VERRA)

cf_vcs_comp_def_df$cf_source = as.factor(cf_vcs_comp_def_df$cf_source)

# write aligned certified vs. statistical dataset to csv
write.csv(cf_vcs_comp_def_df, file.path("csvs", "certified_pact_qems_amounts.csv"))

# calculate summary statistics for each project
cf_vcs_avd_comp_mean_df = cf_vcs_comp_def_df %>%
  filter(ID != 904) %>%
  group_by(ID) %>%
  summarise(
    n = n(),
    cf_avd_mean = mean(cf_avd),
    cf_avd_min = min(cf_avd),
    cf_avd_max = max(cf_avd),
    cf_avd_sd = sd(cf_avd),
    cf_avd_se = cf_avd_sd / sqrt(n),
    cf_avd_ci = 1.96 * cf_avd_se,
    cf_avd_ci_min = cf_avd_mean - cf_avd_ci,
    cf_avd_ci_max = cf_avd_mean + cf_avd_ci,
    cf_Aadj_mean = mean(Aadj),
    cf_Aadj_min = min(Aadj),
    cf_Aadj_max = max(Aadj),
    cf_Aadj_sd = sd(Aadj),
    cf_Aadj_se = cf_Aadj_sd / sqrt(n),
    cf_Aadj_ci = 1.96 * cf_Aadj_se,
    cf_Aadj_ci_min = cf_Aadj_mean - cf_Aadj_ci,
    cf_Aadj_ci_max = cf_Aadj_mean + cf_Aadj_ci,
    VERRA = VERRA[1]
  )
# filter negative add ratio points for manual addition
neg_points_df = cf_vcs_avd_comp_mean_df %>%
  filter(VERRA < 0) %>%
  select(ID, cf_avd_mean, cf_Aadj_mean, VERRA, n) %>%
  mutate(y_axis = 0,
         cf_Aadj_mean = cf_Aadj_mean * -1)
# write summary statistics to csv
write.csv(cf_vcs_avd_comp_mean_df, file.path("csvs", "additionality_ratios.csv"), row.names = FALSE)

# calculate overall means for plots
cf_vcs_avd_comp_mean_df = cf_vcs_avd_comp_mean_df %>%
  filter(VERRA >= 0)
aadj_mean = mean(cf_vcs_avd_comp_mean_df$cf_Aadj_mean)

# wilcoxon signed-rank test for difference
wilcox.test(cf_vcs_avd_comp_mean_df$VERRA, cf_vcs_avd_comp_mean_df$cf_avd_mean, paired = TRUE, alternative = "greater")

# mann-whitney u test for additionality ratios
ones = rep(1, length(cf_vcs_avd_comp_mean_df$cf_Aadj_mean))
wilcox.test(cf_vcs_avd_comp_mean_df$cf_Aadj_mean, ones, alternative = "less")
zeros = rep(0, length(cf_vcs_avd_comp_mean_df$cf_Aadj_mean))
wilcox.test(cf_vcs_avd_comp_mean_df$cf_Aadj_mean, zeros, alternative = "greater")

aadj_w_mean = weighted.mean(cf_vcs_avd_comp_mean_df$cf_Aadj_mean, cf_vcs_avd_comp_mean_df$VERRA)
aadj_median = median(cf_vcs_avd_comp_mean_df$cf_Aadj_mean)
aadj_w_median = median(rep(cf_vcs_avd_comp_mean_df$cf_Aadj_mean, cf_vcs_avd_comp_mean_df$VERRA)) 
1/aadj_median
1/aadj_w_median
1/aadj_w_mean


# prepare data for plotting fig 2a (line data)
new_dat_df = data.frame(cf_avd_mean = seq(from = min(cf_vcs_avd_comp_mean_df$cf_avd_min),
                                          to = max(cf_vcs_avd_comp_mean_df$cf_avd_max), by = 1)) %>%
  mutate(VERRA = cf_avd_mean)



# plot fig 2a (scatter and line plot)
fig2a_plot = cf_vcs_avd_comp_mean_df %>%
  ggplot(aes(y = VERRA, x = cf_avd_mean)) +
  geom_line(data = new_dat_df, col = "black", linetype = 2, linewidth = 0.75) +
  geom_vline(xintercept = 0, col = "black", linetype = 1, linewidth = 0.5) +
  geom_hline(yintercept = 0, col = "black", linetype = 1, linewidth = 0.5) +
  geom_point(size = 3, alpha = 0.8, aes(color = as.factor(n))) +
  geom_errorbar(aes(xmin = cf_avd_ci_min, xmax = cf_avd_ci_max, color = as.factor(n))) +
  scale_x_continuous(limits = c(-1000, 3500)) +
  scale_y_continuous(trans = "log10", limits = c(50, 10000)) +
  scale_color_manual(values = my_colors, labels = c("1", "2", "3", "4")) +
  ylab("Certified avoided deforestation (ha/year)") + 
  xlab("Quasi-experimental assessment of avoided deforestation (ha/year)") +
  theme(axis.line = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13),
        legend.position = "bottom",
        legend.spacing = unit(0, "cm")) +
  geom_point(data = neg_points_df, aes(x = cf_avd_mean, y = y_axis, color = as.factor(n)),
             shape = 1, size = 3) +
  guides(shape = guide_legend(ncol = 2, bycol = TRUE, title = NULL,
                              override.aes = list(size = 4, col = "black")),
         color = guide_legend(ncol = 4, bycol = TRUE, title = "Number of studies",
                              override.aes = list(shape = 16, size = 3, alpha = 0.5)))

# plot fig 2b (additionality ratio plot)
fig2b_plot = cf_vcs_avd_comp_mean_df %>%
  mutate(Aadj_mean = cf_avd_mean / VERRA) %>% 
  ggplot(aes(x = VERRA, y = Aadj_mean, label = ID)) +
  geom_vline(xintercept = 0, col = "black", linetype = 1, linewidth = 0.5) +
  geom_hline(yintercept = 0, col = "black", linetype = 1, linewidth = 0.5) +
  geom_hline(yintercept = 1, col = "black", linetype = 2, linewidth = 0.75) +
  geom_hline(yintercept = aadj_mean, linewidth = 0.75, col = "grey") +
  geom_hline(yintercept = aadj_w_mean, linewidth = 0.75, col = "grey", linetype = 2) +
  geom_point(size = 3, alpha = 0.8, aes(color = as.factor(n))) +
  geom_errorbar(aes(ymin = cf_Aadj_ci_min, ymax = cf_Aadj_ci_max, color = as.factor(n))) +
  scale_color_manual(values = my_colors, labels = c("1", "2", "3", "4")) +
  xlab("Certified avoided deforestation (ha/year)") + 
  ylab("Additionality ratio") +
  scale_x_continuous(trans = "log10") +
  theme(axis.line = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13),
        legend.position = "bottom",
        legend.spacing = unit(0, "cm")) +
  geom_point(data = neg_points_df, aes(y = cf_Aadj_mean, x = 0, color = as.factor(n)),
             shape = 1, size = 3) +
  guides(shape = guide_legend(ncol = 2, bycol = TRUE, title = NULL,
                              override.aes = list(size = 4, col = "black")),
         color = guide_legend(ncol = 4, bycol = TRUE, title = "Number of studies",
                              override.aes = list(shape = 16, size = 3, alpha = 0.5)))

# combine fig 2a and fig 2b vertically into fig 2
fig2_plot = fig2a_plot / fig2b_plot +
  plot_layout(ncol = 1, heights = c(1, 1.2)) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(size = 25))

# save final figure to png
ggsave(fig2_plot, filename = file.path("pngs", "fig2_raw.png"), dpi = 300, width = 7, height = 10)
