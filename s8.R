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

# set global theme
theme_set(theme_classic())

# statistical assessments
tw_2020_df = read.csv(file.path("csvs", "tw_2020_avoided_amounts.csv"))
tw_2023_df = read.csv(file.path("csvs", "tw_2023_avoided_amounts.csv"))
tw_2024_df = read.csv(file.path("csvs", "tw_2024_avoided_amounts.csv"))
ag_2022_df = read.csv(file.path("csvs", "ag_2022_avoided_amounts.csv"))
ag_2025_df = read.csv(file.path("csvs", "ag_2025_avoided_amounts.csv")) %>%
  select(ID = id, Avd_def = avoided_ha) %>%
  mutate(Avd_def = -Avd_def)  # reverse sign for avoided deforestation
tang_2025_df = read.csv(file.path("csvs", "tang_2025_avoided_amounts.csv"))

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
ag_2022_df$Start = ag_2022_df$start
ag_2022_df$End = ag_2022_df$start + 5
ag_2022_df = ag_2022_df %>%
  select(ID, Start, End, avoided_deforestation) %>%
  rename(Avd_def = avoided_deforestation) %>%
  mutate(source = "AG_2022",
         label = "guizar-coutino 22")

# align statistical assessments for ag_2025
ag_2025_df = ag_2025_df %>%
  mutate(Start = 0, End = 5) %>%
  select(c("ID", "Start", "End", "Avd_def")) %>%
  mutate(source = "ag_2025",
         label = "guizar-coutino 24")

# align pact assessments
pact_v2_df = pact_v2_df %>%
  select(c("ID", "Start", "End", "avoided_disturbance_ha")) %>%
  rename(Avd_def = avoided_disturbance_ha) %>%
  mutate(source = "PACTv2",
         label = "pactv2")

# align tang 2025 assessments
tang_2025_df = tang_2025_df %>%
  select(ID = id, Start = start, End = end, Avd_def = avoided_def) %>%
  mutate(source = "tang_2025",
         label = "tang 25")

# combine all datasets into one
combined_df = rbind(tw_2020_df, tw_2023_df, tw_2024_df, ag_2022_df, ag_2025_df, tang_2025_df, pact_v2_df, vcs_df) %>%
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
  select(ID, avd_def_yr, source, country) %>%
  group_by(ID) %>%
  pivot_wider(names_from = source, values_from = avd_def_yr) %>%
  filter(!is.na(VERRA)) %>%
  pivot_longer(cols = c(-ID, -VERRA, -country), names_to = "cf_source", values_to = "cf_avd") %>%
  filter(!all(is.na(cf_avd))) %>%
  ungroup() %>%
  filter(VERRA > 0)

# add in verra method
cf_vcs_comp_def_df = cf_vcs_comp_def_df %>%
  left_join(all_projects_df %>% mutate(ID = as.factor(ID)) %>%
              select(ID, method = method), by = "ID") %>%
  mutate(cf_source = as.factor(cf_source))

cf_vcs_comp_def_df$cf_source = as.factor(cf_vcs_comp_def_df$cf_source)

# table of the number of projects per source wher cf_avd is not na
cf_vcs_comp_def_df %>%
  group_by(cf_source) %>%
  # sum up the number iof projects per source (don't add if the cf_avd is zero)
  summarise(n = sum(!is.na(cf_avd))) %>%
  arrange(desc(n))

# table of the number of projects per method
cf_vcs_comp_def_df %>%
  group_by(method) %>%
  # sum up the number of unique projects per method
  summarise(n = n_distinct(ID)) %>%
  arrange(desc(n))

# calculate summary statistics for each project
cf_vcs_avd_comp_mean_df = cf_vcs_comp_def_df %>%
  group_by(ID) %>%
  summarise(
    n = n() - sum(is.na(cf_avd)),
    cf_avd_mean = mean(cf_avd, na.rm = TRUE),
    cf_avd_min = min(cf_avd, na.rm = TRUE),
    cf_avd_max = max(cf_avd, na.rm = TRUE),
    cf_avd_sd = sd(cf_avd, na.rm = TRUE),
    cf_avd_se = cf_avd_sd / sqrt(n),
    cf_avd_ci = 1.96 * cf_avd_se,
    cf_avd_ci_min = cf_avd_mean - cf_avd_ci,
    cf_avd_ci_max = cf_avd_mean + cf_avd_ci,
    VERRA = VERRA[1],
    country = country[1]
  )


cf_vcs_avd_comp_mean_df$cf_Aadj_mean = cf_vcs_avd_comp_mean_df$VERRA / cf_vcs_avd_comp_mean_df$cf_avd_mean
proportions_df = cf_vcs_avd_comp_mean_df$cf_avd_mean / cf_vcs_avd_comp_mean_df$VERRA
proportions_df = ifelse(proportions_df < 0, 0, proportions_df)
mean_proportion = mean(proportions_df, na.rm = TRUE)

# prepare data for plotting fig 2a (line data)
new_dat_df = data.frame(cf_avd_mean = seq(from = min(cf_vcs_avd_comp_mean_df$cf_avd_min),
                                          to = max(cf_vcs_avd_comp_mean_df$cf_avd_max), by = 1)) %>%
  mutate(VERRA = cf_avd_mean)

# inverse hyperbolic sine transformation function
asinh_trans = function(x) asinh(x)
asinh_inv = function(x) sinh(x)

# plot fig s8a (scatter and line plot)
s8a_plot = ggplot(data = cf_vcs_avd_comp_mean_df, 
                  aes(y = VERRA, x = cf_avd_mean)) +
  geom_line(data = new_dat_df, 
            aes(x = cf_avd_mean, y = VERRA), 
            col = "darkred", linetype = 2, linewidth = 0.75) +
  geom_vline(xintercept = 0, col = "black", linetype = 1, linewidth = 0.5) +
  geom_hline(yintercept = 50, col = "black", linetype = 1, linewidth = 0.5) +
  geom_errorbar(aes(xmin = cf_avd_ci_min, xmax = cf_avd_ci_max,
                    color = as.factor(country)),
                width = 0) +
  geom_point(aes(color = as.factor(country)), 
             size = 2, alpha = 0.8) +
  geom_text_repel(data = data.frame(x = 2000, y = 3200, label = "Over Crediting"),
                  aes(x = x, y = y, label = label), size = 4, color = "black", force = 0) +
  geom_text_repel(data = data.frame(x = 2000, y = 1100, label = "Under Crediting"),
                  aes(x = x, y = y, label = label), size = 4, color = "black", force = 0) +
  geom_text_repel(data = data.frame(x = 1000, y = 19000, label = "Decreased Deforestation →"),
                  aes(x = x, y = y, label = label), size = 4, color = "black", force = 0) +
  geom_text_repel(data = data.frame(x = -1000, y = 19000, label = "← Increased Deforestation"),
                  aes(x = x, y = y, label = label), size = 4, color = "black", force = 0) +
  scale_x_continuous(
    limits = c(-1700, 2800),
    breaks = c(-1500, -1000, -500, 0, 500, 1000, 1500, 2000)
  ) +
  scale_y_continuous(
    trans = scales::trans_new("asinh", asinh_trans, asinh_inv),
    limits = c(50, 20000), 
    breaks = c(100, 1000, 10000)
  ) +
  scale_color_manual(
    # 9 distinct colors 
    values = c("#E41A1C", "#377EB8", "#4DAF4A", "#FF7F00", "#FFFF33", 
               "#A65628", "#984EA3", "#999999", "#D55E00", "#66C2A5"),
    name = "Country"
  ) +
  labs(
    y = "Certified avoided deforestation (ha/year)",
    x = "Quasi-experimental avoided deforestation (ha/year)"
  ) +
  theme(
    axis.line = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13),
    legend.position = "bottom",
    legend.spacing = unit(0, "cm")
  ) +
  guides(
    color = guide_legend(
      ncol = 4, 
      override.aes = list(shape = 16, size = 3, alpha = 0.5)
    )
  )

# calculate global mean for additionality ratio
cf_avd_mean = sum(cf_vcs_avd_comp_mean_df$VERRA) / sum(cf_vcs_avd_comp_mean_df$cf_avd_mean)

# plot fig s8b (additionality ratio plot)
cf_vcs_avd_comp_mean_df_neg = cf_vcs_avd_comp_mean_df %>%
  filter(cf_Aadj_mean < 0) %>%
  mutate(cf_Aadj_mean = 1400)

s8b_plot = cf_vcs_avd_comp_mean_df %>%
  filter(cf_Aadj_mean > 0) %>%
  ggplot(aes(x = VERRA, y = cf_Aadj_mean, colour = as.factor(country))) +
  geom_hline(yintercept = 0, col = "black", linetype = 1, linewidth = 0.5) +
  geom_hline(yintercept = 1, col = "darkred", linetype = 2, linewidth = 0.75) +
  geom_hline(yintercept = cf_avd_mean, linewidth = 0.75, col = "grey", linetype = 'dotdash') +
  geom_hline(yintercept = 1/mean_proportion, col = "darkgreen", linetype = 'dotdash', linewidth = 0.75) +
  geom_point(size = 3, alpha = 0.8) +
  geom_point(data = cf_vcs_avd_comp_mean_df_neg, aes(x = VERRA, y = cf_Aadj_mean, color = as.factor(country)),
             shape = 15, size = 2.5, ) +
  xlab("Certified avoided deforestation (ha/year)") +
  ylab("Additionality ratios") +
  geom_text_repel(data = data.frame(x = 8000, y = 1.8, label = "Over Crediting ↑"),
                  aes(x = x, y = y, label = label), size = 4, color = "black", force = 0) +
  geom_text_repel(data = data.frame(x = 8000, y = 0.4, label = "Under Crediting ↓"),
                  aes(x = x, y = y, label = label), size = 4, color = "black", force = 0) +
  geom_text_repel(data = data.frame(x = 8000, y = cf_avd_mean + 1.7, label = "italic('Global Mean (9.3)')"),
                  aes(x = x, y = y, label = label), size = 4, color = "black", parse = TRUE, force = 0) +
  geom_text_repel(data = data.frame(x = 9000, y = 1/mean_proportion + 1, label = "italic('Project Mean (4.3)')"),
                  aes(x = x, y = y, label = label), size = 4, color = "black", parse = TRUE, force = 0) +
  scale_x_continuous(trans = scales::trans_new("asinh", asinh_trans, asinh_inv),
                     limits = c(50, 15000), breaks = c(100, 1000, 10000)) +
  scale_y_continuous(trans = scales::trans_new("asinh", asinh_trans, asinh_inv),
                     breaks = c(1,10,100,1000)) +
  scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A", "#FF7F00", "#FFFF33", 
                                "#A65628", "#984EA3", "#999999", "#D55E00", "#66C2A5"),
                     name = "Country") +
  theme(axis.line = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13),
        legend.position = "bottom",
        legend.spacing = unit(0, "cm")) +
  guides(
    color = guide_legend(
      ncol = 4, 
      override.aes = list(shape = 16, size = 3, alpha = 0.5)
    )
  )
s8b_plot
# combine
s8_plot = s8a_plot / s8b_plot +
  plot_layout(ncol = 1,
              heights = c(1, 1),
              widths = c(1, 0.66, 1)) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(size = 25))

# save final figure to png
ggsave(s8_plot, filename = file.path("pngs", "s8_plot_raw.png"), dpi = 300, width = 6.6, height = 14)

# filter for madagascar projects
madagascar_df = cf_vcs_avd_comp_mean_df %>%
  filter(country == "Madagascar") %>%
  select(ID, VERRA, cf_avd_mean, cf_Aadj_mean)
madagascar_mean = sum(madagascar_df$VERRA) / sum(madagascar_df$cf_avd_mean)

# print madagascar mean
print(paste("Madagascar mean additionality ratio:", round(madagascar_mean, 2)))

