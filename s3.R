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

# write aligned certified vs. statistical dataset to csv
write.csv(cf_vcs_comp_def_df, file.path("csvs", "certified_pact_qems_amounts.csv"))

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
    VERRA = VERRA[1]
  )

cf_vcs_avd_comp_mean_df$cf_Aadj_mean = cf_vcs_avd_comp_mean_df$VERRA / cf_vcs_avd_comp_mean_df$cf_avd_mean

# write summary statistics to csv
write.csv(cf_vcs_avd_comp_mean_df, file.path("csvs", "additionality_ratios.csv"), row.names = FALSE)

# spearmans rank correlation between certified and quasi-experimental
correlation = cor.test(cf_vcs_avd_comp_mean_df$VERRA, cf_vcs_avd_comp_mean_df$cf_avd_mean, method = "pearson")
cat(sprintf("Spearman's rank correlation: %.3f (p-value: %.3f)\n", 
            correlation$estimate, correlation$p.value))


# calculate n() = 1
ones = sum(cf_vcs_avd_comp_mean_df$n == 1)
cat(sprintf("Number of projects with only one study: %i\n", ones))

# calculate the number of projects with a difference greater than 0
cf_vcs_avd_comp_mean_df$diff = cf_vcs_avd_comp_mean_df$VERRA - cf_vcs_avd_comp_mean_df$cf_avd_mean
positive_diff = sum(cf_vcs_avd_comp_mean_df$diff > 0)

cat(sprintf("Projects with a difference greater than 0: %.0f", positive_diff))

# calculate the number of projects with a difference greater than with 95% confidence
cf_vcs_avd_comp_mean_df$diff_95 = cf_vcs_avd_comp_mean_df$VERRA - cf_vcs_avd_comp_mean_df$cf_avd_ci_max
positive_diff_95 = sum(cf_vcs_avd_comp_mean_df$diff_95 > 0, na.rm = TRUE)

cat(sprintf("Projects with a difference greater than 0 (95%% CI): %.0f", positive_diff_95))

# calculate the percentage of projects with a additionality ratio greater than 0
additional = sum(cf_vcs_avd_comp_mean_df$cf_avd_mean > 0)

cat(sprintf("Projects with postive additionality: %i", additional))

# determine projects with 95% confidence higher diff
additional_95 = sum(cf_vcs_avd_comp_mean_df$cf_avd_ci_min > 0, na.rm = TRUE)

cat(sprintf("Projects with postive additionality (95%% CI): %i", additional_95))

# determine median cov
median_cov = median(abs(cf_vcs_avd_comp_mean_df$cf_avd_sd / cf_vcs_avd_comp_mean_df$cf_avd_mean), na.rm = TRUE)

cat(sprintf("Median QEM Coefficient of Variation: %.3f", median_cov))

# wilcoxon signed-rank test if verra is greater than quasi-experimental
wilcox.test(cf_vcs_avd_comp_mean_df$VERRA, cf_vcs_avd_comp_mean_df$cf_avd_mean, paired = TRUE, alternative = "greater")

# calculate overall means for plots
cf_avd_mean = sum(cf_vcs_avd_comp_mean_df$VERRA) / sum(cf_vcs_avd_comp_mean_df$cf_avd_mean)
cat(sprintf("Global Certified/Quasi-experimental: %.2f", cf_avd_mean))


# calculate cf_avd_mean when excuding the 9 highest VERRA values
cf_avd_mean_excl = sum(cf_vcs_avd_comp_mean_df$VERRA[-order(cf_vcs_avd_comp_mean_df$VERRA)[(nrow(cf_vcs_avd_comp_mean_df)-8):nrow(cf_vcs_avd_comp_mean_df)]]) / 
  sum(cf_vcs_avd_comp_mean_df$cf_avd_mean[-order(cf_vcs_avd_comp_mean_df$VERRA)[(nrow(cf_vcs_avd_comp_mean_df)-8):nrow(cf_vcs_avd_comp_mean_df)]])
cat(sprintf("Global Certified/Quasi-experimental (excl. 9 highest VERRA): %.2f", cf_avd_mean_excl))

# calculate cf_avd_mean when excuding the 8 highest VERRA values
cf_avd_mean_excl8 = sum(cf_vcs_avd_comp_mean_df$VERRA[-order(cf_vcs_avd_comp_mean_df$VERRA)[(nrow(cf_vcs_avd_comp_mean_df)-7):nrow(cf_vcs_avd_comp_mean_df)]]) / 
  sum(cf_vcs_avd_comp_mean_df$cf_avd_mean[-order(cf_vcs_avd_comp_mean_df$VERRA)[(nrow(cf_vcs_avd_comp_mean_df)-7):nrow(cf_vcs_avd_comp_mean_df)]])
cat(sprintf("Global Certified/Quasi-experimental (excl. 8 highest VERRA): %.2f", cf_avd_mean_excl8))

# bootstrap if cf_avd_mean is greater than 1 
n_bootstraps = 10000
boot_ratios_df = replicate(n_bootstraps, {
  sample_idx = sample(length(cf_vcs_avd_comp_mean_df$VERRA), replace = TRUE)
  sum(cf_vcs_avd_comp_mean_df$VERRA[sample_idx]) / sum(cf_vcs_avd_comp_mean_df$cf_avd_mean[sample_idx])
})
cat(sprintf("Global Certified/Quasi-experimental: %.2f", cf_avd_mean), 
    "\n95% CI:", quantile(boot_ratios_df, c(0.025, 0.975)))

# proportions
proportions_df = cf_vcs_avd_comp_mean_df$cf_avd_mean / cf_vcs_avd_comp_mean_df$VERRA
proportions_df = ifelse(proportions_df < 0, 0, proportions_df)
mean_proportion = mean(proportions_df, na.rm = TRUE)
1/mean_proportion

# p value for the mean
p_value = t.test(proportions_df, mu = 1, alternative = "less")$p.value
p_value

# Bootstrap 95% CI
boot_proportions_df <- replicate(10000, {
  sample_idx <- sample(length(proportions_df), replace = TRUE)
  mean(proportions_df[sample_idx], na.rm = TRUE)
})
cat("Mean Project Over Crediting:", 1/mean_proportion, 
    "\n95% CI:", quantile(1/boot_proportions_df, c(0.025, 0.975)))

# median proportion
median_proportion = median(proportions_df, na.rm = TRUE)
1/median_proportion
# Bootstrap 95% CI for median
boot_median_proportions_df <- replicate(10000, {
  sample_idx <- sample(length(proportions_df), replace = TRUE)
  median(proportions_df[sample_idx], na.rm = TRUE)
})
cat("Median Project Over Crediting:", 1/median_proportion, 
    "\n95% CI:", quantile(1/boot_median_proportions_df, c(0.025, 0.975)))

# prepare data for plotting fig 2a (line data)
new_dat_df = data.frame(cf_avd_mean = seq(from = min(cf_vcs_avd_comp_mean_df$cf_avd_min),
                                          to = max(cf_vcs_avd_comp_mean_df$cf_avd_max), by = 1)) %>%
  mutate(VERRA = cf_avd_mean)

# inverse hyperbolic sine transformation function
asinh_trans = function(x) asinh(x)
asinh_inv = function(x) sinh(x)

positive_colors = data.frame(n = 1:5, color = c("#8ef2ef", "#5aebe7", "#16b9b5", "#129693", "#0c625f"))
negative_colors = data.frame(n = 1:5, color = c("#fcae91", "#f07d80", "#e5252a", "#851013", "#67000d"))
my_colors = c(positive_colors$color, negative_colors$color)
cf_vcs_avd_comp_mean_df = cf_vcs_avd_comp_mean_df %>%
  mutate(color = ifelse(cf_avd_mean < 0,
                        negative_colors$color[match(n, negative_colors$n)],
                        positive_colors$color[match(n, positive_colors$n)]))

# plot fig 2a (scatter and line plot)
fig2a_plot = 
  ggplot() +
  geom_vline(xintercept = 0, col = "black", linetype = 1, linewidth = 0.5) +
  geom_hline(yintercept = 50, col = "black", linetype = 1, linewidth = 0.5) +
  geom_line(data = new_dat_df, col = "darkred", linetype = 2, linewidth = 0.75,
            aes(x = VERRA, y = cf_avd_mean)) +
  geom_errorbar(data = cf_vcs_avd_comp_mean_df,
                aes(x = cf_avd_mean, y = VERRA, xmin = cf_avd_ci_min, xmax = cf_avd_ci_max, color = color)) +
  geom_point(data = cf_vcs_avd_comp_mean_df, 
             aes(y = VERRA, x = cf_avd_mean, color = color,
                 size = 5, alpha = 1)) +
  geom_text_repel(data = cf_vcs_avd_comp_mean_df, aes(x = cf_avd_mean, y = VERRA, label = ID), 
                  size = 3, max.overlaps = 50) +
  scale_x_continuous(limits = c(-1700, 2500), breaks = c(-1500, -1000, -500, 0, 500, 1000, 1500, 2000)) +
  scale_y_continuous(trans = scales::trans_new("asinh", asinh_trans, asinh_inv),
                     limits = c(50, 19000), breaks = c(100, 1000, 10000)) +
  ylab("Certified avoided deforestation (ha/year)") +
  xlab("Quasi-experimental avoided deforestation (ha/year)") +
  geom_text_repel(data = data.frame(x = 1800, y = 3000, label = "Over Crediting"),
                  aes(x = x, y = y, label = label), size = 8, color = "black", force = 0) +
  geom_text_repel(data = data.frame(x = 1800, y = 940, label = "Under Crediting"),
                  aes(x = x, y = y, label = label), size = 8, color = "black", force = 0) +
  geom_text_repel(data = data.frame(x = 1000, y = 18000, label = "Decreased Deforestation →"),
                  aes(x = x, y = y, label = label), size = 8, color = "black", force = 0) +
  geom_text_repel(data = data.frame(x = -1000, y = 18000, label = "← Increased Deforestation"),
                  aes(x = x, y = y, label = label), size = 8, color = "black", force = 0) +
  theme(axis.line = element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 23),
        legend.position = "none",
        legend.spacing = unit(0, "cm"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18))

# create df of negative additionality ratios for plotting
cf_vcs_avd_comp_mean_df_neg = cf_vcs_avd_comp_mean_df %>%
  filter(cf_Aadj_mean < 0) %>%
  mutate(cf_Aadj_mean = 1400)

# plot fig 2b (additionality ratio plot)
fig2b_plot = cf_vcs_avd_comp_mean_df %>%
  filter(cf_Aadj_mean > 0) %>%
  ggplot(aes(x = VERRA, y = cf_Aadj_mean)) +
  geom_hline(yintercept = 0, col = "black", linetype = 1, linewidth = 0.5) +
  geom_hline(yintercept = 1, col = "darkred", linetype = 2, linewidth = 0.75) +
  geom_hline(yintercept = cf_avd_mean, linewidth = 0.75,
             col = "grey", linetype = 'dotdash') +
  geom_hline(yintercept = 1/mean_proportion, col = "darkgreen", linetype = 'dotdash', linewidth = 0.75) +
  geom_point(size = 5, alpha = 1, aes(color = color)) +
  geom_text_repel(aes(x = VERRA, y = cf_Aadj_mean, label = ID), 
                  size = 3, max.overlaps = 50) +
  geom_point(data = cf_vcs_avd_comp_mean_df_neg, aes(x = VERRA, y = cf_Aadj_mean, color = color),
             shape = 18, size = 5) +
  geom_text_repel(data = cf_vcs_avd_comp_mean_df_neg, aes(x = VERRA, y = cf_Aadj_mean, label = ID), 
                  size = 3, max.overlaps = 50) +
  xlab("Certified avoided deforestation (ha/year)") +
  ylab("Over-crediting ratio") +
  geom_text_repel(data = data.frame(x = 9000, y = 1.8, label = "Over Crediting ↑"),
                  aes(x = x, y = y, label = label), size = 8, color = "black", force = 0) +
  geom_text_repel(data = data.frame(x = 9000, y = 0.4, label = "Under Crediting ↓"),
                  aes(x = x, y = y, label = label), size = 8, color = "black", force = 0) +
  geom_text_repel(data = data.frame(x = 9000, y = cf_avd_mean + 2, label = "italic('Global Mean (10.7)')"),
                  aes(x = x, y = y, label = label), size = 8, color = "black", parse = TRUE, force = 0) +
  geom_text_repel(data = data.frame(x = 9000, y = 1/mean_proportion + 1, label = "italic('Project Mean (4.1)')"),
                  aes(x = x, y = y, label = label), size = 8, color = "black", parse = TRUE, force = 0) +
  scale_x_continuous(trans = scales::trans_new("asinh", asinh_trans, asinh_inv),
                     limits = c(50, 16000), breaks = c(100, 1000, 10000)) +
  scale_y_continuous(trans = scales::trans_new("asinh", asinh_trans, asinh_inv),
                     breaks = c(1,10,100,1000)) +
  theme(axis.line = element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 23),
        legend.position = "bottom",
        legend.spacing = unit(0, "cm"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18))


# legend data frame
legend_data <- data.frame(
  n = rep(1:5, 2),
  sign = rep(c("Reduced\ndeforestation", "Elevated\ndeforestation"), each = 5),
  color = my_colors,
  label = rep(1:5, 2)  # Using numbers 1-5 as labels
)

# legend as a table-like plot
legend_plot <- ggplot(legend_data, aes(x = factor(n), y = sign, fill = color)) +
  geom_tile(color = "white", linewidth = 10) +
  scale_fill_identity() +
  scale_x_discrete(position = "top") +
  labs(x = "Number of studies\n", y = NULL) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 16),
    axis.title.x = element_text(size = 20, margin = margin(b = 30)),
    panel.grid = element_blank(),
    plot.margin = margin(5, 5, 5, 5)
  )

# spacers between plots for more breathing room
fig2_plot <- fig2a_plot / plot_spacer() / fig2b_plot / plot_spacer() / legend_plot +
  plot_layout(
    nrow = 5,
    heights = c(1, 0.05, 1, 0.05, 0.2),
    widths = c(1, 1, 1, 1, 0.5)
  ) +
  plot_annotation(tag_levels = list(c("a", "b", "", "", ""))) &
  theme(plot.tag = element_text(size = 30)) &
  scale_color_identity()


# save final figure to svg
ggsave(fig2_plot, filename = file.path("pngs", "s3_raw.svg"), width = 12, height = 18)
ggsave(fig2_plot, filename = file.path("pngs", "s3_raw.png"), width = 12, height = 18, dpi = 300)


