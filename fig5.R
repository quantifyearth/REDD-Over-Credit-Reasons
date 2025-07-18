library(tidyverse)
library(ggsignif)
library(ggtext)
library(patchwork)
library(ggpattern)

# ---- READ AND PROCESS DATA ----

# 1. read pact data (previously acc qem data)
pact_control_df = read_csv("csvs/acc_pact_control_rates.csv") %>%
  rename(pact_control_rate = rate)
pact_project_df = read_csv("csvs/acc_pact_project_rates.csv") %>%
  rename(pact_project_rate = rate)

# join pact control and project data
pact_df = pact_control_df %>%
  left_join(pact_project_df, by = "project_no")

# 2. read certified data
certified_control_df = read_csv("csvs/certified_control_rates.csv") %>%
  rename(certified_control_rate = rate)
certified_project_df = read_csv("csvs/certified_project_rates.csv") %>%
  rename(certified_project_rate = rate)

# 3. read acc certified control data (for additional measures)
acc_certified_control_df = read_csv("csvs/acc_certified_control_rates.csv") %>%
  rename(acc_certified_control_rate = rate)

# 4. join all data by project_no
comparison_df = acc_certified_control_df %>%
  left_join(pact_df, by = "project_no") %>%
  left_join(certified_project_df, by = "project_no") %>%
  left_join(certified_control_df, by = "project_no")

comparison_df$correction_coef = median(comparison_df$certified_project_rate / comparison_df$pact_project_rate)
# histogram of correction_coef
ggplot(comparison_df, aes(x = certified_project_rate / pact_project_rate)) +
  geom_histogram(bins = 10, fill = "#26547c", color = "black") +
  xlab("Correction Coefficient") +
  ylab("Frequency") +
  # add median line
  geom_vline(xintercept = median(comparison_df$certified_project_rate / comparison_df$pact_project_rate), 
             linetype = "dashed", color = "#ef476f", linewidth = 1) +
  # add smoothed density line
  geom_density(aes(y = ..count..), color = "#ef476f", linewidth = 1) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 22),
        axis.text.x = element_text(size = 16),
        axis.title.y = element_text(size = 22),
        axis.text.y = element_text(size = 16))

# shapiro-wilk test for normality
shapiro.test(comparison_df$certified_project_rate / comparison_df$pact_project_rate)

comparison_df$acc_certified_control_bespoke_rate = comparison_df$acc_certified_control_rate * comparison_df$correction_coef
comparison_df$acc_certified_control_bespoke_rate

# ---- COMPUTE THE MEASURES ----
# compute all six measures, only use four
comparison_df = comparison_df %>%
  mutate(
    # 1. difference between pact control and pact project rates
    pact_control_pact_project =
      pact_control_rate - pact_project_rate,
    # 2. difference between pact control and certified project rates
    pact_control_certified_project =
      pact_control_rate - certified_project_rate,
    # 3. difference between acc certified control rates and certified project rates
    acc_certified_control_certified_project =
      acc_certified_control_rate - certified_project_rate,
    # 4. difference between acc certified control bespoke rates and certified project rates
    acc_certified_control_bespoke_certified_project =
      acc_certified_control_bespoke_rate - certified_project_rate,
    # 5. difference between certified control and certified project rates
    certified_control_certified_project =
      certified_control_rate - certified_project_rate,
  ) %>%
  # select project_no and the five measures
  select(project_no, 
         pact_control_pact_project, 
         pact_control_certified_project, 
         acc_certified_control_certified_project,
         acc_certified_control_bespoke_certified_project,
         certified_control_certified_project)

median(comparison_df$pact_control_pact_project)
median(comparison_df$certified_control_certified_project)
median(comparison_df$acc_certified_control_bespoke_certified_project)


# ---- RESHAPE DATA FOR PROCESSING ----
# select the five newly named measures and pivot into long format
comparison_long = comparison_df %>%
  pivot_longer(cols = -project_no, names_to = "measure", values_to = "avoided_rate")


# --- CREATE PLOT LABELS ---
labels = c(
  pact_control_pact_project = "ACC QE Control\n- ACC QE Project",
  pact_control_certified_project = "ACC QE Control\n- Certified Project",
  acc_certified_control_certified_project = "ACC Certified Control\n- Certified Project",
  acc_certified_control_bespoke_certified_project = "Bespoke* Certified Control\n- Certified Project",
  certified_control_certified_project = "Certified Control\n- Certified Project"
)

# map the labels onto a new column in the long data frame
comparison_long = comparison_long %>%
  mutate(measure_label = recode(measure, !!!labels))


# ---- PLOT ERROR BARS WITH CORRECTED BAR ----

# create percentage contributions
total_difference = median(comparison_df$certified_control_certified_project) - median(comparison_df$pact_control_pact_project)
pact_control_certified_project_contribution = (median(comparison_df$pact_control_certified_project) - median(comparison_df$pact_control_pact_project)) / total_difference
acc_certified_control_certified_project_contribution = (median(comparison_df$acc_certified_control_certified_project) - median(comparison_df$pact_control_certified_project)) / total_difference
acc_certified_control_bespoke_certified_project_contribution = (median(comparison_df$acc_certified_control_bespoke_certified_project) - median(comparison_df$acc_certified_control_certified_project)) / total_difference
certified_control_certified_project_contribution_one = (median(comparison_df$certified_control_certified_project) - median(comparison_df$acc_certified_control_bespoke_certified_project)) / total_difference 
certified_control_certified_project_contribution_two = (median(comparison_df$certified_control_certified_project) - median(comparison_df$acc_certified_control_certified_project)) / total_difference

# round all to 2 dp
pact_control_certified_project_contribution = round(pact_control_certified_project_contribution, 2)
acc_certified_control_certified_project_contribution = round(acc_certified_control_certified_project_contribution, 2)
acc_certified_control_bespoke_certified_project_contribution = round(acc_certified_control_bespoke_certified_project_contribution, 2)
certified_control_certified_project_contribution_one = round(certified_control_certified_project_contribution_one, 2)
certified_control_certified_project_contribution_two = round(certified_control_certified_project_contribution_two, 2)


# determine the median values for each measure

medians = comparison_long %>%
  group_by(measure_label) %>%
  summarise(median_val = median(avoided_rate, na.rm = TRUE)) %>%
  arrange(match(measure_label, c(
    "ACC QE Control\n- ACC QE Project",
    "ACC QE Control\n- Certified Project",
    "ACC Certified Control\n- Certified Project",
    "Bespoke* Certified Control\n- Certified Project",
    "Certified Control\n- Certified Project"
  )))

# determine the net contribution of each measure

medians$contribution = c(0, diff(medians$median_val, lag = 1))

# determine the percentage contribution of each measure

medians$percentage_contribution = round(medians$contribution / total_difference, 2) * 100

# drop first row
label_medians = medians[-1, ]

# positions for difference labels
label_data = data.frame(
  x = c(1.5, 2.5, 3.5, 4.5),
  y = (medians$median_val[-1] + medians$median_val[-nrow(medians)])/2,
  label = sprintf("%+.0f%%", label_medians$percentage_contribution)
)

label_data$label_text = c("\nProject Area Remote Sensing", 
                          "\nControl Area Selection",
                          "\nControl Area Remote Sensing",
                          "\n Ex Ante Modelling")

# combine the label text with the contribution
label_data$label = paste(label_data$label, label_data$label_text, sep = "")

fig5 = ggplot(comparison_long, 
                aes(x = measure_label, 
                    y = avoided_rate, 
                    colour = measure_label,
                    linetype = measure_label)) +
    annotate("segment", 
           x = 1, xend = 2, 
           y = medians$median_val[1], yend = medians$median_val[2],
           linetype = "dotted", color = "black", linewidth = 0.5) +
  annotate("segment", 
           x = 2, xend = 3, 
           y = medians$median_val[2], yend = medians$median_val[3],
           linetype = "dotted", color = "black", linewidth = 0.5) +
  annotate("segment", 
           x = 3, xend = 4, 
           y = medians$median_val[3], yend = medians$median_val[4],
           linetype = "dotted", color = "black", linewidth = 0.5) +
  annotate("segment", 
           x = 4, xend = 5, 
           y = medians$median_val[4], yend = medians$median_val[5],
           linetype = "dotted", color = "black", linewidth = 0.5) +
  
  # Add difference labels using annotate()
  annotate("text",
           x = label_data$x,
           y = label_data$y,
           label = label_data$label,
           color = "black", size = 2.8, vjust = 2) +
  
  # Original plot elements
  stat_summary(fun.data = function(y) {
    data.frame(
      y    = median(y, na.rm = TRUE),
      ymin = quantile(y, 0.25, na.rm = TRUE),
      ymax = quantile(y, 0.75, na.rm = TRUE)
    )
  }, geom = "errorbar", width = 0.2, linewidth = 1,
  position = position_dodge(width = 0.5)) +
  stat_summary(fun = median, geom = "crossbar", 
               width = 0.2, linewidth = 1,
               position = position_dodge(width = 0.5)) +
  
  # Rest of your original code...
  scale_y_continuous(labels = function(x) sprintf("%.1f", x)) +
  scale_colour_manual(values = c(
    "ACC QE Control\n- ACC QE Project"             = "#06d6a0",  
    "ACC QE Control\n- Certified Project"            = "#f77f00",  
    "ACC Certified Control\n- Certified Project"       = "#26547c",
    "Bespoke* Certified Control\n- Certified Project" = "#B6C7D6",
    "Certified Control\n- Certified Project"           = "#ef476f"
  )) +
  scale_linetype_manual(values = c(
    "ACC QE Control\n- ACC QE Project"             = "solid",  
    "ACC QE Control\n- Certified Project"            = "solid",  
    "ACC Certified Control\n- Certified Project"       = "solid",
    "Bespoke* Certified Control\n- Certified Project" = "solid",
    "Certified Control\n- Certified Project"           = "solid"
  )) +
  scale_x_discrete(limits = c(
    "ACC QE Control\n- ACC QE Project",
    "ACC QE Control\n- Certified Project",
    "ACC Certified Control\n- Certified Project",
    "Bespoke* Certified Control\n- Certified Project",
    "Certified Control\n- Certified Project"
  )) +
  geom_hline(yintercept = median(comparison_df$pact_control_pact_project, na.rm = TRUE),
             linetype = "dashed", linewidth = 0.75, colour = "#06d6a0") +
  geom_hline(yintercept = median(comparison_df$certified_control_certified_project, na.rm = TRUE),
             linetype = "dashed", linewidth = 0.75, colour = "#ef476f") +
  xlab("Choice of project and control rate") +
  theme_classic() +
  theme(axis.title.y = element_text(size = 22),
        axis.text.y  = element_text(size = 13),
        axis.title.x = element_text(size = 22),
        axis.text.x = element_text(angle = 30, hjust = 1, size = 16),
        legend.position = "none") +
  coord_cartesian(ylim = c(0, 1.5))
p_labels = c("Combined Over-Crediting Effect **")

# add significance bracket between the first and last measure
fig5 = fig5 + 
  geom_signif(comparisons = list(c("ACC QE Control\n- ACC QE Project",
                                    "Certified Control\n- Certified Project")),
              test = "wilcox.test",  # Wilcoxon test for comparing the distributions
              map_signif_level = FALSE,
              y_position = c(1.2),
              textsize = 5.4, 
              tip_length = 0.01,
              colour = "black",
              annotations = p_labels)

# add y axis label
fig5 = fig5 + 
  labs(y = "Avoided Deforestation Rate (%)") +
  theme(axis.title.y = element_text(size = 22))

# save the plot as a svg file
ggsave("pngs/fig5_raw.svg", fig5, width = 10, height = 9, units = "in", dpi = 300)

# ----- WILCOXON TESTS ----
wilcox.test(comparison_df$certified_control_certified_project, comparison_df$pact_control_pact_project, paired = T, alternative = "greater")
wilcox.test(comparison_df$pact_control_certified_project, comparison_df$pact_control_pact_project, paired = T, alternative = "greater")
wilcox.test(comparison_df$acc_certified_control_certified_project, comparison_df$pact_control_certified_project, paired = T, alternative = "greater")
wilcox.test(comparison_df$certified_control_certified_project, comparison_df$acc_certified_control_certified_project, paired = T, alternative = "greater")





