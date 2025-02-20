library(tidyverse)
library(ggsignif)
library(ggtext)

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
comparison_df$pact_project_bespoke_rate = comparison_df$pact_project_rate * comparison_df$correction_coef
comparison_df$acc_certified_control_bespoke_rate = comparison_df$acc_certified_control_rate * comparison_df$correction_coef

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
  pact_control_pact_project = "ACC QE Control\n- ACC QE Project\n(n = 17)",
  pact_control_certified_project = "ACC QE Control\n- Certified Project\n(n = 17)",
  acc_certified_control_certified_project = "ACC Certified Control\n- Certified Project\n(n = 17)",
  acc_certified_control_bespoke_certified_project = "ACC to Bespoke Certified Control\n- Certified Project\n(n = 17)",
  certified_control_certified_project = "Certified Control\n- Certified Project\n(n = 17)"
)

# map the labels onto a new column in the long data frame
comparison_long = comparison_long %>%
  mutate(measure_label = recode(measure, !!!labels))


# ---- PLOT ERROR BARS WITH CORRECTED BAR ----
# set the width of the polygons
poly_width = 0.3 

# create percentage contributions
total_difference = median(comparison_df$certified_control_certified_project) - median(comparison_df$pact_control_pact_project)
pact_control_certified_project_contribution = (median(comparison_df$pact_control_certified_project) - median(comparison_df$pact_control_pact_project)) / total_difference
acc_certified_control_certified_project_contribution = (median(comparison_df$acc_certified_control_certified_project) - median(comparison_df$pact_control_certified_project)) / total_difference
acc_certified_control_bespoke_certified_project_contribution = (median(comparison_df$acc_certified_control_bespoke_certified_project) - median(comparison_df$pact_control_certified_project)) / total_difference
certified_control_certified_project_contribution_one = (median(comparison_df$certified_control_certified_project) - median(comparison_df$acc_certified_control_bespoke_certified_project)) / total_difference 
certified_control_certified_project_contribution_two = (median(comparison_df$certified_control_certified_project) - median(comparison_df$acc_certified_control_certified_project)) / total_difference

# round all to 2 dp
pact_control_certified_project_contribution = round(pact_control_certified_project_contribution, 2)
acc_certified_control_certified_project_contribution = round(acc_certified_control_certified_project_contribution, 2)
acc_certified_control_bespoke_certified_project_contribution = round(acc_certified_control_bespoke_certified_project_contribution, 2)
certified_control_certified_project_contribution_one = round(certified_control_certified_project_contribution_one, 2)
certified_control_certified_project_contribution_two = round(certified_control_certified_project_contribution_two, 2)

# create the plot
fig5 = ggplot(comparison_long, 
       aes(x = measure_label, 
           y = avoided_rate, 
           colour = measure_label,
           linetype = measure_label)) +
  stat_summary(fun.data = function(y) {
    data.frame(
      y    = median(y, na.rm = TRUE),
      ymin = quantile(y, 0.25, na.rm = TRUE),
      ymax = quantile(y, 0.75, na.rm = TRUE)
    )
  },
  geom = "errorbar", width = 0.2, linewidth = 1,
  position = position_dodge(width = 0.5)) +
  stat_summary(fun = median, geom = "crossbar", 
               width = 0.2, linewidth = 1,
               position = position_dodge(width = 0.5)) +
  scale_y_continuous(labels = function(x) sprintf("%.1f", x)) +
  scale_colour_manual(values = c(
    "ACC QE Control\n- ACC QE Project\n(n = 17)"             = "#06d6a0",  
    "ACC QE Control\n- Certified Project\n(n = 17)"            = "#f77f00",  
    "ACC Certified Control\n- Certified Project\n(n = 17)"       = "#26547c",
    "ACC to Bespoke Certified Control\n- Certified Project\n(n = 17)" = "#B6C7D6",
    "Certified Control\n- Certified Project\n(n = 17)"           = "#ef476f"
  )) +
  scale_linetype_manual(values = c(
    "ACC QE Control\n- ACC QE Project\n(n = 17)"             = "solid",  
    "ACC QE Control\n- Certified Project\n(n = 17)"            = "solid",  
    "ACC Certified Control\n- Certified Project\n(n = 17)"       = "solid",
    "ACC to Bespoke Certified Control\n- Certified Project\n(n = 17)" = "solid",
    "Certified Control\n- Certified Project\n(n = 17)"           = "solid"
  )) +
  scale_x_discrete(limits = c(
    "ACC QE Control\n- ACC QE Project\n(n = 17)",
    "ACC QE Control\n- Certified Project\n(n = 17)",
    "ACC Certified Control\n- Certified Project\n(n = 17)",
    "ACC to Bespoke Certified Control\n- Certified Project\n(n = 17)",
    "Certified Control\n- Certified Project\n(n = 17)"
  )) +

# Replace each segment with a polygon defined by its four corners:
  annotate("polygon",
           x = c(1.5 - poly_width/2, 1.5 + poly_width/2,
                 1.5 + poly_width/2, 1.5 - poly_width/2),
           y = c(median(comparison_df$pact_control_pact_project, na.rm = TRUE),
                 median(comparison_df$pact_control_pact_project, na.rm = TRUE),
                 median(comparison_df$pact_control_certified_project, na.rm = TRUE),
                 median(comparison_df$pact_control_certified_project, na.rm = TRUE)),
           fill = "#f77f00") +
  annotate("polygon",
           x = c(2.5 - poly_width/2, 2.5 + poly_width/2,
                 2.5 + poly_width/2, 2.5 - poly_width/2),
           y = c(median(comparison_df$pact_control_pact_project, na.rm = TRUE),
                 median(comparison_df$pact_control_pact_project, na.rm = TRUE),
                 median(comparison_df$pact_control_certified_project, na.rm = TRUE),
                 median(comparison_df$pact_control_certified_project, na.rm = TRUE)),
           fill = "#f77f00") +
  annotate("polygon",
           x = c(2.425 - poly_width/4, 2.425 + poly_width/4,
                 2.425 + poly_width/4, 2.425 - poly_width/4),
           y = c(median(comparison_df$pact_control_certified_project, na.rm = TRUE),
                 median(comparison_df$pact_control_certified_project, na.rm = TRUE),
                 median(comparison_df$acc_certified_control_certified_project, na.rm = TRUE),
                 median(comparison_df$acc_certified_control_certified_project, na.rm = TRUE)),
           fill = "#26547c") +
  annotate("polygon",
           x = c(3.5 - poly_width/2, 3.5 + poly_width/2,
                 3.5 + poly_width/2, 3.5 - poly_width/2),
           y = c(median(comparison_df$pact_control_pact_project, na.rm = TRUE),
                 median(comparison_df$pact_control_pact_project, na.rm = TRUE),
                 median(comparison_df$pact_control_certified_project, na.rm = TRUE),
                 median(comparison_df$pact_control_certified_project, na.rm = TRUE)),
           fill = "#f77f00") +
  annotate("polygon",
           x = c(3.575 - poly_width/4, 3.575 + poly_width/4,
                 3.575 + poly_width/4, 3.575 - poly_width/4),
           y = c(median(comparison_df$pact_control_certified_project, na.rm = TRUE),
                 median(comparison_df$pact_control_certified_project, na.rm = TRUE),
                 median(comparison_df$acc_certified_control_bespoke_certified_project, na.rm = TRUE),
                 median(comparison_df$acc_certified_control_bespoke_certified_project, na.rm = TRUE)),
           fill = "#B6C7D6") +
  annotate("polygon",
           x = c(4.5 - poly_width/2, 4.5 + poly_width/2,
                 4.5 + poly_width/2, 4.5 - poly_width/2),
           y = c(median(comparison_df$pact_control_pact_project, na.rm = TRUE),
                 median(comparison_df$pact_control_pact_project, na.rm = TRUE),
                 median(comparison_df$pact_control_certified_project, na.rm = TRUE),
                 median(comparison_df$pact_control_certified_project, na.rm = TRUE)),
           fill = "#f77f00") +
  annotate("polygon",
           x = c(4.425 - poly_width/4, 4.425 + poly_width/4,
                 4.425 + poly_width/4, 4.425 - poly_width/4),
           y = c(median(comparison_df$pact_control_certified_project, na.rm = TRUE),
                 median(comparison_df$pact_control_certified_project, na.rm = TRUE),
                 median(comparison_df$acc_certified_control_certified_project, na.rm = TRUE),
                 median(comparison_df$acc_certified_control_certified_project, na.rm = TRUE)),
           fill = "#26547c") +
  annotate("polygon",
           x = c(4.575 - poly_width/4, 4.575 + poly_width/4,
                 4.575 + poly_width/4, 4.575 - poly_width/4),
           y = c(median(comparison_df$pact_control_certified_project, na.rm = TRUE),
                 median(comparison_df$pact_control_certified_project, na.rm = TRUE),
                 median(comparison_df$acc_certified_control_bespoke_certified_project, na.rm = TRUE),
                 median(comparison_df$acc_certified_control_bespoke_certified_project, na.rm = TRUE)),
           fill = "#B6C7D6") +
  annotate("polygon",
           x = c(4.575 - poly_width/4, 4.575 + poly_width/4,
                 4.575 + poly_width/4, 4.575 - poly_width/4),
           y = c(
             median(comparison_df$acc_certified_control_bespoke_certified_project, na.rm = TRUE),
             median(comparison_df$acc_certified_control_bespoke_certified_project, na.rm = TRUE),
             median(comparison_df$acc_certified_control_certified_project, na.rm = TRUE),
             median(comparison_df$acc_certified_control_certified_project, na.rm = TRUE)
           ),
           fill = "#ef476f") +
  annotate("polygon",
           x = c(4.5 - poly_width/2, 4.5 + poly_width/2,
                 4.5 + poly_width/2, 4.5 - poly_width/2),
           y = c(median(comparison_df$acc_certified_control_certified_project, na.rm = TRUE),
                 median(comparison_df$acc_certified_control_certified_project, na.rm = TRUE),
                 median(comparison_df$certified_control_certified_project, na.rm = TRUE),
                 median(comparison_df$certified_control_certified_project, na.rm = TRUE)),
           fill = "#ef476f") +
  annotate("text", x = 1.5, y = median(comparison_df$pact_control_certified_project), label = paste0("Increase: +", pact_control_certified_project_contribution * 100, "%"),
           size = 4, hjust = 0.5, vjust = -0.5) +
  annotate("text", x = 2.5, y = median(comparison_df$acc_certified_control_certified_project), label = paste0("Increase: +", acc_certified_control_certified_project_contribution * 100, "%"),
           size = 4, hjust = 0.5, vjust = -0.5) +
  annotate("text", x = 3.5, y = median(comparison_df$acc_certified_control_bespoke_certified_project), label = paste0("Increase: +", acc_certified_control_bespoke_certified_project_contribution * 100, "%"),
           size = 4, hjust = 0.5, vjust = -0.5) +
  annotate("text", x = 4.5, y = median(comparison_df$certified_control_certified_project), label = paste0("Increase: +", certified_control_certified_project_contribution_two * 100, "% to +", certified_control_certified_project_contribution_one * 100, "%"),
           size = 4, hjust = 0.5, vjust = -0.5) +
  geom_hline(yintercept = median(comparison_df$pact_control_pact_project, na.rm = TRUE),
             linetype = "dashed", linewidth = 0.75, colour = "#06d6a0") +
  geom_hline(yintercept = median(comparison_df$certified_control_certified_project, na.rm = TRUE),
             linetype = "dashed", linewidth = 0.75, colour = "#ef476f") +
  ylab("Avoided Deforestation Rate (%/year)") +
  xlab("") +
  theme_classic() +
  theme(axis.title.y = element_text(size = 18),
        axis.text.x  = element_text(size = 13),
        axis.text.y  = element_text(size = 13),
        legend.position = "none")

p_labels = c("Combined Over-Crediting Effect **",
             "Isolated Effect:\nRemote sensing of\ndeforestation in project area",
             "Isolated Effect:\nControl area selection",
             "Hypothesised Effect:\nRemote sensing of control area",
             "Inferred Effect:\nEx-ante modelling")

fig5 = fig5 + geom_signif(comparisons = list(c("ACC QE Control\n- ACC QE Project\n(n = 17)",
                                   "Certified Control\n- Certified Project\n(n = 17)"),
                                 c("ACC QE Control\n- Certified Project\n(n = 17)",
                                   "ACC QE Control\n- ACC QE Project\n(n = 17)"),
                                 c("ACC QE Control\n- Certified Project\n(n = 17)",
                                   "ACC to Bespoke Certified Control\n- Certified Project\n(n = 17)"),
                                 c("ACC to Bespoke Certified Control\n- Certified Project\n(n = 17)",
                                   "ACC Certified Control\n- Certified Project\n(n = 17)"),
                                 c("Certified Control\n- Certified Project\n(n = 17)",
                                   "ACC Certified Control\n- Certified Project\n(n = 17)")),
    test = "wilcox.test",  # Wilcoxon test for comparing the distributions
    map_signif_level = FALSE,
    y_position = c(1.6, 1.0, 1.2, 1, 1.4),
    textsize = 5.4, 
    tip_length = 0.01,
    colour = "black",
    annotations = p_labels  
  )

  
ggsave("pngs/fig5_raw.png", fig5, width = 13, height = 10, units = "in", dpi = 500)
fig5

# ----- WILCOXON TESTS ----
wilcox.test(comparison_df$certified_control_certified_project, comparison_df$pact_control_pact_project, paired = T, alternative = "greater")
wilcox.test(comparison_df$pact_control_certified_project, comparison_df$pact_control_pact_project, paired = T, alternative = "greater")
wilcox.test(comparison_df$acc_certified_control_certified_project, comparison_df$pact_control_certified_project, paired = T, alternative = "greater")
wilcox.test(comparison_df$certified_control_certified_project, comparison_df$acc_certified_control_certified_project, paired = T, alternative = "greater")

# ---- PAIRWISE MEDIAN DIFFERENCE ----
median(comparison_df$pact_control_pact_project)
median(comparison_df$certified_control_certified_project)



