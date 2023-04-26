## DAG

status_colors <- c(exposure = renoir[2], outcome = renoir[8], latent = "grey50")

ponv_dag <- dagify(
  ponv ~ age + ponv_history + opioids + female_risk + volatile + procedure + 
    case_type + case_duration + year + asa_class + race_ethnicity + prophylaxis + 
    practice_patterns,
  practice_patterns ~ hospital + clinician,
  risk_score ~ age + ponv_history + opioids + female_risk + volatile + procedure,
  prophylaxis ~ risk_score + case_type + year + asa_class + race_ethnicity + case_duration + 
    clinician + hospital,
  exposure = "prophylaxis",
  outcome = "ponv",
  coords = list(x = c(prophylaxis = 1, opioids = 5, female_risk = 7, volatile = 9, procedure = 11, age = 13, ponv_history = 15,
                      ponv = 19,
                      risk_score = 5,
                      case_duration = 6, case_type = 8, year = 10, asa_class = 12, race_ethnicity = 14,
                      hospital = 9, clinician = 9, practice_patterns = 12),
                y = c(prophylaxis = 3, opioids = 5, female_risk = 5, volatile = 5, procedure = 5, age = 5, ponv_history = 5,
                      ponv = 3,
                      risk_score = 4,
                      case_duration = 1, case_type = 1, year = 1, asa_class = 1, race_ethnicity = 1,
                      hospital = 2.25, clinician = 2.75, practice_patterns = 2.5)),
  labels = c(ponv = "PONV", age = "Age 3+ Years", ponv_history = "PONV History", opioids = "Long-Acting Opioids",
             female_risk = "Post-Pubertal Female", volatile = "Volatile 30+ Minutes", procedure = "High Risk Procedure",
             prophylaxis = "PONV Prophylaxis", risk_score = "Guideline Risk Score",
             case_type = "Case Type", year = "Year", "asa_class" = "ASA Class", race_ethnicity = "Race",
             case_duration = "Case Duration",
             clinician = "Clinician", hospital = "Hospital", practice_patterns = "Practice Patterns"))

# Turn DAG into a tidy data frame for plotting
ponv_dag_tidy <- ponv_dag %>% 
  tidy_dagitty() %>%
  node_status()   # Add column for exposure/outcome/latent

# Fancier graph
pcrc_dag <- 
  ggplot(ponv_dag_tidy, aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_edges() +
    geom_dag_point(aes(color = status)) +
    geom_label(aes(label = label, fill = status),
               color = "white", fontface = "bold", nudge_y = -0.25,
               size = 5) +
    scale_color_manual(values = status_colors, na.value = "grey20") +
    scale_fill_manual(values = status_colors, na.value = "grey20") +
    guides(color = "none", fill = "none") +
    theme_dag()

## Previous Study Plots

load("~/Desktop/projects/mpog_ponv/data/aa_revision_dataframes_03.08.2023.RData")
load("~/Desktop/projects/mpog_ponv/data/aa_revision_all_models_03.17.2023.RData")
load("~/Desktop/projects/mpog_ponv/data/arr_plot_data.RData")

# OR Plot

or_outcomes <- 
  unweighted_unadjusted %>%
  tidy_draws() %>%
  mutate(or = exp(b_mpog_04_passed1), model = "uwua") %>%
  select(or, model) %>%
  rbind(unweighted_adjusted %>%
          tidy_draws() %>%
          mutate(or = exp(b_mpog_04_passed1), model = "uwa") %>%
          select(or, model)) %>%
  rbind(unadjusted_brms %>%
          tidy_draws() %>%
          mutate(or = exp(b_mpog_04_passed1), model = "wua") %>%
          select(or, model)) %>%
  rbind(adjusted_brms %>%
          tidy_draws() %>%
          mutate(or = exp(b_mpog_04_passed1), model = "wa") %>%
          select(or, model))

or_plot <- 
  ggplot(or_outcomes) +
  stat_halfeye(aes(x = or, y = factor(model, levels = c(c("uwua", "uwa", "wua", "wa"))),
                   fill = factor(model), alpha = stat(x) > 1),
               .width = c(0.66, 0.95),
               shape = 21,
               interval_size_range = c(1.2, 2.2),
               fatten_point = 1.75,
               point_color = "white",
               point_fill = "black",
               interval_color = "black",
               stroke = 0.4) + 
  geom_vline(xintercept = 1, linetype = "dashed") +
  annotate(geom = "label", x = 0.45, y = 1.5, label = "Unweighted & Unadjusted",
           fill = colorspace::lighten(d3[2], 0.1), color = "white", hjust = 0,
           size = 3) +
  annotate(geom = "label", x = 0.45, y = 2.5, label = "Unweighted & Adjusted", 
           fill = colorspace::lighten(d3[1], 0.1), color = "white", hjust = 0,
           size = 3) +
  annotate(geom = "label", x = 0.45, y = 3.5, label = "Weighted & Unadjusted", 
           fill = colorspace::lighten(d3[4], 0.1), color = "white", hjust = 0,
           size = 3) +
  annotate(geom = "label", x = 0.45, y = 4.5, label = "Weighted & Adjusted", 
           fill = colorspace::lighten(d3[3], 0.1), color = "white", hjust = 0,
           size = 3) +
  labs(x = "Odds Ratio for Appropriate PONV Prophylaxis",
       y = NULL,
       title = "") + 
  scale_fill_manual(name = NULL, breaks = c("uwua", "uwa", "wua", "wa"),
                    labels = c("Unweighted & Unadjusted",
                               "Unweighted & Adjusted", 
                               "Overlap Weighted & Unadjusted",
                               "Overlap Weighted & Adjusted"),
                    guide = "none",
                    values = c(d3[2], d3[1], d3[4], d3[3])) +
  scale_alpha_manual(values = c(1, 0.5), guide = "none") +
  coord_cartesian(xlim = c(0.45, 1.1)) + 
  scale_x_continuous(breaks = c(0.5, 0.6, 0.7, 0.8, 0.9, 1, 1.1)) + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold",
                                    margin = margin(20, 0, 0, 0)))

# Interaction Plot

summary_counts <-
  analysis_model %>%
  group_by(mpog_04_sum_five) %>%
  summarize(count = n())

count_labels <- paste0(
  paste0(c("1\n(n = ", "2\n(n = ", "3\n(n = ", "4\n(n = ", "5+\n(n = "), summary_counts$count),
  ")")

data_grid <- expand_grid(mpog_04_sum_five = c(1:5),
                         mpog_04_passed = c(0, 1))

draws_unweighted <- unweighted_int_score %>%
  epred_draws(data_grid) %>%
  mutate(model = "unweighted")

draws_weighted <- int_brms %>%
  epred_draws(data_grid) %>%
  mutate(model = "weighted")

draws_combined <- 
  bind_rows(draws_unweighted, draws_weighted)

## Calculate posterior comparisons 
unweighted_passed <- 
  unweighted_int_score %>% 
  epred_draws(data_grid %>% filter(mpog_04_passed == 1)) %>%
  rename(passed = .epred) %>%
  ungroup() %>%
  select(-mpog_04_passed, -.chain, -.iteration)

unweighted_failed <- 
  unweighted_int_score %>% 
  epred_draws(data_grid %>% filter(mpog_04_passed == 0)) %>%
  rename(failed = .epred) %>%
  ungroup() %>%
  select(-mpog_04_passed, -.chain, -.iteration)

unweighted_all <-
  left_join(unweighted_passed, unweighted_failed,
            by = c("mpog_04_sum_five", ".row", ".draw")) %>%
  mutate(model = "unweighted")

weighted_passed <- 
  int_brms %>% 
  epred_draws(data_grid %>% filter(mpog_04_passed == 1)) %>%
  rename(passed = .epred) %>%
  ungroup() %>%
  select(-mpog_04_passed, -.chain, -.iteration)

weighted_failed <- 
  int_brms %>% 
  epred_draws(data_grid %>% filter(mpog_04_passed == 0)) %>%
  rename(failed = .epred) %>%
  ungroup() %>%
  select(-mpog_04_passed, -.chain, -.iteration)

weighted_all <-
  left_join(weighted_passed, weighted_failed,
            by = c("mpog_04_sum_five", ".row", ".draw")) %>%
  mutate(model = "weighted")

full_model <-
  bind_rows(unweighted_all, weighted_all) %>%
  mutate(diff = failed - passed)

model_labs <- c("Unweighted", "Weighted")
names(model_labs) <- c("unweighted", "weighted")

interaction_plot <- 
  ggplot() +
  stat_pointinterval(data = draws_combined,
                     aes(x = factor(mpog_04_sum_five), y = .epred,
                         point_fill = factor(mpog_04_passed),
                         interval_color = factor(mpog_04_passed)),
                     .width = c(0.66, 0.95),
                     shape = 21,
                     interval_size_range = c(1.8, 3),
                     fatten_point = 1.5,
                     point_color = "white",
                     position = position_dodge(0.5)) + 
  facet_wrap(~ model,
             labeller = labeller(model = model_labs)) + 
  labs(x = "Sum of PONV Risk Factors",
       y = "Probability of PONV") + 
  scale_discrete_manual(aesthetics = c("point_fill", "interval_color"),
                        name = "Prophylaxis Status",
                        breaks = c(0, 1),
                        labels = c("Inadequate", "Adequate"),
                        values = c("#BC3C29FF", "#0072B5FF")) + 
  scale_x_discrete(labels = count_labels) + 
  scale_y_continuous(breaks = seq(0, 0.5, by = 0.1)) +
  coord_cartesian(ylim = c(0, 0.5)) + 
  theme(axis.title.x = element_text(size = 28,
                                    margin = margin(15, 0, 0, 0)),
        axis.title.y = element_text(size = 28,
                                    margin = margin(0, 15, 0, 0)),
        axis.text = element_text(size = 25),
        legend.text = element_text(size = 18, face = "bold"),
        legend.title = element_text(size = 18, face = "bold"),
        legend.position = c(0.1, 0.89),
        legend.background = element_rect(fill = "white",
                                         color = "black",
                                         size = 0.2),
        strip.text = element_text(size = 28, face = "bold"),
        strip.background = element_blank(),
        panel.spacing = unit(2, "lines"),
        panel.grid.major.x = element_blank(),
        panel.background = element_rect(fill = NA, color = "black"),
        plot.margin = margin(15, 15, 15, 15))

interaction_plot_arr <-
  ggplot() +
  stat_pointinterval(data = full_model,
                     aes(x = factor(mpog_04_sum_five), y = diff),
                     .width = c(0.66, 0.95),
                     shape = 21,
                     interval_size_range = c(1.8, 3),
                     fatten_point = 1.5,
                     point_color = "black",
                     point_fill = "black",
                     position = position_dodge(0.5)) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  facet_wrap(~ model,
             labeller = labeller(model = model_labs)) + 
  labs(x = "Sum of PONV Risk Factors",
       y = "Absolute Risk Reduction") + 
  scale_x_discrete(labels = count_labels) + 
  coord_cartesian(ylim = c(-0.3, 0.3)) +
  scale_y_continuous(breaks = c(-0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3),
                     labels = c("-30%", "-20%", "-10%", "0%", "10%", "20%", "30%")) + 
  theme(axis.title.x = element_text(size = 28,
                                    margin = margin(15, 0, 0, 0)),
        axis.title.y = element_text(size = 28,
                                    margin = margin(0, 15, 0, 0)),
        axis.text = element_text(size = 25),
        legend.position = c(0.09, 0.9),
        legend.background = element_rect(fill = "white",
                                         color = "black",
                                         size = 0.2),
        strip.text = element_text(size = 28, face = "bold"),
        strip.background = element_blank(),
        panel.spacing = unit(2, "lines"),
        panel.grid.major.x = element_blank(),
        panel.background = element_rect(fill = NA, color = "black"),
        plot.caption = element_text(face = "italic", hjust = 0,
                                    size = 14, margin = margin(10, 0, 0, 0)),
        plot.margin = margin(15, 15, 15, 15))

interaction_grid <- plot_grid(interaction_plot, interaction_plot_arr, ncol = 1,
                              labels = c("A", "B"),
                              label_size = 35)

# Patient-Specific Disributions

arr_plot <-
  outcomes_summary %>% 
  mutate(column_label = as.factor(column_label)) %>%
  ggplot(aes(x = fct_reorder(column_label, y, min))) +
  geom_linerange(aes(ymin = ymin, ymax = ymax),
                 linewidth = 0.04,
                 alpha = 0.5) + 
  geom_hline(yintercept = 0, color = "red",
             linetype = "dashed") +
  geom_point(aes(y = y), 
             color = d3[3],
             alpha = 1, size = 0.05) +
  scale_y_continuous(limits = c(-0.02, 0.12),
                     breaks = c(-0.02, 0, 0.02, 0.04, 0.06, 0.08, 0.1, 0.12),
                     labels = c("-2%", "0%", "2%", "4%", "6%", "8%", "10%", "12%")) +
  labs(x = "Individual Study Patients (n = 14,747; arranged in ascending order of median ARR)",
       y = "Covariate-Adjusted Absolute Risk Reduction") + 
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(face = "bold", size = 10, margin = margin(12, 0, 0, 0)),
        axis.title.y = element_text(face = "bold", size = 10, margin = margin(12, 10, 0, 0)),
        strip.text = element_text(size = 12, face = "bold")) 
