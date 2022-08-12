# summarize_simulated_data.R ----------------------------------------------
#
# Produce summaries of simulated datasets.

library(arrow)
library(dplyr)
library(ggdist)
library(ggplot2)
library(ggrepel)
library(patchwork)

summarize_simulated_data <- function(data_path) {
  
  data_path <- "/poppy/data/derived_data/ascherli/output/selbias/simulated_data"
  
  sim_data <- arrow::open_dataset(data_path)
  
  params <- sim_data %>%
    select(simulation, unit_var, time_var, policy_speed, effect_magnitude,
           effect_direction, prior_control, bias_type, bias_size, 
           n_implementation_periods) %>%
    collect() %>%
    unique()
  
  
  sim41 <- sim_data %>% filter(simulation==41) %>% collect()
  sim41it1 <- sim41 %>% filter(iteration==1)
  
  # plot outcomes over time
  
  p1 <- sim41 %>%
    filter(iteration <= 5) %>% 
    mutate(treated = factor(treatment > 0)) %>% 
    ggplot(aes(x = year, y = crude.rate.old, group = state, 
               color = treated, alpha = treated)) + 
    scale_alpha_manual(values = c(0.1, 1)) + 
    scale_color_manual(values = c('black', 'steelblue')) + 
    scale_y_continuous(limits = c(0, 45)) + 
    geom_line() + 
    theme_minimal() + 
    facet_wrap(facets = 'iteration', ncol = 1)
  
  p2 <- sim41 %>%
    filter(iteration <= 5) %>% 
    mutate(treated = factor(treatment > 0)) %>% 
    ggplot(aes(x = year, y = crude.rate, group = state, 
               color = treated, alpha = treated)) + 
    scale_alpha_manual(values = c(0.1, 1)) + 
    scale_color_manual(values = c('black', 'steelblue')) + 
    scale_y_continuous(limits = c(0, 45)) + 
    geom_line() + 
    theme_minimal() + 
    facet_wrap(facets = 'iteration', ncol = 1)
  
  p3 <- sim41 %>%
    filter(iteration <= 5) %>% 
    mutate(treated = factor(treatment > 0)) %>% 
    ggplot(aes(x = year, y = crude.rate - crude.rate.old, 
               group = state, color = treated, alpha = treated)) + 
    scale_alpha_manual(values = c(0.1, 1)) + 
    scale_color_manual(values = c('black', 'steelblue')) + 
    geom_line() + 
    theme_minimal() + 
    facet_wrap(facets = 'iteration', ncol = 1)

  p <- p1 + p3 + plot_layout(guides = 'collect') 
  ggsave('/poppy/data/derived_data/ascherli/output/selbias/augmented_vs_unaugmented_outcomes.png',
         width = 8, height = 8)
  
  
  # scatterplot of outcomes and unemployment rate
  
  cor(sim41it1$crude.rate.old, sim41it1$unemploymentrate)
  
  p1 <- sim41 %>%
    filter(iteration <= 3) %>% 
    mutate(treated = factor(treatment > 0)) %>% 
    ggplot(aes(x = unemploymentrate, y = crude.rate.old, 
               color = treated, alpha = treated)) + 
    scale_alpha_manual(values = c(0.05, 1)) + 
    scale_color_manual(values = c('black', 'steelblue')) + 
    scale_y_continuous(limits = c(0, 45)) + 
    geom_point() + 
    theme_minimal() + 
    facet_wrap(facets = 'iteration', ncol = 1)
  
  cor(sim41it1$crude.rate, sim41it1$unemploymentrate) 
  
  p2 <- sim41 %>%
    filter(iteration <= 3) %>% 
    mutate(treated = factor(treatment > 0)) %>% 
    ggplot(aes(x = unemploymentrate, y = crude.rate, 
               color = treated, alpha = treated)) + 
    scale_alpha_manual(values = c(0.05, 1)) + 
    scale_color_manual(values = c('black', 'steelblue')) + 
    scale_y_continuous(limits = c(0, 45)) + 
    geom_point() + 
    theme_minimal() + 
    facet_wrap(facets = 'iteration', ncol = 1) 
  
  p <- p1 + p2 + plot_layout(guides = 'collect') 
  
  ggsave('/poppy/data/derived_data/ascherli/output/selbias/augmented_vs_unaugmented_outcomes_vs_unemp.png',
         width = 8, height = 8)

  
  # scatterplot of outcomes and prior control
  
  cor(sim41it1$crude.rate.old, sim41it1$prior_control_mva3_OLD, use='complete.obs')
  
  p1 <- sim41 %>%
    filter(iteration <= 3) %>% 
    mutate(treated = factor(treatment > 0)) %>% 
    ggplot(aes(x = prior_control_mva3_OLD, y = crude.rate.old, 
               color = treated, alpha = treated)) + 
    scale_alpha_manual(values = c(0.05, 1)) + 
    scale_color_manual(values = c('black', 'steelblue')) + 
    scale_y_continuous(limits = c(0, 45)) + 
    geom_point() + 
    theme_minimal() + 
    facet_wrap(facets = 'iteration', ncol = 1)
  
  cor(sim41it1$crude.rate, sim41it1$prior_control_mva3_OLD, use='complete.obs')
  
  p2 <- sim41 %>%
    filter(iteration <= 3) %>% 
    mutate(treated = factor(treatment > 0)) %>% 
    ggplot(aes(x = prior_control_mva3_OLD, y = crude.rate, 
               color = treated, alpha = treated)) + 
    scale_alpha_manual(values = c(0.05, 1)) + 
    scale_color_manual(values = c('black', 'steelblue')) + 
    scale_y_continuous(limits = c(0, 40)) + 
    geom_point() + 
    theme_minimal() + 
    facet_wrap(facets = 'iteration', ncol = 1) 
  
  p <- p1 + p2 + plot_layout(guides = 'collect') 
  
  ggsave('/poppy/data/derived_data/ascherli/output/selbias/augmented_vs_unaugmented_outcomes_vs_mva3.png',
         width = 8, height = 8)
  
  
  # summaries by treatment status
  p1 <- sim41 %>% 
    mutate(treated = factor(treatment > 0)) %>% 
    ggplot(aes(x = year, y = crude.rate)) +
    stat_lineribbon() +
    scale_fill_brewer() +
    theme_minimal() + 
    facet_wrap(facets = 'treated', ncol = 2)
  
  p2 <- sim41 %>% 
    mutate(treated = factor(treatment > 0)) %>% 
    ggplot(aes(x = unemploymentrate,
               fill = stat(cut_cdf_qi(cdf)))) +
    stat_halfeye() + 
    scale_fill_brewer() +
    theme_minimal() + 
    facet_wrap(facets = 'treated', ncol = 2)
  
  # number of states treated
  sim41 %>%
    group_by(iteration, state) %>%
    summarize(treated = any(treatment > 0),
              .groups = 'drop') %>%
    group_by(iteration) %>%
    summarize(n_treated = sum(treated),
              .groups = 'drop') %>%
    ggplot(aes(x = n_treated,
           fill = stat(cut_cdf_qi(cdf)))) +
    stat_halfeye() + 
    scale_fill_brewer() +
    theme_minimal()
  
  sim_data %>%
    select(simulation, iteration, state, treatment,
           effect_magnitude, prior_control, bias_type, bias_size) %>%
    collect() %>% 
    mutate(
      simulation = factor(simulation, levels = 60:1), 
      magnitude_control_type_size = paste0(
        round(effect_magnitude,3), "-", prior_control, "-", bias_type, "-", bias_size
      )) %>%
    group_by(simulation, magnitude_control_type_size, iteration, state) %>%
    summarize(treated = any(treatment > 0),
              .groups = 'drop') %>%
    group_by(simulation, magnitude_control_type_size, iteration) %>%
    summarize(n_treated = sum(treated),
              .groups = 'drop') %>%
    ggplot(aes(y = simulation,
               x = n_treated,
               fill = stat(cut_cdf_qi(cdf)))) +
    stat_halfeye() + 
    scale_fill_brewer() +
    theme_minimal()
  
    
}