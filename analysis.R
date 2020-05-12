## @knitr analysis-setup
library(ggplot2)
library(magrittr)
individual <- readr::read_csv(here::here("data", "individual.csv")) %>%
  dplyr::select(stemDiameter, height, growthForm)
summary(individual)

## @knitr analysis-filter-data
anaylsis_df <- individual %>%
  dplyr::filter(!is.na(growthForm), growthForm != "liana")

## @knitr analysis-set-factor-levels
gf_levels <- table(anaylsis_df$growthForm) %>%
  sort() %>%
  names()
anaylsis_df %<>%
  dplyr::mutate(growthForm = factor(growthForm, levels = gf_levels))

## @knitr analysis-fig1-barplot
anaylsis_df %>%
  ggplot(aes(y = growthForm, colour = growthForm, fill = growthForm)) +
  geom_bar(alpha = 0.5, show.legend = FALSE)

## @knitr fig2-violin-plot
anaylsis_df %>%
  tidyr::pivot_longer(cols = c(stemDiameter, height),
                      names_to = "var",
                      values_to = "value") %>%
  ggplot(aes(x = log(value), y = growthForm, colour = growthForm,
             fill = growthForm)) +
  geom_violin(alpha = 0.5, trim = TRUE, show.legend = FALSE) +
  geom_boxplot(alpha = 0.7, show.legend = FALSE) +
  facet_grid(~var)

## @knitr analysis-lm-overall
library(broom)
lm_overall <- lm(log(stemDiameter) ~ log(height), data = anaylsis_df)
lm_overall %>%
  broom::glance()
lm_overall %>%
  broom::tidy()

## @knitr analysis-lm-fig3-overall
anaylsis_df %>%
  ggplot(aes(x = log(height), y = log(stemDiameter))) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm")


## @knitr analysis-growth
lm_growth <- lm(log(stemDiameter) ~ log(height) * growthForm, data = anaylsis_df)
lm_growth %>%
  broom::glance()
lm_growth %>%
  broom::tidy()

## @knitr analysis-lm-fig4-growth
anaylsis_df %>%
  ggplot(aes(x = log(height), y = log(stemDiameter), colour = growthForm)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm")


