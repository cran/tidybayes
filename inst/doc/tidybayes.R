params <-
list(EVAL = TRUE)

## ----chunk_options, include=FALSE-------------------------------------------------------------------------------------
if (requireNamespace("pkgdown", quietly = TRUE) && pkgdown::in_pkgdown()) {
  tiny_width = small_width = med_width = 6.75
  tiny_height = small_height = med_height = 4.5
  large_width = 8
  large_height = 5.25
} else {
  tiny_width = 5.5
  tiny_height = 3 + 2/3
  small_width = med_width = 6.75
  small_height = med_height = 4.5
  large_width = 8
  large_height = 5.25
}

knitr::opts_chunk$set(
  fig.width = small_width,
  fig.height = small_height,
  eval = if (isTRUE(exists("params"))) params$EVAL else FALSE
)
if (capabilities("cairo") && Sys.info()[['sysname']] != "Darwin") {
  knitr::opts_chunk$set(
    dev.args = list(png = list(type = "cairo"))
  )
}
dir.create("models", showWarnings = FALSE)

## ----setup, message = FALSE, warning = FALSE--------------------------------------------------------------------------
library(magrittr)
library(dplyr)
library(forcats)
library(modelr)
library(ggdist)
library(tidybayes)
library(ggplot2)
library(cowplot)
library(emmeans)
library(broom)
library(rstan)
library(rstanarm)
library(brms)
library(bayesplot)
library(RColorBrewer)

theme_set(theme_tidybayes() + panel_border())

## ---- eval=FALSE------------------------------------------------------------------------------------------------------
#  rstan_options(auto_write = TRUE)
#  options(mc.cores = parallel::detectCores())

## ----hidden_options, include=FALSE------------------------------------------------------------------------------------
# While the previous code chunk is the actual recommended approach,
# CRAN vignette building policy limits us to 2 cores, so we use at most
# 2 to build this vignette (but show the previous chunk to
# the reader as a best pratice example)
rstan_options(auto_write = TRUE)
options(mc.cores = 1) #min(2, parallel::detectCores()))

options(width = 120)

## ---------------------------------------------------------------------------------------------------------------------
set.seed(5)
n = 10
n_condition = 5
ABC =
  tibble(
    condition = rep(c("A","B","C","D","E"), n),
    response = rnorm(n * 5, c(0,1,2,1,-1), 0.5)
  )

## ---------------------------------------------------------------------------------------------------------------------
head(ABC, 10)

## ----fig.width = tiny_width, fig.height = tiny_height-----------------------------------------------------------------
ABC %>%
  ggplot(aes(x = response, y = fct_rev(condition))) +
  geom_point(alpha = 0.5) +
  ylab("condition")

## ---------------------------------------------------------------------------------------------------------------------
compose_data(ABC)

## ----message = FALSE, results = 'hide'--------------------------------------------------------------------------------
m = sampling(ABC_stan, data = compose_data(ABC), control = list(adapt_delta = 0.99))

## ---------------------------------------------------------------------------------------------------------------------
m

## ---------------------------------------------------------------------------------------------------------------------
str(rstan::extract(m))

## ---------------------------------------------------------------------------------------------------------------------
m %>%
  spread_draws(condition_mean[condition]) %>%
  head(10)

## ---------------------------------------------------------------------------------------------------------------------
m %>%
  recover_types(ABC) %>%
  spread_draws(condition_mean[condition]) %>%
  head(10)

## ---------------------------------------------------------------------------------------------------------------------
m %<>% recover_types(ABC)

## ---------------------------------------------------------------------------------------------------------------------
m %>%
  spread_draws(overall_mean, response_sd) %>%
  head(10)

## ---------------------------------------------------------------------------------------------------------------------
m %>%
  spread_draws(overall_mean, response_sd) %>%
  median_qi(overall_mean, response_sd)

## ---------------------------------------------------------------------------------------------------------------------
m %>%
  spread_draws(overall_mean, response_sd) %>%
  median_qi()

## ---------------------------------------------------------------------------------------------------------------------
m %>%
  spread_draws(condition_mean[condition]) %>%
  median_qi()

## ---------------------------------------------------------------------------------------------------------------------
m %>%
  spread_draws(condition_mean[condition]) %>%
  group_by(condition) %>%    # this line not necessary (done automatically by spread_draws)
  median_qi(condition_mean)

## ---------------------------------------------------------------------------------------------------------------------
m %>%
  spread_draws(condition_mean[condition]) %>%
  summarise_draws()

## ----fig.width = tiny_width, fig.height = tiny_height-----------------------------------------------------------------
m %>%
  spread_draws(condition_mean[condition]) %>%
  ggplot(aes(y = fct_rev(condition), x = condition_mean)) +
  stat_pointinterval()

## ----fig.width = tiny_width, fig.height = tiny_height-----------------------------------------------------------------
m %>%
  spread_draws(condition_mean[condition]) %>%
  ggplot(aes(y = fct_rev(condition), x = condition_mean)) +
  stat_halfeye(.width = c(.90, .5))

## ----fig.width = tiny_width, fig.height = tiny_height-----------------------------------------------------------------
m %>%
  spread_draws(condition_mean[condition]) %>%
  ggplot(aes(y = fct_rev(condition), x = condition_mean, fill = after_stat(abs(x) < .8))) +
  stat_halfeye() +
  geom_vline(xintercept = c(-.8, .8), linetype = "dashed") +
  scale_fill_manual(values = c("gray80", "skyblue"))

## ---------------------------------------------------------------------------------------------------------------------
m %>%
  spread_draws(condition_mean[condition]) %>%
  median_qi(.width = c(.95, .8, .5))

## ----fig.width = tiny_width, fig.height = tiny_height-----------------------------------------------------------------
m %>%
  spread_draws(condition_mean[condition]) %>%
  median_qi(.width = c(.95, .66)) %>%
  ggplot(aes(
    y = fct_rev(condition), x = condition_mean, xmin = .lower, xmax = .upper,
    # size = -.width means smaller probability interval => thicker line
    # this can be omitted, geom_pointinterval includes it automatically
    # if a .width column is in the input data.
    linewidth = -.width
  )) +  
  geom_pointinterval()

## ----fig.width = tiny_width, fig.height = tiny_height-----------------------------------------------------------------
m %>%
  spread_draws(condition_mean[condition]) %>%
  ggplot(aes(x = condition_mean, y = fct_rev(condition))) +
  stat_dotsinterval(quantiles = 100)

## ---------------------------------------------------------------------------------------------------------------------
set.seed(123)
multimodal_draws = tibble(
  x = c(rnorm(5000, 0, 1), rnorm(2500, 4, 1))
)

## ---------------------------------------------------------------------------------------------------------------------
multimodal_draws %>%
  mode_hdi(x, .width = .80)

## ----fig.width = large_width, fig.height = large_height/2-------------------------------------------------------------
multimodal_draws %>%
  ggplot(aes(x = x)) +
  stat_slab(aes(y = 0)) +
  stat_pointinterval(aes(y = -0.5), point_interval = median_qi, .width = c(.95, .80)) +
  annotate("text", label = "median, 80% and 95% quantile intervals", x = 6, y = -0.5, hjust = 0, vjust = 0.3) +
  stat_pointinterval(aes(y = -0.25), point_interval = mode_hdi, .width = c(.95, .80)) +
  annotate("text", label = "mode, 80% and 95% highest-density intervals", x = 6, y = -0.25, hjust = 0, vjust = 0.3) +
  xlim(-3.25, 18) +
  scale_y_continuous(breaks = NULL)

## ---------------------------------------------------------------------------------------------------------------------
m %>% 
  spread_draws(overall_mean, condition_mean[condition]) %>%
  head(10)

## ---------------------------------------------------------------------------------------------------------------------
m %>%
  spread_draws(overall_mean, condition_mean[condition]) %>%
  mutate(condition_offset = condition_mean - overall_mean) %>%
  median_qi(condition_offset)

## ----fig.width = tiny_width, fig.height = tiny_height-----------------------------------------------------------------
m %>%
  spread_draws(condition_mean[condition], response_sd) %>%
  mutate(y_rep = rnorm(n(), condition_mean, response_sd)) %>%
  median_qi(y_rep, .width = c(.95, .8, .5)) %>%
  ggplot(aes(y = fct_rev(condition), x = y_rep)) +
  geom_interval(aes(xmin = .lower, xmax = .upper)) + #auto-sets aes(color = fct_rev(ordered(.width)))
  geom_point(aes(x = response), data = ABC) +
  scale_color_brewer()

## ----fig.width = tiny_width, fig.height = tiny_height-----------------------------------------------------------------
draws = m %>%
  spread_draws(condition_mean[condition], response_sd)

reps = draws %>%
  mutate(y_rep = rnorm(n(), condition_mean, response_sd))

ABC %>%
  ggplot(aes(y = condition)) +
  stat_interval(aes(x = y_rep), .width = c(.95, .8, .5), data = reps) +
  stat_pointinterval(aes(x = condition_mean), .width = c(.95, .66), position = position_nudge(y = -0.3), data = draws) +
  geom_point(aes(x = response)) +
  scale_color_brewer()

## ----fig.width = tiny_width, fig.height = tiny_height-----------------------------------------------------------------
m %>%
  spread_draws(condition_mean[condition]) %>%
  compare_levels(condition_mean, by = condition) %>%
  ggplot(aes(y = condition, x = condition_mean)) +
  stat_halfeye()

## ---------------------------------------------------------------------------------------------------------------------
m %>%
  gather_draws(overall_mean, condition_mean[condition]) %>%
  median_qi()

## ---------------------------------------------------------------------------------------------------------------------
m %>%
  spread_draws(overall_mean, condition_mean[condition]) %>%
  mutate(condition_offset = condition_mean - overall_mean) %>%
  gather_variables() %>%
  median_qi()

## ---------------------------------------------------------------------------------------------------------------------
m %>%
  tidy_draws() %>%
  head(10)

## ---------------------------------------------------------------------------------------------------------------------
m %>%
  tidy_draws() %>%
  gather_variables() %>%
  head(10)

## ---------------------------------------------------------------------------------------------------------------------
m %>%
  spread_draws(`condition_.*`[condition], regex = TRUE) %>%
  head(10)

## ----m_mpg_brms, results = "hide", message = FALSE, warning = FALSE, cache = TRUE-------------------------------------
m_mpg = brm(
  mpg ~ hp * cyl, 
  data = mtcars, 
  
  file = "models/tidybayes_m_mpg.rds"  # cache model (can be removed)
)

## ---------------------------------------------------------------------------------------------------------------------
mtcars %>%
  group_by(cyl) %>%
  data_grid(hp = seq_range(hp, n = 51)) %>%
  add_epred_draws(m_mpg) %>%
  ggplot(aes(x = hp, y = mpg, color = ordered(cyl))) +
  stat_lineribbon(aes(y = .epred)) +
  geom_point(data = mtcars) +
  scale_fill_brewer(palette = "Greys") +
  scale_color_brewer(palette = "Set2")

## ---------------------------------------------------------------------------------------------------------------------
mtcars %>%
  group_by(cyl) %>%
  data_grid(hp = seq_range(hp, n = 101)) %>%
  # NOTE: this shows the use of ndraws to subsample within add_epred_draws()
  # ONLY do this IF you are planning to make spaghetti plots, etc.
  # NEVER subsample to a small sample to plot intervals, densities, etc.
  add_epred_draws(m_mpg, ndraws = 100) %>%
  ggplot(aes(x = hp, y = mpg, color = ordered(cyl))) +
  geom_line(aes(y = .epred, group = paste(cyl, .draw)), alpha = .1) +
  geom_point(data = mtcars) +
  scale_color_brewer(palette = "Dark2")

## ---------------------------------------------------------------------------------------------------------------------
m_linear = lm(response ~ condition, data = ABC)

## ---------------------------------------------------------------------------------------------------------------------
linear_results = m_linear %>% 
  emmeans(~ condition) %>% 
  tidy(conf.int = TRUE) %>%
  mutate(model = "OLS")

linear_results

## ---------------------------------------------------------------------------------------------------------------------
bayes_results = m %>%
  spread_draws(condition_mean[condition]) %>%
  median_qi(estimate = condition_mean) %>%
  to_broom_names() %>%
  mutate(model = "Bayes")

bayes_results

## ----fig.width = tiny_width, fig.height = tiny_height-----------------------------------------------------------------
bind_rows(linear_results, bayes_results) %>%
  mutate(condition = fct_rev(condition)) %>%
  ggplot(aes(y = condition, x = estimate, xmin = conf.low, xmax = conf.high, color = model)) +
  geom_pointinterval(position = position_dodge(width = .3))

## ---------------------------------------------------------------------------------------------------------------------
m %>%
  spread_draws(condition_mean[condition]) %>%
  compare_levels(condition_mean, by = condition) %>%
  head(10)

## ---------------------------------------------------------------------------------------------------------------------
m %>%
  spread_draws(condition_mean[condition]) %>%
  compare_levels(condition_mean, by = condition) %>%
  unspread_draws(condition_mean[condition]) %>%
  head(10)

## ----fig.width = tiny_width, fig.height = tiny_height-----------------------------------------------------------------
m %>%
  spread_draws(condition_mean[condition]) %>%
  compare_levels(condition_mean, by = condition) %>%
  unspread_draws(condition_mean[condition], drop_indices = TRUE) %>%
  bayesplot::mcmc_areas()

## ----results = "hide", message = FALSE--------------------------------------------------------------------------------
m_rst = stan_glm(response ~ condition, data = ABC)

## ---------------------------------------------------------------------------------------------------------------------
m_rst %>%
  emmeans( ~ condition) %>%
  gather_emmeans_draws() %>%
  median_qi()

## ---------------------------------------------------------------------------------------------------------------------
m_rst %>%
  emmeans( ~ condition) %>%
  contrast(method = "pairwise") %>%
  gather_emmeans_draws() %>%
  median_qi()

## ----fig.width = tiny_width, fig.height = tiny_height-----------------------------------------------------------------
m_rst %>%
  emmeans( ~ condition) %>%
  contrast(method = "pairwise") %>%
  gather_emmeans_draws() %>%
  ggplot(aes(x = .value, y = contrast)) +
  stat_halfeye()

## ---------------------------------------------------------------------------------------------------------------------
m_rst %>%
  emmeans(pairwise ~ condition) %>%
  gather_emmeans_draws() %>%
  median_qi()

