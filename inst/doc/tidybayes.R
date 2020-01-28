params <-
list(EVAL = TRUE)

## ----chunk_options, include=FALSE-------------------------------------------------------------------------------------
knitr::opts_chunk$set(
  fig.width = 4.5,
  fig.height = 3,
  eval = if (isTRUE(exists("params"))) params$EVAL else FALSE
)
if (capabilities("cairo")) {
  knitr::opts_chunk$set(
    dev.args = list(png = list(type = "cairo"))
  )
}

## ----setup, message = FALSE, warning = FALSE--------------------------------------------------------------------------
library(magrittr)
library(dplyr)
library(forcats)
library(ggplot2)
library(ggstance)
library(emmeans)
library(broom)
library(rstan)
library(rstanarm)
library(brms)
library(modelr)
library(bayesplot)
library(MCMCglmm)
library(tidybayes)
library(cowplot)
library(RColorBrewer)

theme_set(theme_tidybayes() + panel_border())

## ---- eval=FALSE------------------------------------------------------------------------------------------------------
#  rstan_options(auto_write = TRUE)
#  options(mc.cores = parallel::detectCores())

## ----hidden_options, include=FALSE----------------------------------------------------------------
# While the previous code chunk is the actual recommended approach,
# CRAN vignette building policy limits us to 2 cores, so we use at most
# 2 to build this vignette (but show the previous chunk to
# the reader as a best pratice example)
rstan_options(auto_write = TRUE)
options(mc.cores = min(2, parallel::detectCores()))

options(width = 100)

## -------------------------------------------------------------------------------------------------
set.seed(5)
n = 10
n_condition = 5
ABC =
  tibble(
    condition = rep(c("A","B","C","D","E"), n),
    response = rnorm(n * 5, c(0,1,2,1,-1), 0.5)
  )

## -------------------------------------------------------------------------------------------------
head(ABC, 10)

## -------------------------------------------------------------------------------------------------
ABC %>%
  ggplot(aes(x = response, y = fct_rev(condition))) +
  geom_point(alpha = 0.5) +
  ylab("condition")

## -------------------------------------------------------------------------------------------------
compose_data(ABC)

## -------------------------------------------------------------------------------------------------
m = sampling(ABC_stan, data = compose_data(ABC), control = list(adapt_delta=0.99))

## -------------------------------------------------------------------------------------------------
print(m, pars = c("overall_mean", "condition_mean_sd", "condition_mean", "response_sd"))

## -------------------------------------------------------------------------------------------------
str(rstan::extract(m))

## -------------------------------------------------------------------------------------------------
m %>%
  spread_draws(condition_mean[condition]) %>%
  head(10)

## -------------------------------------------------------------------------------------------------
m %>%
  recover_types(ABC) %>%
  spread_draws(condition_mean[condition]) %>%
  head(10)

## -------------------------------------------------------------------------------------------------
m %<>% recover_types(ABC)

## -------------------------------------------------------------------------------------------------
m %>%
  spread_draws(overall_mean, response_sd) %>%
  head(10)

## -------------------------------------------------------------------------------------------------
m %>%
  spread_draws(overall_mean, response_sd) %>%
  median_qi(overall_mean, response_sd)

## -------------------------------------------------------------------------------------------------
m %>%
  spread_draws(overall_mean, response_sd) %>%
  median_qi()

## -------------------------------------------------------------------------------------------------
m %>%
  spread_draws(condition_mean[condition]) %>%
  median_qi()

## -------------------------------------------------------------------------------------------------
m %>%
  spread_draws(condition_mean[condition]) %>%
  group_by(condition) %>%    # this line not necessary (done automatically by spread_draws)
  median_qi(condition_mean)

## ----fig.width = 3, fig.height = 2.5--------------------------------------------------------------
m %>%
  spread_draws(condition_mean[condition]) %>%
  median_qi() %>%
  # `geom_pointintervalh` includes `xmin = .lower` and `xmax = .upper` in its default 
  # aesthetics, so we can omit them here
  ggplot(aes(y = fct_rev(condition), x = condition_mean)) +
  geom_pointintervalh()

## ----fig.width = 3, fig.height = 2.5--------------------------------------------------------------
m %>%
  spread_draws(condition_mean[condition]) %>%
  ggplot(aes(y = fct_rev(condition), x = condition_mean)) +
  stat_pointintervalh()

## ----fig.width = 4, fig.height = 2.5--------------------------------------------------------------
m %>%
  spread_draws(condition_mean[condition]) %>%
  ggplot(aes(y = fct_rev(condition), x = condition_mean)) +
  stat_eyeh()

## ----fig.width = 4, fig.height = 2.5--------------------------------------------------------------
m %>%
  spread_draws(condition_mean[condition]) %>%
  ggplot(aes(y = fct_rev(condition), x = condition_mean)) +
  stat_halfeyeh(.width = c(.99, .8))

## ----fig.width = 4.75, fig.height = 2.5-----------------------------------------------------------
m %>%
  spread_draws(condition_mean[condition]) %>%
  ggplot(aes(y = fct_rev(condition), x = condition_mean, fill = stat(abs(x) < .8))) +
  stat_halfeyeh() +
  geom_vline(xintercept = c(-.8, .8), linetype = "dashed") +
  scale_fill_manual(values = c("gray80", "skyblue"))

## -------------------------------------------------------------------------------------------------
m %>%
  spread_draws(condition_mean[condition]) %>%
  median_qi(.width = c(.95, .8, .5))

## ----fig.width = 3, fig.height = 2.5--------------------------------------------------------------
m %>%
  spread_draws(condition_mean[condition]) %>%
  median_qi(.width = c(.95, .66)) %>%
  ggplot(aes(y = fct_rev(condition), x = condition_mean, 
    size = -.width)) +               # smaller probability interval => thicker line
  geom_pointintervalh()

## ----fig.width = 3, fig.height = 2.5--------------------------------------------------------------
m %>%
  spread_draws(condition_mean[condition]) %>%
  median_qi(.width = c(.95, .66)) %>%
  ggplot(aes(y = fct_rev(condition), x = condition_mean)) +
  geom_pointintervalh()

## ----fig.width = 4, fig.height = 2.5--------------------------------------------------------------
m %>%
  spread_draws(condition_mean[condition]) %>%
  median_qi(.width = c(.95, .8, .5)) %>%
  ggplot(aes(y = fct_rev(condition), x = condition_mean)) +
  geom_pointintervalh(interval_size_range = c(0.5, 2))

## ----fig.width = 5, fig.height = 3.5--------------------------------------------------------------
m %>%
  spread_draws(condition_mean[condition]) %>%
  ggplot(aes(x = condition_mean, y = fct_rev(condition))) +
  stat_dotsh(quantiles = 100)

## -------------------------------------------------------------------------------------------------
set.seed(123)
multimodal_draws = tibble(
  x = c(rnorm(5000, 0, 1), rnorm(2500, 4, 1))
)

## -------------------------------------------------------------------------------------------------
multimodal_draws %>%
  mode_hdi(x, .width = .80)

## ---- fig.height = 3, fig.width = 6.5-------------------------------------------------------------
multimodal_draws %>%
  ggplot(aes(x = x)) +
  stat_slabh(aes(y = 0)) +
  stat_pointintervalh(aes(y = -0.5), point_interval = median_qi, .width = c(.95, .80)) +
  annotate("text", label = "median, 80% and 95% quantile intervals", x = 6, y = -0.5, hjust = 0, vjust = 0.3) +
  stat_pointintervalh(aes(y = -0.25), point_interval = mode_hdi, .width = c(.95, .80)) +
  annotate("text", label = "mode, 80% and 95% highest-density intervals", x = 6, y = -0.25, hjust = 0, vjust = 0.3) +
  xlim(-3.25, 18) +
  scale_y_continuous(breaks = NULL)

## -------------------------------------------------------------------------------------------------
m %>% 
  spread_draws(overall_mean, condition_mean[condition]) %>%
  head(10)

## -------------------------------------------------------------------------------------------------
m %>%
  spread_draws(overall_mean, condition_mean[condition]) %>%
  mutate(condition_offset = condition_mean - overall_mean) %>%
  median_qi(condition_offset)

## ----fig.width = 4, fig.height = 2.5--------------------------------------------------------------
m %>%
  spread_draws(condition_mean[condition], response_sd) %>%
  mutate(y_rep = rnorm(n(), condition_mean, response_sd)) %>%
  ggplot(aes(y = fct_rev(condition), x = y_rep)) +
  stat_slabh()

## ----fig.width = 4, fig.height = 2.5--------------------------------------------------------------
m %>%
  spread_draws(condition_mean[condition], response_sd) %>%
  mutate(y_rep = rnorm(n(), condition_mean, response_sd)) %>%
  median_qi(y_rep, .width = c(.95, .8, .5)) %>%
  ggplot(aes(y = fct_rev(condition), x = y_rep)) +
  geom_intervalh() + #auto-sets aes(xmin = .lower, xmax = .upper, color = fct_rev(ordered(.width)))
  geom_point(aes(x = response), data = ABC) +
  scale_color_brewer()

## ----fig.width = 4, fig.height = 2.75-------------------------------------------------------------
draws = m %>%
  spread_draws(condition_mean[condition], response_sd)

reps = draws %>%
  mutate(y_rep = rnorm(n(), condition_mean, response_sd)) %>%
  median_qi(y_rep, .width = c(.95, .8, .5))

means = draws %>%
  median_qi(condition_mean, .width = c(.95, .66))

ABC %>%
  ggplot(aes(y = condition)) +
  geom_intervalh(aes(x = y_rep), data = reps) +
  geom_pointintervalh(aes(x = condition_mean), position = position_nudge(y = -0.3), data = means) +
  geom_point(aes(x = response)) +
  scale_color_brewer()

## ---- fig.width = 4, fig.height = 2.5-------------------------------------------------------------
#N.B. the syntax for compare_levels is experimental and may change
m %>%
  spread_draws(condition_mean[condition]) %>%
  compare_levels(condition_mean, by = condition) %>%
  ggplot(aes(y = condition, x = condition_mean)) +
  stat_halfeyeh()

## -------------------------------------------------------------------------------------------------
m %>%
  gather_draws(overall_mean, condition_mean[condition]) %>%
  median_qi()

## -------------------------------------------------------------------------------------------------
m %>%
  spread_draws(overall_mean, condition_mean[condition]) %>%
  mutate(condition_offset = condition_mean - overall_mean) %>%
  gather_variables() %>%
  median_qi()

## -------------------------------------------------------------------------------------------------
m %>%
  tidy_draws() %>%
  head(10)

## -------------------------------------------------------------------------------------------------
m %>%
  tidy_draws() %>%
  gather_variables() %>%
  head(10)

## -------------------------------------------------------------------------------------------------
m %>%
  spread_draws(`condition_.*`[condition], regex = TRUE) %>%
  head(10)

## ----m_mpg_brms, results = "hide", message = FALSE, warning = FALSE, cache = TRUE-----------------
m_mpg = brm(mpg ~ hp * cyl, data = mtcars)

## -------------------------------------------------------------------------------------------------
mtcars %>%
  group_by(cyl) %>%
  data_grid(hp = seq_range(hp, n = 51)) %>%
  add_fitted_draws(m_mpg) %>%
  ggplot(aes(x = hp, y = mpg, color = ordered(cyl))) +
  stat_lineribbon(aes(y = .value)) +
  geom_point(data = mtcars) +
  scale_fill_brewer(palette = "Greys") +
  scale_color_brewer(palette = "Set2")

## -------------------------------------------------------------------------------------------------
mtcars %>%
  group_by(cyl) %>%
  data_grid(hp = seq_range(hp, n = 101)) %>%
  add_fitted_draws(m_mpg, n = 100) %>%
  ggplot(aes(x = hp, y = mpg, color = ordered(cyl))) +
  geom_line(aes(y = .value, group = paste(cyl, .draw)), alpha = .1) +
  geom_point(data = mtcars) +
  scale_color_brewer(palette = "Dark2")

## -------------------------------------------------------------------------------------------------
m_linear = lm(response ~ condition, data = ABC)

## -------------------------------------------------------------------------------------------------
linear_results = m_linear %>% 
  emmeans(~ condition) %>% 
  tidy() %>%
  mutate(model = "OLS")

linear_results

## -------------------------------------------------------------------------------------------------
bayes_results = m %>%
  spread_draws(condition_mean[condition]) %>%
  median_qi(estimate = condition_mean) %>%
  to_broom_names() %>%
  mutate(model = "Bayes")

bayes_results

## ----fig.width = 4, fig.height = 2.5--------------------------------------------------------------
bind_rows(linear_results, bayes_results) %>%
  mutate(condition = fct_rev(condition)) %>%
  ggplot(aes(y = condition, x = estimate, xmin = conf.low, xmax = conf.high, color = model)) +
  geom_pointintervalh(position = position_dodgev(height = .3))

## ---- warning = FALSE, fig.width = 4, fig.height = 2.5--------------------------------------------
bind_rows(linear_results, bayes_results) %>%
  rename(term = condition) %>%
  dotwhisker::dwplot()

## -------------------------------------------------------------------------------------------------
m %>%
  spread_draws(condition_mean[condition]) %>%
  compare_levels(condition_mean, by = condition) %>%
  head(10)

## -------------------------------------------------------------------------------------------------
m %>%
  spread_draws(condition_mean[condition]) %>%
  compare_levels(condition_mean, by = condition) %>%
  unspread_draws(condition_mean[condition]) %>%
  head(10)

## ----fig.width = 4, fig.height = 2.5--------------------------------------------------------------
m %>%
  spread_draws(condition_mean[condition]) %>%
  compare_levels(condition_mean, by = condition) %>%
  unspread_draws(condition_mean[condition], drop_indices = TRUE) %>%
  mcmc_areas()

## -------------------------------------------------------------------------------------------------
m_rst = stan_glm(response ~ condition, data = ABC)

## -------------------------------------------------------------------------------------------------
m_rst %>%
  emmeans( ~ condition) %>%
  gather_emmeans_draws() %>%
  median_qi()

## -------------------------------------------------------------------------------------------------
m_rst %>%
  emmeans( ~ condition) %>%
  contrast(method = "pairwise") %>%
  gather_emmeans_draws() %>%
  median_qi()

## ----fig.width = 4, fig.height = 2.5--------------------------------------------------------------
m_rst %>%
  emmeans( ~ condition) %>%
  contrast(method = "pairwise") %>%
  gather_emmeans_draws() %>%
  ggplot(aes(x = .value, y = contrast)) +
  stat_halfeyeh()

## -------------------------------------------------------------------------------------------------
m_rst %>%
  emmeans(pairwise ~ condition) %>%
  gather_emmeans_draws() %>%
  median_qi()

