params <-
list(EVAL = TRUE)

## ----chunk_options, include=FALSE-----------------------------------------------------------------
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

## ----setup, message = FALSE, warning = FALSE------------------------------------------------------
library(magrittr)
library(dplyr)
library(purrr)
library(forcats)
library(tidyr)
library(modelr)
library(ggdist)
library(tidybayes)
library(ggplot2)
library(cowplot)
library(rstan)
library(rstanarm)
library(RColorBrewer)

theme_set(theme_tidybayes() + panel_border())

## ----eval=FALSE-----------------------------------------------------------------------------------
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
  ggplot(aes(y = condition, x = response)) +
  geom_point()

## ----message = FALSE, results = 'hide'--------------------------------------------------------------------------------
m = stan_lmer(response ~ (1|condition), data = ABC, 
  prior = normal(0, 1, autoscale = FALSE),
  prior_aux = student_t(3, 0, 1, autoscale = FALSE),
  adapt_delta = .99)

## ---------------------------------------------------------------------------------------------------------------------
m

## ---------------------------------------------------------------------------------------------------------------------
get_variables(m)

## ---------------------------------------------------------------------------------------------------------------------
m %>%
  spread_draws(b[term,group]) %>%
  head(10)

## ---------------------------------------------------------------------------------------------------------------------
m %>%
  spread_draws(b[t,g]) %>%
  head(10)

## ---------------------------------------------------------------------------------------------------------------------
m %>%
  spread_draws(b[,group]) %>%
  head(10)

## ---------------------------------------------------------------------------------------------------------------------
m %>% 
  spread_draws(b[,group]) %>%
  separate(group, c("group", "condition"), ":") %>%
  head(10)

## ---------------------------------------------------------------------------------------------------------------------
m %>% 
  spread_draws(b[,group,condition], sep = "[, :]") %>%
  head(10)

## ---------------------------------------------------------------------------------------------------------------------
m %>%
  spread_draws(`(Intercept)`, sigma) %>%
  head(10)

## ---------------------------------------------------------------------------------------------------------------------
m %>%
  spread_draws(`(Intercept)`, sigma) %>%
  median_qi(`(Intercept)`, sigma)

## ---------------------------------------------------------------------------------------------------------------------
m %>%
  spread_draws(`(Intercept)`, sigma) %>%
  median_qi()

## ---------------------------------------------------------------------------------------------------------------------
m %>%
  gather_draws(`(Intercept)`, sigma) %>%
  median_qi()

## ---------------------------------------------------------------------------------------------------------------------
m %>%
  spread_draws(b[,group]) %>%
  median_qi()

## ---------------------------------------------------------------------------------------------------------------------
m %>%
  spread_draws(b[,group]) %>%
  group_by(group) %>%       # this line not necessary (done by spread_draws)
  median_qi(b)                # b is not necessary (it is the only non-group column)

## ---------------------------------------------------------------------------------------------------------------------
m %>%
  spread_draws(b[,group]) %>%
  summarise_draws()

## ---------------------------------------------------------------------------------------------------------------------
m %>% 
  spread_draws(`(Intercept)`, b[,group]) %>%
  head(10)

## ---------------------------------------------------------------------------------------------------------------------
m %>%
  spread_draws(`(Intercept)`, b[,group]) %>%
  mutate(condition_mean = `(Intercept)` + b) %>%
  median_qi(condition_mean)

## ---------------------------------------------------------------------------------------------------------------------
m %>%
  spread_draws(`(Intercept)`, b[,group]) %>%
  median_qi(condition_mean = `(Intercept)` + b)

## ---------------------------------------------------------------------------------------------------------------------
m %>%
  spread_draws(`(Intercept)`, b[,group]) %>%
  median_qi(condition_mean = `(Intercept)` + b, .width = c(.95, .8, .5))

## ----fig.width = tiny_width, fig.height = tiny_height-----------------------------------------------------------------
m %>%
  spread_draws(`(Intercept)`, b[,group]) %>%
  median_qi(condition_mean = `(Intercept)` + b, .width = c(.95, .66)) %>%
  ggplot(aes(y = group, x = condition_mean, xmin = .lower, xmax = .upper)) +
  geom_pointinterval()

## ----fig.width = tiny_width, fig.height = tiny_height-----------------------------------------------------------------
m %>%
  spread_draws(`(Intercept)`, b[,group]) %>%
  mutate(condition_mean = `(Intercept)` + b) %>%
  ggplot(aes(y = group, x = condition_mean)) +
  stat_halfeye()

## ----fig.width = tiny_width, fig.height = tiny_height-----------------------------------------------------------------
m %>%
  spread_draws(`(Intercept)`, b[,group]) %>%
  mutate(condition_mean = `(Intercept)` + b) %>%
  ggplot(aes(y = group, x = condition_mean, fill = after_stat(abs(x) < .8))) +
  stat_halfeye() +
  geom_vline(xintercept = c(-.8, .8), linetype = "dashed") +
  scale_fill_manual(values = c("gray80", "skyblue"))

## ---------------------------------------------------------------------------------------------------------------------
ABC %>%
  data_grid(condition) %>%
  add_epred_draws(m) %>%
  head(10)

## ----fig.width = tiny_width, fig.height = tiny_height-----------------------------------------------------------------
ABC %>%
  data_grid(condition) %>%
  add_epred_draws(m) %>%
  ggplot(aes(x = .epred, y = condition)) +
  stat_pointinterval(.width = c(.66, .95))

## ----fig.width = tiny_width, fig.height = tiny_height-----------------------------------------------------------------
ABC %>%
  data_grid(condition) %>%
  add_epred_draws(m) %>%
  ggplot(aes(x = .epred, y = condition)) +
  stat_dotsinterval(quantiles = 100)

## ----fig.width = tiny_width, fig.height = tiny_height-----------------------------------------------------------------
grid = ABC %>%
  data_grid(condition)

means = grid %>%
  add_epred_draws(m)

preds = grid %>%
  add_predicted_draws(m)

ABC %>%
  ggplot(aes(y = condition, x = response)) +
  stat_interval(aes(x = .prediction), data = preds) +
  stat_pointinterval(aes(x = .epred), data = means, .width = c(.66, .95), position = position_nudge(y = -0.3)) +
  geom_point() +
  scale_color_brewer()

## ----m_mpg_stan_glm, results = "hide", message = FALSE, warning = FALSE, cache = TRUE---------------------------------
m_mpg = stan_glm(mpg ~ hp * cyl, data = mtcars)

## ----fig.width = tiny_width, fig.height = tiny_height-----------------------------------------------------------------
mtcars %>%
  group_by(cyl) %>%
  data_grid(hp = seq_range(hp, n = 51)) %>%
  add_epred_draws(m_mpg) %>%
  ggplot(aes(x = hp, y = mpg, color = ordered(cyl))) +
  stat_lineribbon(aes(y = .epred)) +
  geom_point(data = mtcars) +
  scale_fill_brewer(palette = "Greys") +
  scale_color_brewer(palette = "Set2")

## ----fig.width = tiny_width, fig.height = tiny_height-----------------------------------------------------------------
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

## ----fig.width = tiny_width, fig.height = tiny_height-----------------------------------------------------------------
mtcars %>%
  group_by(cyl) %>%
  data_grid(hp = seq_range(hp, n = 101)) %>%
  add_predicted_draws(m_mpg) %>%
  ggplot(aes(x = hp, y = mpg, color = ordered(cyl), fill = ordered(cyl))) +
  stat_lineribbon(aes(y = .prediction), .width = c(.95, .80, .50), alpha = 1/4) +
  geom_point(data = mtcars) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Dark2")

## ----fig.width = tiny_width, fig.height = tiny_height-----------------------------------------------------------------
m %>%
  spread_draws(b[,,condition], sep = "[, :]") %>%
  compare_levels(b, by = condition) %>%
  ungroup() %>%
  mutate(condition = reorder(condition, b)) %>%
  ggplot(aes(y = condition, x = b)) +
  stat_halfeye() +
  geom_vline(xintercept = 0, linetype = "dashed") 

## ----m_esoph_rs_stan_polr, cache = TRUE, message = FALSE, results = "hide"--------------------------------------------
data(esoph)
m_esoph_rs = stan_polr(tobgp ~ agegp, data = esoph, prior = R2(0.25), prior_counts = rstanarm::dirichlet(1))

## ----fig.width = tiny_width, fig.height = tiny_height-----------------------------------------------------------------
esoph %>%
  data_grid(agegp) %>%
  add_linpred_draws(m_esoph_rs) %>%
  ggplot(aes(x = as.numeric(agegp), y = .linpred)) +
  stat_lineribbon() +
  scale_fill_brewer(palette = "Greys")

## ---------------------------------------------------------------------------------------------------------------------
get_variables(m_esoph_rs)

## ---------------------------------------------------------------------------------------------------------------------
thresholds = m_esoph_rs %>%
  gather_draws(`.*[|].*`, regex = TRUE) %>%
  group_by(.draw) %>%
  select(.draw, threshold = .value) %>%
  summarise_all(list) %>%
  mutate(threshold = map(threshold, ~ c(., Inf)))

head(thresholds, 10)

## ---------------------------------------------------------------------------------------------------------------------
thresholds[1,]$threshold

## ----fig.width = med_width, fig.height = med_height-------------------------------------------------------------------
esoph %>%
  data_grid(agegp) %>%
  add_linpred_draws(m_esoph_rs) %>%
  inner_join(thresholds, by = ".draw", multiple = "all") %>%
  mutate(`P(Y = category)` = map2(threshold, .linpred, function(alpha, beta_x)
      # this part is logit^-1(alpha_j - beta*x) - logit^-1(alpha_j-1 - beta*x)
      plogis(alpha - beta_x) - 
      plogis(lag(alpha, default = -Inf) - beta_x)
    )) %>%
  mutate(.category = list(levels(esoph$tobgp))) %>%
  unnest(c(threshold, `P(Y = category)`, .category)) %>%
  ggplot(aes(x = agegp, y = `P(Y = category)`, color = .category)) +
  stat_pointinterval(position = position_dodge(width = .4)) +
  scale_size_continuous(guide = "none") +
  scale_color_manual(values = brewer.pal(6, "Blues")[-c(1,2)]) 

## ----fig.width = med_width, fig.height = med_height/2-----------------------------------------------------------------
esoph_plot = esoph %>%
  data_grid(agegp) %>%
  add_linpred_draws(m_esoph_rs) %>%
  inner_join(thresholds, by = ".draw", multiple = "all") %>%
  mutate(`P(Y = category)` = map2(threshold, .linpred, function(alpha, beta_x)
      # this part is logit^-1(alpha_j - beta*x) - logit^-1(alpha_j-1 - beta*x)
      plogis(alpha - beta_x) - 
      plogis(lag(alpha, default = -Inf) - beta_x)
    )) %>%
  mutate(.category = list(levels(esoph$tobgp))) %>%
  unnest(c(threshold, `P(Y = category)`, .category)) %>%
  ggplot(aes(x = `P(Y = category)`, y = .category)) +
  coord_cartesian(expand = FALSE) +
  facet_grid(. ~ agegp, switch = "x") +
  theme_classic() +
  theme(strip.background = element_blank(), strip.placement = "outside") +
  ggtitle("P(tobacco consumption category | age group)") +
  xlab("age group")

esoph_plot +
  stat_summary(fun = median, geom = "bar", fill = "gray65", width = 1, color = "white") +
  stat_pointinterval()

## ----fig.width = med_width, fig.height = med_height/2-----------------------------------------------------------------
esoph_plot +
  stat_ccdfinterval() +
  expand_limits(x = 0) #ensure bars go to 0

