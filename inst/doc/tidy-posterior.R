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
library(dplyr)
library(purrr)
library(modelr)
library(ggdist)
library(tidybayes)
library(ggplot2)
library(cowplot)
library(rstan)
library(brms)
library(ggrepel)
library(RColorBrewer)
library(posterior)
library(distributional)

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
options(mc.cores = 1) #min(2, parallel::detectCores()))

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

## ----fig.width = tiny_width, fig.height = tiny_height---------------------------------------------
ABC %>%
  ggplot(aes(y = condition, x = response)) +
  geom_point()

## ----m_brm, cache = TRUE--------------------------------------------------------------------------
m = brm(
  response ~ (1|condition), 
  data = ABC, 
  prior = c(
    prior(normal(0, 1), class = Intercept),
    prior(student_t(3, 0, 1), class = sd),
    prior(student_t(3, 0, 1), class = sigma)
  ),
  control = list(adapt_delta = .99),
  
  file = "models/tidy-brms_m.rds" # cache model (can be removed)  
)

## -------------------------------------------------------------------------------------------------
summarise_draws(tidy_draws(m))

## -------------------------------------------------------------------------------------------------
get_variables(m)

## -------------------------------------------------------------------------------------------------
m %>%
  spread_rvars(r_condition[condition,term])

## -------------------------------------------------------------------------------------------------
m %>%
  spread_rvars(r_condition[condition,term]) %>%
  pull(r_condition)

## -------------------------------------------------------------------------------------------------
m %>%
  spread_rvars(r_condition[c,t])

## -------------------------------------------------------------------------------------------------
m %>%
  spread_rvars(r_condition[condition,])

## -------------------------------------------------------------------------------------------------
m %>%
  spread_rvars(r_condition[,term])

## -------------------------------------------------------------------------------------------------
m %>%
  spread_rvars(b_Intercept, sigma)

## -------------------------------------------------------------------------------------------------
m %>%
  spread_rvars(b_Intercept, sigma) %>%
  median_qi(b_Intercept, sigma)

## -------------------------------------------------------------------------------------------------
m %>%
  spread_rvars(b_Intercept, sigma) %>%
  median_qi()

## -------------------------------------------------------------------------------------------------
m %>%
  gather_rvars(b_Intercept, sigma)

## -------------------------------------------------------------------------------------------------
m %>%
  gather_rvars(b_Intercept, sigma) %>%
  median_qi(.value)

## -------------------------------------------------------------------------------------------------
m %>%
  spread_rvars(r_condition[condition,]) %>%
  median_qi(r_condition)

## -------------------------------------------------------------------------------------------------
m %>%
  spread_rvars(r_condition[condition,]) %>%
  mutate(summarise_draws(r_condition))

## -------------------------------------------------------------------------------------------------
m %>% 
  spread_rvars(b_Intercept, r_condition[condition,])

## -------------------------------------------------------------------------------------------------
m %>%
  spread_rvars(`b_Intercept`, r_condition[condition,Intercept]) %>%
  mutate(condition_mean = b_Intercept + r_condition)

## ----fig.width = tiny_width, fig.height = tiny_height---------------------------------------------
m %>%
  spread_rvars(b_Intercept, r_condition[condition,]) %>%
  mutate(condition_mean = b_Intercept + r_condition) %>%
  ggplot(aes(y = condition, dist = condition_mean)) +
  stat_dist_pointinterval()

## -------------------------------------------------------------------------------------------------
m %>%
  spread_rvars(b_Intercept, r_condition[condition,]) %>%
  median_qi(condition_mean = b_Intercept + r_condition, .width = c(.95, .8, .5))

## ----fig.width = tiny_width, fig.height = tiny_height---------------------------------------------
m %>%
  spread_rvars(b_Intercept, r_condition[condition,]) %>%
  mutate(condition_mean = b_Intercept + r_condition) %>%
  ggplot(aes(y = condition, dist = condition_mean)) +
  stat_dist_halfeye()

## ----fig.width = tiny_width, fig.height = tiny_height---------------------------------------------
m %>%
  spread_rvars(b_Intercept, r_condition[condition,]) %>%
  mutate(condition_mean = b_Intercept + r_condition) %>%
  ggplot(aes(y = condition, dist = condition_mean, fill = stat(abs(x) < .8))) +
  stat_dist_halfeye() +
  geom_vline(xintercept = c(-.8, .8), linetype = "dashed") +
  scale_fill_manual(values = c("gray80", "skyblue"))

## -------------------------------------------------------------------------------------------------
ABC %>%
  data_grid(condition) %>%
  add_epred_rvars(m)

## ----fig.width = tiny_width, fig.height = tiny_height---------------------------------------------
ABC %>%
  data_grid(condition) %>%
  add_epred_rvars(m) %>%
  ggplot(aes(dist = .epred, y = condition)) +
  stat_dist_dotsinterval(quantiles = 100)

## ----fig.width = tiny_width, fig.height = tiny_height---------------------------------------------
ABC %>%
  data_grid(condition) %>%
  add_predicted_rvars(m) %>%
  ggplot(aes(y = condition)) +
  stat_dist_interval(aes(dist = .prediction), .width = c(.50, .80, .95, .99)) +
  geom_point(aes(x = response), data = ABC) +
  scale_color_brewer()

## ----fig.width = tiny_width, fig.height = tiny_height---------------------------------------------
ABC %>%
  data_grid(condition) %>%
  add_epred_rvars(m) %>%
  add_predicted_rvars(m) %>%
  ggplot(aes(y = condition)) +
  stat_dist_interval(aes(dist = .prediction)) +
  stat_dist_pointinterval(aes(dist = .epred), position = position_nudge(y = -0.3)) +
  geom_point(aes(x = response), data = ABC) +
  scale_color_brewer()

## -------------------------------------------------------------------------------------------------
ABC %>%
  data_grid(condition) %>%
  add_epred_rvars(m, dpar = c("mu", "sigma"))

## ----fig.width = tiny_width, fig.height = tiny_height---------------------------------------------
ABC %>%
  data_grid(condition) %>%
  add_epred_rvars(m, dpar = c("mu", "sigma")) %>%
  unnest_rvars() %>%
  sample_draws(30) %>%
  ggplot(aes(y = condition)) +
  stat_dist_slab(
    aes(dist = dist_normal(mu, sigma)), 
    color = "gray65", alpha = 1/10, fill = NA
  ) +
  geom_point(aes(x = response), data = ABC, shape = 21, fill = "#9ECAE1", size = 2)

## ----m_mpg_brm, results = "hide", message = FALSE, warning = FALSE, cache = TRUE------------------
m_mpg = brm(
  mpg ~ hp * cyl, 
  data = mtcars, 
  
  file = "models/tidy-brms_m_mpg.rds"  # cache model (can be removed)
)

## ----fig.width = tiny_width, fig.height = tiny_height---------------------------------------------
mtcars %>%
  group_by(cyl) %>%
  data_grid(hp = seq_range(hp, n = 51)) %>%
  add_epred_rvars(m_mpg) %>%
  ggplot(aes(x = hp, color = ordered(cyl))) +
  stat_dist_lineribbon(aes(dist = .epred)) +
  geom_point(aes(y = mpg), data = mtcars) +
  scale_fill_brewer(palette = "Greys") +
  scale_color_brewer(palette = "Set2")

## ----fig.width = tiny_width, fig.height = tiny_height---------------------------------------------
mtcars %>%
  group_by(cyl) %>%
  data_grid(hp = seq_range(hp, n = 101)) %>%
  add_predicted_rvars(m_mpg) %>%
  ggplot(aes(x = hp, color = ordered(cyl), fill = ordered(cyl))) +
  stat_dist_lineribbon(aes(dist = .prediction), .width = c(.95, .80, .50), alpha = 1/4) +
  geom_point(aes(y = mpg), data = mtcars) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Dark2")

## ----fig.width = tiny_width, fig.height = tiny_height---------------------------------------------
set.seed(1234)
AB = tibble(
  group = rep(c("a", "b"), each = 20),
  response = rnorm(40, mean = rep(c(1, 5), each = 20), sd = rep(c(1, 3), each = 20))
)

AB %>%
  ggplot(aes(x = response, y = group)) +
  geom_point()

## ----m_ab_brm, cache = TRUE-----------------------------------------------------------------------
m_ab = brm(
  bf(
    response ~ group,
    sigma ~ group
  ),
  data = AB,
  
  file = "models/tidy-brms_m_ab.rds"  # cache model (can be removed)
)

## ----fig.width = tiny_width, fig.height = tiny_height---------------------------------------------
AB %>%
  data_grid(group) %>%
  add_epred_rvars(m_ab) %>%
  add_predicted_rvars(m_ab) %>%
  ggplot(aes(y = group)) +
  stat_dist_halfeye(aes(dist = .epred), scale = 0.6, position = position_nudge(y = 0.175)) +
  stat_dist_interval(aes(dist = .prediction)) +
  geom_point(aes(x = response), data = AB) +
  scale_color_brewer()

## ----fig.width = tiny_width, fig.height = tiny_height---------------------------------------------
AB %>%
  data_grid(group) %>%
  add_epred_rvars(m_ab, dpar = TRUE) %>%
  ggplot(aes(dist = sigma, y = group)) +
  stat_dist_halfeye() +
  geom_vline(xintercept = 0, linetype = "dashed")

## ----fig.width = tiny_width, fig.height = tiny_height---------------------------------------------
m %>%
  spread_rvars(r_condition[condition,]) %>%
  compare_levels(r_condition, by = condition) %>%
  ungroup() %>%
  mutate(condition = reorder(condition, r_condition)) %>%
  ggplot(aes(y = condition, dist = r_condition)) +
  stat_dist_halfeye() +
  geom_vline(xintercept = 0, linetype = "dashed") 

## -------------------------------------------------------------------------------------------------
mtcars_clean = mtcars %>%
  mutate(cyl = ordered(cyl))

head(mtcars_clean)

## ----m_cyl_brm, cache = TRUE----------------------------------------------------------------------
m_cyl = brm(
  cyl ~ mpg, 
  data = mtcars_clean, 
  family = cumulative,
  seed = 58393,
  
  file = "models/tidy-brms_m_cyl.rds"  # cache model (can be removed)
)

## -------------------------------------------------------------------------------------------------
tibble(mpg = c(21,22)) %>%
  add_epred_rvars(m_cyl)

## -------------------------------------------------------------------------------------------------
tibble(mpg = c(21,22)) %>%
  add_epred_rvars(m_cyl, columns_to = "cyl")

## ----fig.width = med_width, fig.height = med_height-----------------------------------------------
data_plot = mtcars_clean %>%
  ggplot(aes(x = mpg, y = cyl, color = cyl)) +
  geom_point() +
  scale_color_brewer(palette = "Dark2", name = "cyl")

fit_plot = mtcars_clean %>%
  data_grid(mpg = seq_range(mpg, n = 101)) %>%
  add_epred_rvars(m_cyl, value = "P(cyl | mpg)", columns_to = "cyl") %>%
  ggplot(aes(x = mpg, color = cyl)) +
  stat_dist_lineribbon(aes(dist = `P(cyl | mpg)`, fill = cyl), alpha = 1/5) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  labs(y = "P(cyl | mpg)")

plot_grid(ncol = 1, align = "v",
  data_plot,
  fit_plot
)

## -------------------------------------------------------------------------------------------------
tibble(mpg = c(21,22)) %>%
  # note we are *not* using `columns_to` anymore
  add_epred_rvars(m_cyl, value = "P(cyl | mpg)") %>%
  mutate(cyl = `P(cyl | mpg)` %**% c(4,6,8))

## ----fig.width = med_width, fig.height = med_height-----------------------------------------------
label_data_function = . %>% 
  ungroup() %>%
  filter(mpg == quantile(mpg, .47)) %>%
  summarise_if(is.numeric, mean)

data_plot_with_mean = mtcars_clean %>%
  data_grid(mpg = seq_range(mpg, n = 101)) %>%
  # NOTE: use of ndraws = 100 here subsets draws for the creation of spaghetti plots;
  # DOT NOT do this if you are making other chart types like intervals or densities
  add_epred_rvars(m_cyl, value = "P(cyl | mpg)", ndraws = 100) %>%
  # calculate expected cylinder value
  mutate(cyl = `P(cyl | mpg)` %**% c(4,6,8)) %>%
  # transform in long-data-frame-of-draws format for making spaghetti plots
  unnest_rvars() %>%
  ggplot(aes(x = mpg, y = cyl)) +
  geom_line(aes(group = .draw), alpha = 5/100) +
  geom_point(aes(y = as.numeric(as.character(cyl)), fill = cyl), data = mtcars_clean, shape = 21, size = 2) +
  geom_text(aes(x = mpg + 4), label = "E[cyl | mpg]", data = label_data_function, hjust = 0) +
  geom_segment(aes(yend = cyl, xend = mpg + 3.9), data = label_data_function) +
  scale_fill_brewer(palette = "Set2", name = "cyl")

plot_grid(ncol = 1, align = "v",
  data_plot_with_mean,
  fit_plot
)

## -------------------------------------------------------------------------------------------------
draws_cyl = m_cyl %>%
  tidy_draws() %>%
  as_draws_rvars()

draws_cyl

## -------------------------------------------------------------------------------------------------
beta = draws_cyl$b_Intercept
beta

## -------------------------------------------------------------------------------------------------
x_intercept = with(draws_cyl, b_Intercept / b_mpg)
x_intercept

## ---- fig.width = med_width, fig.height = med_width-----------------------------------------------
beta_2_color = brewer.pal(n = 3, name = "Dark2")[[3]]
beta_1_color = brewer.pal(n = 3, name = "Dark2")[[1]]

# vertical lines we will use to show the relationship between the linear 
# predictor and P(cyl | mpg)
x_intercept_lines = geom_vline(
  # this works because `rvar`s define median() to take the median of the 
  # distribution of each element, see vignette("rvar", package = "posterior")
  xintercept = median(x_intercept),
  color = "gray50",
  alpha = 0.2,
  size = 1
)

thresholds_plot = mtcars_clean %>%
  data_grid(mpg = seq_range(mpg, n = 101)) %>%
  add_linpred_rvars(m_cyl) %>%
  ggplot(aes(x = mpg)) +
  stat_dist_lineribbon(
    aes(dist = beta[2] - beta[1]),
    color = beta_2_color, fill = beta_2_color, 
    alpha = 1/30, .width = ppoints(30),
    size = 1, linetype = "21"
  ) +
  geom_line(aes(y = 0), size = 1, color = beta_1_color, linetype = "21") +
  stat_dist_lineribbon(
    aes(dist = .linpred - beta[1]),
    fill = "black", color = "black",
    alpha = 1/30, .width = ppoints(30)
  ) +
  labs(y = expression("linear predictor" - beta[1])) + 
  annotate("label",
    label = "beta[1]", parse = TRUE,
    x = max(mtcars_clean$mpg), y = 0, hjust = 0.8,
    color = beta_1_color
  ) +
  annotate("label",
    label = "beta[2] - beta[1]", parse = TRUE,
    x = max(mtcars_clean$mpg), y = median(beta[2] - beta[1]), hjust = 0.9,
    color = beta_2_color
  ) +
  coord_cartesian(ylim = c(-10, 10))

plot_grid(ncol = 1, align = "v", axis = "lr",
  data_plot_with_mean + x_intercept_lines,
  fit_plot + x_intercept_lines,
  thresholds_plot + x_intercept_lines
)

