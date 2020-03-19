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
library(purrr)
library(forcats)
library(tidyr)
library(modelr)
library(tidybayes)
library(ggplot2)
library(ggstance)
library(cowplot)
library(rstan)
library(brms)
library(ggrepel)
library(RColorBrewer)
library(gganimate)

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
options(mc.cores = min(2, parallel::detectCores()))

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

## ----fig.width = 3, fig.height = 2.5----------------------------------------------------------------------------------
ABC %>%
  ggplot(aes(y = condition, x = response)) +
  geom_point()

## ----m_brm, cache = TRUE----------------------------------------------------------------------------------------------
m = brm(response ~ (1|condition), data = ABC, control = list(adapt_delta = .99),
  prior = c(
    prior(normal(0, 1), class = Intercept),
    prior(student_t(3, 0, 1), class = sd),
    prior(student_t(3, 0, 1), class = sigma)
  ))

## ---------------------------------------------------------------------------------------------------------------------
m

## ---------------------------------------------------------------------------------------------------------------------
get_variables(m)

## ---------------------------------------------------------------------------------------------------------------------
m %>%
  spread_draws(r_condition[condition,term]) %>%
  head(10)

## ---------------------------------------------------------------------------------------------------------------------
m %>%
  spread_draws(r_condition[c,t]) %>%
  head(10)

## ---------------------------------------------------------------------------------------------------------------------
m %>%
  spread_draws(r_condition[condition,]) %>%
  head(10)

## ---------------------------------------------------------------------------------------------------------------------
m %>%
  spread_draws(b_Intercept, sigma) %>%
  head(10)

## ---------------------------------------------------------------------------------------------------------------------
m %>%
  spread_draws(b_Intercept, sigma) %>%
  median_qi(b_Intercept, sigma)

## ---------------------------------------------------------------------------------------------------------------------
m %>%
  spread_draws(b_Intercept, sigma) %>%
  median_qi()

## ---------------------------------------------------------------------------------------------------------------------
m %>%
  gather_draws(b_Intercept, sigma) %>%
  median_qi()

## ---------------------------------------------------------------------------------------------------------------------
m %>%
  spread_draws(r_condition[condition,]) %>%
  median_qi()

## ---------------------------------------------------------------------------------------------------------------------
m %>%
  spread_draws(r_condition[condition,]) %>%
  group_by(condition) %>%   # this line not necessary (done by spread_draws)
  median_qi(r_condition)      # b is not necessary (it is the only non-group column)

## ---------------------------------------------------------------------------------------------------------------------
m %>% 
  spread_draws(b_Intercept, r_condition[condition,]) %>%
  head(10)

## ---------------------------------------------------------------------------------------------------------------------
m %>%
  spread_draws(`b_Intercept`, r_condition[condition,]) %>%
  mutate(condition_mean = b_Intercept + r_condition) %>%
  median_qi(condition_mean)

## ---------------------------------------------------------------------------------------------------------------------
m %>%
  spread_draws(b_Intercept, r_condition[condition,]) %>%
  median_qi(condition_mean = b_Intercept + r_condition)

## ----fig.width = 4, fig.height = 2.5----------------------------------------------------------------------------------
m %>%
  spread_draws(b_Intercept, r_condition[condition,]) %>%
  median_qi(condition_mean = b_Intercept + r_condition) %>%
  ggplot(aes(y = condition, x = condition_mean, xmin = .lower, xmax = .upper)) +
  geom_pointintervalh()

## ---------------------------------------------------------------------------------------------------------------------
m %>%
  spread_draws(b_Intercept, r_condition[condition,]) %>%
  median_qi(condition_mean = b_Intercept + r_condition, .width = c(.95, .8, .5))

## ----fig.width = 4, fig.height = 2.5----------------------------------------------------------------------------------
m %>%
  spread_draws(b_Intercept, r_condition[condition,]) %>%
  median_qi(condition_mean = b_Intercept + r_condition, .width = c(.95, .66)) %>%
  ggplot(aes(y = condition, x = condition_mean)) +
  geom_pointintervalh() 

## ----fig.width = 4, fig.height = 2.5----------------------------------------------------------------------------------
m %>%
  spread_draws(b_Intercept, r_condition[condition,]) %>%
  mutate(condition_mean = b_Intercept + r_condition) %>%
  ggplot(aes(y = condition, x = condition_mean)) +
  stat_halfeyeh()

## ----fig.width = 4.75, fig.height = 2.5-------------------------------------------------------------------------------
m %>%
  spread_draws(b_Intercept, r_condition[condition,]) %>%
  mutate(condition_mean = b_Intercept + r_condition) %>%
  ggplot(aes(y = condition, x = condition_mean, fill = stat(abs(x) < .8))) +
  stat_halfeyeh() +
  geom_vline(xintercept = c(-.8, .8), linetype = "dashed") +
  scale_fill_manual(values = c("gray80", "skyblue"))

## ---------------------------------------------------------------------------------------------------------------------
ABC %>%
  data_grid(condition) %>%
  add_fitted_draws(m) %>%
  head(10)

## ----fig.width = 3, fig.height = 2.5----------------------------------------------------------------------------------
ABC %>%
  data_grid(condition) %>%
  add_fitted_draws(m) %>%
  ggplot(aes(x = .value, y = condition)) +
  stat_pointintervalh(.width = c(.66, .95))

## ----fig.width = 5, fig.height = 3.5----------------------------------------------------------------------------------
ABC %>%
  data_grid(condition) %>%
  add_fitted_draws(m) %>%
  ggplot(aes(x = .value, y = condition)) +
  stat_dotsh(quantiles = 100)

## ----fig.width = 4, fig.height = 2.5----------------------------------------------------------------------------------
ABC %>%
  data_grid(condition) %>%
  add_predicted_draws(m) %>%
  ggplot(aes(x = .prediction, y = condition)) +
  stat_slabh()

## ----fig.width = 4, fig.height = 2.5----------------------------------------------------------------------------------
ABC %>%
  data_grid(condition) %>%
  add_predicted_draws(m) %>%
  ggplot(aes(y = condition, x = .prediction)) +
  stat_intervalh(.width = c(.50, .80, .95, .99)) +
  geom_point(aes(x = response), data = ABC) +
  scale_color_brewer()

## ----fig.width = 4, fig.height = 2.75---------------------------------------------------------------------------------
grid = ABC %>%
  data_grid(condition)

fits = grid %>%
  add_fitted_draws(m)

preds = grid %>%
  add_predicted_draws(m)

ABC %>%
  ggplot(aes(y = condition, x = response)) +
  stat_intervalh(aes(x = .prediction), data = preds) +
  stat_pointintervalh(aes(x = .value), data = fits, .width = c(.66, .95), position = position_nudge(y = -0.3)) +
  geom_point() +
  scale_color_brewer()

## ----fig.width = 4, fig.height = 3------------------------------------------------------------------------------------
ABC %>%
  data_grid(condition) %>%
  add_fitted_draws(m, dpar = c("mu", "sigma")) %>%
  sample_draws(30) %>%
  ggplot(aes(y = condition)) +
  stat_dist_slabh(aes(dist = "norm", arg1 = mu, arg2 = sigma), 
    slab_color = "gray65", alpha = 1/10, fill = NA
  ) +
  geom_point(aes(x = response), data = ABC, shape = 21, fill = "#9ECAE1", size = 2)

## ----fig.width = 4, fig.height = 3------------------------------------------------------------------------------------
ABC %>%
  data_grid(condition) %>%
  add_fitted_draws(m, dpar = c("mu", "sigma")) %>%
  ggplot(aes(x = condition)) +
  stat_dist_slab(aes(dist = "norm", arg1 = mu, arg2 = sigma), 
    slab_color = "gray65", alpha = 1/10, fill = NA, data = . %>% sample_draws(30), scale = .5
  ) +
  stat_halfeye(aes(y = .value), side = "bottom", scale = .5) +
  geom_point(aes(y = response), data = ABC, shape = 21, fill = "#9ECAE1", size = 2, position = position_nudge(x = -.2))

## ----m_mpg_brm, results = "hide", message = FALSE, warning = FALSE, cache = TRUE--------------------------------------
m_mpg = brm(mpg ~ hp * cyl, data = mtcars)

## ---------------------------------------------------------------------------------------------------------------------
mtcars %>%
  group_by(cyl) %>%
  data_grid(hp = seq_range(hp, n = 51)) %>%
  add_fitted_draws(m_mpg) %>%
  ggplot(aes(x = hp, y = mpg, color = ordered(cyl))) +
  stat_lineribbon(aes(y = .value)) +
  geom_point(data = mtcars) +
  scale_fill_brewer(palette = "Greys") +
  scale_color_brewer(palette = "Set2")

## ---------------------------------------------------------------------------------------------------------------------
mtcars %>%
  group_by(cyl) %>%
  data_grid(hp = seq_range(hp, n = 101)) %>%
  add_fitted_draws(m_mpg, n = 100) %>%
  ggplot(aes(x = hp, y = mpg, color = ordered(cyl))) +
  geom_line(aes(y = .value, group = paste(cyl, .draw)), alpha = .1) +
  geom_point(data = mtcars) +
  scale_color_brewer(palette = "Dark2")

## ---------------------------------------------------------------------------------------------------------------------
set.seed(123456)
# to keep the example small we use 20 frames, 
# but something like 100 would be better
ndraws = 20

p = mtcars %>%
  group_by(cyl) %>%
  data_grid(hp = seq_range(hp, n = 101)) %>%
  add_fitted_draws(m_mpg, n = ndraws) %>%
  ggplot(aes(x = hp, y = mpg, color = ordered(cyl))) +
  geom_line(aes(y = .value, group = paste(cyl, .draw))) +
  geom_point(data = mtcars) +
  scale_color_brewer(palette = "Dark2") +
  transition_states(.draw, 0, 1) +
  shadow_mark(future = TRUE, color = "gray50", alpha = 1/20)

animate(p, nframes = ndraws, fps = 2.5, width = 432, height = 288, res = 96, dev = "png", type = "cairo")

## ---------------------------------------------------------------------------------------------------------------------
mtcars %>%
  group_by(cyl) %>%
  data_grid(hp = seq_range(hp, n = 101)) %>%
  add_predicted_draws(m_mpg) %>%
  ggplot(aes(x = hp, y = mpg, color = ordered(cyl), fill = ordered(cyl))) +
  stat_lineribbon(aes(y = .prediction), .width = c(.95, .80, .50), alpha = 1/4) +
  geom_point(data = mtcars) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Dark2")

## ---------------------------------------------------------------------------------------------------------------------
mtcars %>%
  group_by(cyl) %>%
  data_grid(hp = seq_range(hp, n = 101)) %>%
  add_predicted_draws(m_mpg) %>%
  ggplot(aes(x = hp, y = mpg)) +
  stat_lineribbon(aes(y = .prediction), .width = c(.99, .95, .8, .5), color = brewer.pal(5, "Blues")[[5]]) +
  geom_point(data = mtcars) +
  scale_fill_brewer() +
  facet_grid(. ~ cyl, space = "free_x", scales = "free_x")

## ----fig.width = 3, fig.height = 1.75---------------------------------------------------------------------------------
set.seed(1234)
AB = tibble(
  group = rep(c("a", "b"), each = 20),
  response = rnorm(40, mean = rep(c(1, 5), each = 20), sd = rep(c(1, 3), each = 20))
)

AB %>%
  ggplot(aes(x = response, y = group)) +
  geom_point()

## ----m_ab_brm, cache = TRUE-------------------------------------------------------------------------------------------
m_ab = brm(
  bf(
    response ~ group,
    sigma ~ group
  ),
  data = AB
)

## ----fig.width = 4, fig.height = 2.5----------------------------------------------------------------------------------
grid = AB %>%
  data_grid(group)

fits = grid %>%
  add_fitted_draws(m_ab)

preds = grid %>%
  add_predicted_draws(m_ab)

AB %>%
  ggplot(aes(x = response, y = group)) +
  stat_halfeyeh(aes(x = .value), scale = 0.6, position = position_nudge(y = 0.175), data = fits) +
  stat_intervalh(aes(x = .prediction), data = preds) +
  geom_point(data = AB) +
  scale_color_brewer()

## ----fig.width = 3, fig.height = 1.75---------------------------------------------------------------------------------
grid %>%
  add_fitted_draws(m_ab, dpar = TRUE) %>%
  ggplot(aes(x = sigma, y = group)) +
  stat_halfeyeh() +
  geom_vline(xintercept = 0, linetype = "dashed")

## ---- fig.width = 4, fig.height = 2.5---------------------------------------------------------------------------------
#N.B. the syntax for compare_levels is experimental and may change
m %>%
  spread_draws(r_condition[condition,]) %>%
  compare_levels(r_condition, by = condition) %>%
  ggplot(aes(y = condition, x = r_condition)) +
  stat_halfeyeh()

## ---- fig.width = 4, fig.height = 2.5---------------------------------------------------------------------------------
#N.B. the syntax for compare_levels is experimental and may change
m %>%
  spread_draws(r_condition[condition,]) %>%
  compare_levels(r_condition, by = condition) %>%
  ungroup() %>%
  mutate(condition = reorder(condition, r_condition)) %>%
  ggplot(aes(y = condition, x = r_condition)) +
  stat_halfeyeh() +
  geom_vline(xintercept = 0, linetype = "dashed") 

## ---------------------------------------------------------------------------------------------------------------------
mtcars_clean = mtcars %>%
  mutate(cyl = ordered(cyl))

head(mtcars_clean)

## ----m_cyl_brm, cache = TRUE------------------------------------------------------------------------------------------
m_cyl = brm(cyl ~ mpg, data = mtcars_clean, family = cumulative, seed = 58393)

## ---------------------------------------------------------------------------------------------------------------------
tibble(mpg = 21) %>%
  add_fitted_draws(m_cyl) %>%
  median_qi(.value)

## ---------------------------------------------------------------------------------------------------------------------
tibble(mpg = 21) %>%
  add_fitted_draws(m_cyl) %>%
  
  # recover original factor labels
  mutate(cyl = levels(mtcars_clean$cyl)[.category]) %>%
  group_by(cyl, .add = TRUE) %>%
  
  median_qi(.value)

## ---- fig.width = 6, fig.height = 4-----------------------------------------------------------------------------------
data_plot = mtcars_clean %>%
  ggplot(aes(x = mpg, y = cyl, color = cyl)) +
  geom_point() +
  scale_color_brewer(palette = "Dark2", name = "cyl")

fit_plot = mtcars_clean %>%
  data_grid(mpg = seq_range(mpg, n = 101)) %>%
  add_fitted_draws(m_cyl, value = "P(cyl | mpg)") %>%
  # recover original factor labels
  mutate(cyl = levels(mtcars_clean$cyl)[.category]) %>%
  ggplot(aes(x = mpg, y = `P(cyl | mpg)`, color = cyl)) +
  stat_lineribbon(aes(fill = cyl), alpha = 1/5) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2")

plot_grid(ncol = 1, align = "v",
  data_plot,
  fit_plot
)

## ---------------------------------------------------------------------------------------------------------------------
# to keep the example small we use 20 frames, 
# but something like 100 would be better
ndraws = 20

p = mtcars_clean %>%
  data_grid(mpg = seq_range(mpg, n = 101)) %>%
  add_fitted_draws(m_cyl, value = "P(cyl | mpg)") %>%
  # recover original factor labels
  mutate(cyl = levels(mtcars_clean$cyl)[.category]) %>%
  ggplot(aes(x = mpg, y = `P(cyl | mpg)`, color = cyl)) +
  # we remove the `.draw` column from the data for stat_lineribbon so that the same ribbons
  # are drawn on every frame (since we use .draw to determine the transitions below)
  stat_lineribbon(aes(fill = cyl), alpha = 1/5, color = NA, data = . %>% select(-.draw)) +
  # we use sample_draws to subsample at the level of geom_line (rather than for the full dataset
  # as in previous HOPs examples) because we need the full set of draws for stat_lineribbon above
  geom_line(aes(group = paste(.draw, cyl)), size = 1, data = . %>% sample_draws(ndraws)) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  transition_manual(.draw)

animate(p, nframes = ndraws, fps = 2.5, width = 576, height = 192, res = 96, dev = "png", type = "cairo")

## ---------------------------------------------------------------------------------------------------------------------
tibble(mpg = 20) %>%
  add_fitted_draws(m_cyl, value = "P(cyl | mpg = 20)") %>%
  # recover original factor labels
  mutate(cyl = levels(mtcars_clean$cyl)[.category]) %>%
  ungroup() %>%
  select(.draw, cyl, `P(cyl | mpg = 20)`) %>%
  gather_pairs(cyl, `P(cyl | mpg = 20)`, triangle = "both") %>%
  filter(.row != .col) %>%
  ggplot(aes(.x, .y)) +
  geom_point(alpha = 1/50) +
  facet_grid(.row ~ .col) +
  ylab("P(cyl = row | mpg = 20)") +
  xlab("P(cyl = col | mpg = 20)")

## ---------------------------------------------------------------------------------------------------------------------
label_data_function = . %>% 
  ungroup() %>%
  filter(mpg == quantile(mpg, .47)) %>%
  summarise_if(is.numeric, mean)

data_plot_with_mean = mtcars_clean %>%
  data_grid(mpg = seq_range(mpg, n = 101)) %>%
  add_fitted_draws(m_cyl, value = "P(cyl | mpg)", n = 100) %>%
  # recover original factor labels (and convert into numbers)
  mutate(cyl = as.numeric(levels(mtcars_clean$cyl)[.category])) %>%
  group_by(mpg, .draw) %>%
  # calculate expected cylinder value
  summarise(cyl = sum(cyl * `P(cyl | mpg)`)) %>%
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

## ---- fig.width = 6, fig.height = 4-----------------------------------------------------------------------------------
mtcars_clean %>%
  # we use `select` instead of `data_grid` here because we want to make posterior predictions
  # for exactly the same set of observations we have in the original data
  select(mpg) %>%
  add_predicted_draws(m_cyl, seed = 1234) %>%
  # recover original factor labels
  mutate(cyl = levels(mtcars_clean$cyl)[.prediction]) %>%
  ggplot(aes(x = mpg, y = cyl)) +
  geom_count(color = "gray75") +
  geom_point(aes(fill = cyl), data = mtcars_clean, shape = 21, size = 2) +
  scale_fill_brewer(palette = "Dark2") +
  geom_label_repel(
    data = . %>% ungroup() %>% filter(cyl == "8") %>% filter(mpg == max(mpg)) %>% dplyr::slice(1),
    label = "posterior predictions", xlim = c(26, NA), ylim = c(NA, 2.8), point.padding = 0.3,
    label.size = NA, color = "gray50", segment.color = "gray75"
  ) +
  geom_label_repel(
    data = mtcars_clean %>% filter(cyl == "6") %>% filter(mpg == max(mpg)) %>% dplyr::slice(1),
    label = "observed data", xlim = c(26, NA), ylim = c(2.2, NA), point.padding = 0.2,
    label.size = NA, segment.color = "gray35"
  )

## ---- fig.width = 6.5, fig.height = 4---------------------------------------------------------------------------------
mtcars_clean %>%
  select(mpg) %>%
  add_predicted_draws(m_cyl, n = 100, seed = 12345) %>%
  # recover original factor labels
  mutate(cyl = levels(mtcars_clean$cyl)[.prediction]) %>%
  ggplot(aes(x = cyl)) +
  stat_count(aes(group = NA), geom = "line", data = mtcars_clean, color = "red", size = 3, alpha = .5) +
  stat_count(aes(group = .draw), geom = "line", position = "identity", alpha = .05) +
  geom_label(data = data.frame(cyl = "4"), y = 9.5, label = "posterior\npredictions",
    hjust = 1, color = "gray50", lineheight = 1, label.size = NA) +
  geom_label(data = data.frame(cyl = "8"), y = 14, label = "observed data",
    hjust = 0, color = "red", label.size = NA)

## ---------------------------------------------------------------------------------------------------------------------
set.seed(12345)

mtcars_clean %>%
  select(mpg) %>%
  add_predicted_draws(m_cyl) %>%
  # recover original factor labels. Must ungroup first so that the
  # factor is created in the same way in all groups; this is a workaround
  # because brms no longer returns labelled predictions (hopefully that
  # is fixed then this will no longer be necessary)
  ungroup() %>%
  mutate(cyl = factor(levels(mtcars_clean$cyl)[.prediction])) %>%
  # need .drop = FALSE to ensure 0 counts are not dropped
  group_by(.draw, .drop = FALSE) %>%
  count(cyl) %>%
  gather_pairs(cyl, n) %>%
  ggplot(aes(.x, .y)) +
  geom_count(color = "gray75") +
  geom_point(data = mtcars_clean %>% count(cyl) %>% gather_pairs(cyl, n), color = "red") +
  facet_grid(vars(.row), vars(.col)) +
  xlab("Number of observations with cyl = col") +
  ylab("Number of observations with cyl = row") 

## ----m_esoph_brm, cache = TRUE----------------------------------------------------------------------------------------
data(esoph)
m_esoph_brm = brm(tobgp ~ agegp, data = esoph, family = cumulative())

## ---------------------------------------------------------------------------------------------------------------------
esoph %>%
  data_grid(agegp) %>%
  add_fitted_draws(m_esoph_brm, dpar = TRUE) %>%
  mutate(tobgp = levels(esoph$tobgp)[.category]) %>%
  ggplot(aes(x = agegp, y = .value, color = tobgp)) +
  stat_pointinterval(position = position_dodge(width = .4)) +
  scale_size_continuous(guide = FALSE) +
  scale_color_manual(values = brewer.pal(6, "Blues")[-c(1,2)])

## ----fig.height = 2.25, fig.width = 7---------------------------------------------------------------------------------
esoph_plot = esoph %>%
  data_grid(agegp) %>%
  add_fitted_draws(m_esoph_brm) %>%
  mutate(tobgp = levels(esoph$tobgp)[.category]) %>%
  ggplot(aes(x = .value, y = tobgp)) +
  coord_cartesian(expand = FALSE) +
  facet_grid(. ~ agegp, switch = "x") +
  theme_classic() +
  theme(strip.background = element_blank(), strip.placement = "outside") +
  ggtitle("P(tobacco consumption category | age group)") +
  xlab("age group")

esoph_plot +
  stat_summaryh(fun.x = median, geom = "barh", fill = "gray75", width = 1, color = "white") +
  stat_pointintervalh()

## ----fig.height = 2.25, fig.width = 7---------------------------------------------------------------------------------
esoph_plot +
  stat_ccdfintervalh() +
  expand_limits(x = 0) #ensure bars go to 0

