params <-
list(EVAL = TRUE)

## ----chunk_options, include=FALSE---------------------------------------------
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

## ----setup, message = FALSE, warning = FALSE----------------------------------
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
library(brms)
library(ggrepel)
library(RColorBrewer)
library(gganimate)
library(posterior)
library(distributional)

theme_set(theme_tidybayes() + panel_border())

## ---- eval=FALSE--------------------------------------------------------------
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

## ----m_brm, results = "hide", message = FALSE, cache = TRUE-----------------------------------------------------------
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
  spread_draws(r_condition[condition,]) %>%
  summarise_draws()

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

## ---------------------------------------------------------------------------------------------------------------------
m %>%
  spread_draws(b_Intercept, r_condition[condition,]) %>%
  median_qi(condition_mean = b_Intercept + r_condition, .width = c(.95, .8, .5))

## ----fig.width = tiny_width, fig.height = tiny_height-----------------------------------------------------------------
m %>%
  spread_draws(b_Intercept, r_condition[condition,]) %>%
  median_qi(condition_mean = b_Intercept + r_condition, .width = c(.95, .66)) %>%
  ggplot(aes(y = condition, x = condition_mean, xmin = .lower, xmax = .upper)) +
  geom_pointinterval() 

## ----fig.width = tiny_width, fig.height = tiny_height-----------------------------------------------------------------
m %>%
  spread_draws(b_Intercept, r_condition[condition,]) %>%
  mutate(condition_mean = b_Intercept + r_condition) %>%
  ggplot(aes(y = condition, x = condition_mean)) +
  stat_halfeye()

## ----fig.width = tiny_width, fig.height = tiny_height-----------------------------------------------------------------
m %>%
  spread_draws(b_Intercept, r_condition[condition,]) %>%
  mutate(condition_mean = b_Intercept + r_condition) %>%
  ggplot(aes(y = condition, x = condition_mean, fill = after_stat(abs(x) < .8))) +
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
ABC %>%
  data_grid(condition) %>%
  add_predicted_draws(m) %>%
  ggplot(aes(x = .prediction, y = condition)) +
  stat_slab()

## ----fig.width = tiny_width, fig.height = tiny_height-----------------------------------------------------------------
ABC %>%
  data_grid(condition) %>%
  add_predicted_draws(m) %>%
  ggplot(aes(y = condition, x = .prediction)) +
  stat_interval(.width = c(.50, .80, .95, .99)) +
  geom_point(aes(x = response), data = ABC) +
  scale_color_brewer()

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

## ----fig.width = tiny_width, fig.height = tiny_height-----------------------------------------------------------------
ABC %>%
  data_grid(condition) %>%
  add_epred_draws(m, dpar = c("mu", "sigma")) %>%
  sample_draws(30) %>%
  ggplot(aes(y = condition)) +
  stat_slab(aes(xdist = dist_normal(mu, sigma)), 
    slab_color = "gray65", alpha = 1/10, fill = NA
  ) +
  geom_point(aes(x = response), data = ABC, shape = 21, fill = "#9ECAE1", size = 2)

## ----fig.width = tiny_width, fig.height = tiny_height-----------------------------------------------------------------
ABC %>%
  data_grid(condition) %>%
  add_epred_draws(m, dpar = c("mu", "sigma")) %>%
  ggplot(aes(x = condition)) +
  stat_slab(aes(ydist = dist_normal(mu, sigma)), 
    slab_color = "gray65", alpha = 1/10, fill = NA, data = . %>% sample_draws(30), scale = .5
  ) +
  stat_halfeye(aes(y = .epred), side = "bottom", scale = .5) +
  geom_point(aes(y = response), data = ABC, shape = 21, fill = "#9ECAE1", size = 2, position = position_nudge(x = -.2))

## ----m_mpg_brm, results = "hide", message = FALSE, warning = FALSE, cache = TRUE--------------------------------------
m_mpg = brm(
  mpg ~ hp * cyl, 
  data = mtcars, 
  
  file = "models/tidy-brms_m_mpg.rds"  # cache model (can be removed)
)

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

## ----hops_lines, results='hide'---------------------------------------------------------------------------------------
set.seed(123456)
# NOTE: using a small number of draws to keep this example
# small, but in practice you probably want 50 or 100
ndraws = 20

p = mtcars %>%
  group_by(cyl) %>%
  data_grid(hp = seq_range(hp, n = 101)) %>%
  add_epred_draws(m_mpg, ndraws = ndraws) %>%
  ggplot(aes(x = hp, y = mpg, color = ordered(cyl))) +
  geom_line(aes(y = .epred, group = paste(cyl, .draw))) +
  geom_point(data = mtcars) +
  scale_color_brewer(palette = "Dark2") +
  transition_states(.draw, 0, 1) +
  shadow_mark(future = TRUE, color = "gray50", alpha = 1/20)

animate(p, nframes = ndraws, fps = 2.5, width = 432, height = 288, res = 96, dev = "png", type = "cairo")

## ----echo=FALSE, results='asis'---------------------------------------------------------------------------------------
# animate() doesn't seem to put the images in the right place for pkgdown, so this is a manual workaround
anim_save("tidy-brms_hops_lines.gif")
cat("![](tidy-brms_hops_lines.gif)\n")

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
mtcars %>%
  group_by(cyl) %>%
  data_grid(hp = seq_range(hp, n = 101)) %>%
  add_predicted_draws(m_mpg) %>%
  ggplot(aes(x = hp, y = mpg)) +
  stat_lineribbon(aes(y = .prediction), .width = c(.99, .95, .8, .5), color = brewer.pal(5, "Blues")[[5]]) +
  geom_point(data = mtcars) +
  scale_fill_brewer() +
  facet_grid(. ~ cyl, space = "free_x", scales = "free_x")

## ----fig.width = tiny_width, fig.height = tiny_height-----------------------------------------------------------------
set.seed(1234)
AB = tibble(
  group = rep(c("a", "b"), each = 20),
  response = rnorm(40, mean = rep(c(1, 5), each = 20), sd = rep(c(1, 3), each = 20))
)

AB %>%
  ggplot(aes(x = response, y = group)) +
  geom_point()

## ----m_ab_brm, results = "hide", message = FALSE, cache = TRUE--------------------------------------------------------
m_ab = brm(
  bf(
    response ~ group,
    sigma ~ group
  ),
  data = AB,
  
  file = "models/tidy-brms_m_ab.rds"  # cache model (can be removed)
)

## ----fig.width = tiny_width, fig.height = tiny_height-----------------------------------------------------------------
grid = AB %>%
  data_grid(group)

means = grid %>%
  add_epred_draws(m_ab)

preds = grid %>%
  add_predicted_draws(m_ab)

AB %>%
  ggplot(aes(x = response, y = group)) +
  stat_halfeye(aes(x = .epred), scale = 0.6, position = position_nudge(y = 0.175), data = means) +
  stat_interval(aes(x = .prediction), data = preds) +
  geom_point(data = AB) +
  scale_color_brewer()

## ----fig.width = tiny_width, fig.height = tiny_height-----------------------------------------------------------------
grid %>%
  add_epred_draws(m_ab, dpar = TRUE) %>%
  ggplot(aes(x = sigma, y = group)) +
  stat_halfeye() +
  geom_vline(xintercept = 0, linetype = "dashed")

## ----fig.width = tiny_width, fig.height = tiny_height-----------------------------------------------------------------
m %>%
  spread_draws(r_condition[condition,]) %>%
  compare_levels(r_condition, by = condition) %>%
  ungroup() %>%
  mutate(condition = reorder(condition, r_condition)) %>%
  ggplot(aes(y = condition, x = r_condition)) +
  stat_halfeye() +
  geom_vline(xintercept = 0, linetype = "dashed") 

## ---------------------------------------------------------------------------------------------------------------------
mtcars_clean = mtcars %>%
  mutate(cyl = ordered(cyl))

head(mtcars_clean)

## ----m_cyl_brm, results = "hide", message = FALSE, cache = TRUE-------------------------------------------------------
m_cyl = brm(
  cyl ~ mpg, 
  data = mtcars_clean, 
  family = cumulative,
  seed = 58393,
  
  file = "models/tidy-brms_m_cyl.rds"  # cache model (can be removed)
)

## ---------------------------------------------------------------------------------------------------------------------
tibble(mpg = 21) %>%
  add_epred_draws(m_cyl) %>%
  median_qi(.epred)

## ----fig.width = med_width, fig.height = med_height-------------------------------------------------------------------
data_plot = mtcars_clean %>%
  ggplot(aes(x = mpg, y = cyl, color = cyl)) +
  geom_point() +
  scale_color_brewer(palette = "Dark2", name = "cyl")

fit_plot = mtcars_clean %>%
  data_grid(mpg = seq_range(mpg, n = 101)) %>%
  add_epred_draws(m_cyl, value = "P(cyl | mpg)", category = "cyl") %>%
  ggplot(aes(x = mpg, y = `P(cyl | mpg)`, color = cyl)) +
  stat_lineribbon(aes(fill = cyl), alpha = 1/5) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2")

plot_grid(ncol = 1, align = "v",
  data_plot,
  fit_plot
)

## ----hops_ordinal_ribbon_lines, results='hide'------------------------------------------------------------------------
# NOTE: using a small number of draws to keep this example
# small, but in practice you probably want 50 or 100
ndraws = 20

p = mtcars_clean %>%
  data_grid(mpg = seq_range(mpg, n = 101)) %>%
  add_epred_draws(m_cyl, value = "P(cyl | mpg)", category = "cyl") %>%
  ggplot(aes(x = mpg, y = `P(cyl | mpg)`, color = cyl)) +
  # we remove the `.draw` column from the data for stat_lineribbon so that the same ribbons
  # are drawn on every frame (since we use .draw to determine the transitions below)
  stat_lineribbon(aes(fill = cyl), alpha = 1/5, color = NA, data = . %>% select(-.draw)) +
  # we use sample_draws to subsample at the level of geom_line (rather than for the full dataset
  # as in previous HOPs examples) because we need the full set of draws for stat_lineribbon above
  geom_line(aes(group = paste(.draw, cyl)), linewidth = 1, data = . %>% sample_draws(ndraws)) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  transition_manual(.draw)

animate(p, nframes = ndraws, fps = 2.5, width = 576, height = 192, res = 96, dev = "png", type = "cairo")

## ----echo=FALSE, results='asis'---------------------------------------------------------------------------------------
# animate() doesn't seem to put the images in the right place for pkgdown, so this is a manual workaround
anim_save("tidy-brms_hops_ordinal_ribbon_lines.gif")
cat("![](tidy-brms_hops_ordinal_ribbon_lines.gif)\n")

## ----fig.width = tiny_width, fig.height = tiny_height-----------------------------------------------------------------
tibble(mpg = 20) %>%
  add_epred_draws(m_cyl, value = "P(cyl | mpg = 20)", category = "cyl") %>%
  ungroup() %>%
  select(.draw, cyl, `P(cyl | mpg = 20)`) %>%
  gather_pairs(cyl, `P(cyl | mpg = 20)`, triangle = "both") %>%
  filter(.row != .col) %>%
  ggplot(aes(.x, .y)) +
  geom_point(alpha = 1/50) +
  facet_grid(.row ~ .col) +
  ylab("P(cyl = row | mpg = 20)") +
  xlab("P(cyl = col | mpg = 20)")

## ----fig.width = med_width, fig.height = med_height-------------------------------------------------------------------
label_data_function = . %>% 
  ungroup() %>%
  filter(mpg == quantile(mpg, .47)) %>%
  summarise_if(is.numeric, mean)

data_plot_with_mean = mtcars_clean %>%
  data_grid(mpg = seq_range(mpg, n = 101)) %>%
  # NOTE: this shows the use of ndraws to subsample within add_epred_draws()
  # ONLY do this IF you are planning to make spaghetti plots, etc.
  # NEVER subsample to a small sample to plot intervals, densities, etc.
  add_epred_draws(m_cyl, value = "P(cyl | mpg)", category = "cyl", ndraws = 100) %>%
  group_by(mpg, .draw) %>%
  # calculate expected cylinder value
  mutate(cyl = as.numeric(as.character(cyl))) %>%
  summarise(cyl = sum(cyl * `P(cyl | mpg)`), .groups = "drop") %>%
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

## ----fig.width = large_width, fig.height = med_height-----------------------------------------------------------------
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

## ----fig.width = tiny_width, fig.height = tiny_height-----------------------------------------------------------------
mtcars_clean %>%
  select(mpg) %>%
  add_predicted_draws(m_cyl, ndraws = 100, seed = 12345) %>%
  # recover original factor labels
  mutate(cyl = levels(mtcars_clean$cyl)[.prediction]) %>%
  ggplot(aes(x = cyl)) +
  stat_count(aes(group = NA), geom = "line", data = mtcars_clean, color = "red", linewidth = 3, alpha = .5) +
  stat_count(aes(group = .draw), geom = "line", position = "identity", alpha = .05) +
  geom_label(data = data.frame(cyl = "4"), y = 9.5, label = "posterior\npredictions",
    hjust = 1, color = "gray50", lineheight = 1, label.size = NA) +
  geom_label(data = data.frame(cyl = "8"), y = 14, label = "observed\ndata",
    hjust = 0, color = "red", lineheight = 1, label.size = NA)

## ----fig.width = tiny_width, fig.height = tiny_height-----------------------------------------------------------------
set.seed(12345)

mtcars_clean %>%
  select(mpg) %>%
  add_predicted_draws(m_cyl) %>%
  # recover original factor labels. Must ungroup first so that the
  # factor is created in the same way in all groups; this is a workaround
  # because brms no longer returns labelled predictions (hopefully that
  # is fixed then this will no longer be necessary)
  ungroup() %>%
  mutate(cyl = ordered(levels(mtcars_clean$cyl)[.prediction], levels(mtcars_clean$cyl))) %>%
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

## ----m_esoph_brm, results = "hide", message = FALSE, cache = TRUE-----------------------------------------------------
data(esoph)
m_esoph_brm = brm(
  tobgp ~ agegp, 
  data = esoph, 
  family = cumulative(),

  file = "models/tidy-brms_m_esoph_brm.rds"  
)

## ----fig.width = tiny_width, fig.height = tiny_height-----------------------------------------------------------------
esoph %>%
  data_grid(agegp) %>%
  add_epred_draws(m_esoph_brm, dpar = TRUE, category = "tobgp") %>%
  ggplot(aes(x = agegp, y = .epred, color = tobgp)) +
  stat_pointinterval(position = position_dodge(width = .4)) +
  scale_size_continuous(guide = "none") +
  scale_color_manual(values = brewer.pal(6, "Blues")[-c(1,2)])

## ----fig.width = med_width, fig.height = med_height/2-----------------------------------------------------------------
esoph_plot = esoph %>%
  data_grid(agegp) %>%
  add_epred_draws(m_esoph_brm, category = "tobgp") %>%
  ggplot(aes(x = .epred, y = tobgp)) +
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

