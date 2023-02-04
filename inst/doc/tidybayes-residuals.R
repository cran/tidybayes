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
library(tidyr)
library(ggdist)
library(tidybayes)
library(ggplot2)
library(cowplot)
library(rstan)
library(brms)
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
options(mc.cores = 1) #min(2, parallel::detectCores()))

options(width = 120)

## ---------------------------------------------------------------------------------------------------------------------
set.seed(4118)
n = 100

cens_df =
  tibble(
    y_star = rnorm(n, 0.5, 1),
    y_lower = floor(y_star),
    y_upper = ceiling(y_star),
    censoring = "interval"
  )

## ---------------------------------------------------------------------------------------------------------------------
head(cens_df, 10)

## ---- fig.width = large_height/2.8, fig.height = large_height---------------------------------------------------------
uncensored_plot = cens_df %>%
  ggplot(aes(y = "", x = y_star)) +
  stat_slab() +
  geom_jitter(aes(y = 0.75, color = ordered(y_lower)), position = position_jitter(height = 0.2), show.legend = FALSE) +
  ylab(NULL) +
  scale_x_continuous(breaks = -4:4, limits = c(-4, 4)) +
  background_grid("x")

censored_plot = cens_df %>%
  ggplot(aes(y = "", x = (y_lower + y_upper)/2)) +
  geom_dotplot(
    aes(fill = ordered(y_lower)),
    method = "histodot", origin = -4, binwidth = 1, dotsize = 0.5, stackratio = .8, show.legend = FALSE,
    stackgroups = TRUE, binpositions = "all", color = NA
  ) +
  geom_segment(
    aes(x = y + 0.5, xend = y + 0.5, y = 1.75, yend = 1.5, color = ordered(y)),
    data = data.frame(y = unique(cens_df$y_lower)), show.legend = FALSE,
    arrow = arrow(type = "closed", length = unit(7, "points")), linewidth = 1
  ) +
  ylab(NULL) +
  xlab("interval-censored y") +
  scale_x_continuous(breaks = -4:4, limits = c(-4, 4)) +
  background_grid("x")

plot_grid(align = "v", ncol = 1, rel_heights = c(1, 2.5),
  uncensored_plot,
  censored_plot
)

## ----m_ideal_brm, cache = TRUE----------------------------------------------------------------------------------------
m_ideal = brm(
  y_star ~ 1, 
  data = cens_df, 
  family = student,
  
  file = "models/tidybayes-residuals_m_ideal.rds"  # cache model (can be removed)
)

## ---------------------------------------------------------------------------------------------------------------------
m_ideal

## ---- fig.width = tiny_height, fig.height = tiny_height---------------------------------------------------------------
cens_df %>%
  add_residual_draws(m_ideal) %>%
  ggplot(aes(x = .row, y = .residual)) +
  stat_pointinterval()

## ---- fig.width = tiny_height, fig.height = tiny_height---------------------------------------------------------------
cens_df %>%
  add_residual_draws(m_ideal) %>%
  median_qi() %>%
  ggplot(aes(sample = .residual)) +
  geom_qq() +
  geom_qq_line()

## ---- fig.width = tiny_height, fig.height = tiny_height---------------------------------------------------------------
cens_df %>%
  add_predicted_draws(m_ideal) %>%
  summarise(
    p_residual = mean(.prediction < y_star),
    z_residual = qnorm(p_residual),
    .groups = "drop_last"
  ) %>%
  ggplot(aes(sample = z_residual)) +
  geom_qq() +
  geom_abline()

## ----m_brm, cache = TRUE----------------------------------------------------------------------------------------------
m = brm(
  y_lower | cens(censoring, y_upper) ~ 1, 
  data = cens_df,
  
  file = "models/tidybayes-residuals_m.rds"  # cache model (can be removed)
)

## ---------------------------------------------------------------------------------------------------------------------
m

## ---- fig.width = tiny_height, fig.height = tiny_height---------------------------------------------------------------
cens_df %>%
  add_residual_draws(m) %>%
  ggplot(aes(x = .row, y = .residual)) +
  stat_pointinterval()

## ---- fig.width = tiny_height, fig.height = tiny_height---------------------------------------------------------------
cens_df %>%
  add_residual_draws(m) %>%
  median_qi(.residual) %>%
  ggplot(aes(sample = .residual)) +
  geom_qq() +
  geom_qq_line()

## ---- fig.width = tiny_height, fig.height = tiny_height---------------------------------------------------------------
cens_df %>%
  add_predicted_draws(m) %>%
  summarise(
    p_lower = mean(.prediction < y_lower),
    p_upper = mean(.prediction < y_upper),
    p_residual = runif(1, p_lower, p_upper),
    z_residual = qnorm(p_residual),
    .groups = "drop_last"
  ) %>%
  ggplot(aes(x = .row, y = z_residual)) +
  geom_point()

## ---- fig.width = tiny_height, fig.height = tiny_height---------------------------------------------------------------
cens_df %>%
  add_predicted_draws(m) %>%
  summarise(
    p_lower = mean(.prediction < y_lower),
    p_upper = mean(.prediction < y_upper),
    p_residual = runif(1, p_lower, p_upper),
    z_residual = qnorm(p_residual),
    .groups = "drop_last"
  ) %>%
  ggplot(aes(sample = z_residual)) +
  geom_qq() +
  geom_abline()

## ----resid_hops_1, fig.width = tiny_height, fig.height = tiny_height, results='hide'----------------------------------
# NOTE: ordinarily I would use a large number of frames (k), 
# say 50 or 100, but I am keeping it small for the sake of 
# keeping these examples small
k = 10

p = cens_df %>%
  add_predicted_draws(m) %>%
  summarise(
    p_lower = mean(.prediction < y_lower),
    p_upper = mean(.prediction < y_upper),
    p_residual = list(runif(k, p_lower, p_upper)),
    residual_draw = list(1:k),
    .groups = "drop_last"
  ) %>%
  unnest(c(p_residual, residual_draw)) %>%
  mutate(z_residual = qnorm(p_residual)) %>%
  ggplot(aes(sample = z_residual)) +
  geom_qq() +
  geom_abline() +
  transition_manual(residual_draw)

animate(p, nframes = k, width = 384, height = 384, res = 96, dev = "png", type = "cairo")

## ----echo=FALSE, results='asis'---------------------------------------------------------------------------------------
# animate() doesn't seem to put the images in the right place for pkgdown, so this is a manual workaround
anim_save("tidybayes-residuals_resid_hops_1.gif")
cat("![](tidybayes-residuals_resid_hops_1.gif)\n")

## ---------------------------------------------------------------------------------------------------------------------
set.seed(41181)
n = 100

cens_df_t =
  tibble(
    y = rt(n, 3) + 0.5,
    y_lower = floor(y),
    y_upper = ceiling(y),
    censoring = "interval"
  )

## ---- fig.width = tiny_height, fig.height = tiny_width----------------------------------------------------------------
uncensored_plot = cens_df_t %>%
  ggplot(aes(y = "", x = y)) +
  stat_slab() +
  geom_jitter(aes(y = 0.75, color = ordered(y_lower)), position = position_jitter(height = 0.2), show.legend = FALSE) +
  ylab(NULL) +
  scale_x_continuous(breaks = -10:10, limits = c(-10, 10)) +
  background_grid("x")

censored_plot = cens_df_t %>%
  ggplot(aes(y = "", x = (y_lower + y_upper)/2)) +
  geom_dotplot(
    aes(fill = ordered(y_lower)),
    method = "histodot", origin = -4, binwidth = 1, dotsize = 0.5, stackratio = .8, show.legend = FALSE,
    stackgroups = TRUE, binpositions = "all", color = NA
  ) +
  geom_segment(
    aes(x = y + 0.5, xend = y + 0.5, y = 1.75, yend = 1.5, color = ordered(y)),
    data = data.frame(y = unique(cens_df_t$y_lower)), show.legend = FALSE,
    arrow = arrow(type = "closed", length = unit(7, "points")), linewidth = 1
  ) +
  ylab(NULL) +
  xlab("interval-censored y") +
  scale_x_continuous(breaks = -10:10, limits = c(-10, 10)) +
  background_grid("x")

plot_grid(align = "v", ncol = 1, rel_heights = c(1, 2.25),
  uncensored_plot,
  censored_plot
)

## ----m_t1_brm, cache = TRUE-------------------------------------------------------------------------------------------
m_t1 = brm(
  y_lower | cens(censoring, y_upper) ~ 1,
  data = cens_df_t,

  file = "models/tidybayes-residuals_m_t1"  # cache model (can be removed)
)

## ---- fig.width = tiny_height, fig.height = tiny_height---------------------------------------------------------------
cens_df_t %>%
  add_residual_draws(m_t1) %>%
  median_qi(.residual) %>%
  ggplot(aes(sample = .residual)) +
  geom_qq() +
  geom_qq_line()

## ---- fig.width = tiny_height, fig.height = tiny_height---------------------------------------------------------------
cens_df_t %>%
  add_predicted_draws(m_t1) %>%
  summarise(
    p_lower = mean(.prediction < y_lower),
    p_upper = mean(.prediction < y_upper),
    p_residual = runif(1, p_lower, p_upper),
    z_residual = qnorm(p_residual),
    .groups = "drop_last"
  ) %>%
  ggplot(aes(sample = z_residual)) +
  geom_qq() +
  geom_abline()

## ----m_t2_brm, cache = TRUE-------------------------------------------------------------------------------------------
m_t2 = brm(
  y_lower | cens(censoring, y_upper) ~ 1, 
  data = cens_df_t,
  family = student,
  
  file = "models/tidybayes-residuals_m_t2.rds"  # cache model (can be removed)
)

## ---- fig.width = tiny_height, fig.height = tiny_height---------------------------------------------------------------
cens_df_t %>%
  add_residual_draws(m_t2) %>%
  median_qi(.residual) %>%
  ggplot(aes(sample = .residual)) +
  geom_qq() +
  geom_qq_line()

## ---- fig.width = tiny_height, fig.height = tiny_height---------------------------------------------------------------
cens_df_t %>%
  add_predicted_draws(m_t2) %>%
  summarise(
    p_lower = mean(.prediction < y_lower),
    p_upper = mean(.prediction < y_upper),
    p_residual = runif(1, p_lower, p_upper),
    z_residual = qnorm(p_residual),
    .groups = "drop_last"
  ) %>%
  ggplot(aes(sample = z_residual)) +
  geom_qq() +
  geom_abline()

## ---------------------------------------------------------------------------------------------------------------------
cens_df_o = cens_df_t %>%
  mutate(y_factor = ordered(y_lower))

## ----m_o_brm, cache = TRUE--------------------------------------------------------------------------------------------
m_o = brm(
  y_factor ~ 1, 
  data = cens_df_o, 
  family = cumulative, 
  prior = prior(normal(0, 10), class = Intercept), 
  control = list(adapt_delta = 0.99),
  
  file = "models/tidybayes-residuals_m_o.rds"  # cache model (can be removed)
)

## ---- error = TRUE----------------------------------------------------------------------------------------------------
cens_df_o %>%
  add_residual_draws(m_o) %>%
  median_qi(.residual) %>%
  ggplot(aes(sample = .residual)) +
  geom_qq() +
  geom_qq_line()

## ---- fig.width = tiny_height, fig.height = tiny_height---------------------------------------------------------------
cens_df_o %>%
  add_predicted_draws(m_o) %>%
  mutate(.prediction = ordered(levels(y_factor)[.prediction], levels = levels(y_factor))) %>%
  summarise(
    p_lower = mean(.prediction < y_factor),
    p_upper = mean(.prediction <= y_factor),
    p_residual = runif(1, p_lower, p_upper),
    z_residual = qnorm(p_residual),
    .groups = "drop_last"
  ) %>%
  ggplot(aes(sample = z_residual)) +
  geom_qq() +
  geom_abline()

## ---------------------------------------------------------------------------------------------------------------------
library(rlang)
make_probability_residuals = function(data, prediction, y, y_upper = NA, n = 1) {
  .prediction = enquo(prediction)
  .y = enquo(y)
  .y_upper = enquo(y_upper)

  if (eval_tidy(expr(is.factor(!!.prediction) && !is.ordered(!!.prediction)), data)) {
    data = mutate(data, !!.prediction := ordered(!!.prediction, levels = levels(!!.prediction)))
  }
  
  if (is.na(get_expr(.y_upper))) {
    #no y_upper provided, use y as y_upper
    data = summarise(data,
      .p_lower = mean(!!.prediction < !!.y),
      .p_upper = mean(!!.prediction <= !!.y),
      .groups = "drop_last"
    )
  } else {
    #y_upper should be a vector, and if an entry in it is NA, use the entry from y
    data = summarise(data,
      .p_lower = mean(!!.prediction < !!.y),
      .p_upper = mean(!!.prediction <= ifelse(is.na(!!.y_upper), !!.y, !!.y_upper)),
      .groups = "drop_last"
    )
  }
  
  data %>%
    mutate(
      .p_residual = map2(.p_lower, .p_upper, runif, n = !!n),
      .residual_draw = map(.p_residual, seq_along)
    ) %>%
    unnest(c(.p_residual, .residual_draw)) %>%
    mutate(.z_residual = qnorm(.p_residual))
}

## ---------------------------------------------------------------------------------------------------------------------
set.seed(51919)

bin_df = tibble(
  y = runif(100) > 0.3
)

## ----m_bin_brm, cache = TRUE------------------------------------------------------------------------------------------
m_bin = brm(
  y ~ 1, 
  data = bin_df, 
  family = bernoulli,
  
  file = "models/tidybayes-residuals_m_bin.rds"  # cache model (can be removed)
)

## ---- fig.width = tiny_height, fig.height = tiny_height---------------------------------------------------------------
bin_df %>%
  add_residual_draws(m_bin) %>%
  median_qi() %>%
  ggplot(aes(sample = .residual)) +
  geom_qq() +
  geom_qq_line()

## ----resid_hops_2, fig.width = tiny_height, fig.height = tiny_height, results = "hide"--------------------------------
# NOTE: ordinarily I would use a large number of frames (k), 
# say 50 or 100, but I am keeping it small for the sake of 
# keeping these examples small
k = 10

p = bin_df %>%
  add_predicted_draws(m_bin) %>%
  make_probability_residuals(.prediction, y, n = k) %>%
  ggplot(aes(sample = .p_residual)) +
  geom_qq(distribution = qunif) +
  geom_abline() +
  transition_manual(.residual_draw)

animate(p, nframes = k, width = 384, height = 384, res = 96, dev = "png", type = "cairo")

## ----echo=FALSE, results='asis'---------------------------------------------------------------------------------------
# animate() doesn't seem to put the images in the right place for pkgdown, so this is a manual workaround
anim_save("tidybayes-residuals_resid_hops_2.gif")
cat("![](tidybayes-residuals_resid_hops_2.gif)\n")

