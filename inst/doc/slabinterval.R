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
library(dplyr)
library(tidyr)
library(tidybayes)
library(ggplot2)
library(cowplot)
library(brms)
library(ggstance)

theme_set(theme_tidybayes())

## ----hidden_options, include=FALSE------------------------------------------------------------------------------------
options(width = 120)

## ----slabinterval_family, fig.height = 5.5, fig.width = 7, echo = FALSE-----------------------------------------------
dists_df = tibble(
  # enforce order
  geom = rev(c(
    "halfeye[h]", 
    "eye[h]",
    "gradientinterval[h]", 
    "ccdfinterval[h]", 
    "cdfinterval[h]",
    "interval[h]",
    "pointinterval[h]",
    "slab[h]",
    "dots[h]",
    "dotsinterval[h]",
    "histinterval[h]"
    )) %>%
    factor(., levels = .),
  dist = "norm",
  args = list(list(4, 1))
)

hist_df = tibble(
  geom = "histinterval[h]",
  x = qnorm(ppoints(1000), 4, 1),
  dist = NA,
  args = NA
)

dists_plot = dists_df %>%
  ggplot(aes(y = geom, dist = dist, args = args)) +
  geom_blank() + # ensures order
  stat_dist_eyeh(data = . %>% filter(geom == "eye[h]")) +
  stat_dist_halfeyeh(data = . %>% filter(geom == "halfeye[h]"), position = position_nudge(y = - 0.2)) +
  stat_dist_gradientintervalh(data = . %>% filter(geom == "gradientinterval[h]"), scale = .5) +
  stat_dist_ccdfintervalh(data = . %>% filter(geom == "ccdfinterval[h]"), scale = .5) +
  stat_dist_cdfintervalh(data = . %>% filter(geom == "cdfinterval[h]"), scale = .5) +
  stat_intervalh(aes(x = x, y = "interval[h]"), data = hist_df, color = "gray65", alpha = 1/3, size = 10,
    position = position_nudge(y = -.1)) +
  stat_pointintervalh(aes(x = x, y = "pointinterval[h]"), data = hist_df) +
  stat_dist_slabh(data = . %>% filter(geom == "slab[h]"), position = position_nudge(y = - 0.2)) +
  stat_dist_dotsintervalh(data = . %>% filter(geom == "dotsinterval[h]"), position = position_nudge(y = - 0.3)) +
  stat_dist_dotsh(data = . %>% filter(geom == "dots[h]"), position = position_nudge(y = - 0.3)) +
  stat_histintervalh(aes(x = x), data = hist_df, position = position_nudge(y = - 0.4)) +
  scale_slab_alpha_continuous(guide = FALSE) +
  scale_x_continuous(limits = c(0,8), expand = c(0,0)) +
  labs(
    subtitle = "The stat_slabinterval / geom_slabinterval family",
    x = NULL,
    y = NULL
  )

annotation_plot = tribble(
    ~geom,                 ~prefix,
    "halfeye[h]",          c("stat_...", "stat_dist_..."),
    "eye[h]",              c("stat_...", "stat_dist_..."),
    "gradientinterval[h]", c("stat_...", "stat_dist_..."),
    "ccdfinterval[h]",     c("stat_...", "stat_dist_..."),
    "cdfinterval[h]",      c("stat_...", "stat_dist_..."),
    "interval[h]",         c("stat_...", "stat_dist_...", "geom_..."),
    "pointinterval[h]",    c("stat_...", "stat_dist_...", "geom_..."),
    "slab[h]",             c("stat_...", "stat_dist_...", "geom_..."),
    "dotsinterval[h]",     c("stat_...", "stat_dist_...", "geom_..."),
    "dots[h]",             c("stat_...", "stat_dist_...", "geom_..."),
    "histinterval[h]",     c("stat_..."),
  ) %>%
  unnest(prefix) %>%
  mutate(
    geom = factor(geom, levels = levels(dists_df$geom)),
    prefix = factor(prefix, levels = c("stat_...", "stat_dist_...", "geom_..."))
  ) %>%
  ggplot(aes(x = prefix, y = geom)) +
  geom_hline(aes(yintercept = as.numeric(geom) - .1), color = "gray80", data = . %>% filter(prefix == "stat_...")) +
  geom_point(size = 5, color = "gray65", position = position_nudge(y = -.1)) +
  scale_x_discrete(position = "top") +
  scale_y_discrete(breaks = NULL, expand = c(0,.6)) +
  labs(y = NULL, x = NULL) +
  theme(axis.line.x = element_blank(), axis.line.y = element_blank(), axis.ticks = element_blank()) 

plot_grid(ncol = 2, align = "h", rel_widths = c(0.65, 0.35),
  dists_plot,
  annotation_plot
)

## ----sample_data------------------------------------------------------------------------------------------------------
set.seed(1234)
df = tribble(
    ~group, ~subgroup, ~value,
    "a",          "h", rnorm(1000, mean = 5),
    "b",          "h", rnorm(1000, mean = 7, sd = 1.5),
    "c",          "h", rnorm(1000, mean = 8),
    "c",          "i", rnorm(1000, mean = 9),
    "c",          "j", rnorm(1000, mean = 7)
  ) %>%
  unnest(value)

## ----group_eye, fig.width = 3, fig.height = 3-------------------------------------------------------------------------
df %>%
  ggplot(aes(x = group, y = value)) +
  stat_eye() +
  ggtitle("stat_eye()")

## ----group_halfeye, fig.width = 3, fig.height = 3---------------------------------------------------------------------
df %>%
  ggplot(aes(x = group, y = value)) +
  stat_halfeye() +
  ggtitle("stat_halfeye()")

## ----eye_side, fig.width = 5, fig.height = 3--------------------------------------------------------------------------
p = df %>%
  ggplot(aes(x = group, y = value)) +
  panel_border()

plot_grid(ncol = 3, align = "hv",
  p + stat_eye(side = "left") + labs(title = "stat_eye()", subtitle = "side = 'left'"),
  p + stat_eye(side = "both") + labs(subtitle = "side = 'both'"),
  p + stat_eye(side = "right")  + labs(subtitle = "side = 'right'")
)

## ----halfeyeh, fig.width = 3, fig.height = 3--------------------------------------------------------------------------
df %>%
  ggplot(aes(x = value, y = group)) +
  stat_halfeyeh() +
  ggtitle("stat_halfeyeh()")

## ----halfeyeh_family, fig.width = 5, fig.height = 5-------------------------------------------------------------------
p = df %>%
  ggplot(aes(x = group, y = value)) +
  panel_border()

ph = df %>%
  ggplot(aes(y = group, x = value)) +
  panel_border()

plot_grid(ncol = 2, align = "hv",
  p + stat_eye() + labs(title = "stat_[half]eye[h]", subtitle = "stat_eye()"),
  p + stat_halfeye() + labs(subtitle = "stat_halfeye()"),
  ph + stat_eyeh() + labs(subtitle = "stat_eyeh()"),
  ph + stat_halfeyeh()  + labs(subtitle = "stat_halfeyeh()")
)

## ----eyeh_side, fig.width = 5, fig.height = 3-------------------------------------------------------------------------
p = df %>%
  ggplot(aes(x = value, y = group)) +
  panel_border()

plot_grid(ncol = 3, align = "hv", 
  # side = "left" would give the same result
  p + stat_eyeh(side = "left") + ggtitle("stat_eyeh()") + labs(subtitle = "side = 'bottom'"),
  p + stat_eyeh(side = "both") + labs(subtitle = "side = 'both'"),
  # side = "right" would give the same result
  p + stat_eyeh(side = "right") + labs(subtitle = "side = 'top'")
)

## ----eye_dodge--------------------------------------------------------------------------------------------------------
df %>%
  ggplot(aes(x = group, y = value, fill = subgroup)) +
  stat_eye(position = "dodge") +
  ggtitle("stat_eye(position = 'dodge')")

## ----dist_data--------------------------------------------------------------------------------------------------------
dist_df = tribble(
    ~group, ~subgroup, ~mean, ~sd,
    "a",          "h",     5,   1,
    "b",          "h",     7,   1.5,
    "c",          "h",     8,   1,
    "c",          "i",     9,   1,
    "c",          "j",     7,   1
)

## ----dist_eye_dodge---------------------------------------------------------------------------------------------------
dist_df %>%
  ggplot(aes(x = group, dist = "norm", arg1 = mean, arg2 = sd, fill = subgroup)) +
  stat_dist_eye(position = "dodge") +
  ggtitle("stat_dist_eye(position = 'dodge')")

## ----beta_stacked-----------------------------------------------------------------------------------------------------
data.frame(alpha = seq(5, 100, length.out = 10)) %>%
  ggplot(aes(y = alpha, dist = "beta", arg1 = alpha, arg2 = 10)) +
  stat_dist_halfeyeh() +
  labs(
    title = "stat_dist_halfeyeh()",
    x = "Beta(alpha,10) distribution"
  )

## ----beta_overplotted_slabh-------------------------------------------------------------------------------------------
data.frame(alpha = seq(5, 100, length.out = 10)) %>%
  ggplot(aes(y = "", dist = "beta", arg1 = alpha, arg2 = 10, color = alpha)) +
  stat_dist_slabh(fill = NA) +
  coord_cartesian(expand = FALSE) +
  scale_color_viridis_c() +
  labs(
    title = "stat_dist_slabh(fill = NA)",
    x = "Beta(alpha,10) distribution",
    y = NULL
  )

## ----norm_vs_t, fig.width = 3, fig.height = 3-------------------------------------------------------------------------
tribble(
  ~ dist,      ~ args,
  "norm",      list(0, 1),
  "student_t", list(3, 0, 1)
) %>%
  ggplot(aes(y = dist, dist = dist, args = args)) +
  stat_dist_halfeyeh() +
  ggtitle("stat_dist_halfeyeh()")

## ----priors-----------------------------------------------------------------------------------------------------------
# NB these priors are made up!
c(
  prior(normal(0,1), class = b),
  prior(lognormal(0,1), class = sigma)
)

## ----parse_dist-------------------------------------------------------------------------------------------------------
c(
  prior(normal(0,1), class = b),
  prior(lognormal(0,1), class = sigma)
) %>%
  parse_dist(prior)

## ----prior_dist_halfeyeh----------------------------------------------------------------------------------------------
c(
  prior(normal(0,1), class = b),
  prior(lognormal(0,1), class = sigma)
) %>%
  parse_dist(prior) %>%
  ggplot(aes(y = class, dist = .dist, args = .args)) +
  stat_dist_halfeyeh() +
  labs(
    title = "stat_dist_halfeyeh()",
    subtitle = "with brms::prior() and tidybayes::parse_dist() to visualize priors",
    x = NULL
  )

## ----dist_halfeyeh_log_scale, fig.width = 3, fig.height = 2.5---------------------------------------------------------
data.frame(dist = "lnorm") %>% ggplot(aes(y = 1, dist = dist, arg1 = log(10), arg2 = 2*log(10))) +
  stat_dist_halfeyeh() +
  scale_x_log10(breaks = 10^seq(-5,7, by = 2))

## ----stat_histintervalh, fig.width = 5, fig.height = 3----------------------------------------------------------------
p = df %>%
  ggplot(aes(x = group, y = value)) +
  panel_border()

ph = df %>%
  ggplot(aes(y = group, x = value)) +
  panel_border()

plot_grid(ncol = 2, align = "hv",
  p + stat_histinterval() + labs(title = "stat_histinterval[h]", subtitle = "stat_histinterval()"),
  ph + stat_histintervalh() + labs(subtitle = "stat_histintervalh()")
)

## ----cdfinterval_family, fig.width = 4.5, fig.height = 4.5------------------------------------------------------------
p = df %>%
  ggplot(aes(x = group, y = value)) +
  panel_border()

ph = df %>%
  ggplot(aes(y = group, x = value)) +
  panel_border()

plot_grid(ncol = 2, align = "hv",
  p + stat_ccdfinterval() + labs(title = "stat_[c]cdfinterval[h]", subtitle = "stat_ccdfinterval()"),
  ph + stat_ccdfintervalh() + labs(subtitle = "stat_ccdfintervalh()"),
  p + stat_cdfinterval() + labs(subtitle = "stat_cdfinterval()"),
  ph + stat_cdfintervalh()  + labs(subtitle = "stat_cdfintervalh()")
)

## ----ccdf_barplot-----------------------------------------------------------------------------------------------------
df %>%
  ggplot(aes(x = group, y = value, fill = subgroup, group = subgroup)) +
  stat_ccdfinterval(position = "dodge") +
  ggtitle("stat_ccdfinterval(position = 'dodge')") 

## ----ccdf_dodge-------------------------------------------------------------------------------------------------------
df %>%
  ggplot(aes(x = group, y = value, fill = subgroup)) +
  stat_ccdfinterval(position = "dodge") +
  expand_limits(y = 0) +
  # plus coord_cartesian so there is no space between bars and axis
  coord_cartesian(expand = FALSE) +
  ggtitle("stat_ccdfinterval(position = 'dodge')")

## ----ccdf_justification-----------------------------------------------------------------------------------------------
df %>%
  ggplot(aes(x = group, y = value, fill = subgroup)) +
  stat_ccdfinterval(position = "dodge", justification = 1) +
  expand_limits(y = 0) +
  # clip = "off" needed here to ensure interval at the edge is visible
  coord_cartesian(expand = FALSE, clip = "off") +
  ggtitle("stat_ccdfinterval(position = 'dodge', justification = 1)")

## ----ccdf_side, fig.width = 5, fig.height = 2-------------------------------------------------------------------------
p = df %>%
  ggplot(aes(x = value, y = group)) +
  expand_limits(x = 0) +
  panel_border()

plot_grid(ncol = 3, align = "hv", 
  # side = "left" would give the same result
  p + stat_ccdfintervalh(side = "bottom") + ggtitle("stat_ccdfintervalh()") + labs(subtitle = "side = 'bottom'"),
  p + stat_ccdfintervalh(side = "both") + labs(subtitle = "side = 'both'"),
  # side = "right" would give the same result
  p + stat_ccdfintervalh(side = "top") + labs(subtitle = "side = 'top'")
)

## ----dist_ccdf_dodge--------------------------------------------------------------------------------------------------
dist_df %>%
  ggplot(aes(x = group, dist = "norm", arg1 = mean, arg2 = sd, fill = subgroup)) +
  stat_dist_ccdfinterval(position = "dodge") +
  expand_limits(y = 0) +
  ggtitle("stat_dist_ccdfinterval(position = 'dodge')") +
  coord_cartesian(expand = FALSE)

## ----gradient_dodge---------------------------------------------------------------------------------------------------
df %>%
  ggplot(aes(x = group, y = value, fill = subgroup)) +
  stat_gradientinterval(position = "dodge") +
  labs(title = "stat_gradientinterval(position = 'dodge')")

## ----dist_gradient_dodge----------------------------------------------------------------------------------------------
dist_df %>%
  ggplot(aes(x = group, dist = "norm", arg1 = mean, arg2 = sd, fill = subgroup)) +
  stat_dist_gradientinterval(position = "dodge") +
  labs(title = "stat_dist_gradientinterval(position = 'dodge')")

## ----dots_dodge, fig.width = 5, fig.height = 3.33---------------------------------------------------------------------
df %>%
  ggplot(aes(x = group, y = value, fill = subgroup)) +
  stat_dots(position = "dodge") +
  labs(title = "stat_dots(position = 'dodge')")

## ----dots_dodge_nocolor, fig.width = 5, fig.height = 3.33-------------------------------------------------------------
df %>%
  ggplot(aes(x = group, y = value, fill = subgroup, color = subgroup)) +
  stat_dots(position = "dodge") +
  labs(title = "stat_dots(slab_color = NA)")

## ----quantile_dots_dodge, fig.width = 5, fig.height = 3.5-------------------------------------------------------------
df %>%
  ggplot(aes(x = group, y = value, fill = subgroup)) +
  stat_dots(position = "dodge", quantiles = 50, color = NA) +
  labs(title = "stat_dots(quantiles = 50)")

## ----dist_dots_shape_color, fig.width = 5.5, fig.height = 3.5---------------------------------------------------------
dist_df %>%
  ggplot(aes(x = group, dist = "norm", arg1 = mean, arg2 = sd, fill = stat(y < 5), shape = subgroup)) +
  stat_dist_dots(position = "dodge", color = NA) +
  labs(title = "stat_dist_dots(aes(fill = stat(y < 5), shape = subgroup))") +
  # we'll use these shapes since they retain outlines
  scale_shape_manual(values = c(21,22,23))

## ----dist_dots_violin, fig.width = 5.5, fig.height = 3.5--------------------------------------------------------------
dist_df %>%
  ggplot(aes(x = group, dist = "norm", arg1 = mean, arg2 = sd, fill = subgroup)) +
  stat_dist_dotsinterval(position = "dodge", side = "both", slab_color = NA) +
  labs(title = "stat_dist_dotsinterval(side = 'both', slab_color = NA)") 

## ----ccdf_gradient, fig.width = 5.3, fig.height = 3.5-----------------------------------------------------------------
df %>%
  ggplot(aes(x = group, y = value, fill = subgroup)) +
  stat_ccdfinterval(aes(slab_alpha = stat(f)), thickness = 1, position = "dodge") +
  expand_limits(y = 0) +
  # plus coord_cartesian so there is no space between bars and axis
  coord_cartesian(expand = FALSE) +
  ggtitle("stat_ccdfinterval(aes(slab_alpha = stat(f)), thickness = 1)")

## ----norm_vs_t_2, fig.width = 3.75, fig.height = 3--------------------------------------------------------------------
priors = tribble(
  ~ dist,      ~ args,
  "norm",      list(0, 1),
  "student_t", list(3, 0, 1)
) 

priors %>%
  ggplot(aes(y = dist, dist = dist, args = args)) +
  stat_dist_halfeyeh() +
  ggtitle("stat_dist_halfeyeh()")

## ----norm_vs_t_highlight, fig.width = 5, fig.height = 3---------------------------------------------------------------
priors %>%
  ggplot(aes(y = dist, dist = dist, args = args)) +
  stat_dist_halfeyeh(aes(fill = stat(abs(x) < 1.5))) +
  ggtitle("stat_dist_halfeyeh(aes(fill = stat(abs(x) < 1.5)))") +
  # we'll use a nicer palette than the default for highlighting:
  scale_fill_manual(values = c("gray85", "skyblue"))

## ----norm_vs_t_gradient_eye, fig.width = 5.25, fig.height = 3---------------------------------------------------------
priors %>%
  ggplot(aes(y = dist, dist = dist, args = args)) +
  stat_dist_eyeh(aes(alpha = stat(f), fill = stat(x > 1))) +
  ggtitle("stat_dist_eyeh(aes(alpha = stat(f), fill = stat(x > 1)))") +
  # we'll use a nicer palette than the default for highlighting:
  scale_fill_manual(values = c("gray75", "skyblue"))

