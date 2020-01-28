params <-
list(EVAL = TRUE)

## ----chunk_options, include=FALSE---------------------------------------------
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

## ----setup, message = FALSE, warning = FALSE----------------------------------
library(dplyr)
library(tidyr)
library(tidybayes)
library(ggplot2)
library(brms)       # needed for dstudent_t(), etc; not for model fitting
library(broom)
library(modelr)

theme_set(theme_tidybayes())

## ----hidden_options, include=FALSE------------------------------------------------------------------------------------
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
ABC %>%
  ggplot(aes(x = response, y = condition)) +
  geom_point(alpha = 0.5) +
  ylab("condition")

## ---------------------------------------------------------------------------------------------------------------------
m_ABC = lm(response ~ condition, data = ABC)

## ---------------------------------------------------------------------------------------------------------------------
summary(m_ABC)

## ---------------------------------------------------------------------------------------------------------------------
tidy(m_ABC)

## ---------------------------------------------------------------------------------------------------------------------
m_ABC %>%
  tidy() %>%
  ggplot(aes(y = term)) +
  stat_dist_halfeyeh(
    aes(dist = "student_t", arg1 = df.residual(m_ABC), arg2 = estimate, arg3 = std.error)
  )

## ---------------------------------------------------------------------------------------------------------------------
ABC %>%
  data_grid(condition) %>%
  augment(m_ABC, newdata = .) %>%
  ggplot(aes(y = condition)) +
  stat_dist_halfeyeh(
    aes(dist = "student_t", arg1 = df.residual(m_ABC), arg2 = .fitted, arg3 = .se.fit), 
    scale = .5
  ) +
  # we'll add the data back in too (scale = .5 above adjusts the halfeye height so
  # that the data fit in as well)
  geom_point(aes(x = response), data = ABC, pch = "|", size = 2, position = position_nudge(y = -.15))

## ---------------------------------------------------------------------------------------------------------------------
ABC %>%
  data_grid(condition) %>%
  augment(m_ABC, newdata = .) %>%
  ggplot(aes(y = condition)) +
  stat_dist_gradientintervalh(
    aes(dist = "student_t", arg1 = df.residual(m_ABC), arg2 = .fitted, arg3 = .se.fit), 
    scale = .5
  )

## ---------------------------------------------------------------------------------------------------------------------
ABC %>%
  data_grid(condition) %>%
  augment(m_ABC, newdata = .) %>%
  ggplot(aes(y = condition)) +
  stat_dist_ccdfintervalh(
    aes(dist = "student_t", arg1 = df.residual(m_ABC), arg2 = .fitted, arg3 = .se.fit)
  )

## ---------------------------------------------------------------------------------------------------------------------
ABC %>%
  data_grid(condition) %>%
  augment(m_ABC, newdata = .) %>%
  ggplot(aes(y = condition)) +
  stat_dist_dotsh(
    quantiles = 100,
    aes(dist = "student_t", arg1 = df.residual(m_ABC), arg2 = .fitted, arg3 = .se.fit)
  )

## ---------------------------------------------------------------------------------------------------------------------
m_mpg = lm(mpg ~ hp * cyl, data = mtcars)

## ---------------------------------------------------------------------------------------------------------------------
mtcars %>%
  group_by(cyl) %>%
  data_grid(hp = seq_range(hp, n = 101)) %>%
  augment(m_mpg, newdata = .) %>%
  ggplot(aes(x = hp, fill = ordered(cyl), color = ordered(cyl))) +
  stat_dist_lineribbon(
    aes(dist = "student_t", arg1 = df.residual(m_mpg), arg2 = .fitted, arg3 = .se.fit), 
    alpha = 1/4
  ) +
  geom_point(aes(y = mpg), data = mtcars) +
  
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Dark2") +
  labs(
    color = "cyl",
    fill = "cyl",
    y = "mpg"
  )

