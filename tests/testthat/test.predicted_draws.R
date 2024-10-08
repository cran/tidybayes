# Tests for [add_]predicted_draws
#
# Author: mjskay
###############################################################################

suppressWarnings(suppressMessages({
  library(dplyr)
  library(tidyr)
  library(magrittr)
}))




# data
mtcars_tbl = mtcars %>%
  set_rownames(seq_len(nrow(.))) %>%
  as_tibble()

# for reliable testing, need to use only a single core (otherwise random
# numbers do not seem to always be reproducible on brms)
options(mc.cores = 1)


test_that("[add_]predicted_draws throws an error on unsupported models", {
  data("RankCorr", package = "ggdist")

  expect_error(predicted_draws(RankCorr, data.frame()),
    'no applicable method')
  expect_error(add_predicted_draws(data.frame(), RankCorr),
    'no applicable method')
})


test_that("[add_]predicted_draws and basic arguments works on a simple rstanarm model", {
  skip_if_not_installed("rstanarm")
  m_hp_wt = readRDS(test_path("../models/models.rstanarm.m_hp_wt.rds"))

  preds = rstanarm::posterior_predict(m_hp_wt, mtcars_tbl, draws = 10, seed = 123) %>%
    as.data.frame() %>%
    mutate(
      .chain = NA_integer_,
      .iteration = NA_integer_,
      .draw = seq_len(n())
    ) %>%
    gather(.row, .prediction, -.chain, -.iteration, -.draw) %>%
    as_tibble()

  ref = mtcars_tbl %>%
    mutate(.row = rownames(.)) %>%
    inner_join(preds, by = ".row", multiple = "all") %>%
    mutate(.row = as.integer(.row)) %>%
    group_by(mpg, cyl, disp, hp, drat, wt, qsec, vs, am, gear, carb, .row)

  expect_equal(predicted_draws(m_hp_wt, mtcars_tbl, ndraws = 10, seed = 123), ref)
  expect_equal(add_predicted_draws(mtcars_tbl, m_hp_wt, ndraws = 10, seed = 123), ref)
  expect_warning(
    expect_equal(predicted_draws(m_hp_wt, mtcars_tbl, n = 10, seed = 123), ref),
    "`n`.*deprecated.*`ndraws`"
  )
  expect_warning(
    expect_equal(add_predicted_draws(mtcars_tbl, m_hp_wt, n = 10, seed = 123), ref),
    "`n`.*deprecated.*`ndraws`"
  )

  # should still work using default implementation
  expect_equal(predicted_draws.default(m_hp_wt, mtcars_tbl, draws = 10, seed = 123), ref)
})


test_that("[add_]predicted_draws and basic arguments works on an rstanarm model with random effects", {
  skip_if_not_installed("rstanarm")
  m_cyl = readRDS(test_path("../models/models.rstanarm.m_cyl.rds"))

  preds = rstanarm::posterior_predict(m_cyl, mtcars_tbl, draws = 10, seed = 123) %>%
    as.data.frame() %>%
    mutate(
      .chain = NA_integer_,
      .iteration = NA_integer_,
      .draw = seq_len(n())
    ) %>%
    gather(.row, .prediction, -.chain, -.iteration, -.draw) %>%
    as_tibble()

  ref = mtcars_tbl %>%
    mutate(.row = rownames(.)) %>%
    inner_join(preds, by = ".row", multiple = "all") %>%
    mutate(.row = as.integer(.row)) %>%
    group_by(mpg, cyl, disp, hp, drat, wt, qsec, vs, am, gear, carb, .row)

  expect_equal(predicted_draws(m_cyl, mtcars_tbl, ndraws = 10, seed = 123), ref)
  expect_equal(add_predicted_draws(mtcars_tbl, m_cyl, ndraws = 10, seed = 123), ref)
})


test_that("[add_]predicted_draws works on a simple brms model", {
  skip_if_not_installed("brms")
  m_hp = readRDS(test_path("../models/models.brms.m_hp.rds"))

  set.seed(123)
  preds = predict(m_hp, mtcars_tbl, summary = FALSE, ndraws = 10) %>%
    as.data.frame() %>%
    set_names(seq_len(ncol(.))) %>%
    mutate(
      .chain = NA_integer_,
      .iteration = NA_integer_,
      .draw = seq_len(n())
    ) %>%
    gather(.row, .prediction, -.chain, -.iteration, -.draw) %>%
    as_tibble()

  ref = mtcars_tbl %>%
    mutate(.row = rownames(.)) %>%
    inner_join(preds, by = ".row", multiple = "all") %>%
    mutate(.row = as.integer(.row)) %>%
    group_by(mpg, cyl, disp, hp, drat, wt, qsec, vs, am, gear, carb, .row)

  expect_equal(predicted_draws(m_hp, mtcars_tbl, ndraws = 10, seed = 123), ref)
  expect_equal(add_predicted_draws(mtcars_tbl, m_hp, ndraws = 10, seed = 123), ref)
})

test_that("[add_]predicted_draws works on brms models with categorical outcomes", {
  skip_if_not_installed("brms")
  m_cyl_mpg = readRDS(test_path("../models/models.brms.m_cyl_mpg.rds"))

  set.seed(1234)
  raw_preds = predict(m_cyl_mpg, mtcars_tbl, summary = FALSE, ndraws = 10)
  preds = raw_preds %>%
    array2df(list(.draw = NA, .row = NA), label.x = ".prediction") %>%
    mutate(
      .chain = NA_integer_,
      .iteration = NA_integer_,
      .draw = as.integer(.draw),
      .row = as.character(.row),
    )
  # get this test to pass for now on brms versions that don't output levels anymore
  if (!is.null(attr(raw_preds, "levels"))) {
    preds %<>% mutate(
      .prediction = factor(.prediction, levels = 1:3, labels = paste0("c", c(4,6,8)))
    )
  }

  ref = mtcars_tbl %>%
    mutate(.row = rownames(.)) %>%
    inner_join(preds, by = ".row", multiple = "all") %>%
    mutate(.row = as.integer(.row)) %>%
    select(mpg:.row, .chain, .iteration, .draw, everything()) %>%
    group_by(mpg, cyl, disp, hp, drat, wt, qsec, vs, am, gear, carb, .row)

  expect_equal(predicted_draws(m_cyl_mpg, mtcars_tbl, seed = 1234, ndraws = 10), ref)
  expect_equal(add_predicted_draws(mtcars_tbl, m_cyl_mpg, seed = 1234, ndraws = 10), ref)
})

test_that("[add_]predicted_draws works on brms models with dirichlet responses", {
  skip_if_not_installed("brms")
  skip_if_not(getRversion() >= "4")

  m_dirich = readRDS(test_path("../models/models.brms.m_dirich.rds"))

  set.seed(1234)
  grid = tibble(x = c("A", "B"))
  preds = predict(m_dirich, grid, summary = FALSE, ndraws = 10) %>%
    array2df(list(.draw = NA, .row = NA, .category = TRUE), label.x = ".prediction") %>%
    mutate(
      .chain = NA_integer_,
      .iteration = NA_integer_,
      .row = as.integer(.row),
      .draw = as.integer(.draw)
    )

  ref = grid %>%
    mutate(.row = as.integer(rownames(.))) %>%
    inner_join(preds, by = ".row", multiple = "all") %>%
    select(x, .row, .chain, .iteration, .draw, .category, everything()) %>%
    group_by(x, .row, .category)

  expect_equal(predicted_draws(m_dirich, grid, seed = 1234, ndraws = 10), ref)
})

test_that("[add_]predicted_draws works on brms models with multinomial responses", {
  skip_if_not_installed("brms")
  skip_if_not(getRversion() >= "4")

  m_multinom = readRDS(test_path("../models/models.brms.m_multinom.rds"))

  set.seed(1234)
  # use a low number for total so there are some 0s
  grid = tibble(total = c(10, 20))
  preds = predict(m_multinom, grid, summary = FALSE, ndraws = 10) %>%
    array2df(list(.draw = NA, .row = NA, .category = TRUE), label.x = ".prediction") %>%
    mutate(
      .chain = NA_integer_,
      .iteration = NA_integer_,
      .row = as.integer(.row),
      .draw = as.integer(.draw)
    )

  ref = grid %>%
    mutate(.row = as.integer(rownames(.))) %>%
    inner_join(preds, by = ".row", multiple = "all") %>%
    select(total, .row, .chain, .iteration, .draw, .category, everything()) %>%
    group_by(total, .row, .category)

  expect_equal(predicted_draws(m_multinom, grid, seed = 1234, ndraws = 10), ref)
})

test_that("[add_]predicted_draws throws an error when draws is called instead of ndraws in rstanarm", {
  skip_if_not_installed("rstanarm")
  m_hp_wt = readRDS(test_path("../models/models.rstanarm.m_hp_wt.rds"))

  expect_error(
    m_hp_wt %>% predicted_draws(mtcars_tbl, draws = 10),
    "`draws.*.`ndraws`.*.See the documentation for additional details."
  )
  expect_error(
    mtcars_tbl %>% add_predicted_draws(m_hp_wt, draws = 10),
    "`draws.*.`ndraws`.*.See the documentation for additional details."
  )
})

test_that("[add_]predicted_draws throws an error when re.form is called instead of re_formula in rstanarm", {
  skip_if_not_installed("rstanarm")
  m_hp_wt = readRDS(test_path("../models/models.rstanarm.m_hp_wt.rds"))

  expect_error(
    m_hp_wt %>% predicted_draws(mtcars_tbl, re.form = NULL),
    "`re.form.*.`re_formula`.*.See the documentation for additional details."
  )
  expect_error(
    mtcars_tbl %>% add_predicted_draws(m_hp_wt, re.form = NULL),
    "`re.form.*.`re_formula`.*.See the documentation for additional details."
  )
})
