# [add_]epred_draws
#
# Author: mjskay
###############################################################################


# epred_draws aliases -------------------------------------------------

#' @rdname add_predicted_draws
#' @export
add_epred_draws = function(
  newdata, object, ...,
  value = ".epred", ndraws = NULL, seed = NULL, re_formula = NULL,
  category = ".category", dpar = NULL
) {
  epred_draws(
    object = object, newdata = newdata, ...,
    value = value, ndraws = ndraws, seed = seed, re_formula = re_formula,
    category = category, dpar = dpar
  )
}

#' @rdname add_predicted_draws
#' @export
epred_draws = function(
  object, newdata, ...,
  value = ".epred", ndraws = NULL, seed = NULL, re_formula = NULL,
  category = ".category", dpar = NULL
) {
  UseMethod("epred_draws")
}


# epred_draws generics -------------------------------------------------

#' @rdname add_predicted_draws
#' @export
epred_draws.default = function(
  object, newdata, ...,
  value = ".epred", seed = NULL, category = NULL
) {
  pred_draws_default_(
    .name = "epred_draws",
    .f = rstantools::posterior_epred, ...,
    object = object, newdata = newdata, output_name = value,
    seed = seed, category = category
  )
}

#' @rdname add_predicted_draws
#' @export
epred_draws.stanreg = function(
  object, newdata, ...,
  value = ".epred", ndraws = NULL, seed = NULL, re_formula = NULL,
  category = ".category", dpar = NULL
) {
  stop_on_non_generic_arg_(
    names(enquos(...)), "[add_]epred_draws", re_formula = "re.form"
  )

  pred_draws_(
    .f = rstantools::posterior_epred, ...,
    object = object, newdata = newdata, output_name = value,
    draws = ndraws, seed = seed, category = category, re.form = re_formula
  )
}

#' @rdname add_predicted_draws
#' @importFrom rlang is_true is_false is_empty
#' @importFrom dplyr select_at
#' @export
epred_draws.brmsfit = function(
  object, newdata, ...,
  value = ".epred", ndraws = NULL, seed = NULL, re_formula = NULL,
  category = ".category", dpar = NULL
) {
  pred_draws_(
    .f = rstantools::posterior_epred, ...,
    object = object, newdata = newdata, output_name = value,
    ndraws = ndraws, seed = seed, re_formula = re_formula, category = category, dpar = dpar
  )
}
