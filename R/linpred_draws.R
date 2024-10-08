# [add_]linpred_draws
#
# Author: mjskay
###############################################################################


# linpred_draws aliases -------------------------------------------------

#' @rdname add_predicted_draws
#' @export
add_linpred_draws = function(
  newdata, object, ...,
  value = ".linpred", ndraws = NULL, seed = NULL, re_formula = NULL,
  category = ".category", dpar = NULL,
  # deprecated arguments
  n
) {
  ndraws = .Deprecated_argument_alias(ndraws, n)
  linpred_draws(
    object = object, newdata = newdata, ...,
    value = value, ndraws = ndraws, seed = seed, re_formula = re_formula,
    category = category, dpar = dpar
  )
}

#' @rdname add_predicted_draws
#' @export
linpred_draws = function(
  object, newdata, ...,
  value = ".linpred", ndraws = NULL, seed = NULL, re_formula = NULL,
  category = ".category", dpar = NULL,
  # deprecated arguments
  n, scale
) {
  ndraws = .Deprecated_argument_alias(ndraws, n)
  # we need to update the argument list as well when there are deprecated
  # arguments, otherwise partial matching might assign `n` to `newdata`
  if (!missing(scale)) {
    # scale used to be an alias for transform but is not anymore
    .Deprecated_argument_alias(transform, scale)
    transform = scale == "response"
    linpred_draws(
      object = object, newdata = newdata, ...,
      value = value, ndraws = ndraws, seed = seed, re_formula = re_formula,
      category = category, dpar = dpar,
      transform = transform
    )
  } else if (!missing(n)) {
    linpred_draws(
      object = object, newdata = newdata, ...,
      value = value, ndraws = ndraws, seed = seed, re_formula = re_formula,
      category = category, dpar = dpar
    )
  } else {
    UseMethod("linpred_draws")
  }
}


# linpred_draws generics -------------------------------------------------

#' @rdname add_predicted_draws
#' @export
linpred_draws.default = function(
  object, newdata, ...,
  value = ".linpred", seed = NULL, category = NULL
) {
  pred_draws_default_(
    .name = "linpred_draws",
    .f = rstantools::posterior_linpred, ...,
    object = object, newdata = newdata, output_name = value,
    seed = seed, category = category
  )
}

#' @rdname add_predicted_draws
#' @export
linpred_draws.stanreg = function(
  object, newdata, ...,
  value = ".linpred", ndraws = NULL, seed = NULL, re_formula = NULL,
  category = ".category", dpar = NULL
) {
  stop_on_non_generic_arg_(
    names(enquos(...)), "[add_]linpred_draws", re_formula = "re.form"
  )

  pred_draws_(
    .f = rstantools::posterior_linpred, ...,
    object = object, newdata = newdata, output_name = value,
    draws = ndraws, seed = seed, category = category, re.form = re_formula
  )
}

#' @rdname add_predicted_draws
#' @importFrom rlang is_true is_false is_empty
#' @importFrom dplyr select_at
#' @export
linpred_draws.brmsfit = function(
  object, newdata, ...,
  value = ".linpred", ndraws = NULL, seed = NULL, re_formula = NULL,
  category = ".category", dpar = NULL
) {
  pred_draws_(
    .f = rstantools::posterior_linpred, ...,
    object = object, newdata = newdata, output_name = value,
    ndraws = ndraws, seed = seed, re_formula = re_formula, category = category, dpar = dpar
  )
}
