as_constructor = function(x) UseMethod("as_constructor")

#' @export
as_constructor.default = function(x) identity

#' @export
as_constructor.factor = function(x) {
  x_levels = levels(x)
  x_is_ordered = is.ordered(x)
  function(x) factor(x, levels = seq_along(x_levels), labels = x_levels, ordered = x_is_ordered)
}

#' @export
as_constructor.character = function(x) {
  x_levels = levels(as.factor(x))
  function(x) x_levels[x]
}

#' @export
as_constructor.logical = function(x) as.logical


#' @export
apply_prototypes = function(...) {
  .Deprecated("recover_types", package = "tidybayes") # nocov
  recover_types(...)            # nocov
}

#' Decorate a model fit or sample with data types recovered from the input data
#'
#' Decorate a Bayesian model fit or a sample from it with types for
#' variable and dimension data types. Meant to be used before calling
#' [spread_draws()] or [gather_draws()] so that the values returned by
#' those functions are translated back into useful data types.
#'
#' Each argument in `...` specifies a list or data.frame. The `model`
#' is decorated with a list of constructors that can convert a numeric column
#' into the data types in the lists in `...`.
#'
#' Then, when [spread_draws()] or [gather_draws()] is called on the decorated
#' `model`, each list entry with the same name as the variable or a dimension
#' in `variable_spec` is a used as a prototype for that variable or dimension ---
#' i.e., its type is taken to be the expected type of that variable or dimension.
#' Those types are used to translate numeric values of variables back into
#' useful values (for example, levels of a factor).
#'
#' The most common use of `recover_types` is to automatically translate
#' dimensions of a variable that correspond to levels of a factor in the original data back into
#' levels of that factor. The simplest way to do this is to pass in the data
#' frame from which the original data came.
#'
#' Supported types of prototypes are factor, ordered, and logical. For example:
#'
#' \itemize{ \item if `prototypes$v` is a factor, the v column in the
#' returned draws is translated into a factor using `factor(v,
#' labels=levels(prototypes$v), ordered=is.ordered(prototypes$v))`.  \item if
#' `prototypes$v` is a logical, the v column is translated into a logical
#' using `as.logical(v)`. }
#'
#' Additional data types can be supported by providing a custom implementation
#' of the generic function `as_constructor`.
#'
#' @template param-model
#' @param ...  Lists (or data frames) providing data prototypes used to convert
#' columns returned by [spread_draws()] and [gather_draws()] back into useful data types.
#' See *Details*.
#' @return A decorated version of `model`.
#' @author Matthew Kay
#' @aliases apply_prototypes
#' @seealso [spread_draws()], [gather_draws()], [compose_data()].
#' @keywords manip
#' @examples
#' \dontrun{
#'
#' library(dplyr)
#' library(magrittr)
#' library(rstan)
#'
#' # Here's an example dataset with a categorical predictor (`condition`) with several levels:
#' set.seed(5)
#' n = 10
#' n_condition = 5
#' ABC = tibble(
#'   condition = factor(rep(c("A","B","C","D","E"), n)),
#'   response = rnorm(n * 5, c(0,1,2,1,-1), 0.5)
#' )
#'
#' # We'll fit the following model to it:
#' stan_code = "
#'   data {
#'     int<lower=1> n;
#'     int<lower=1> n_condition;
#'     int<lower=1, upper=n_condition> condition[n];
#'     real response[n];
#'   }
#'   parameters {
#'     real overall_mean;
#'     vector[n_condition] condition_zoffset;
#'     real<lower=0> response_sd;
#'     real<lower=0> condition_mean_sd;
#'   }
#'   transformed parameters {
#'     vector[n_condition] condition_mean;
#'     condition_mean = overall_mean + condition_zoffset * condition_mean_sd;
#'   }
#'   model {
#'     response_sd ~ cauchy(0, 1);       // => half-cauchy(0, 1)
#'     condition_mean_sd ~ cauchy(0, 1); // => half-cauchy(0, 1)
#'     overall_mean ~ normal(0, 5);
#'
#'     //=> condition_mean ~ normal(overall_mean, condition_mean_sd)
#'     condition_zoffset ~ normal(0, 1);
#'
#'     for (i in 1:n) {
#'       response[i] ~ normal(condition_mean[condition[i]], response_sd);
#'     }
#'   }
#' "
#'
#' m = stan(model_code = stan_code, data = compose_data(ABC), control = list(adapt_delta=0.99),
#'   # 1 chain / few iterations just so example runs quickly
#'   # do not use in practice
#'   chains = 1, iter = 500)
#'
#' # without using recover_types(), the `condition` column returned by spread_draws()
#' # will be an integer:
#' m %>%
#'   spread_draws(condition_mean[condition]) %>%
#'   median_qi()
#'
#' # If we apply recover_types() first, subsequent calls to other tidybayes functions will
#' # automatically back-convert factors so that they are labeled with their original levels
#' # (assuming the same name is used)
#' m %<>% recover_types(ABC)
#'
#' # now the `condition` column with be a factor with levels "A", "B", "C", ...
#' m %>%
#'   spread_draws(condition_mean[condition]) %>%
#'   median_qi()
#'
#' }
#' @export
recover_types = function(model, ...) {
  if (!is.list(attr(model, "tidybayes_constructors"))) {
    attr(model, "tidybayes_constructors") = list()
  }

  character_variables = character()
  for (prototypes in list(...)) {
    if (!is.list(prototypes)) {
      cli::cli_abort(c(
        "All arguments to {.help recover_types} (except the model) must be lists or data frames.",
        ">" = "Did you mean to do something like {.code recover_types(model, list(...))}?"
      ))
    }

    #we iterate this way instead of building a list directly
    #so that existing names are overwritten
    for (variable_name in names(prototypes)) {
      variable = prototypes[[variable_name]]
      if (is.character(variable)) {
        character_variables = c(character_variables, variable_name)
      }
      attr(model, "tidybayes_constructors")[[variable_name]] = as_constructor(variable)
    }
  }

  # deprecation warning for character variables
  if (length(character_variables) > 0) {
    cli::cli_warn(c(
      "It is no longer recommended to use character vectors with {.help recover_types}, as the intended order of the levels of the variable is ambiguous.",
      "i" = "The following character vectors were provided to {.fun recover_types}: {.var {character_variables}}",
      ">" = "Instead of using character vectors, convert the variable to a {.fun factor}
        {.emph before} passing the data into your model, then pass the {.emph same}
        factor variable to {.fun recover_types} after the model is fit. This will
        ensure that the level order matches exactly when variable indices are
        extracted from the model."
    ))
  }

  model
}
