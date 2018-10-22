as_constructor = function(x) UseMethod("as_constructor")

as_constructor.default = function(x) identity

as_constructor.factor = function(x) {
  x_levels = levels(x)
  x_is_ordered = is.ordered(x)
  function(x) factor(x, levels = seq_along(x_levels), labels = x_levels, ordered = x_is_ordered)
}

as_constructor.character = function(x) {
  x_levels = levels(as.factor(x))
  function(x) x_levels[x]
}

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
#' \code{\link{spread_draws}} or \code{\link{gather_draws}} so that the values returned by
#' those functions are translated back into useful data types.
#'
#' Each argument in \code{...} specifies a list or data.frame. The \code{model}
#' is decorated with a list of constructors that can convert a numeric column
#' into the data types in the lists in \code{...}.
#'
#' Then, when \code{\link{spread_draws}} or \code{\link{gather_draws}} is called on the decorated
#' \code{model}, each list entry with the same name as the variable or a dimension
#' in \code{variable_spec} is a used as a prototype for that variable or dimension ---
#' i.e., its type is taken to be the expected type of that variable or dimension.
#' Those types are used to translate numeric values of variables back into
#' useful values (for example, levels of a factor).
#'
#' The most common use of \code{recover_types} is to automatically translate
#' dimensions of a variable that correspond to levels of a factor in the original data back into
#' levels of that factor. The simplest way to do this is to pass in the data
#' frame from which the original data came.
#'
#' Supported types of prototypes are factor, ordered, and logical. For example:
#'
#' \itemize{ \item if \code{prototypes$v} is a factor, the v column in the
#' returned draws is translated into a factor using \code{factor(v,
#' labels=levels(prototypes$v), ordered=is.ordered(prototypes$v))}.  \item if
#' \code{prototypes$v} is a logical, the v column is translated into a logical
#' using \code{as.logical(v)}. }
#'
#' Additional data types can be supported by providing a custom implementation
#' of the generic function \code{as_constructor}.
#'
#' @param model A supported Bayesian model fit. Tidybayes supports a variety of model objects;
#' for a full list of supported models, see \link{tidybayes-models}.
#' @param ...  Lists (or data frames) providing data prototypes used to convert
#' columns returned by \code{\link{spread_draws}} and \code{\link{gather_draws}} back into useful data types.
#' See `Details`.
#' @return A decorated version of \code{model}.
#' @author Matthew Kay
#' @aliases apply_prototypes
#' @seealso \code{\link{spread_draws}}, \code{\link{gather_draws}}, \code{\link{compose_data}}.
#' @keywords manip
#' @examples
#' \donttest{
#'
#' library(dplyr)
#' library(magrittr)
#'
#' if(require("rstan", quietly = TRUE)) {
#'
#'   # Here's an example dataset with a categorical predictor (`condition`) with several levels:
#'   set.seed(5)
#'   n = 10
#'   n_condition = 5
#'   ABC =
#'     data_frame(
#'       condition = rep(c("A","B","C","D","E"), n),
#'       response = rnorm(n * 5, c(0,1,2,1,-1), 0.5)
#'     )
#'
#'   # We'll fit the following model to it:
#'   stan_code = "
#'     data {
#'       int<lower=1> n;
#'       int<lower=1> n_condition;
#'       int<lower=1, upper=n_condition> condition[n];
#'       real response[n];
#'     }
#'     parameters {
#'       real overall_mean;
#'       vector[n_condition] condition_zoffset;
#'       real<lower=0> response_sd;
#'       real<lower=0> condition_mean_sd;
#'     }
#'     transformed parameters {
#'       vector[n_condition] condition_mean;
#'       condition_mean = overall_mean + condition_zoffset * condition_mean_sd;
#'     }
#'     model {
#'       response_sd ~ cauchy(0, 1);       // => half-cauchy(0, 1)
#'       condition_mean_sd ~ cauchy(0, 1); // => half-cauchy(0, 1)
#'       overall_mean ~ normal(0, 5);
#'
#'       //=> condition_mean ~ normal(overall_mean, condition_mean_sd)
#'       condition_zoffset ~ normal(0, 1);
#'
#'       for (i in 1:n) {
#'         response[i] ~ normal(condition_mean[condition[i]], response_sd);
#'       }
#'     }
#'     "
#'
#'   m = stan(model_code = stan_code, data = compose_data(ABC), control = list(adapt_delta=0.99),
#'     # 1 chain / few iterations just so example runs quickly
#'     # do not use in practice
#'     chains = 1, iter = 500)
#'
#'   # without using recover_types(), the `condition` column returned by spread_draws()
#'   # will be an integer:
#'   m %>%
#'     spread_draws(condition_mean[condition]) %>%
#'     median_qi()
#'
#'   # If we apply recover_types() first, subsequent calls to other tidybayes functions will
#'   # automatically back-convert factors so that they are labeled with their original levels
#'   # (assuming the same name is used)
#'   m %<>% recover_types(ABC)
#'
#'   # now the `condition` column with be a factor with levels "A", "B", "C", ...
#'   m %>%
#'     spread_draws(condition_mean[condition]) %>%
#'     median_qi()
#'
#' }
#' }
#' @export
recover_types = function(model, ...) {
  if (!is.list(attr(model, "constructors"))) {
    attr(model, "constructors") = list()
  }

  for (prototypes in list(...)) {
    #we iterate this way instead of building a list directly
    #so that existing names are overwritten
    for (variable_name in names(prototypes)) {
      attr(model, "constructors")[[variable_name]] = as_constructor(prototypes[[variable_name]])
    }
  }

  model
}