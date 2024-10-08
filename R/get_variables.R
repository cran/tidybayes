# get_variables
#
# Author: mjskay
###############################################################################


#' Get the names of the variables in a fitted Bayesian model
#'
#' Get a character vector of the names of the variables in a variety of fitted
#' Bayesian model types. All models supported by [tidy_draws()] are
#' supported.
#'
#' This function is often useful for inspecting a model interactively in order
#' to construct calls to [spread_draws()] or [gather_draws()]
#' in order to extract draws from models in a tidy format.
#'
#' @template param-model
#' @return A character vector of variable names in the fitted model.
#' @author Matthew Kay
#' @seealso [spread_draws()], [gather_draws()].
#' @keywords manip
#' @examples
#'
#' data(line, package = "coda")
#' get_variables(line)
#'
#' data(RankCorr, package = "ggdist")
#' get_variables(RankCorr)
#'
#' @importFrom magrittr %>%
#' @export
get_variables = function(model) UseMethod("get_variables")

#' @rdname get_variables
#' @export
get_variables.default = function(model) {
  #default method just uses tidy_draws; could add faster model-type-specific methods
  model %>%
    tidy_draws() %>%
    names() %>%
    setdiff(c(".chain", ".iteration", ".draw"))
}


#' @rdname get_variables
#' @export
get_variables.mcmc = function(model) {
  coda::varnames(model, allow.null = FALSE)
}

#' @rdname get_variables
#' @export
get_variables.mcmc.list = get_variables.mcmc
