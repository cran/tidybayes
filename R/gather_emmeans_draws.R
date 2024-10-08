# gather_emmeans_draws
#
# Author: mjskay
###############################################################################


# gather_emmeans_draws -----------------------------

#' Extract a tidy data frame of draws of posterior distributions of "estimated marginal means" (emmeans/lsmeans) from
#' a Bayesian model fit.
#'
#' Extract draws from the result of a call to [emmeans::emmeans()] (formerly `lsmeans`)
#' or [emmeans::ref_grid()] applied to a Bayesian model.
#'
#' [emmeans::emmeans()] provides a convenient syntax for generating draws from "estimated marginal means" from a model,
#' and can be applied to various Bayesian models, like [rstanarm::stanreg-objects] and
#' [MCMCglmm::MCMCglmm()]. Given a [emmeans::ref_grid()] object as returned by functions like
#' [emmeans::ref_grid()] or [emmeans::emmeans()] applied to a Bayesian model,
#' `gather_emmeans_draws` returns a tidy format data frame of draws from
#' the marginal posterior distributions generated by [emmeans::emmeans()].
#'
#' @param object An `emmGrid` object such as returned by
#' [emmeans::ref_grid()] or [emmeans::emmeans()].
#' @param value The name of the output column to use to contain the values of draws. Defaults to `".value"`.
#' @param grid If `object` is an [emmeans::emm_list()], the name of the output column to use to contain the name of the
#' reference grid that a given row corresponds to. Defaults to `".grid"`.
#' @param ... Additional arguments passed to the underlying method for the type of object given.
#'
#' @return A tidy data frame of draws. The columns of the reference grid are returned as-is, with an
#' additional column called `.value` (by default) containing marginal draws. The resulting data
#' frame is grouped by the columns from the reference grid to make use of summary functions like
#' [point_interval()] straightforward.
#'
#' If `object` is an [emmeans::emm_list()], which contains estimates from different reference grids,
#' an additional column with the default name of `".grid"` is added to indicate the reference grid for each row in the output.
#' The name of this column is controlled by the `grid` argument.
#'
#' @author Matthew Kay
#' @seealso [emmeans::emmeans()]
#' @keywords manip
#' @examples
#' \dontrun{
#'
#' library(dplyr)
#' library(magrittr)
#' library(brms)
#' library(emmeans)
#'
#' # Here's an example dataset with a categorical predictor (`condition`) with several levels:
#' set.seed(5)
#' n = 10
#' n_condition = 5
#' ABC = tibble(
#'   condition = rep(c("A","B","C","D","E"), n),
#'   response = rnorm(n * 5, c(0,1,2,1,-1), 0.5)
#' )
#'
#' m = brm(response ~ condition, data = ABC,
#'   # 1 chain / few iterations just so example runs quickly
#'   # do not use in practice
#'   chains = 1, iter = 500)
#'
#' # Once we've fit the model, we can use emmeans() (and functions
#' # from that package) to get whatever marginal distributions we want.
#' # For example, we can get marginal means by condition:
#' m %>%
#'   emmeans(~ condition) %>%
#'   gather_emmeans_draws() %>%
#'   median_qi()
#'
#' # or we could get pairwise differences:
#' m %>%
#'   emmeans(~ condition) %>%
#'   contrast(method = "pairwise") %>%
#'   gather_emmeans_draws() %>%
#'   median_qi()
#'
#' # see the documentation of emmeans() for more examples of types of
#' # contrasts supported by that packge.
#'
#' }
#' @importFrom magrittr %>%
#' @importFrom tibble as_tibble
#' @importFrom rlang syms
#' @export
gather_emmeans_draws = function(object, value = ".value", ...) UseMethod("gather_emmeans_draws")

#' @rdname gather_emmeans_draws
#' @export
gather_emmeans_draws.default = function(object, value = ".value", ...) {
  grid = as_tibble(object@grid)

  # this matrix will have n_iterations rows and nrow(grid) columns,
  # where mat[, i] is the posterior distribution for grid[i, ]
  mat = object@post.beta %*% t(object@linfct)

  draws = map_dfr_(seq_len(nrow(grid)), function(i) {
    post = as.vector(mat[, i])
    if (!is.null(offset <- object@grid[i, ".offset."])) {
      post = post + offset
    }
    cbind(
      grid[i, ],
      .chain = NA_integer_,
      .iteration = NA_integer_,
      .draw = seq_along(post),
      .value = post
    )
  })

  names(draws)[names(draws) == ".value"] = value

  draws[, setdiff(names(draws), c(".wgt.", ".offset."))] %>%
    as_tibble() %>%
    group_by_at(setdiff(names(.), c(".chain", ".iteration", ".draw", value)))
}

#' @importFrom dplyr mutate_if
#' @rdname gather_emmeans_draws
#' @export
gather_emmeans_draws.emm_list = function(object, value = ".value", grid = ".grid", ...) {
  # lapply does not work properly on raw emm_list objects, hence the need for
  # unclass() here, which makes object a list of emmGrids
  list_of_draw_tibbles = lapply(unclass(object), gather_emmeans_draws, value = value, ...)
  group_names = lapply(list_of_draw_tibbles, group_vars) %>%
    reduce_(union)

  list_of_draw_tibbles %>%
    # bind_rows on a tibble throws irrelevant (in this case) messages about
    # implicit NAs in factors in the case where factor columns do not have values
    # in all tibbles being merge, which may happen in the case where a `contrast`
    # column is output from one emmeans grid but not another. So we'll avoid
    # that by converting factor columns to characters first
    lapply(ungroup) %>%
    lapply(mutate_if, is.factor, as.character) %>%
    bind_rows(.id = grid) %>%
    group_by_at(c(group_names, grid))
}
