# gather_pairs: gather pairwise combinations of values key/value columns in a long-format data frame
#
# Author: mjskay
###############################################################################

# Names that should be suppressed from global variable check by codetools
# Names used broadly should be put in _global_variables.R
globalVariables(c(".chain", ".iteration"))


#' Gather pairwise combinations of values from key/value columns in a long-format data frame
#'
#' Fast method for producing combinations of values in a value column for different levels of a key column,
#' assuming long-format (tidy) data with an equal number of values per key. Among other things, this is
#' useful for producing scatter-plot matrices.
#'
#' @param data Tidy data frame.
#' @param key Bare name of column in `data` containing the key .
#' @param value Bare name of column in `data` containing the value.
#' @param row Character vector giving the name of the output column identifying rows in the matrix
#' of pairs (takes values of `key`).
#' @param col Character vector giving the name of the output column identifying columns in the matrix
#' of pairs (takes values of `key`).
#' @param x Character vector giving the name of the output column with x values in the matrix
#' of pairs (takes values of `value`).
#' @param y Character vector giving the name of the output column with y values in the matrix
#' of pairs (takes values of `value`).
#' @param triangle Should the upper or lower triangle of the matrix of all possible combinations be returned?
#' The default, `"lower only"`, returns the lower triangle without the diagonal; `"lower"` returns
#' the lower triangle with the diagonal (`"upper"` and `"upper only"` operate analogously),
#' `"both"` returns the full set of possible combinations, and `"both only"` returns all
#' combinations except the diagonal.
#'
#' This method is particularly useful for constructing scatterplot matrices. See examples below.
#'
#' @return A tidy data frame of combinations of values in `key` and `value`, with columns `row`
#' and `col` (default names `".row"` and `".col"`) containing values from `key`,
#' and columns `y` and `x` (default names `".y"` and `".x"`) containing values
#' from `value`.
#'
#' @author Matthew Kay
#' @seealso [emmeans::emmeans()]
#' @keywords manip
#' @examples
#' \donttest{
#'
#' library(ggplot2)
#' library(dplyr)
#'
#' t_a = rnorm(100)
#' t_b = rnorm(100, t_a * 2)
#' t_c = rnorm(100)
#'
#' df = rbind(
#'   data.frame(g = "a", t = t_a),
#'   data.frame(g = "b", t = t_b),
#'   data.frame(g = "c", t = t_c)
#' )
#'
#' df %>%
#'   gather_pairs(g, t, row = "g_row", col = "g_col", x = "t_x", y = "t_y") %>%
#'   ggplot(aes(t_x, t_y)) +
#'   geom_point() +
#'   facet_grid(vars(g_row), vars(g_col))
#'
#' df %>%
#'   gather_pairs(g, t, triangle = "upper") %>%
#'   ggplot(aes(.x, .y)) +
#'   geom_point() +
#'   facet_grid(vars(.row), vars(.col))
#'
#' df %>%
#'   gather_pairs(g, t, triangle = "both") %>%
#'   ggplot(aes(.x, .y)) +
#'   geom_point() +
#'   facet_grid(vars(.row), vars(.col))
#'
#' data(line, package = "coda")
#'
#' line %>%
#'   tidy_draws() %>%
#'   gather_variables() %>%
#'   gather_pairs(.variable, .value) %>%
#'   ggplot(aes(.x, .y)) +
#'   geom_point(alpha = .25) +
#'   facet_grid(vars(.row), vars(.col))
#'
#' line %>%
#'   tidy_draws() %>%
#'   gather_variables() %>%
#'   gather_pairs(.variable, .value) %>%
#'   ggplot(aes(.x, .y, color = factor(.chain))) +
#'   geom_density_2d(alpha = .5) +
#'   facet_grid(vars(.row), vars(.col))
#'
#' }
#' @importFrom magrittr %>% %<>%
#' @importFrom dplyr rename group_by_at ungroup group_vars
#' @export
gather_pairs = function(data, key, value, row = ".row", col = ".col", x = ".x", y = ".y",
    triangle = c("lower only", "upper only", "lower", "upper", "both only", "both")
  ) {
  key = enquo(key)
  value = enquo(value)

  data %<>%
    rename(!!as.name(row) := !!key, !!as.name(y) := !!value)

  if (!is.factor(data[[row]])) {
    data[[row]] = factor(data[[row]])
  }

  # the approach to looking up levels ensures that the ordering is based
  # on the order of level names in the original variable, not on string order
  levels_ = 1:nlevels(data[[row]])
  names(levels_) = levels(data[[row]])
  triangle_test = switch(match.arg(triangle),
    `lower only` = function(row, col) levels_[[row]] > levels_[[col]],
    `upper only` = function(row, col) levels_[[row]] < levels_[[col]],
    lower = function(row, col) levels_[[row]] >= levels_[[col]],
    upper = function(row, col) levels_[[row]] <= levels_[[col]],
    `both only` = function(row, col) levels_[[row]] != levels_[[col]],
    both = function(row, col) TRUE
  )

  groups_ = group_vars(data) %>%
    setdiff(row)

  data %>%
    ungroup() %>%
    gather_pairs_(triangle_test, row, col, x, y) %>%
    group_by_at(c(groups_, row, col))
}

gather_pairs_ = function(data, triangle_test, row, col, x, y) {
  row_data = split(data, data[[row]])
  col_data = lapply(row_data, . %>% select(!!as.name(col) := !!as.name(row), !!as.name(x) := !!as.name(y)))
  imap_dfr_(row_data, function(row_df, row) {
    imap_dfr_(col_data, function(col_df, col) {
      if (triangle_test(row, col)) {
        cbind(row_df, col_df)
      }
    })
  })
}
