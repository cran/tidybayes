# gather_variables
#
# Author: mjskay
###############################################################################


# gather_variables --------------------------------------------------------

#' Gather variables from a tidy data frame of draws from variables into a single column
#'
#' Given a data frame such as might be returned by [tidy_draws()] or [spread_draws()],
#' gather variables and their values from that data frame into a `".variable"` and `".value"` column.
#'
#' This function gathers every column except grouping columns and those matching the expression
#' `exclude` into key/value columns `".variable"` and `".value"`.
#'
#' Imagine a data frame `data` as returned by `spread_draws(fit, a[i], b[i,v])`, like this:
#' \itemize{
#'      \item column `".chain"`: the chain number
#'      \item column `".iteration"`: the iteration number
#'      \item column `".draw"`: the draw number
#'      \item column `"i"`: value in `1:5`
#'      \item column `"v"`: value in `1:10`
#'      \item column `"a"`: value of `"a[i]"` for draw number `".draw"`
#'      \item column `"b"`: value of `"b[i,v]"` for draw number `".draw"`
#'  }
#'
#' `gather_variables(data)` on that data frame would return a grouped
#' data frame (grouped by `i` and `v`), with:
#' \itemize{
#'      \item column `".chain"`: the chain number
#'      \item column `".iteration"`: the iteration number
#'      \item column `".draw"`: the draw number
#'      \item column `"i"`: value in `1:5`
#'      \item column `"v"`: value in `1:10`
#'      \item column `".variable"`: value in `c("a", "b")`.
#'      \item column `".value"`: value of `"a[i]"` (when `".variable"` is `"a"`;
#'          repeated for every value of `"v"`) or `"b[i,v]"` (when `".variable"` is
#'          `"b"`) for draw number `".draw"`
#'  }
#'
#' In this example, this call:
#'
#' \preformatted{gather_variables(data)}
#'
#' Is roughly equivalent to:
#'
#' \preformatted{data \%>\%
#'   gather(.variable, .value, -c(.chain, .iteration, .draw, i, v)) \%>\%
#'   group_by(.variable, .add = TRUE)
#' }
#'
#' @param data A data frame with variable names spread across columns, such as one returned by
#' [tidy_draws()] or [spread_draws()].
#' @param exclude A character vector of names of columns to be excluded from the gather. Default
#' ignores several meta-data column names used in tidybayes.
#' @return A data frame.
#' @author Matthew Kay
#' @seealso [spread_draws()], [tidy_draws()].
#' @keywords manip
#' @examples
#' \donttest{
#'
#' library(dplyr)
#'
#' data(RankCorr, package = "ggdist")
#'
#' RankCorr %>%
#'   spread_draws(b[i,v], tau[i]) %>%
#'   gather_variables() %>%
#'   median_qi()
#'
#' # the first three lines below are roughly equivalent to ggmcmc::ggs(RankCorr)
#' RankCorr %>%
#'   tidy_draws() %>%
#'   gather_variables() %>%
#'   median_qi()
#'
#' }
#' @importFrom magrittr %>%
#' @importFrom tidyr gather
#' @importFrom dplyr group_vars
#' @export
gather_variables = function(data, exclude = c(".chain", ".iteration", ".draw", ".row")) {
  groups_ = group_vars(data)

  # get a list of the names of columns that are explicitly excluded or
  # which are grouping columns (these are dimensions from the spec)
  special_columns = names(data) %>%
    intersect(exclude) %>%
    union(groups_)

  # translate that list into quoted negations of those column names
  # so we can exclude them from the gather()
  #  -> e.g. list(~ -.chain, ~ -.iteration, ~ -.draw, ~ -i)
  columns_excluded_from_gather = special_columns %>%
    lapply(function(x) quo(-!!as.name(x)))

  data %>%
    gather(".variable", ".value", !!!columns_excluded_from_gather) %>%
    group_by_at(c(groups_, ".variable"), .add = TRUE)
}
