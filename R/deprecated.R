# Deprecated functions
#
# Author: mjskay
###############################################################################

# Names that should be suppressed from global variable check by codetools
# Names used broadly should be put in _global_variables.R
globalVariables(c(".lower", ".upper", ".width"))


# tidybayes-deprecated ----------------------------------------------------

#' Deprecated functions, arguments, and column names in tidybayes
#'
#' Deprecated functions, arguments, and column names and their alternatives are listed below.
#' Many of the deprecations are due to a naming scheme overhaul in tidybayes version 1.0
#' (see *Deprecated Functions* and *Deprecated Arguments and Column Names* below) or due to
#' the deprecation of horizontal shortcut geoms and stats in tidybayes 2.1 (see
#' *Deprecated Horizontal Shortcut Geoms and Stats*).
#'
#' @section Deprecated Functions:
#'
#' Several deprecated versions of functions use slightly different output
#' formats (e.g., they use names like `term` and `estimate` where new
#' functions use `.variable` and `.value`; or they set `.iteration` even
#' when iteration information is not available --- new functions always set `.draw`
#' but may not set `.iteration`), so be careful when upgrading to new function names.
#' See *Deprecated Arguments and Column Names*, below, for more information.
#'
#' Functions deprecated in tidybayes 3.0:
#'
#' - `fitted_draws` and `add_fitted_draws` are deprecated because their names
#'   were confusing: it was unclear to many users if these functions returned
#'   draws from the posterior predictive, the mean of the posterior predictive,
#'   or the linear predictor (and depending on model type it might have been
#'   either of the latter). Use [epred_draws()]/[add_epred_draws()] if you
#'   want the expectation of the posterior predictive and use
#'   [linpred_draws()]/[add_linpred_draws()] if you want the linear predictor.
#'
#' Functions deprecated in tidybayes 1.0:
#'
#' \itemize{
#'
#'   \item `spread_samples`, `extract_samples`, and `tidy_samples` are
#'   deprecated names for [spread_draws()]. The spread/gather terminology
#'   better distinguishes the resulting data frame format, and *draws* is more
#'   correct terminology than *samples* for describing multiple realizations from
#'   a posterior distribution.
#'
#'   \item `gather_samples` is a deprecated name for [gather_draws()],
#'   reflecting a package-wide move to using *draws* instead of *samples* for
#'   describing multiple realizations from a distribution.
#'
#'   \item `unspread_samples` is a deprecated name for [unspread_draws()],
#'   reflecting a package-wide move to using *draws* instead of *samples* for
#'   describing multiple realizations from a distribution.
#'
#'   \item `ungather_samples` is a deprecated name for [ungather_draws()],
#'   reflecting a package-wide move to using *draws* instead of *samples* for
#'   describing multiple realizations from a distribution.
#'
#'   \item `fitted_samples` / `add_fitted_samples` are deprecated names for
#'   `fitted_draws` / `add_fitted_draws`,
#'   reflecting a package-wide move to using *draws* instead of *samples* for
#'   describing multiple realizations from a distribution. (though see
#'   the note above about the deprecation of `fitted_draws` in favor of
#'   [epred_draws()] and [linpred_draws()]).
#'
#'   \item `predicted_samples` / `add_predicted_samples` are deprecated names for
#'   [predicted_draws()] / [add_predicted_draws()],
#'   reflecting a package-wide move to using *draws* instead of *samples* for
#'   describing multiple realizations from a distribution.
#'
#'   \item `gather_lsmeans_samples` and `gather_emmeans_samples` are deprecated aliases
#'   for [gather_emmeans_draws()]. The new name (estimated marginal means) is more
#'   appropriate for Bayesian models than the old name (least-squares means), and reflects the
#'   naming of the newer `emmeans` package. It also reflects
#'   a package-wide move to using *draws* instead of *samples* for
#'   describing multiple realizations from a distribution.
#'
#'   \item `as_sample_tibble` and `as_sample_data_frame` are deprecated aliases
#'   for [tidy_draws()]. The original intent of `as_sample_tibble` was to be
#'   used primarily internally (hence its less user-friendly name); however, increasingly
#'   I have come across use cases of `tidy_draws` that warrant a more user-friendly name.
#'   It also reflects a package-wide move to using *draws* instead of *samples* for
#'   describing multiple realizations from a distribution.
#'
#'   \item `ggeye` is deprecated: for a package whose goal is flexible and customizable
#'   visualization, monolithic functions are inflexible and do not sufficiently capitalize on users'
#'   existing knowledge of ggplot; instead, I think it is more flexible to design geoms and stats
#'   that can used within a complete ggplot workflow. [stat_eye()] offers a horizontal
#'   eye plot geom that can be used instead of `ggeye`.
#'
#'   \item See the sections below for additional deprecated functions, including
#'   horizontal geoms, stats, and point_intervals
#'
#' }
#'
#' @section Deprecated Eye Geom Spellings:
#'
#' `geom_eye`, `geom_eyeh`, and `geom_halfeyeh` are deprecated spellings of [stat_eye()] and
#' [stat_halfeye()] from before name standardization of stats and geoms. Use those functions instead.
#'
#' @section Deprecated Horizontal Shortcut Geoms and Stats:
#'
#' Due to the introduction of automatic orientation detection in tidybayes 2.1,
#' shortcut geoms and stats (which end in `h`) are no longer necessary, and are
#' deprecated. In most cases, these can simply be replaced with the same
#' geom without the `h` suffix and they will remain horizontal; e.g.
#' `stat_halfeyeh(...)` can simply be replaced with `stat_halfeye(...)`.
#' If automatic orientation detection fails, override it with the `orientation`
#' parameter; e.g. `stat_halfeye(orientation = "horizontal")`.
#'
#' These deprecated stats and geoms include:
#'
#' - `stat_eyeh` / `stat_dist_eyeh`
#' - `stat_halfeyeh` / `stat_dist_halfeyeh`
#' - `geom_slabh` / `stat_slabh` / `stat_dist_slabh`
#' - `geom_intervalh` / `stat_intervalh` / `stat_dist_intervalh`
#' - `geom_pointintervalh` / `stat_pointintervalh` / `stat_dist_pointintervalh`
#' - `stat_gradientintervalh` / `stat_dist_gradientintervalh`
#' - `stat_cdfintervalh` / `stat_dist_cdfintervalh`
#' - `stat_ccdfintervalh` / `stat_dist_ccdfintervalh`
#' - `geom_dotsh` / `stat_dotsh` / `stat_dist_dotsh`
#' - `geom_dotsintervalh` / `stat_intervalh` / `stat_dist_intervalh`
#' - `stat_histintervalh`
#'
#' @section Deprecated Horizontal Point/Interval Functions:
#'
#' These functions ending in `h` (e.g., `point_intervalh`, `median_qih`)
#' used to be needed for use with `ggstance::stat_summaryh`, but are
#' no longer necessary because `ggplot2::stat_summary()` supports
#' automatic orientation detection, so they have been deprecated.
#' They behave identically to the corresponding function without the `h`,
#' except that when passed a vector, they return a data frame with
#' `x`/`xmin`/`xmax` instead of `y`/`ymin`/`ymax`.
#'
#' - `point_intervalh`
#' - `mean_qih` / `median_qih` / `mode_qih`
#' - `mean_hdih` / `median_hdih` / `mode_hdih`
#' - `mean_hdcih` / `median_hdcih` / `mode_hdcih`
#'
#' @section Deprecated Arguments and Column Names:
#'
#' Arguments deprecated in tidybayes 3.0 are:
#'
#' - The `n` argument is now called `ndraws` in `predicted_draws()`, `linpred_draws()`, etc.
#'   This prevents some bugs due to partial matching of argument names where `n` might
#'   be mistaken for `newdata`.
#' - The `value` argument in `linpred_draws()` is now spelled `linpred` and defaults to
#'   `".linpred"` in the same way that the `predicted_draws()` and `epred_draws()` functions
#'   work.
#' - The `scale` argument in `linpred_draws()` is no longer allowed (use `transform` instead)
#'   as this naming scheme only made sense when `linpred_draws()` was an alias for
#'   `fitted_draws()`, which it no longer is (see note above about the deprecation of
#'   `fitted_draws()`).
#'
#' Versions of tidybayes before version 1.0 used a different naming scheme for several
#' arguments and output columns.
#'
#' Arguments and column names deprecated in tidybayes 1.0 are:
#'
#' \itemize{
#'   \item `term` is now `.variable`
#'   \item `estimate` is now `.value`
#'   \item `pred` is now `.prediction`
#'   \item `conf.low` is now `.lower`
#'   \item `conf.high` is now `.upper`
#'   \item `.prob` is now `.width`
#'   \item The `.draw` column was added, and should be used instead of `.chain`
#'     and `.iteration` to uniquely identify draws when you do not care about chains. (`.chain` and
#'     `.iteration` are still provided for identifying draws *within* chains, if desired).
#' }
#'
#' To translate to/from the old naming scheme in output, use [to_broom_names()]
#' and [from_broom_names()].
#'
#' Many of these names were updated in version 1.0 in order to
#' make terminology more consistent and in order to satisfy these criteria:
#'
#' \itemize{
#'   \item Ignore compatibility with broom names on the assumption an adapter function can be created.
#'   \item Use names that could be compatible with frequentist approaches (hence `.width` instead of `.prob`).
#'   \item Always precede with "." to avoid collisions with variable names in models.
#'   \item No abbreviations (remembering if something is abbreviated or not can be a pain).
#'   \item No two-word names (multi-word names can always be standardized on and used in documentation, but I think data frame output should be succinct).
#'   \item Names should be nouns (I made an exception for lower/upper because they are common).
#' }
#'
#' @format NULL
#' @usage NULL
#' @author Matthew Kay
#' @name tidybayes-deprecated
#' @import ggplot2
#' @export
ggeye = function(data = NULL, mapping = NULL, ...) {
  .Deprecated("stat_eye", package = "tidybayes")
  ggplot(data = data, mapping = mapping) + geom_eye(...) + coord_flip()
}


# [add_]fitted_draws -------------------------------------------------

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
add_fitted_draws = function(newdata, model, ..., n = NULL) {
  fitted_draws(model = model, newdata = newdata, ..., n = n)
}

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
fitted_draws = function(
  model, newdata, ..., value = ".value", n = NULL, scale = c("response", "linear")
) {
  UseMethod("fitted_draws")
}

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
fitted_draws.default = function(
  model, newdata, ..., value = ".value", n = NULL, scale = c("response", "linear")
) {
  scale = match.arg(scale)
  deprecation_message_base = paste0(
    "`fitted_draws` and `add_fitted_draws` are deprecated as their names were confusing.\n",
    "- Use [add_]epred_draws() to get the expectation of the posterior predictive.\n",
    "- Use [add_]linpred_draws() to get the distribution of the linear predictor.\n"
  )
  arg_note = paste0(
    "NOTE: When updating to the new functions, note that the `model` parameter is now\n",
    "  named `object` and the `n` parameter is now named `ndraws`."
  )
  switch(scale,
    response = {
      .Deprecated("epred_draws", "tidybayes", paste0(
        deprecation_message_base,
        '- For example, you used [add_]fitted_draws(..., scale = "response"), which\n',
        '  means you most likely want [add_]epred_draws(...).\n',
        arg_note
      ))
      epred_draws(
        object = model, newdata = newdata, ...,
        value = value, ndraws = n
      )
    },
    linear = {
      .Deprecated("linpred_draws", "tidybayes", paste0(
        deprecation_message_base,
        '- For example, you used [add_]fitted_draws(..., scale = "linear"), which\n',
        '  means you most likely want [add_]linpred_draws(...).\n',
        arg_note
      ))
      linpred_draws(
        object = model, newdata = newdata, ...,
        value = value, ndraws = n
      )
    }
  )
}


# [add_]fitted_draws aliases -------------------------------

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
fitted_samples = function(model, newdata, ..., n = NULL) {
  # no deprecation message here as it will be handled in fitted_draws
  fitted_samples_(model, newdata, ..., n = n)       # nocov
}
fitted_samples_ = function(model, newdata, var = "estimate", ..., n = NULL, category = "category") {
  combine_chains_for_deprecated_(fitted_draws(                      # nocov
    model, newdata, value = var, ..., n = n, category = category    # nocov
  ))                                                                # nocov
}

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
add_fitted_samples = function(newdata, model, ..., n = NULL) {
  # no deprecation message here as it will be handled in fitted_draws
  fitted_samples_(model, newdata, ..., n = n)            # nocov
}


# [add_]predicted_draws aliases ----------------------------

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
predicted_samples = function(model, newdata, ..., n = NULL) {
  .Deprecated("predicted_draws", package = "tidybayes") # nocov
  predicted_samples_(model, newdata, ..., n = n) # nocov
}
predicted_samples_ = function(model, newdata, var = "pred", ..., n = NULL) {
  combine_chains_for_deprecated_(predicted_draws( # nocov
    model, newdata, prediction = var, ..., n = n  # nocov
  ))                                              # nocov
}


#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
add_predicted_samples = function(newdata, model, ..., n = NULL) {
  .Deprecated("add_predicted_draws", package = "tidybayes") # nocov
  predicted_samples_(model, newdata, ..., n = n)         # nocov
}


# gather_draws aliases --------------------------------------

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
gather_samples = function(...) {
  .Deprecated("gather_draws", package = "tidybayes") # nocov
  to_broom_names(gather_draws(...))  # nocov
}


# gather_emmeans_draws aliases -----------------------------

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
gather_lsmeans_samples = function(...) {
  .Deprecated("gather_emmeans_draws", package = "tidybayes") # nocov
  combine_chains_for_deprecated_(gather_emmeans_draws(..., value = "estimate")) # nocov
}

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
gather_emmeans_samples = function(...) {
  .Deprecated("gather_emmeans_draws", package = "tidybayes") # nocov
  combine_chains_for_deprecated_(gather_emmeans_draws(..., value = "estimate")) # nocov
}


# gather_variables aliases -----------------------------------

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
gather_terms = function(...) {
  .Deprecated("gather_variables", package = "tidybayes") # nocov
  to_broom_names(gather_variables(...)) # nocov
}


# spread_draws aliases --------------------------------------

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
extract_samples = function(...) {
  .Deprecated("spread_draws", package = "tidybayes") # nocov
  spread_draws(...)               # nocov
}

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
tidy_samples = function(...) {
  .Deprecated("spread_draws", package = "tidybayes") # nocov
  spread_draws(...)              # nocov
}

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
spread_samples = function(...) {
  .Deprecated("spread_draws", package = "tidybayes") # nocov
  spread_draws(...)              # nocov
}


# tidy_draws aliases --------------------------------------

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
as_sample_tibble = function(...) {
  .Deprecated("tidy_draws", package = "tidybayes") # nocov
  tidy_draws(...)                                  # nocov
}

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
as_sample_data_frame = function(...) {
  .Deprecated("tidy_draws", package = "tidybayes") # nocov
  tidy_draws(...)                                  # nocov
}


# ungather_draws aliases --------------------------------------

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
ungather_samples = function(..., term = "term", estimate = "estimate", indices = c(".chain", ".iteration", ".draw")) {
  .Deprecated("ungather_draws", package = "tidybayes") # nocov
  ungather_draws(..., variable = term, value = estimate, draw_indices = indices)  # nocov
}


# unspread_draws alises --------------------------------------

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
unspread_samples = function(..., indices = c(".chain", ".iteration", ".draw")) {
  .Deprecated("unspread_draws", package = "tidybayes") # nocov
  unspread_draws(..., draw_indices = indices)               # nocov
}


# get_variables aliases --------------------------------------

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
parameters = function(model) {
  .Deprecated("get_variables", package = "tidybayes") # nocov
  get_variables(model)  # nocov
}


# layer_geom_slabinterval -------------------------------------------------

# internal helper function for geom creation from ggdist removed in ggdist 3.1
# (replaced by the AbstractGeom / make_geom() framework)
layer_geom_slabinterval = function(
  mapping = NULL,
  default_mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  geom = GeomSlabinterval,
  ...,

  show.legend = NA,
  inherit.aes = TRUE
) {

  .Deprecated_arguments(
    c("size_domain", "size_range"), ..., which = -2,
    message = "Use the interval_size_domain and interval_size_range arguments instead."
  )

  l = layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,

    params = list(
      ...
    )
  )

  if (!is.null(default_mapping)) {
    ggdist:::add_default_computed_aesthetics(l, default_mapping)
  } else {
    l
  }
}


# eye geom aliases -------------------------------------------

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
geom_eye = function(
  ...,
  scale = 0.9,
  .width = c(.66, .95),

  #deprecated arguments
  relative_scale,
  .prob
) {
  .Deprecated("stat_eye", package = "tidybayes")
  .width = .Deprecated_argument_alias(.width, .prob)
  scale = .Deprecated_argument_alias(scale, relative_scale)

  stat_eye(..., .width = .width, scale = scale)
}

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
geom_eyeh = function(
  ...,
  scale = 0.9,
  .width = c(.66, .95),

  #deprecated arguments
  relative_scale,
  .prob
) {
  .Deprecated("stat_eye", package = "tidybayes")
  .width = .Deprecated_argument_alias(.width, .prob)
  scale = .Deprecated_argument_alias(scale, relative_scale)

  stat_eye(..., .width = .width, scale = scale, orientation = "horizontal")
}

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
geom_halfeyeh = function(
  ...,
  scale = 0.9,
  .width = c(.66, .95),

  #deprecated arguments
  relative_scale,
  .prob
) {
  .Deprecated("stat_halfeye", package = "tidybayes")
  .width = .Deprecated_argument_alias(.width, .prob)
  scale = .Deprecated_argument_alias(scale, relative_scale)

  stat_halfeye(..., .width = .width, scale = scale, orientation = "horizontal")
}


# horizontal geoms -----------------------------------

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
geom_slabh = function(..., orientation = "horizontal") {
  .Deprecated("geom_slab", package = "tidybayes")
  geom_slab(..., orientation = orientation)
}

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
geom_intervalh = function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,

  side = "both",
  orientation = "horizontal",
  interval_size_range = c(1, 6),
  show_slab = FALSE,
  show_point = FALSE
) {
  .Deprecated("geom_interval", package = "tidybayes")

  layer_geom_slabinterval(
    data = data,
    mapping = mapping,
    default_mapping = aes(xmin = .lower, xmax = .upper, color = fct_rev_(ordered(.width))),
    stat = stat,
    geom = GeomIntervalh,
    position = position,
    ...,

    side = side,
    orientation = orientation,
    interval_size_range = interval_size_range,
    show_slab = show_slab,
    show_point = show_point,

    datatype = "interval"
  )
}
#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
GeomIntervalh = ggproto("GeomIntervalh", ggdist::GeomSlabinterval,
  default_aes = defaults(aes(
    datatype = "interval"
  ), ggdist::GeomSlabinterval$default_aes),

  default_key_aes = defaults(aes(
    size = 4,
    fill = NA
  ), ggdist::GeomSlabinterval$default_key_aes),

  default_params = defaults(list(
    side = "both",
    orientation = "horizontal",
    interval_size_range = c(1, 6),
    show_slab = FALSE,
    show_point = FALSE
  ), ggdist::GeomSlabinterval$default_params),

  default_datatype = "interval"
)

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
geom_pointintervalh = function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,

  side = "both",
  orientation = "horizontal",
  show_slab = FALSE,

  show.legend = c(size = FALSE)
) {
  .Deprecated("geom_pointinterval", package = "tidybayes")

  layer_geom_slabinterval(
    data = data,
    mapping = mapping,
    default_mapping = aes(xmin = .lower, xmax = .upper, size = -.width),
    stat = stat,
    geom = GeomPointintervalh,
    position = position,
    ...,

    side = side,
    orientation = orientation,
    show_slab = show_slab,

    datatype = "interval",

    show.legend = show.legend
  )
}
#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @import ggplot2
#' @export
GeomPointintervalh = ggproto("GeomPointintervalh", ggdist::GeomSlabinterval,
  default_aes = defaults(aes(
    datatype = "interval"
  ), ggdist::GeomSlabinterval$default_aes),

  default_key_aes = defaults(aes(
    fill = NA
  ), ggdist::GeomSlabinterval$default_key_aes),

  default_params = defaults(list(
    side = "both",
    orientation = "horizontal",
    show_slab = FALSE
  ), ggdist::GeomSlabinterval$default_params),

  default_datatype = "interval"
)

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
geom_dotsh = function(..., orientation = "horizontal") {
  .Deprecated("geom_dots", package = "tidybayes")
  geom_dots(..., orientation = orientation)
}

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
geom_dotsintervalh = function(..., orientation = "horizontal") {
  .Deprecated("geom_dotsinterval", package = "tidybayes")
  geom_dotsinterval(..., orientation = orientation)
}

# horizontal stat_dists ----------------------------------------

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
stat_dist_eyeh = function(..., side = "both", orientation = "horizontal") {
  .Deprecated("stat_dist_eye", package = "tidybayes")
  stat_dist_slabinterval(..., side = side, orientation = orientation)
}

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
stat_dist_halfeyeh = function(..., orientation = "horizontal") {
  .Deprecated("stat_dist_halfeye", package = "tidybayes")
  stat_dist_slabinterval(..., orientation = orientation)
}

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
stat_dist_slabh = function(..., orientation = "horizontal") {
  .Deprecated("stat_dist_slab", package = "tidybayes")
  stat_dist_slab(..., orientation = orientation)
}

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
stat_dist_intervalh = function(..., orientation = "horizontal") {
  .Deprecated("stat_dist_interval", package = "tidybayes")
  stat_dist_interval(..., orientation = orientation)
}

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
stat_dist_pointintervalh = function(..., show_slab = FALSE, orientation = "horizontal") {
  .Deprecated("stat_dist_pointinterval", package = "tidybayes")
  stat_dist_slabinterval(..., show_slab = show_slab, orientation = orientation)
}

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
stat_dist_gradientintervalh = function(..., orientation = "horizontal") {
  .Deprecated("stat_dist_gradientinterval", package = "tidybayes")
  stat_dist_gradientinterval(..., orientation = orientation)
}

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
stat_dist_cdfintervalh = function(...,
  slab_type = "cdf", justification = 0.5, side = "topleft", orientation = "horizontal", normalize = "none"
) {
  .Deprecated("stat_dist_cdfinterval", package = "tidybayes")
  stat_dist_slabinterval(...,
    slab_type = slab_type, justification = justification, side = side, orientation = orientation, normalize = normalize
  )
}

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
stat_dist_ccdfintervalh = function(...,
  slab_type = "ccdf", justification = 0.5, side = "topleft", orientation = "horizontal", normalize = "none"
) {
  .Deprecated("stat_dist_ccdfinterval", package = "tidybayes")
  stat_dist_slabinterval(...,
    slab_type = slab_type, justification = justification, side = side, orientation = orientation, normalize = normalize
  )
}

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
stat_dist_dotsh = function(..., orientation = "horizontal") {
  .Deprecated("stat_dist_dots", package = "tidybayes")
  stat_dist_dots(..., orientation = orientation)
}

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
stat_dist_dotsintervalh = function(..., orientation = "horizontal") {
  .Deprecated("stat_dist_dotsinterval", package = "tidybayes")
  stat_dist_dotsinterval(..., orientation = orientation)
}


# horizontal stats ---------------------------------------------

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
stat_eyeh = function(..., side = "both", orientation = "horizontal") {
  .Deprecated("stat_eye", package = "tidybayes")
  stat_sample_slabinterval(..., side = side, orientation = orientation)
}

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
stat_halfeyeh = function(..., orientation = "horizontal") {
  .Deprecated("stat_halfeye", package = "tidybayes")
  stat_sample_slabinterval(..., orientation = orientation)
}

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
stat_slabh = function(..., orientation = "horizontal") {
  .Deprecated("stat_slab", package = "tidybayes")
  stat_slab(..., orientation = orientation)
}

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
stat_intervalh = function(
  mapping = NULL,
  data = NULL,
  geom = "interval",
  position = "identity",
  ...,

  orientation = "horizontal",
  interval_function = NULL,
  interval_args = list(),
  point_interval = median_qi,
  .width = c(.50, .80, .95),
  show_point = FALSE,
  show_slab = FALSE,
  na.rm = FALSE,

  show.legend = NA,
  inherit.aes = TRUE,

  #deprecated arguments
  .prob,
  fun.data,
  fun.args
) {
  .Deprecated("stat_interval", package = "tidybayes")
  interval_function = .Deprecated_argument_alias(interval_function, fun.data)
  interval_args = .Deprecated_argument_alias(interval_args, fun.args)
  .width = .Deprecated_argument_alias(.width, .prob)

  layer(
    data = data,
    mapping = mapping,
    stat = StatIntervalh,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      orientation = orientation,
      interval_function = interval_function,
      interval_args = interval_args,
      point_interval = point_interval,
      .width = .width,
      show_point = show_point,
      show_slab = show_slab,
      na.rm = na.rm,
      ...
    )
  )
}
StatIntervalh = ggproto("StatIntervalh", ggdist::StatInterval,
  default_params = defaults(list(
    orientation = "horizontal"
  ), ggdist::StatInterval$default_params)
)

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
stat_pointintervalh = function(
  mapping = NULL,
  data = NULL,
  geom = "pointintervalh",
  position = "identity",
  ...,

  orientation = "horizontal",
  interval_function = NULL,
  interval_args = list(),
  point_interval = median_qi,
  .width = c(.66, .95),
  show_slab = FALSE,
  na.rm = FALSE,

  show.legend = c(size = FALSE),
  inherit.aes = TRUE,

  #deprecated arguments
  .prob,
  fun.data,
  fun.args
) {
  .Deprecated("stat_pointinterval", package = "tidybayes")
  interval_function = .Deprecated_argument_alias(interval_function, fun.data)
  interval_args = .Deprecated_argument_alias(interval_args, fun.args)
  .width = .Deprecated_argument_alias(.width, .prob)

  layer(
    data = data,
    mapping = mapping,
    stat = StatPointintervalh,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      orientation = orientation,
      interval_function = interval_function,
      interval_args = interval_args,
      point_interval = point_interval,
      .width = .width,
      show_slab = show_slab,
      na.rm = na.rm,
      ...
    )
  )
}
StatPointintervalh = ggproto("StatPointintervalh", ggdist::StatPointinterval,
  default_params = defaults(list(
    orientation = "horizontal"
  ), ggdist::StatPointinterval$default_params)
)

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
stat_gradientintervalh = function(..., orientation = "horizontal") {
  .Deprecated("stat_gradientinterval", package = "tidybayes")
  stat_gradientinterval(..., orientation = orientation)
}

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
stat_cdfintervalh = function(...,
  slab_type = "cdf", justification = 0.5, side = "topleft", orientation = "horizontal", normalize = "none"
) {
  .Deprecated("stat_cdfinterval", package = "tidybayes")
  stat_sample_slabinterval(...,
    slab_type = slab_type, justification = justification, side = side, orientation = orientation, normalize = normalize
  )
}

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
stat_ccdfintervalh = function(...,
  slab_type = "ccdf", justification = 0.5, side = "topleft", orientation = "horizontal", normalize = "none"
) {
  .Deprecated("stat_ccdfinterval", package = "tidybayes")
  stat_sample_slabinterval(...,
    slab_type = slab_type, justification = justification, side = side, orientation = orientation, normalize = normalize
  )
}

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
stat_dotsh = function(..., orientation = "horizontal") {
  .Deprecated("stat_dots", package = "tidybayes")
  stat_dots(..., orientation = orientation)
}

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
stat_dotsintervalh = function(..., orientation = "horizontal") {
  .Deprecated("stat_dotsinterval", package = "tidybayes")
  stat_dotsinterval(..., orientation = orientation)
}

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
stat_histintervalh = function(..., slab_type = "histogram" , orientation = "horizontal") {
  .Deprecated("stat_dotsinterval", package = "tidybayes")
  stat_sample_slabinterval(..., slab_type = slab_type, orientation = orientation)
}


# horizontal point_intervals ----------------------------------------------

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
point_intervalh = flip_aes(point_interval)

#' @export
#' @format NULL
#' @usage NULL
#' @rdname tidybayes-deprecated
mean_qih = flip_aes(mean_qi)

#' @export
#' @format NULL
#' @usage NULL
#' @rdname tidybayes-deprecated
median_qih = flip_aes(median_qi)

#' @export
#' @format NULL
#' @usage NULL
#' @rdname tidybayes-deprecated
mode_qih = flip_aes(mode_qi)

#' @export
#' @format NULL
#' @usage NULL
#' @rdname tidybayes-deprecated
mean_hdih = flip_aes(mean_hdi)

#' @export
#' @format NULL
#' @usage NULL
#' @rdname tidybayes-deprecated
median_hdih = flip_aes(median_hdi)

#' @export
#' @format NULL
#' @usage NULL
#' @rdname tidybayes-deprecated
mode_hdih = flip_aes(mode_hdi)

#' @export
#' @format NULL
#' @usage NULL
#' @rdname tidybayes-deprecated
mean_hdcih = flip_aes(mean_hdci)

#' @export
#' @format NULL
#' @usage NULL
#' @rdname tidybayes-deprecated
median_hdcih = flip_aes(median_hdci)

#' @export
#' @format NULL
#' @usage NULL
#' @rdname tidybayes-deprecated
mode_hdcih = flip_aes(mode_hdci)
