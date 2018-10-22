% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deprecated.R, R/fitted_draws.R,
%   R/gather_draws.R, R/gather_emmeans_draws.R, R/gather_variables.R,
%   R/get_variables.R, R/predicted_draws.R, R/spread_draws.R, R/tidy_draws.R,
%   R/ungather_draws.R, R/unspread_draws.R
\name{tidybayes-deprecated}
\alias{tidybayes-deprecated}
\alias{ggeye}
\alias{fitted_samples}
\alias{add_fitted_samples}
\alias{gather_samples}
\alias{gather_lsmeans_samples}
\alias{gather_emmeans_samples}
\alias{gather_terms}
\alias{parameters}
\alias{predicted_samples}
\alias{add_predicted_samples}
\alias{extract_samples}
\alias{tidy_samples}
\alias{spread_samples}
\alias{as_sample_tibble}
\alias{as_sample_data_frame}
\alias{ungather_samples}
\alias{unspread_samples}
\title{Deprecated functions, arguments, and column names in tidybayes}
\description{
Deprecated functions, arguments, and column names and their alternatives are listed below.
Many of the deprecations are due to a naming scheme overhaul in tidybayes version 1.0.
}
\section{Deprecated Functions}{


Several deprecated versions of functions use slightly different output
formats (e.g., they use names like \code{term} and \code{estimate} where new
functions use \code{.variable} and \code{.value}; or they set \code{.iteration} even
when iteration information is not available --- new functions always set \code{.draw}
but may not set \code{.iteration}), so be careful when upgrading to new function names.
See `Deprecated Arguments and Column Names`, below, for more information.

Deprecated functions are:

\itemize{

  \item \code{spread_samples}, \code{extract_samples}, and \code{tidy_samples} are
  deprecated names for \code{\link{spread_draws}}. The spread/gather terminology
  better distinguishes the resulting data frame format, and \emph{draws} is more
  correct terminology than \emph{samples} for describing multiple realizations from
  a posterior distribution.

  \item \code{gather_samples} is a deprecated name for \code{\link{gather_draws}},
  reflecting a package-wide move to using \emph{draws} instead of \emph{samples} for
  describing multiple realizations from a distribution.

  \item \code{unspread_samples} is a deprecated name for \code{\link{unspread_draws}},
  reflecting a package-wide move to using \emph{draws} instead of \emph{samples} for
  describing multiple realizations from a distribution.

  \item \code{ungather_samples} is a deprecated name for \code{\link{ungather_draws}},
  reflecting a package-wide move to using \emph{draws} instead of \emph{samples} for
  describing multiple realizations from a distribution.

  \item \code{fitted_samples} / \code{add_fitted_samples} are deprecated names for
  \code{\link{fitted_draws}} / \code{\link{add_fitted_draws}},
  reflecting a package-wide move to using \emph{draws} instead of \emph{samples} for
  describing multiple realizations from a distribution.

  \item \code{predicted_samples} / \code{add_predicted_samples} are deprecated names for
  \code{\link{predicted_draws}} / \code{\link{add_predicted_draws}},
  reflecting a package-wide move to using \emph{draws} instead of \emph{samples} for
  describing multiple realizations from a distribution.

  \item \code{gather_lsmeans_samples} and \code{gather_emmeans_samples} are deprecated aliases
  for \code{\link{gather_emmeans_draws}}. The new name (estimated marginal means) is more
  appropriate for Bayesian models than the old name (least-squares means), and reflects the
  naming of the newer \code{emmeans} package. It also reflects
  a package-wide move to using \emph{draws} instead of \emph{samples} for
  describing multiple realizations from a distribution.

  \item \code{as_sample_tibble} and \code{as_sample_data_frame} are deprecated aliases
  for \code{\link{tidy_draws}}. The original intent of \code{as_sample_tibble} was to be
  used primarily internally (hence its less user-friendly name); however, increasingly
  I have come across use cases of \code{tidy_draws} that warrant a more user-friendly name.
  It also reflects a package-wide move to using \emph{draws} instead of \emph{samples} for
  describing multiple realizations from a distribution.

  \item \code{ggeye} is deprecated: for a package whose goal is flexible and customizable
  visualization, monolithic functions are inflexible and do not sufficiently capitalize on users'
  existing knowledge of ggplot; instead, I think it is more flexible to design geoms and stats
  that can used within a complete ggplot workflow. \code{\link{geom_eyeh}} offers a horizontal
  eye plot geom that can be used instead of \code{ggeye}.

}
}

\section{Deprecated Arguments and Column Names}{


Versions of tidybayes before version 1.0 used a different naming scheme for several
arguments and output columns.

Deprecated arguments and column names are:

\itemize{
  \item \code{term} is now \code{.variable}
  \item \code{estimate} is now \code{.value}
  \item \code{pred} is now \code{.prediction}
  \item \code{conf.low} is now \code{.lower}
  \item \code{conf.high} is now \code{.upper}
  \item \code{.prob} is now \code{.width}
  \item The \code{.draw} column was added, and should be used instead of \code{.chain}
    and \code{.iteration} to uniquely identify draws when you do not care about chains. (\code{.chain} and
    \code{.iteration} are still provided for identifying draws \emph{within} chains, if desired).
}

To translate to/from the old naming scheme in output, use \code{\link{to_broom_names}}
and \code{\link{from_broom_names}}.

Many of these names were updated in version 1.0 in order to
make terminology more consistent and in order to satisfy these criteria:

\itemize{
  \item Ignore compatibility with broom names on the assumption an adapter function can be created.
  \item Use names that could be compatible with frequentist approaches (hence \code{.width} instead of \code{.prob}).
  \item Always precede with "." to avoid collisions with variable names in models.
  \item No abbreviations (remembering if something is abbreviated or not can be a pain).
  \item No two-word names (multi-word names can always be standardized on and used in documentation, but I think data frame output should be succinct).
  \item Names should be nouns (I made an exception for lower/upper because they are common).
}
}

\author{
Matthew Kay
}