% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tw_get_others.R
\name{tw_get_description}
\alias{tw_get_description}
\title{Get Wikidata description in given language}
\usage{
tw_get_description(
  id,
  language = tidywikidatar::tw_get_language(),
  id_df = NULL,
  cache = NULL,
  overwrite_cache = FALSE,
  cache_connection = NULL,
  disconnect_db = TRUE,
  wait = 0
)
}
\arguments{
\item{id}{A character vector, must start with Q, e.g. "Q254" for Wolfgang Amadeus Mozart}

\item{language}{Defaults to language set with \code{tw_set_language()}; if not set, "en". Use "all_available" to keep all languages. For available language values, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all}

\item{id_df}{Default to NULL. If given, it should be a dataframe typically generated with \code{tw_get_()}, and is used instead of calling Wikidata or using SQLite cache. Ignored when \code{id} is of length more than one.}

\item{cache}{Defaults to NULL. If given, it should be given either TRUE or FALSE. Typically set with \code{tw_enable_cache()} or \code{tw_disable_cache()}.}

\item{overwrite_cache}{Logical, defaults to FALSE. If TRUE, it overwrites the table in the local sqlite database. Useful if the original Wikidata object has been updated.}

\item{cache_connection}{Defaults to NULL. If NULL, and caching is enabled, \code{tidywikidatar} will use a local sqlite database. A custom connection to other databases can be given (see vignette \code{caching} for details).}

\item{disconnect_db}{Defaults to TRUE. If FALSE, leaves the connection to cache open.}

\item{wait}{In seconds, defaults to 0. Time to wait between queries to Wikidata. If data are cached locally, wait time is not applied. If you are running many queries systematically you may want to add some waiting time between queries.}
}
\value{
A character vector of the same length as the vector of id given, with the Wikidata description in the requested language.
}
\description{
Get Wikidata description in given language
}
\examples{
tw_get_description(
  id = c(
    "Q180099",
    "Q228822"
  ),
  language = "en"
)
}
