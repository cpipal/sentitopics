% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jstManyRuns.R
\name{jstManyRuns}
\alias{jstManyRuns}
\title{Run a Joint Sentiment Topic Model Several Times}
\usage{
jstManyRuns(
  dfm,
  sentiLexInput = NULL,
  numSentiLabs = 3,
  numTopics = 10,
  numIters = 3,
  updateParaStep = -1,
  alpha = -1,
  beta = -1,
  gamma = -1,
  excludeNeutral = FALSE,
  n = 30,
  confidence = 0.95,
  ncores = NA,
  seed = NA
)
}
\arguments{
\item{dfm}{A quanteda dfm object}

\item{sentiLexInput}{Optional: A quanteda dictionary object for semi-supervised learning. If
a dictionary is used, \code{numSentiLabs} will be overridden by the number of categories in the
dictionary object. An extra category will by default be added for neutral words. This can be
turned off by setting \code{excludeNeutral = TRUE}.}

\item{numSentiLabs}{Integer, the number of sentiment labels (defaults to 3)}

\item{numTopics}{Integer, the number of topics (defaults to 10)}

\item{numIters}{Integer, the number of iterations (defaults to 3 for test runs, optimize by hand)}

\item{updateParaStep}{Integer. The number of iterations between optimizations of hyperparameter alpha}

\item{alpha}{Double, hyperparameter for (defaults to .05 * (average docsize/number of sentitopics))}

\item{beta}{Double, hyperparameter for (defaults to .01, with multiplier .9/.1 for sentiment dictionary presence)}

\item{gamma}{Double, hyperparameter for (defaults to .05 * (average docsize/number of sentiment categories))}

\item{excludeNeutral}{Boolean. If a dictionary is used, an extra category is added for neutral
words. Words in the dictionary receive a low probability of being allocated there. If this is set
to \code{TRUE}, the neutral sentiment category will be omitted. The variable is irrelevant if no
dictionary is used. Defaults to \code{FALSE}.}

\item{n}{Number of model runs (defaults to 30)}

\item{confidence}{Confidence level for confidence intervals (defaults to 0.95)}

\item{ncores}{Number of (logical) CPU cores used for the estimation. (defaults to available number of cores - 3)}

\item{seed}{Seed for random number generator for reproducibility (defaults to \code{NA})}
}
\value{
A data.frame with mean sentiment label predictions and uncertainty estimates
}
\description{
Estimates a joint sentiment topic model n times and returns mean sentiment label predictions, see Details for model description.
}
\details{
Works similarly to \code{\link{jst}}, but runs the model estimation n times using parallelization. 
Does not return the full results of each individual model, but mean sentiment label predictions 
with uncertainty estimates.
}
\examples{
results <- jstManyRuns(quanteda::dfm(quanteda::data_corpus_irishbudget2010),
             paradigm(),
             numTopics = 5,
             numIters = 15, # Use more in practice!
             n = 5) # Use more in practice!
}
