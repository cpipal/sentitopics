#' @include topNwords.R
#' @importFrom methods new

#' @title Reverse JST results object
#'
#' @description Contains estimated reversed Joint Sentiment Topic model, see Details for model description.
#'
#' @slot pi Document-level topic estimates
#' @slot theta Document-level sentitopic estimates
#' @slot phi Word-level sentitopic estimates
#' @slot phi.termScores Word-level term scores (suboptimal calculation, only useful for smaller models)
#' @slot numTopics Number of topics
#' @slot numSentiments Number of sentiment categories
#' @slot docvars Document-level metadata from the quanteda object used as input
JST_reversed.result <- setClass(
  'JST_reversed.result',
  representation(
    pi = "data.frame",
    theta = "data.frame",
    phi = "data.frame",
    phi.termScores = "data.frame",
    numTopics = "numeric",
    numSentiments = "numeric",
    docvars = "data.frame"
  )
)

#' Check if an object is a JST_reversed.result object
#'
#' @param x object
#'
#' @return Boolean. True if x is a JST_reversed.result object.
#'
#' @export
is.JST_reversed.result <- function(x) {
  return(inherits(x, 'JST_reversed.result'))
}


#' Run a reversed Joint Sentiment Topic model
#'
#' Estimates a reversed joint sentiment topic model using a Gibbs sampler, see Details for model description.
#'
#' Lin, C., He, Y., Everson, R. and Ruger, S., 2012. Weakly supervised joint sentiment-topic
#' detection from text. IEEE Transactions on Knowledge and Data engineering, 24(6), pp.1134-1145.
#'
#' @param dfm A quanteda dfm object
#' @param sentiLexInput Optional: A quanteda dictionary object for semi-supervised learning. If
#' a dictionary is used, \code{numSentiLabs} will be overridden by the number of categories in the
#' dictionary object. An extra category will by default be added for neutral words. This can be
#' turned off by setting \code{excludeNeutral = TRUE}.
#' @param numSentiLabs Integer, the number of sentiment labels (defaults to 3)
#' @param numTopics Integer, the number of topics (defaults to 10)
#' @param numIters Integer, the number of iterations (defaults to 3 for test runs, optimize by hand)
#' @param updateParaStep Integer. The number of iterations between optimizations
#' of hyperparameter alpha
#' @param alpha Double, hyperparameter for (defaults to .05*(average docsize/number of topics))
#' @param beta Double, hyperparameter for (defaults to .01, with multiplier .9/.1 for sentiment dictionary presence)
#' @param gamma Double, hyperparameter for (defaults to .05 * (average docsize/number of sentitopics)
#' @param excludeNeutral Boolean. If a dictionary is used, an extra category is added for neutral
#' words. Words in the dictionary receive a low probability of being allocated there. If this is set
#' to \code{TRUE}, the neutral sentiment category will be omitted. The variable is irrelevant if no
#' dictionary is used. Defaults to \code{FALSE}.
#' @return A JST_reversed.result object containing a data.frame for each estimated
#' parameter
#'
#' @examples
#' model <- jst(quanteda::dfm(quanteda::data_corpus_irishbudget2010),
#'              paradigm(),
#'              numTopics = 5,
#'              numIters = 150)
#'
#' @export
jst_reversed <- function(dfm,
                         sentiLexInput = NULL,
                         numSentiLabs = 3,
                         numTopics = 10,
                         numIters = 3,
                         updateParaStep = -1,
                         alpha = -1,
                         beta = -1,
                         gamma = -1,
                         excludeNeutral = FALSE) {
  if (!any(class(dfm) %in% c("dfm", "DocumentTermMatrix"))) {
    stop("Please input a quanteda dfm or tidytext tm object as data.")
  }
  if(any(class(dfm) == "DocumentTermMatrix")) {
    dfm <- dfm %>% quanteda::as.dfm()
  }

  sentiWords <- integer()
  sentimentCategory <- integer()

  if (!is.null(sentiLexInput)) {
    if (quanteda::is.dictionary(sentiLexInput)) {
      numSentiLabs_Lex <- length(sentiLexInput)
      numSentiLabs <- numSentiLabs_Lex + 1 - excludeNeutral

      size <- 1
      for (i in c(1:numSentiLabs_Lex)) {
        for (word in sentiLexInput[[i]]) {
          if (word %in% quanteda::featnames(dfm)) {
            sentiWords[size] <-
              as.integer(match(word, quanteda::featnames(dfm)) - 1) #-1 for C++ index
            sentimentCategory[size] <- as.integer(i - excludeNeutral)
            size <- size + 1
          }
        }
      }
    } else {
      stop('The input lexicon needs to be a quanteda dictionary object.')
    }
  }

  res <-
    jstcppreversed(
      dfm,
      sentiWords,
      sentimentCategory,
      numSentiLabs,
      numTopics,
      numIters,
      updateParaStep,
      alpha,
      beta,
      gamma
    )

  if (length(res) == 0) {
    return(0)
  }

  #prepare doc topic distribution data.frame
  docID <- quanteda::docnames(dfm)

  theta <- as.data.frame(res$theta)

  theta.names = character(numTopics)
  for (i in c(1:numTopics)) {
    theta.names[i] <- paste("topic", i, sep = "")
  }
  names(theta) <- theta.names
  rownames(theta) <- docID

  #prepare doc topic/sentiment distribution data.frame
  pi <- as.data.frame(res$pi)
  pi <- as.data.frame(t(pi))

  pi.names <- character(numSentiLabs)
  for (i in c(1:numSentiLabs)) {
    pi.names[i] <- paste('sent', i, sep = '')
  }
  names(pi) <- pi.names

  pi$docID <- docID
  pi$docID <- as.factor(pi$docID)

  topic <- numeric()

  for (i in c(1:numTopics)) {
    topic <- c(topic, rep(i, quanteda::ndoc(dfm)))
  }

  pi$topic <- topic

  pi <- pi[, c('docID', 'topic', pi.names)]

  rownames(pi) <- NULL

  #prepare word topic/sentiment distribtuion data.frame
  phi <- as.data.frame(res$phi)
  phi.termScores <- as.data.frame(res$phi.termScores)

  phi.names = character(numSentiLabs * numTopics)
  for (i in c(1:numTopics)) {
    for (j in c(1:numSentiLabs)) {
      phi.names[j + numSentiLabs * (i - 1)] <-
        paste("topic", i, "sent", j, sep = "")
    }
  }
  names(phi) <- phi.names
  names(phi.termScores) <- phi.names
  rownames(phi) <- quanteda::featnames(dfm)
  rownames(phi.termScores) <- quanteda::featnames(dfm)

  return(
    JST_reversed.result(
      pi = pi,
      theta = theta,
      phi = phi,
      phi.termScores = phi.termScores,
      numTopics = numTopics,
      numSentiments = numSentiLabs,
      docvars = data.frame(quanteda::docvars(dfm), row.names = docID,
                           stringsAsFactors = FALSE)
    )
  )
}

#' @rdname topNwords-method
#' @aliases topNwords,JST_reversed.result,numeric,numeric,numeric-method
setMethod('topNwords', c('JST_reversed.result', 'numeric', 'numeric', 'numeric'),
          function(x, N, topic, sentiment) {
            colname <- paste('topic', topic, 'sent', sentiment, sep = '')

            column <- sapply(x@phi[colname], as.numeric)

            res <- rownames(x@phi)[topNwordSeeds(column, N)]

            res <- as.data.frame(res, stringsAsFactors = FALSE)

            names(res) <- colname

            return(res)
          })

#' @rdname topNwords-method
#' @aliases topNwords,JST_reversed.result,numeric,-method
setMethod('topNwords', c('JST_reversed.result', 'numeric'),
          function(x, N) {
            res <- as.data.frame(matrix(ncol = 0, nrow = N))

            for (topic in c(1:x@numTopics)) {
              for (sentiment in c(1:x@numSentiments)) {
                res <- cbind(res, topNwords(x, N, topic, sentiment))
              }
            }

            return(res)
          })

#' @rdname top20words-method
#' @aliases top20words,JST_reversed.result,numeric,numeric-method
setMethod('top20words', c('JST_reversed.result', 'numeric', 'numeric'),
          function(x, topic, sentiment) {
            return(topNwords(x, 20, topic, sentiment))
          })

#' @rdname top20words-method
#' @aliases top20words,JST_reversed.result-method
setMethod('top20words', c('JST_reversed.result'),
          function(x) {
            return(topNwords(x, 20))
          })
