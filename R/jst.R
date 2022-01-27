#' @include topNwords.R
#' @importFrom methods new

#' @title JST results object
#'
#' @description Contains estimated Joint Sentiment Topic model
#'
#' @slot pi Document-level sentiment estimates
#' @slot theta Document-level sentitopic estimates
#' @slot phi Word-level sentitopic estimates
#' @slot phi.termScores Word-level term scores (suboptimal calculation, only useful for smaller models)
#' @slot numTopics Number of topics
#' @slot numSentiments Number of sentiment categories
#' @slot docvars Document-level metadata from the quanteda object used as input
JST.result <- setClass(
  'JST.result',
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

#' Check if an object is a JST.result object
#'
#' @param x object
#'
#' @return Boolean. True if x is a JST.result object.
#'
#' @export
is.JST.result <- function(x) {
  return(inherits(x, 'JST.result'))
}

#' Run a Joint Sentiment Topic model
#'
#' Estimates a joint sentiment topic model using a Gibbs sampler, see Details for model description.
#'
#' Basic model description:
#'
#' Lin, C. and He, Y., 2009, November. Joint sentiment/topic model for sentiment analysis. In
#' Proceedings of the 18th ACM conference on Information and knowledge management (pp. 375-384). ACM.
#'
#' Weak supervision adopted from:
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
#' @param updateParaStep Integer. The number of iterations between optimizations of hyperparameter alpha
#' @param alpha Double, hyperparameter for (defaults to .05 * (average docsize/number of sentitopics))
#' @param beta Double, hyperparameter for (defaults to .01, with multiplier .9/.1 for sentiment dictionary presence)
#' @param gamma Double, hyperparameter for (defaults to .05 * (average docsize/number of sentiment categories))
#' @param excludeNeutral Boolean. If a dictionary is used, an extra category is added for neutral
#' words. Words in the dictionary receive a low probability of being allocated there. If this is set
#' to \code{TRUE}, the neutral sentiment category will be omitted. The variable is irrelevant if no
#' dictionary is used. Defaults to \code{FALSE}.
#' @return A JST.result object containing a data.frame for each estimated parameter
#'
#' @examples
#' model <- jst(quanteda::dfm(quanteda::data_corpus_irishbudget2010),
#'              paradigm(),
#'              numTopics = 5,
#'              numIters = 15) # Use more in practice!
#' @export
jst <- function(dfm,
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
    jstcpp(
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
    return(".")
  }

  #prepare doc sentiment distribution data.frame
  docID <- quanteda::docnames(dfm)

  pi <- as.data.frame(res$pi)
  pi <- as.data.frame(t(pi))

  pi.names = character(numSentiLabs)
  for (i in c(1:numSentiLabs)) {
    pi.names[i] <- paste("sent", i, sep = "")
  }
  names(pi) <- pi.names
  rownames(pi) <- docID

  #prepare doc sentiment/topic distribution data.frame
  theta <- as.data.frame(res$theta)

  theta.names <- character(numTopics)

  theta.names = character(numSentiLabs * numTopics)
  for (i in c(1:numSentiLabs)) {
    for (j in c(1:numTopics)) {
      theta.names[j + numTopics * (i - 1)] <-
        paste("topic", j, "sent", i, sep = "")
    }
  }

  names(theta) <- theta.names

  theta <- data.frame(docID, theta, row.names = NULL)

  #prepare word topic/sentiment distribtuion data.frame
  phi <- as.data.frame(res$phi)
  phi.termScores <- as.data.frame(res$phi.termScores)

  phi.names = character(numSentiLabs * numTopics)
  for (i in c(1:numSentiLabs)) {
    for (j in c(1:numTopics)) {
      phi.names[j + numTopics * (i - 1)] <-
        paste("topic", j, "sent", i, sep = "")
    }
  }
  names(phi) <- phi.names
  names(phi.termScores) <- phi.names
  rownames(phi) <- quanteda::featnames(dfm)
  rownames(phi.termScores) <- quanteda::featnames(dfm)

  return(
    JST.result(
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
#' @aliases topNwords,JST.result,numeric,numeric,numeric-method
setMethod('topNwords', c('JST.result', 'numeric', 'numeric', 'numeric'),
          function(x, N, topic, sentiment) {
            colname <- paste('topic', topic, 'sent', sentiment, sep = '')

            column <- sapply(x@phi[colname], as.numeric)

            res <- rownames(x@phi)[topNwordSeeds(column, N)]

            res <- as.data.frame(res, stringsAsFactors = FALSE)

            names(res) <- colname

            return(res)
          })

#' @rdname topNwords-method
#' @aliases topNwords,JST.result,numeric,-method
setMethod('topNwords', c('JST.result', 'numeric'),
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
#' @aliases top20words,JST.result,numeric,numeric-method
setMethod('top20words', c('JST.result', 'numeric', 'numeric'),
          function(x, topic, sentiment) {
            return(topNwords(x, 20, topic, sentiment))
          })

#' @rdname top20words-method
#' @aliases top20words,JST.result-method
setMethod('top20words', c('JST.result'),
          function(x) {
            return(topNwords(x, 20))
          })
