#' Show top N words for topics/sentiments
#'
#' This method returns a vector containing the N words with the highest
#' estimated parameter values for any of the models estimated in this package.
#' If topic (and sentiment for the appropriate models) are not specified, the
#' top N words of every topic will be returned.
#'
#' @param x A results object from any of the models in the package
#' @param N Integer. The number of words to return.
#' @param topic (optional) Integer. The topic to return words from.
#' @param sentiment (optional) Integer. The sentiment to return words from.
#'
#' @return If topic and sentiment are specified: A character vector containing
#'  the top N words for the requested topic-sentiment combination. Otherwise a
#'  data.frame containing the top N words for every topic-sentiment combination.
#'
#' @export
#' @docType methods
#' @rdname topNwords-method
#'
#' @examples
#' model <- jst(quanteda::dfm(quanteda::data_corpus_inaugural), paradigm())
#' topNwords(model, N = 30, topic = 2, sentiment = 1)
setGeneric(name='topNwords', function(x,N,topic=NULL,sentiment=NULL) {
  standardGeneric('topNwords')
})

#' Show top 20 words for topics/sentiments
#'
#' This method returns a vector containing the 20 words with the highest
#' estimated parameter values for any of the models estimated in this package.
#' If topic (and sentiment for the appropriate models) are not specified, the
#' top 20 words of every topic will be returned.This method calls the generic
#' \link{topNwords} method.
#'
#' @param x A results object from any of the models in the package
#' @param topic (optional) Integer. The topic to return words from.
#' @param sentiment (optional) Integer. The sentiment to return words from.
#'
#' @return If topic and sentiment are specified: A character vector containing
#'  the top 20 words for the requested topic-sentiment combination. Otherwise a
#'  data.frame containing the top 20 words for every topic-sentiment combination.
#'
#' @export
#' @docType methods
#' @rdname top20words-method
#'
#' @examples
#' model <- jst(quanteda::dfm(quanteda::data_corpus_inaugural), paradigm())
#' top20words(model, topic = 1, sentiment = 1)
setGeneric(name='top20words',function(x,topic=NULL,sentiment=NULL) {
  standardGeneric('top20words')
})

