#' Paradigm word list (Lin and He, 2009, p.379)
#' 
#' Returns the paradigm word list proposed by Lin and He. These words create groupings
#' of positive and negative words which are considered to be domain-independent. However,
#' as always, caution is warranted. The words are mostly suited for review analysis. Review
#' the words before applying the dictionary.
#' 
#' @return A quanteda dictionary2 object containing the paradigm word list
#' 
#' @note Lin, C., & He, Y. (2009). Joint sentiment/topic model for sentiment analysis. 
#' In Proceedings of the 18th ACM conference on Information and knowledge management 
#' (pp. 375-384). ACM.
#' 
#' @export
paradigm <- function() {
  positive = c('dazzling','brilliant','phenomenal','excellent','fantastic','gripping',
               'mesmerizing','riveting','spectacular','cool','awesome','thrilling',
               'moving','exciting','love','wonderful','best','great','superb','still',
               'beautiful')
  negative = c('sucks','terrible','awful','unwatchable','hideous','bad','cliched',
               'boring','stupid','slow','worst','waste','unexcited','rubbish',
               'tedious','unbearable','pointless','cheesy','frustrated','awkward',
               'disappointing')
  return(quanteda::dictionary(list('positive'=positive,'negative'=negative)))
}

#' Wordstem a quanteda Dictionary2 object
#' 
#' Applies Porter stemming to the words in a quanteda dictionary and then removes 
#' duplicates, if the stemming created these.
#' 
#' @param dict A quanteda dictionary2 object
#' @return A quanteda dictionary2 object with all elements stemmed
#' 
#' @export
dictionary_wordstem <- function(dict) {
  sentiments <- names(dict)
  dictList <- as.list(dict)
  newdict <- list()
  for (name in sentiments) {
    newdict[[name]] <- unique(SnowballC::wordStem(dictList[[name]], language='porter'))
  }
  return(quanteda::dictionary(newdict))
}