#' @include jst.R jst_reversed.R

#' @title Get parameter from (reversed) JST results
#'
#' @description Take the results object for the selected parameter
#' and return a data.frame which is similar to tidy standards.
#'
#' @param x A JST_reversed.result or JST.result object
#' @param parameter Character. The parameter to be tidied and returned.
#'        Note that no default is set.
#'
#' @return A data.frame.
#'
#' @examples
#' data <- quanteda::dfm(quanteda::data_corpus_irishbudget2010)
#' model <- jst(data, paradigm(), numTopics = 5, numIters = 50)
#'
#' phi <- get_parameter(model, 'phi')
#'
#' @export
get_parameter <- function(x, parameter = NULL) {
  if (is.JST_reversed.result(x)) {
    if (is.null(parameter)) {
      stop('Please specify which parameter from the object you would like to get')
    } else if (parameter == 'pi') {
      return (get_parameter.JST_reversed.result.pi(x))
    } else if (parameter == 'theta') {
      return (get_parameter.JST_reversed.result.theta(x))
    } else if (parameter == 'phi') {
      return (get_parameter.JST_reversed.result.phi(x))
    } else {
      stop(paste('\'', parameter, '\' is not a valid parameter of the JST_reversed.result model.', sep = ''))
    }
  } else if (is.JST.result(x)) {
    if (is.null(parameter)) {
      stop('Please specify which parameter from the object you would like to get')
    } else if (parameter == 'pi') {
      return (get_parameter.JST.result.pi(x))
    } else if (parameter == 'theta') {
      return (get_parameter.JST.result.theta(x))
    } else if (parameter == 'phi') {
      return (get_parameter.JST.result.phi(x))
    } else {
      stop(paste('\'', parameter, '\' is not a valid parameter of the JST.result model.', sep = ''))
    }
  } else {
    stop('The object to get a parameter from is not a valid (reversed) JST results object.')
  }

}

get_parameter.JST_reversed.result.pi <- function(x) {
  res <- x@pi

  if (length(x@docvars) > 0) {
    docvars <- x@docvars
    docvars$docID <- rownames(docvars)

    res <- merge(docvars, res, by = 'docID')
  }

  return (res)
}

get_parameter.JST_reversed.result.theta <- function(x) {
  if (length(x@docvars) > 0) {
    res <- cbind(x@docvars, x@theta)
  } else {
    res <- x@theta
  }

  return(res)
}

get_parameter.JST_reversed.result.phi <- function(x) {
  res <- x@phi

  res$word <- rownames(res)
  res$word <- as.factor(res$word)
  rownames(res) <- NULL

  res <- reshape2::melt(res, id='word')

  variable <- as.character(res$variable)
  variable <- gsub('topic', '', variable)
  topic <- as.numeric(substr(variable, start = 1, stop = regexpr('s', variable) - 1))
  sentiment <- as.numeric(substr(variable, start = regexpr('t', variable) + 1,
                                 stop = nchar(variable)))

  res <- cbind(res, topic, sentiment)
  res <- subset(res, select=c('word', 'sentiment', 'topic', 'value'))

  return(res)
}

get_parameter.JST.result.pi <- function(x) {
  if (length(x@docvars) > 0) {
    docvars <- x@docvars

    res <- cbind(docvars, x@pi)
  } else {
    res <- x@pi
  }
  return(res)
}

get_parameter.JST.result.theta <- function(x) {
  if (length(x@docvars) > 0) {
    docvars <- x@docvars
    docvars$docID <- rownames(docvars)

    res <- merge(docvars, x@theta, by = 'docID')
  } else {
    res <- x@theta
  }
  return(res)
}

get_parameter.JST.result.phi <- function(x) {
  res <- x@phi

  res$word <- rownames(res)
  res$word <- as.factor(res$word)
  rownames(res) <- NULL

  res <- reshape2::melt(res, id='word')

  variable <- as.character(res$variable)
  variable <- gsub('topic', '', variable)
  sentiment <- as.numeric(substr(variable, start=1, stop=regexpr('s', variable)-1))
  topic <- as.numeric(substr(variable, start = regexpr('t', variable) + 1, stop = nchar(variable)))

  res <- cbind(res, topic, sentiment)
  res <- subset(res, select=c('word', 'sentiment', 'topic', 'value'))

  return(res)
}
