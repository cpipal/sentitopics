#' Run a Joint Sentiment Topic Model Several Times
#'
#' Estimates a joint sentiment topic model n times and returns mean sentiment label predictions, see Details for model description.
#'
#' Works similarly to \code{\link{jst}}, but runs the model estimation n times using parallelization. 
#' Does not return the full results of each individual model, but mean sentiment label predictions 
#' with uncertainty estimates.
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
#' @param n Number of model runs (defaults to 30)
#' @param confidence Confidence level for confidence intervals (defaults to 0.95)
#' @param ncores Number of (logical) CPU cores used for the estimation. (defaults to available number of cores - 3)
#' @param seed Seed for random number generator for reproducibility (defaults to \code{NA})
#' @return A data.frame with mean sentiment label predictions and uncertainty estimates
#'
#' @examples
#' results <- jstManyRuns(quanteda::dfm(quanteda::data_corpus_irishbudget2010),
#'              paradigm(),
#'              numTopics = 5,
#'              numIters = 15, # Use more in practice!
#'              n = 5) # Use more in practice!
#' @export
jstManyRuns <- function(dfm,
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
                        seed = NA){
  
  library(doRNG) # TO DO: Find other solution to use %dorng% from the doRNG-packge here
  
  if(is.na(ncores)){
    ncores <- parallel::detectCores() - 3
  }
  cl <- parallel::makeCluster(ncores) 
  doParallel::registerDoParallel(cl) 
  
  if(!is.na(seed)) set.seed(seed) # TO DO: use withr to make sure rng of user isn't changed 
  
  l <- foreach(i=1:n,
               .packages = c("sentitopics", "dplyr", "magrittr", "readr", "quanteda", "quanteda.textstats")) %dorng% 
    {
      
      jst_out <- sentitopics::jst(dfm = dfm, 
                                  sentiLexInput = sentiLexInput,
                                  numTopics = numTopics,
                                  numIters = numIters,
                                  excludeNeutral = excludeNeutral)  %>% 
        sentitopics::get_parameter("pi")
      
      # extract 3 labels if neutral is included
      if(excludeNeutral == FALSE){
        jst_out <- jst_out %>% 
          dplyr::select(sent1, sent2, sent3) 
      }else{ # otherwise just 2 labels (pos and neg)
        jst_out <- jst_out %>% 
          dplyr::select(sent1, sent2)
      }
      
      return(jst_out)
    }
  
  parallel::stopCluster(cl) 
  
  sent1 <- lapply(l, function(x) x %>% dplyr::select(sent1)) %>% data.frame()
  sent2 <- lapply(l, function(x) x %>% dplyr::select(sent2)) %>% data.frame()
  
  if(excludeNeutral == FALSE){
    sent3 <- lapply(l, function(x) x %>% dplyr::select(sent3)) %>% data.frame()
  }
  
  # calc stats rowwise (each row is one document in dfm, each column is pi from one model run)
  calcStats <- function(df){
    
    df <- df %>%  
      dplyr::mutate(mean = Matrix::rowMeans(.), 
                    sd = matrixStats::rowSds(as.matrix(.)),
                    se = sd/(sqrt(n)))
    
    if(n >= 30){
      df <- df %>%  
        dplyr::mutate(ci_high = mean + (qnorm((confidence + (1 - confidence) / 2)) * se),
                      ci_low = mean - (qnorm((confidence + (1 - confidence) / 2)) * se)) 
    }else{
      df <- df %>%  
        dplyr::mutate(ci_high = mean + (qt((confidence + (1 - confidence) / 2), df = n - 1) * se),
                      ci_low = mean - (qt((confidence + (1 - confidence) / 2), df = n - 1) * se)) 
    }
    
    df <- df %>% dplyr::select(mean, sd, se, ci_high, ci_low)
    return(df)
  }
  
  sent1 <- sent1 %>% calcStats()
  sent2 <- sent2 %>% calcStats()
  if(excludeNeutral == FALSE){
    sent3 <- sent3 %>% calcStats()
  }
  
  res <- data.frame(sent1_mean = sent1$mean,
                    sent1_sd = sent1$sd,
                    sent1_se = sent1$se,
                    sent1_ci_high = sent1$ci_high,
                    sent1_ci_low = sent1$ci_low,
                    sent2_mean = sent2$mean,
                    sent2_sd = sent2$sd,
                    sent2_se = sent2$se,
                    sent2_ci_high = sent2$ci_high,
                    sent2_ci_low = sent2$ci_low)
  
  if(excludeNeutral == FALSE){
    res <- res %>% 
      cbind(data.frame(sent3_mean = sent3$mean,
                       sent3_sd = sent3$sd,
                       sent3_se = sent3$se,
                       sent3_ci_high = sent3$ci_high,
                       sent3_ci_low = sent3$ci_low)) 
  }
  
  res <- cbind(dfm@docvars, res)
  
  return(res)
}