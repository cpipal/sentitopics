#' sentitopics: Joint Estimation of Sentiment and Topcis in Textual Data
#' 
#' sentitopics performs Joint Sentiment Topic modelling and includes a
#' reversed model (for info on the latter, email me) and several
#' methods to evaluate the results and make them more accessible.
#' All models take \pkg{quanteda} dfm objects as inputs. If you prefer \pkg{tidytext},
#' tm objects are internally converted to dfm objects. Furthermore,
#' all models have a defined tidy method. See \pkg{broom} for more
#' information.
#' 
#' @useDynLib sentitopics
"_PACKAGE"