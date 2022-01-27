## ----setup, include=FALSE------------------------------------------------
library(rJST)
library(magrittr)

set.seed(1)

## ----jst test------------------------------------------------------------
data <- quanteda::data_corpus_inaugural %>% #The quanteda example dfm
        quanteda::tokens(remove_numbers = TRUE, remove_punct = TRUE) %>%
        quanteda::dfm(remove = quanteda::stopwords())

model <- jst(data, paradigm(), numIters = 600) #paradigm is a standard sentiment dictionary included in the package, it is however rather tailored towards reviews. For a similar political corpus a diferent dictionary might be more sensible.

## ----top20words----------------------------------------------------------
topNwords(model, N = 15, topic = 2, sentiment = 1)

## ----parameter-----------------------------------------------------------
pi <- get_parameter(model,'pi')
pi[pi$President %in% c('Bush','Obama','Trump') & pi$Year > 2000,]

