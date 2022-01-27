test_that("jst execution correct, unsupervised, with docvars", {
  # The type of tokens doesn't matter for the model, so
  # no cleaning here whatsoever. Reduce to 10 documents
  # for reduced runtime.
  data <- quanteda::dfm(quanteda::data_corpus_inaugural)
  data <- quanteda::dfm_sample(data, 10)

  model_rjst <- jst_reversed(data)

  expect_true(is.JST_reversed.result(model_rjst))

  top20 <- top20words(model_rjst)

  theta <- get_parameter(model_rjst, "theta")
  pi <- get_parameter(model_rjst, "pi")
  phi <- get_parameter(model_rjst, "phi")

  expect_is(theta, "data.frame")
  expect_equal(nrow(theta), quanteda::ndoc(data)) #Equal length
  expect_equal(ncol(theta), ncol(quanteda::docvars(data)) + 10)
  # docvars + 10 topic

  expect_is(pi, "data.frame")
  expect_equal(ncol(pi), 1 + ncol(quanteda::docvars(data)) + 1 + 3)
  # docid + docvars + topic + num topics
  expect_equal(nrow(pi), quanteda::ndoc(data) * 10)
  # one row per topic per document (ndoc * 10)

  expect_is(phi, "data.frame")
  expect_equal(nrow(phi), quanteda::nfeat(data) * 10 * 3) #num features * 10 topic * 3 senti
  expect_equal(ncol(phi), 4)
})

test_that("jst execution correct, unsupervised, without docvars", {
  # The type of tokens doesn't matter for the model, so
  # no cleaning here whatsoever. Reduce to 10 documents
  # for reduced runtime.
  data <- quanteda::dfm(quanteda::data_corpus_inaugural)
  quanteda::docvars(data) <- NULL
  data <- quanteda::dfm_sample(data, 10)

  model_rjst <- jst_reversed(data)

  expect_true(is.JST_reversed.result(model_rjst))

  top20 <- top20words(model_rjst)

  theta <- get_parameter(model_rjst, "theta")
  pi <- get_parameter(model_rjst, "pi")
  phi <- get_parameter(model_rjst, "phi")

  expect_is(theta, "data.frame")
  expect_equal(nrow(theta), quanteda::ndoc(data)) #Equal length
  expect_equal(ncol(theta), ncol(quanteda::docvars(data)) + 10)
  # docvars + 10 topic

  expect_is(pi, "data.frame")
  expect_equal(ncol(pi), 1 + ncol(quanteda::docvars(data)) + 1 + 3)
  # docid + docvars + topic + num topics
  expect_equal(nrow(pi), quanteda::ndoc(data) * 10)
  # one row per topic per document (ndoc * 10)

  expect_is(phi, "data.frame")
  expect_equal(nrow(phi), quanteda::nfeat(data) * 10 * 3) #num features * 10 topic * 3 senti
  expect_equal(ncol(phi), 4)
})

test_that("jst execution correct, supervised, with docvars", {
  # The type of tokens doesn't matter for the model, so
  # no cleaning here whatsoever. Reduce to 10 documents
  # for reduced runtime.
  data <- quanteda::dfm(quanteda::data_corpus_inaugural)
  data <- quanteda::dfm_sample(data, 10)

  model_rjst <- jst_reversed(data, paradigm())

  expect_true(is.JST_reversed.result(model_rjst))

  top20 <- top20words(model_rjst)

  top20 <- top20words(model_rjst)

  theta <- get_parameter(model_rjst, "theta")
  pi <- get_parameter(model_rjst, "pi")
  phi <- get_parameter(model_rjst, "phi")

  expect_is(theta, "data.frame")
  expect_equal(nrow(theta), quanteda::ndoc(data)) #Equal length
  expect_equal(ncol(theta), ncol(quanteda::docvars(data)) + 10)
  # docvars + 10 topic

  expect_is(pi, "data.frame")
  expect_equal(ncol(pi), 1 + ncol(quanteda::docvars(data)) + 1 + 3)
  # docid + docvars + topic + num topics
  expect_equal(nrow(pi), quanteda::ndoc(data) * 10)
  # one row per topic per document (ndoc * 10)

  expect_is(phi, "data.frame")
  expect_equal(nrow(phi), quanteda::nfeat(data) * 10 * 3) #num features * 10 topic * 3 senti
  expect_equal(ncol(phi), 4)
})

test_that("jst execution correct, supervised, without docvars", {
  # The type of tokens doesn't matter for the model, so
  # no cleaning here whatsoever. Reduce to 10 documents
  # for reduced runtime.
  data <- quanteda::dfm(quanteda::data_corpus_inaugural)
  quanteda::docvars(data) <- NULL
  data <- quanteda::dfm_sample(data, 10)

  model_rjst <- jst_reversed(data, paradigm())

  expect_true(is.JST_reversed.result(model_rjst))

  top20 <- top20words(model_rjst)

  top20 <- top20words(model_rjst)

  theta <- get_parameter(model_rjst, "theta")
  pi <- get_parameter(model_rjst, "pi")
  phi <- get_parameter(model_rjst, "phi")

  expect_is(theta, "data.frame")
  expect_equal(nrow(theta), quanteda::ndoc(data)) #Equal length
  expect_equal(ncol(theta), ncol(quanteda::docvars(data)) + 10)
  # docvars + 10 topic

  expect_is(pi, "data.frame")
  expect_equal(ncol(pi), 1 + ncol(quanteda::docvars(data)) + 1 + 3)
  # docid + docvars + topic + num topics
  expect_equal(nrow(pi), quanteda::ndoc(data) * 10)
  # one row per topic per document (ndoc * 10)

  expect_is(phi, "data.frame")
  expect_equal(nrow(phi), quanteda::nfeat(data) * 10 * 3) #num features * 10 topic * 3 senti
  expect_equal(ncol(phi), 4)
})

