test_that("jst execution correct, unsupervised, with docvars", {
  # The type of tokens doesn't matter for the model, so
  # no cleaning here whatsoever. Reduce to 10 documents
  # for reduced runtime.
  data <- quanteda::dfm(quanteda::data_corpus_inaugural)
  data <- quanteda::dfm_sample(data, 10)

  model_jst <- jst(data)

  expect_true(is.JST.result(model_jst))

  top20 <- top20words(model_jst)

  theta <- get_parameter(model_jst, "theta")
  pi <- get_parameter(model_jst, "pi")
  phi <- get_parameter(model_jst, "phi")

  expect_is(theta, "data.frame")
  expect_equal(nrow(theta), quanteda::ndoc(data))
  expect_equal(ncol(theta), 1 + ncol(quanteda::docvars(data)) + 10 * 3)
  # docid + docvars + 10 topic * 3 senti

  expect_is(pi, "data.frame")
  expect_equal(ncol(pi), ncol(quanteda::docvars(data)) + 3) # docvars + 3 senti
  expect_equal(rownames(pi), quanteda::docnames(data)) #implies length

  expect_is(phi, "data.frame")
  expect_equal(nrow(phi), quanteda::nfeat(data) * 10 * 3) #num features * 10 topic * 3 senti
  expect_equal(ncol(phi), 4)
})

test_that("jst execution correct, unsupervised, without docvars", {
  # The type of tokens doesn't matter for the model, so
  # no cleaning here whatsoever. Reduce to 10 documents
  # for reduced runtime.
  data <- quanteda::dfm(quanteda::data_corpus_inaugural)
  quanteda::docvars(data) <- NULL  # Remove docvars
  data <- quanteda::dfm_sample(data, 10)

  model_jst <- jst(data)

  expect_true(is.JST.result(model_jst))

  top20 <- top20words(model_jst)

  theta <- get_parameter(model_jst, "theta")
  pi <- get_parameter(model_jst, "pi")
  phi <- get_parameter(model_jst, "phi")

  expect_is(theta, "data.frame")
  expect_equal(nrow(theta), quanteda::ndoc(data))
  expect_equal(ncol(theta), 1 + ncol(quanteda::docvars(data)) + 10 * 3)
  # docid + docvars + 10 topic * 3 senti

  expect_is(pi, "data.frame")
  expect_equal(ncol(pi), ncol(quanteda::docvars(data)) + 3) # docvars + 3 senti
  expect_equal(rownames(pi), quanteda::docnames(data)) #implies length

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

  model_jst <- jst(data, paradigm())

  expect_true(is.JST.result(model_jst))

  top20 <- top20words(model_jst)

  top20 <- top20words(model_jst)

  theta <- get_parameter(model_jst, "theta")
  pi <- get_parameter(model_jst, "pi")
  phi <- get_parameter(model_jst, "phi")

  expect_is(theta, "data.frame")
  expect_equal(nrow(theta), quanteda::ndoc(data))
  expect_equal(ncol(theta), 1 + ncol(quanteda::docvars(data)) + 10 * 3)
  # docid + docvars + 10 topic * 3 senti

  expect_is(pi, "data.frame")
  expect_equal(ncol(pi), ncol(quanteda::docvars(data)) + 3) # docvars + 3 senti
  expect_equal(rownames(pi), quanteda::docnames(data)) #implies length

  expect_is(phi, "data.frame")
  expect_equal(nrow(phi), quanteda::nfeat(data) * 10 * 3) #num features * 10 topic * 3 senti
  expect_equal(ncol(phi), 4)
})

test_that("jst execution correct, supervised, without docvars", {
  # The type of tokens doesn't matter for the model, so
  # no cleaning here whatsoever. Reduce to 10 documents
  # for reduced runtime.
  data <- quanteda::dfm(quanteda::data_corpus_inaugural)
  quanteda::docvars(data) <- NULL  # Remove docvars
  data <- quanteda::dfm_sample(data, 10)

  model_jst <- jst(data, paradigm())

  expect_true(is.JST.result(model_jst))

  top20 <- top20words(model_jst)

  top20 <- top20words(model_jst)

  theta <- get_parameter(model_jst, "theta")
  pi <- get_parameter(model_jst, "pi")
  phi <- get_parameter(model_jst, "phi")

  expect_is(theta, "data.frame")
  expect_equal(nrow(theta), quanteda::ndoc(data))
  expect_equal(ncol(theta), 1 + ncol(quanteda::docvars(data)) + 10 * 3)
  # docid + docvars + 10 topic * 3 senti

  expect_is(pi, "data.frame")
  expect_equal(ncol(pi), ncol(quanteda::docvars(data)) + 3) # docvars + 3 senti
  expect_equal(rownames(pi), quanteda::docnames(data)) #implies length

  expect_is(phi, "data.frame")
  expect_equal(nrow(phi), quanteda::nfeat(data) * 10 * 3) #num features * 10 topic * 3 senti
  expect_equal(ncol(phi), 4)
})

