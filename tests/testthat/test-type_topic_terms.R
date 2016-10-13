context("type_topic_reweighting")

load(system.file("extdata/sotu_50.Rdata", package = "topicmodeltoolbox"))

test_that(desc="Test base functions",{
  K <- length(unique(state$topic))
  V <- length(levels(state$type))
  N <- nrow(state)
  N_k <- dplyr::summarise(dplyr::group_by(state, topic), n = n())
  N_w <- dplyr::summarise(dplyr::group_by(state, type), n = n())
  beta <- 0.1


  expect_equal(sum(p_wk(state)$p), 1)
  ps <- p_wk(state, beta)
  mass <- (N + V * K * beta)
  expect_equal(sum(ps$p) + ((V * K - nrow(ps)) * beta) / mass, 1)

  expect_equal(sum(p_w_given_k(state)$p), K)
  ps <- p_w_given_k(state, beta)
  p_k <- dplyr::left_join(dplyr::summarise(dplyr::group_by(ps, topic), p = sum(p), non_zero = n()), N_k, by = "topic")
  p_k <- dplyr::mutate(p_k, p_zero = (V - non_zero) * beta / (n + V * beta), mass = NULL, n = NULL, non_zero = NULL)
  expect_equal(p_k$p + p_k$p_zero, rep(1, K))

  expect_equal(sum(p_k_given_w(state)$p), length(levels(state$type)))
  ps <- p_k_given_w(state, beta)
  p_w <- dplyr::left_join(dplyr::summarise(dplyr::group_by(ps, type), p = sum(p), non_zero = n()), N_w, by = "type")
  p_w <- dplyr::mutate(p_w,  p_zero = (K - non_zero) * beta / (n + K * beta), n = NULL, non_zero = NULL)
  expect_equal(p_w$p + p_w$p_zero, rep(1, V))

  expect_equal(sum(p_w(state)$p), 1)
  expect_equal(sum(p_w(state, beta)$p), 1)

  expect_equal(sum(p_k(state)$p), 1)
  expect_equal(sum(p_k(state, beta)$p), 1)

})

