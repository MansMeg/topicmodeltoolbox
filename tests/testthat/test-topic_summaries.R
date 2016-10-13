context("type_topic_reweighting")

load(system.file("extdata/sotu50.Rdata", package = "topicmodeltoolbox"))

test_that(desc="base functions are correct",{
  K <- length(unique(sotu50$topic))
  V <- length(levels(sotu50$type))
  N <- nrow(sotu50)
  N_k <- dplyr::summarise(dplyr::group_by(sotu50, topic), n = n())
  N_w <- dplyr::summarise(dplyr::group_by(sotu50, type), n = n())
  beta <- 0.1


  expect_equal(sum(p_wk(sotu50)$p), 1)
  ps <- p_wk(sotu50, beta)
  mass <- (N + V * K * beta)
  expect_equal(sum(ps$p) + ((V * K - nrow(ps)) * beta) / mass, 1)

  expect_equal(sum(p_w_given_k(sotu50)$p), K)
  ps <- p_w_given_k(sotu50, beta)
  p_k <- dplyr::left_join(dplyr::summarise(dplyr::group_by(ps, topic), p = sum(p), non_zero = n()), N_k, by = "topic")
  p_k <- dplyr::mutate(p_k, p_zero = (V - non_zero) * beta / (n + V * beta), mass = NULL, n = NULL, non_zero = NULL)
  expect_equal(p_k$p + p_k$p_zero, rep(1, K))

  expect_equal(sum(p_k_given_w(sotu50)$p), length(levels(sotu50$type)))
  ps <- p_k_given_w(sotu50, beta)
  p_w <- dplyr::left_join(dplyr::summarise(dplyr::group_by(ps, type), p = sum(p), non_zero = n()), N_w, by = "type")
  p_w <- dplyr::mutate(p_w,  p_zero = (K - non_zero) * beta / (n + K * beta), n = NULL, non_zero = NULL)
  expect_equal(p_w$p + p_w$p_zero, rep(1, V))

  expect_equal(sum(p_w(sotu50)$p), 1)
  expect_equal(sum(p_w(sotu50, beta)$p), 1)

  expect_equal(sum(p_k(sotu50)$p), 1)
  expect_equal(sum(p_k(sotu50, beta)$p), 1)

})



test_that(desc="reweighting methods",{
  j <- 10
  K <- length(unique(sotu50$topic))
  expect_silent(tp <- type_probability(sotu50, j))
  expect_equal(nrow(tp), j * K)

  expect_silent(tp <- topic_probability(sotu50, j))
  expect_equal(nrow(tp), j * K)

  expect_silent(tp <- KR1(sotu50, j))
  expect_equal(nrow(tp), j * K)

  expect_silent(tp <- KR2(sotu50, j))
  expect_equal(nrow(tp), j * K)

  expect_silent(tp <- relevance(sotu50, j))
  expect_equal(nrow(tp), j * K)

})

