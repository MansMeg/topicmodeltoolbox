#' Topic model key term summaries
#'
#' @description
#' The classical way of representing topics by order by the probability for a type within a topic.
#'
#' @details
#' To save space the calculations are done using a sparse format,
#' only returning values for type-topic combination that exist in the model.
#' This means that unless \code{beta} is set to 0, the returning probabilities
#' will not sum to 1.
#'
#' Not all reweighting schemes return a probability (such as KR1, KR2 and relevance)
#'
#'
#' @references
#' Topic and keyword re-ranking for LDA-based topic modeling (2009)
#' LDAvis: A method for visualizing and interpreting topics (2014)
#'
#' @param state A tidy topic model state file
#' @param j The number of top words to return
#' @param beta Beta hyper parameter. Default is 0 (no smoothing).
#' @param lambda Relevance weight. Default is 0.6.
#'
#' @return
#' Returns a data_frame with topic and top terms
#'
#' @export
type_probability <- function(state, j, beta = 0){
  assert_state(state)
  checkmate::assert_int(j, lower = 1)
  checkmate::assert_number(beta, lower = 0)
  top_j_types(p_w_given_k(state, beta), j)
}


#' @rdname type_probability
#' @export
topic_probability <- function(state, j, beta = 0){
  assert_state(state)
  checkmate::assert_int(j, lower = 1)
  checkmate::assert_number(beta, lower = 0)
  top_j_types(p_k_given_w(state, beta), j)
}

#' @rdname type_probability
#' @export
KR1 <- function(state, j, beta = 0){
  library(dplyr)

  assert_state(state)
  checkmate::assert_int(j, lower = 1)
  checkmate::assert_number(beta, lower = 0)

  K <- length(unique(state$topic))
  phi <- p_w_given_k(state, beta)
  phi <- dplyr::left_join(phi, dplyr::summarise(dplyr::group_by(phi, type), sum_p = sum(p), n = n()), by = "type")
  phi <- dplyr::mutate(phi, sum_p = sum_p + (K - n) * beta, n = NULL, p = p/sum_p, sum_p = NULL)
  phi
}

#' @rdname type_probability
#' @export
KR2 <- function(state, j, beta = 0){
  library(dplyr)

    assert_state(state)
    checkmate::assert_int(j, lower = 1)
    checkmate::assert_number(beta, lower = 0)

    stop("not fixed yet")
  phi <- p_w_given_k(x)
  lphi <- log(phi)

  clphi <- matrix(colMeans(lphi), nrow = nrow(x), ncol = ncol(x), byrow = TRUE)
  res <- phi * (lphi - clphi)

  return_top_k_by_column(x = res, k)
}


#' @rdname type_probability
#' @export
relevance <- function(state, j, beta = 0, lambda = 0.6){
  library(dplyr)

  assert_state(state)
  checkmate::assert_int(j, lower = 1)
  checkmate::assert_number(beta, lower = 0)
  checkmate::assert_number(lambda, lower = 0, upper = 1)

  stop("not fixed yet")
  checkmate::assert_number(beta, lower = 0)
  checkmate::assert_number(lambda, lower = 0, upper = 1)
  phi <- p_w_given_k(x)
  p <- p_w(x)
  log_p_mat <- matrix(log(p), nrow = nrow(x), ncol = ncol(x), byrow = TRUE)
  res <- log(phi) - (1 - lambda) * log_p_mat
  return_top_k_by_column(x = res, k)
}



# Below are helper functions used in multiple reweighting methods

#' @title Return top j terms by weight
#' @param wttm a weighted tidy type topic matrix
#' @param j top j types to return (min 1 type)
top_j_types <- function(wtttm, j){
  checkmate::assert_class(wtttm, "tbl_df")
  checkmate::assert_integerish(j, lower = 1)
  checkmate::assert_subset(names(wtttm), c("p", "topic", "type"))
  dplyr::ungroup(dplyr::top_n(dplyr::group_by(wtttm, topic), n = j, wt = p))
}

p_w_given_k <- function(state, beta = 0){
  library(dplyr)

  V <- length(levels(state$type))
  topic_mass <- state %>%
    group_by(topic) %>%
    summarise(weight = n()) %>%
    mutate(weight = weight + beta * V)
  state %>%
    group_by(topic, type) %>%
    summarise(counts = n()) %>%
    left_join(topic_mass, by = "topic") %>%
    mutate(p = (counts + beta) / weight, weight = NULL, counts = NULL) %>%
    ungroup()
}

p_k_given_w <- function(state, beta = 0){
  library(dplyr)

  K <- length(unique(state$topic))

  type_mass <- state %>%
    group_by(type) %>%
    summarise(weight = n()) %>%
    mutate(weight = weight + beta * K)

  state %>%
    group_by(topic, type) %>%
    summarise(counts = n()) %>%
    left_join(type_mass, by = "type") %>%
    mutate(p = (counts + beta) / weight, weight = NULL, counts = NULL) %>%
    ungroup()
}

p_wk <- function(state, beta = 0){
  library(dplyr)

  V <- length(levels(state$type))
  K <- length(unique(state$topic))
  total_mass <- state %>%
    summarise(weight = n()) %>%
    mutate(weight = weight + beta * K * V)

  state %>%
    group_by(topic, type) %>%
    summarise(counts = n()) %>%
    mutate(p = (counts + beta) / total_mass$weight, counts = NULL) %>%
    ungroup()
}

p_w <- function(state, beta = 0){
  library(dplyr)
  V <- length(levels(state$type))
  K <- length(unique(state$topic))
  total_mass <- state %>%
    summarise(weight = n()) %>%
    mutate(weight = weight + beta * K * V)
  state %>%
    group_by(type) %>%
    summarise(counts = n()) %>%
    mutate(p = (counts + beta * K) / total_mass$weight, counts = NULL)
}

p_k <- function(state, beta = 0){
  library(dplyr)
  V <- length(levels(state$type))
  K <- length(unique(state$topic))
  total_mass <- state %>%
    summarise(weight = n()) %>%
    mutate(weight = weight + beta * K * V)
  state %>%
    group_by(topic) %>%
    summarise(counts = n()) %>%
    mutate(p = (counts + beta * V) / total_mass$weight, counts = NULL)
}

