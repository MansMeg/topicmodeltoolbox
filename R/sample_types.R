#' Sample types for a given setup of topic indicators
#'
#' @description
#' The function is sampling new word types based on posterior mode without hyper parameters.
#'
#' This function is used for modeling comparisons using Bayesian checking.
#' See reference for details.
#'
#' @references
#' Mimno, D. and Blei, D. Bayesian Checking for Topic Models
#'
#' @param state a topic model state object
#'
#' @return
#' a type vector of length of the state dataset.
#'
#' @export
sample_types_given_topic <- function(state){
  assert_state(state)
  Nkw <- state %>%
    dplyr::group_by(topic, type) %>%
    dplyr::summarise(n = n()) %>%
    ungroup()

  # Calculate the number of tokens by topic
  Nk <- Nkw %>%
    dplyr::group_by(topic) %>%
    dplyr::summarise(n = sum(n)) %>%
    ungroup()

  # Calculate probability and split
  pdfs <-
    Nkw %>%
    dplyr::group_by(topic) %>%
    mutate(p = n/sum(n), type = as.integer(type)) %>%
    ungroup() %>%
    transmute(topic, type, p)
  pdfs <- split(transmute(pdfs, type, p), pdfs$topic)

  w <- integer(nrow(state))
  for(i in 1:length(pdfs)){
    w[state$topic == i] <- sample(pdfs[[i]]$type, size =  Nk$n[i], TRUE, pdfs[[i]]$p)
  }
  factor(w, labels = levels(state$type))
}
