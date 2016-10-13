# Bayesian checking methods for topic models

#' Calculate IMI
#'
#' @description
#' Function to calculate instantanueous mutual information for types for a given topic.
#'
#' See reference for details.
#'
#' @references
#' Mimno, D. and Blei, D. Bayesian Checking for Topic Models
#'
#' @param state A topic model state file
#' @param k The topic to calculate IMI
#' @param w A tbl_df with types and topics to calculate IMI for. Default is NULL.
#'
#' @details
#'
#' @examples
#' # Load the state of the union topic model
#' load(system.file("extdata/sotu_50.Rdata", package = "topicmodeltoolbox"))
#' w <- type_probability(sotu_50, 10)
#'
#' @export
imi <- function(state, w=NULL){
  library(dplyr)
  assert_state(state)
  checkmate::assert(checkmate::check_class(w, "tbl_df"),
                    checkmate::check_character(w, null.ok = TRUE))

  # Calculate H(D|k)
  HDk <- state %>%
    dplyr::group_by(topic, doc) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::group_by(topic) %>%
    mutate(p = n/sum(n)) %>%
    mutate(pmi = log(p) * p) %>%
    summarise(HDk = sum(pmi)) %>%
    mutate(HDk = -1 * HDk)

  # Calculate H(D|W=w, k)
  if(!is.null(w)) {
    state <- state %>%
        dplyr::right_join(dplyr::transmute(w, topic, type), by = c("topic", "type"))
  }
  state %>%
    dplyr::group_by(topic, doc, type) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::group_by(topic, type) %>%
    mutate(p = n/sum(n)) %>%
    mutate(pmi = log(p) * p) %>%
    summarise(imi = sum(pmi)) %>%
    left_join(HDk, by = "topic") %>%
    mutate(imi = imi + HDk, HDk = NULL) %>%
    ungroup()
}

#' Calculate Mutual information between types and documents
#'
#' @description
#' Function to calculate mutual information between types and documents (MI(D,W|k))
#' for a given topic.
#'
#' See reference for details.
#'
#' @references
#' Mimno, D. and Blei, D. Bayesian Checking for Topic Models
#'
#' @param state A topic model state file
#'
#' @export
mi <- function(state){
  library(dplyr)
  assert_state(state)

  st <-
    state %>%
    dplyr::group_by(topic, doc, type) %>%
    dplyr::summarise(n = n()) %>% ungroup()

  Ndk <- st %>% dplyr::group_by(topic, doc) %>%
    dplyr::summarise(nd = n()) %>% ungroup()
  Nwk <- st %>% dplyr::group_by(topic, type) %>%
    dplyr::summarise(nw = n()) %>% ungroup()
  Nk <- st %>% dplyr::group_by(topic) %>%
    dplyr::summarise(nk = sum(n)) %>% ungroup()

  st %>% dplyr::group_by(topic) %>%
    dplyr::full_join(Ndk, by = c("topic", "doc")) %>%
    dplyr::full_join(Nwk, by = c("topic", "type")) %>%
    dplyr::full_join(Nk, by = c("topic")) %>%
    mutate(part_mi = n/nk * log((n * nk)/(nd * nw))) %>%
    summarise(mi = sum(part_mi))
}
