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
#' @param w A vector of types to calculate IMI for (faster to calculate)
#'
#' @details
#'
#' @export
imi <- function(state, k, w=NULL){
  assert_state(state)
  checkmate::assert_int(k, lower = 1)
  checkmate::assert_character(w, null.ok = TRUE)
  if(!is.null(w)) checkmate::assert_subset(w, levels(state$type))

  # Remove other topics
  st <- dplyr::filter(state, topic == k)

  # Calculate H(D|k)
  HDk <- st %>%
    dplyr::group_by(doc) %>%
    dplyr::summarise(n = n()) %>%
    ungroup() %>%
    mutate(p = n/sum(n)) %>%
    mutate(pmi = log(p) * p) %>%
    summarise(HDk = sum(pmi))
  HDk <- - HDk$HDk[1]

  # Calculate H(D|W=w, k)
  if(!is.null(w)) {
    w <- data_frame(type=w)
    suppressMessages(
      st <- st %>%
        dplyr::mutate(type = as.character(type)) %>%
        dplyr::right_join(w)
    )
  }
  st %>%
    dplyr::group_by(doc, type) %>%
    dplyr::summarise(n = n()) %>%
    ungroup() %>%
    dplyr::group_by(type) %>%
    mutate(p = n/sum(n)) %>%
    mutate(pmi = log(p) * p) %>%
    summarise(imi = HDk - -sum(pmi)) %>%
    ungroup()
}


#' Calculate MI
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
