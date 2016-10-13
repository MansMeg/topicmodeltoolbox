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
IMI <- function(state, k, w=NULL){
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
#' @param k The topic to calculate MI for
#'
#'
#' @export
MI <- function(state, k){
  assert_state(state)
  checkmate::assert_int(k, lower = 1)

  st <- dplyr::filter(state, topic == k) %>%
    dplyr::group_by(doc, type) %>%
    dplyr::summarise(n = n()) %>% ungroup()

  Nd <- st %>% dplyr::group_by(doc) %>%
    dplyr::summarise(nd = n()) %>% ungroup()
  Nw <- st %>% dplyr::group_by(type) %>%
    dplyr::summarise(nw = n()) %>% ungroup()
  Nk <- sum(st$n)
  suppressMessages(
    st <- st %>% dplyr::full_join(Nd) %>%
      dplyr::full_join(Nw) %>%
      mutate(part_mi = n/Nk * log((n * Nk)/(nd * nw)))
  )
  sum(st$part_mi)
}
