#' Assert that \code{file_path} is a valid topic model state
#'
#' @details
#' The topic model state is a \code{tibble} dataset with at least the variables
#' \code{doc}, \code{pos}, \code{type} and \code{topic}.
#'
#' @param x Object to check if it is a valid topic_model_state object
#'
#' @keywords internal
assert_state <- function(x){
  checkmate::assert_class(x, "tbl_df")
  checkmate::assert_subset(c("doc", "pos", "type", "topic"), names(x))
  checkmate::assert_class(x$doc, "integer")
  checkmate::assert_class(x$pos, "integer")
  checkmate::assert_class(x$type, "factor")
  checkmate::assert_class(x$topic, "integer")
}
