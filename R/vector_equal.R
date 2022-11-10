#' Check if two vectors are equivalent, avoiding the issue of recycling values.
#'
#' @param x A vector.
#' @param y A vector.
#'
#' @return `TRUE` or `FALSE` depending on whether the vectors are equal.
#' @export
#' @keywords internal
#'
#' @examples
#' x <- c(1, 2, 3)
#' y <- c(1, 2, 3, 1)
#' vector_equal(x, y)
#' all(x == y)
vector_equal <- function(x, y) {
  is.numeric(x) && is.numeric(y) && length(x) == length(y) &&
    all(x == y)
}
