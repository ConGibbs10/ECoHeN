#' Swap values in two columns based on the order of values in another two columns of a data frame.
#'
#' @param df Dataframe containing columns to compare and swap.
#' @param to_compare A vector of size two of column names. If the value of the first column is larger than the value of the second column, then the values in the columns `to_swap` are swapped.
#' @param to_swap A vector of size two of column names. If the value of the first column of `to_compare` is larger than the value of the second column of `to_compare`, then the values in the columns `to_swap` are swapped.
#'
#' @return Dataframe with the proper ordering of columns.
#' @export
#' @keywords internal
#'
#' @examples
#' df <- data.frame(a = c(1, 8, 3), b = c(1, 2, 27))
#' swap_if(df, to_compare = c('a', 'b'), to_swap = c('a', 'b'))
swap_if <- function(df, to_compare, to_swap) {
  x1 <- df[, to_compare[1]]
  x2 <- df[, to_compare[2]]
  should_swap <- ifelse(x1 <= x2, 0, 1)
  df[should_swap == 1, to_swap] <-
    df[should_swap == 1, rev(to_swap)]

  return(df)
}
