
#' Title
#'
#' @param x
#' @param h
#' @param var
#' @param keep
#'
#' @return
#' @export
#'
#' @examples
transform_to_tree_format <- function(x, h, var, keep = NULL) {
  target <- tail(x[, c(keep, var)], h) %>%
    dplyr::rename(target = dplyr::all_of(var))

  predictors <- tidylags::tidy_lags(x, var, lags = h)

  tree_form <- data.frame()

  for(i in 1:h) {
    lag_name <- paste0(var, "_", "lag", i)

    tree_form <- predictors[, lag_name] %>%
      dplyr::mutate(t = paste("t", 0:(nrow(.)-1), sep = "_")) %>%
      tidyr::pivot_wider(names_from = "t", values_from = dplyr::all_of(lag_name)) %>%
      dplyr::bind_rows(tree_form)
  }

  final_data <- dplyr::bind_cols(tree_form, target)

  final_data
}
