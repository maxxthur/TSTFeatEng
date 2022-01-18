#' Title
#'
#' @param x
#' @param window_sizes
#' @param kick
#'
#' @return
#' @export
#'
#' @examples
trend_features <- function(x, window_sizes = c(4, 12), kick = NULL) {
  trend <- data.frame(lag = 1:nrow(x))

  if(is.null(kick) == F)  x %<>% dplyr::select(-dplyr::matches(kick))

  for(i in window_sizes) {
    window_name <- paste0("window_", i)

    trend[paste0("ewa_", window_name)] <- x[, (ncol(x)-i):(ncol(x)-1)] %>%
      exponential_weighted_mean(x = ., window = i)

    trend[paste0("diff_ewa_", window_name)] <- x[, (ncol(x)-i-1):(ncol(x)-1)] %>%
      diffs(x = ., window = i) %>%
      exponential_weighted_mean(x = ., window = i)

    trend[paste0("diff_min_", window_name)] <- x[, (ncol(x)-i-1):(ncol(x)-1)] %>%
      diffs(x = ., window = i) %>%
      basic_features(x = ., stat = "min")

    trend[paste0("diff_max_", window_name)] <- x[, (ncol(x)-i-1):(ncol(x)-1)] %>%
      diffs(x = ., window = i) %>%
      basic_features(x = ., stat = "max")

    trend[paste0("diff_sd_", window_name)] <- x[, (ncol(x)-i-1):(ncol(x)-1)] %>%
      diffs(x = ., window = i) %>%
      basic_features(x = ., stat = "sd")

    trend[paste0("trend_lm_", window_name)] <- x[, (ncol(x)-i):(ncol(x)-1)] %>%
      basic_features(x = ., stat = "lm")
  }

  trend %>% dplyr::select(-lag)
}
