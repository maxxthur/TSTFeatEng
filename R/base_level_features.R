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
base_level_features <- function(x, window_sizes = c(1, 4, 12), kick = NULL) {
  base <- data.frame(lag = 1:nrow(x))

  if(is.null(kick) == F) x %<>% dplyr::select(-dplyr::matches(kick))

  for(i in window_sizes) {
    window_name <- paste0("window_", i)
    if(i != 1) base[paste0("sd_", window_name)] <- x[, (ncol(x)-i):(ncol(x)-1)] %>%
        basic_features(x = ., stat = "sd")
    if(i != 1) base[paste0("mean_", window_name)] <- x[, (ncol(x)-i):(ncol(x)-1)] %>%
        basic_features(x = ., stat = "mean")
    if(i != 1) base[paste0("median_", window_name)] <- x[, (ncol(x)-i):(ncol(x)-1)] %>%
        basic_features(x = ., stat = "median")
    if(i != 1) base[paste0("min_", window_name)] <- x[, (ncol(x)-i):(ncol(x)-1)] %>%
        basic_features(x = ., stat = "min")
    if(i != 1) base[paste0("sd_", window_name)] <- x[, (ncol(x)-i):(ncol(x)-1)] %>%
        basic_features(x = ., stat = "max")
    if(i == 1) base["previous_value"] <- x[, ncol(x)-1]
  }

  base %>% dplyr::select(-lag)
}
