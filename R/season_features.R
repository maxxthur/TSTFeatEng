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
season_features <- function(x, window_sizes = c(4, 12), kick = NULL) {
  seasons <- data.frame(placeholder = 1:nrow(x))

  if(is.null(kick) == F)  x %<>% dplyr::select(-dplyr::matches(kick))

  for(i in window_sizes) {
    window_name <- paste0("window_", i)

    seasons[paste0("seasons_ewa_", window_name)] <- x %>%
      exponentially_weighted_seasons(x = ., window = i)

    seasons[paste0("seasons_min_", window_name)] <- x %>%
      basic_features(x = ., window = i, stat = "min", seasons = T)

    seasons[paste0("seasons_max_", window_name)] <- x %>%
      basic_features(x = ., window = i, stat = "max", seasons = T)

    seasons[paste0("seasons_median_", window_name)] <- x %>%
      basic_features(x = ., window = i, stat = "median", seasons = T)

    seasons[paste0("seasons_sd_", window_name)] <- x %>%
      basic_features(x = ., window = i, stat = "sd", seasons = T)

    seasons[paste0("seasons_mean_", window_name)] <- x %>%
      basic_features(x = ., window = i, stat = "mean", seasons = T)
  }
  seasons %>% dplyr::select(-placeholder)
}
