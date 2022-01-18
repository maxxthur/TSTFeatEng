#' Title
#'
#' @param x
#' @param window
#'
#' @return
#'
#'
#' @examples
exponentially_weighted_seasons <- function(x, window) {
  cols <- paste0("t_", as.character(sort(seq(from = ncol(x), to = 1, by = -window)[-1])))

  x %>%
    dplyr::select(dplyr::all_of(cols)) %>%
    dplyr::rowwise() %>%
    dplyr::group_split() %>%
    purrr::map(~ tidyr::pivot_longer(.,cols = dplyr::everything(),
                       names_to = "t",
                       values_to = "value") %>%
          dplyr::filter(!is.na(value)) %>%
          dplyr::mutate(ind = nrow(.):1) %>%
          dplyr::mutate(w = ind/exp(ind),
                 sum_w = sum(w),
                 weights = w/sum_w,
                 weighted_values = value * weights) %>%
          dplyr::summarise(season = sum(weighted_values))) %>%
    dplyr::bind_rows()
}
