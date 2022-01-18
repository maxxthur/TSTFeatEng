#' Title
#'
#' @param x
#' @param window
#'
#' @return
#'
#'
#' @examples
exponential_weighted_mean <- function(x, window) {
  weights <- (window:1/exp(window:1))/sum(window:1/exp(window:1))

  x %>%
    dplyr::rowwise() %>%
    dplyr::group_split() %>%
    purrr::map(~ tidyr::pivot_longer(.,cols = dplyr::everything(),
                       names_to = "t",
                       values_to = "value") %>%
          dplyr::mutate(w = weights,
                 w_v = value * w) %>%
          dplyr::summarise(ewa = sum(w_v))) %>%
    dplyr::bind_rows()
}
