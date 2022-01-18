#' Title
#'
#' @param x
#' @param window
#'
#' @return
#'
#'
#' @examples
diffs <- function(x, window) {
  x %>%
    dplyr::rowwise() %>%
    dplyr::group_split() %>%
    purrr::map(~ tidyr::pivot_longer(.,cols = dplyr::everything(),
                       names_to = "t",
                       values_to = "value") %>%
          dplyr::mutate(diffs = value - dplyr::lag(value, k = 1)) %>%
          dplyr::filter(!is.na(diffs)) %>%
          dplyr::select(-value) %>%
          tidyr::pivot_wider(names_from = "t", values_from = diffs)) %>%
    dplyr::bind_rows()

}
