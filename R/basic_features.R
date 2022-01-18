#' Title
#'
#' @param x
#' @param stat
#' @param seasons
#' @param window
#'
#' @return
#'
#'
#' @examples
basic_features <- function(x,
                           stat = c("sd", "mean", "median", "min", "max", "lm"),
                           seasons = F, window = NULL) {

  if(seasons == T & is.null(window) == F) {
    cols <- paste0("t_", as.character(sort(seq(from = ncol(x),
                                               to = 1, by = -window)[-1])))

    x %<>% dplyr::select(dplyr::all_of(cols))
  }

  if(stat == "sd") {
    feat <- x %>%
      dplyr::rowwise() %>%
      dplyr::group_split() %>%
      purrr::map(~ tidyr::pivot_longer(.,cols = dplyr::everything(),
                         names_to = "t",
                         values_to = "value") %>%
            dplyr::summarise(sd = stats::sd(value, na.rm = T))) %>%
      dplyr:::bind_rows()}

  if(stat == "mean") {
    feat <- x %>%
      dplyr::rowwise() %>%
      dplyr::group_split() %>%
      purrr::map(~ tidyr::pivot_longer(.,cols = dplyr::everything(),
                         names_to = "t",
                         values_to = "value") %>%
            dplyr::summarise(mean = mean(value, na.rm = T))) %>%
      dplyr::bind_rows()}

  if(stat == "median") {
    feat <- x %>%
      dplyr::rowwise() %>%
      dplyr::group_split() %>%
      purrr::map(~ tidyr::pivot_longer(.,cols = dplyr::everything(),
                         names_to = "t",
                         values_to = "value") %>%
            dplyr::summarise(median = stats::median(value, na.rm = T))) %>%
      dplyr::bind_rows()}

  if(stat == "min") {
    feat <- x %>%
      dplyr::rowwise() %>%
      dplyr::group_split() %>%
      purrr::map(~ tidyr::pivot_longer(.,cols = dplyr::everything(),
                         names_to = "t",
                         values_to = "value") %>%
            dplyr::summarise(min = min(value, na.rm = T))) %>%
      dplyr::bind_rows()}

  if(stat == "max") {
    feat <- x %>%
      dplyr::rowwise() %>%
      dplyr::group_split() %>%
      purrr::map(~ tidyr::pivot_longer(.,cols = dplyr::everything(),
                         names_to = "t",
                         values_to = "value") %>%
            dplyr::summarise(max = max(value, na.rm = T))) %>%
      dplyr::bind_rows()}

  if(stat == "lm") {
    feat <- x %>%
      dplyr::rowwise() %>%
      dplyr::group_split() %>%
      purrr::map(~ tidyr::pivot_longer(.,cols = dplyr::everything(),
                         names_to = "t",
                         values_to = "value") %>%
            dplyr::mutate(time = 1:nrow(.)) %>%
            stats::lm(data = ., formula = value ~ time) %>%
            broom::tidy() %>%
            dplyr::filter(term == "time") %>%
            dplyr::select(estimate)) %>%
      dplyr::bind_rows()
  }

  feat
}
