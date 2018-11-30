get_games <- function(x) {
  dplyr::tibble(DATE =
           x %>%
           rvest::html_nodes(".h4") %>%
           rvest::html_text(),
         GAMES =
           x %>%
           rvest::html_nodes(".games-wrap") %>%
           purrr::map(~
                        rvest::html_nodes(., ".game-body") %>%
                        rvest::html_table())) %>%
    tidyr::unnest(GAMES) %>%
    dplyr::mutate(GAMES = purrr::map(GAMES, ~purrr::set_names(.,"LOGO","TEAM","SPREAD","WIN_PROB","SCORE"))) %>%
    dplyr::mutate(GAMES = purrr::map(GAMES, ~dplyr::select(., TEAM, WIN_PROB, SCORE))) %>%
    dplyr::mutate(GAMES = purrr::map(GAMES, ~dplyr::mutate(.,
                                             WIN_PROB =
                                               WIN_PROB %>%
                                               stringr::str_replace("%", "") %>%
                                               as.numeric() / 100,
                                             HOME_OR_AWAY = c("AWAY","HOME")))) %>%
    mutate(GAME_ID = row_number())
}

#' Extract 538 NFL Prediction Data
#'
#' @return `data.table`
#' @export
get_538_data <- function() {
  fivethirtyeight_nfl_url <- "https://projects.fivethirtyeight.com/2018-nfl-predictions/games/"

  nodes <-
    xml2::read_html(fivethirtyeight_nfl_url) %>%
    rvest::html_nodes(".week")

  tibble::tibble(WEEK =
           nodes %>%
           purrr::map_int(~rvest::html_nodes(.,".h3") %>%
                            rvest::html_text() %>%
                            stringr::str_replace("Week ", "") %>%
                            as.integer()),
         GAMES =
           nodes %>%
           purrr::map(~rvest::html_nodes(.,".day")) %>%
           purrr::map(get_games)) %>%
    tidyr::unnest(GAMES) %>%
    tidyr::unnest(GAMES) %>%
    dplyr::mutate(TEAM = TEAM %>% stringr::str_replace_all("\\.","")) %>%
    dplyr::arrange(WEEK, desc(HOME_OR_AWAY), TEAM) %>%
    dplyr::group_by(WEEK) %>%
    dplyr::mutate(NEW_GAME_ID = row_number()) %>%
    dplyr::group_by(WEEK, GAME_ID) %>%
    dplyr::mutate(RESULT = dplyr::case_when(is.na(SCORE) ~ rlang::na_chr,
                              max(SCORE) == min(SCORE) ~ "TIE",
                              SCORE == max(SCORE) ~ "WIN",
                              TRUE ~ "LOSS")) %>%
    dplyr::mutate(NEW_GAME_ID = min(NEW_GAME_ID)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(GAME_ID = NEW_GAME_ID) %>%
    dplyr::select(-NEW_GAME_ID) %>%
    data.table::setDT()
}

