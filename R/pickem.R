#' Read Jeremy's Pick'Em excel file
#'
#' @param filename path to the excel file
#' @param week week that data should be pulled from
#' @param probs
#'
#' @return data.table
#' @export

read_pickem <- function(filename, week, probs) {
  sheet <- paste("Pick'Em Week", week)
  filename
  week
  probs

  ws <-
    openxlsx::read.xlsx(filename, sheet = sheet, rows = 2:22) %>%
    dplyr::slice(-1) %>%
    dplyr::arrange(X3)

  out <-
    bind_cols(
      ws[,-(1:5)][,c(T,F)] %>% dplyr::mutate(GAME_ID = row_number()) %>%  tidyr::gather("NAME","PICK",-GAME_ID),
      ws[,-(1:5)][,c(F,T)] %>% tidyr::gather("COL","POINTS") %>% dplyr::select(POINTS)
    ) %>%
    dplyr::filter(PICK != "Points", !is.na(PICK)) %>%
    dplyr::mutate(WEEK = week) %>%
    dplyr::select(WEEK, GAME_ID, NAME, PICK, POINTS) %>%
    dplyr::mutate(POINTS = as.integer(POINTS))

  out <- merge(out, probs[,.(WEEK,GAME_ID,TEAM, HOME_OR_AWAY)], all.x = T,
               by.x = c("WEEK","GAME_ID","PICK"),
               by.y = c("WEEK","GAME_ID","TEAM")) %>%
    data.table::as.data.table()
}

#' Generate all possible outcomes for a given week and assign scenario
#' probability
#'
#' @param probs
#' @param week
#'
#' @return data.table

generate_outcomes <- function(probs, week) {
  num_games <- probs %>% dplyr::filter(WEEK == week) %>% dplyr::summarise(max(GAME_ID)) %>% pull(1)

  outcomes <-
    dplyr::tibble(GAME_ID = c("HOME","AWAY")) %>%
    rep(num_games) %>%
    purrr::lift_dl(crossing)(.) %>%
    purrr::set_names(1:num_games) %>%
    dplyr::mutate(SIM_ID = row_number()) %>%
    tidyr::gather("GAME_ID", "OUTCOME", -SIM_ID) %>%
    dplyr::mutate_at(vars(GAME_ID),as.integer) %>%
    dplyr::mutate(WEEK = week)

  outcomes <- setDT(outcomes)
  scenario_probs <- merge(outcomes, probs,
                          by.x = c("WEEK","GAME_ID","OUTCOME"),
                          by.y = c("WEEK","GAME_ID","HOME_OR_AWAY"),
                          all.x = T)

  scenario_probs <- scenario_probs[,.(SCENARIO_PROB = prod(WIN_PROB)), by = .(WEEK, SIM_ID)]

  scenario_probs[outcomes, on = c("WEEK","SIM_ID")]
}

#' Calculates points for each scenario
#'
#' @param pickem
#' @param outcomes
#' @param losers
#'
#' @return
#' @export
calculate_points <- function(pickem, outcomes, losers = NULL) {
  results <- merge(pickem,
                   outcomes,
                   by.x = c("WEEK", "GAME_ID", "HOME_OR_AWAY"),
                   by.y = c("WEEK", "GAME_ID", "OUTCOME"),
                   all.y = T,
                   allow.cartesian = T)

  #invalid_sims <- losers[outcomes, on = .(WEEK,GAME_ID, HOME_OR_AWAY = OUTCOME)][,SIM_ID] %>% unique
  invalid_sims <- NULL

  # ties <- losers[ , if(.N > 1) .SD, by = GAME_ID]
  # outcomes[GAME_ID %in% ties$GAME_ID, SCENARIO_PROB == 0]

  #losers <- losers[ , if(.N == 1) .SD, by = GAME_ID]

  if(!is.null(losers))
    invalid_sims <- merge(losers,
                          outcomes,
                          by.x = c("WEEK", "GAME_ID", "HOME_OR_AWAY"),
                          by.y = c("WEEK", "GAME_ID", "OUTCOME"))[,SIM_ID] %>% unique()

  results <- results[!(SIM_ID %in% invalid_sims)]
  summarised_results <- results[,.(SCENARIO_POINTS = sum(POINTS)),by = .(WEEK, SIM_ID, NAME, SCENARIO_PROB)]
}

#' Calculate ranks
#'
#' @param results_points
#'
#' @return
#' @export
#'
#' @examples
calculate_ranks <- function(results_points) {
  results_points %>%
    arrange(desc(SCENARIO_POINTS)) %>%
    group_by(WEEK, SIM_ID, SCENARIO_PROB) %>%
    mutate(RANK = dense_rank(desc(SCENARIO_POINTS))) %>%
    ungroup()
}

#' Calculate wins
#'
#' @param result_ranks
#'
#' @return
#' @export
#'
#' @examples
calculate_wins <- function(result_ranks) {


  result_ranks %>%
    #filter(RANK == 1) %>%
    group_by(WEEK, SIM_ID, SCENARIO_PROB) %>%
    mutate(WIN = 1 / n() * (RANK == 1)) %>%
    group_by(WEEK, NAME) %>%
    summarise(WIN_COUNT = weighted.mean(WIN, SCENARIO_PROB),
              MEAN_POINTS = weighted.mean(SCENARIO_POINTS, SCENARIO_PROB)) %>%
    mutate(WIN_PERCENT = WIN_COUNT / sum(WIN_COUNT, na.rm = T)) %>%
    arrange(desc(WIN_COUNT)) %>%
    #mutate(WIN_PERCENT = scales::percent(WIN_PERCENT, 0.01)) %>%
    as.data.table()
}

#' Title
#'
#' @param probs
#' @param week
#'
#' @return
#' @export
#'
#' @examples
get_losers <- function(probs, week) {
  losers <- probs[WEEK == week]
  losers <- losers[RESULT == "LOSS" | RESULT == "TIE"]
  losers <- losers[,.(WEEK, GAME_ID, TEAM, HOME_OR_AWAY)]
}

#' Find projected results of NFL Pick'Em pool
#'
#' @param input_file filename where the source xlsm is located
#' @param pickem data.frame
#' @param probs data.table containing game predictions.  By default, calls \link{get_538_data}
#' @param week which week to return results for
#' @param losers see \link{get_losers}
#'
#' @return `data.table` with results
#' @export
#'
#' @examples
#' ## Not run:
#' get_results("input/NFL Pool.xlsm", 11)
#'
#' ## End(Not run)
get_results <- function(input_file,
                        week,
                        probs = get_538_data(),
                        pickem = read_pickem(input_file,week, probs),
                        losers = get_losers(probs, week)) {
  outcomes <- generate_outcomes(probs, week)

  out <- merge(
    calculate_points(pickem, outcomes) %>% calculate_ranks() %>% calculate_wins(),
    calculate_points(pickem, outcomes, losers) %>% calculate_ranks() %>% calculate_wins(),
    by.x = c("NAME","WEEK"),
    by.y = c("NAME","WEEK"),
    all.x = T,
    suffixes = c("_BEG","_END")
  )

  out[order(-MEAN_POINTS_END)][!is.na(NAME)]
}

# season_results <- map_df(1:11, ~get_results(pickem, probs_538, .))[!is.na(NAME)][!is.na(WIN_PERCENT_END)]
# season_results %>% group_by(NAME) %>% summarise(WIN_PERCENT_BEG = mean(WIN_PERCENT_BEG)) %>% arrange(desc(WIN_PERCENT_BEG)) %>% as.data.table() %>% print

################################################################################
################################################################################

# optimal_picks <- probs_538[WEEK==week][order(-WIN_PROB)][,.SD[1,],by = GAME_ID][order(WIN_PROB)][,.(WEEK = week, NAME = "OPTIMO", GAME_ID, PICK = TEAM,POINTS = .I)]
# optimal_picks <- merge(optimal_picks,
#                        probs_538[,.(WEEK,GAME_ID,TEAM, HOME_OR_AWAY)],
#                        all.x = T,
#                        by.x = c("WEEK","GAME_ID","PICK"),
#                        by.y = c("WEEK","GAME_ID","TEAM")) %>% as.data.table()
#
# pickem_filtered <- pickem[NAME != "Tyler"]
# pickem_filtered <- rbind(pickem_filtered, optimal_picks)
# calculate_points(pickem_filtered, outcomes) %>% calculate_ranks() %>% calculate_wins()

################################################################################
################################################################################


# simulate_picks <- function(probs_538) {
#   probs_538[WEEK == week][order(-WIN_PROB)][,.SD[1,],by = GAME_ID][order(WIN_PROB)][,.(WEEK = week, NAME = "OPTIMO", GAME_ID, PICK = TEAM, POINTS = .I)]
# }


# simulate <- function(outcomes, probs_538, week, n = 100) {
#
#   x <- probs_538[WEEK == week][HOME_OR_AWAY == "HOME"][order(GAME_ID)]
#   probs <- x[,WIN_PROB]
#   num_games <- length(probs)
#   points <- 1:num_games
#   points <- points[order(-x$WIN_PROB)]
#
#   trials <- map(probs, ~rbinom(n, 1, .x)) %>% set_names(glue::glue("GAME_{1:length(probs)}")) %>% as.tibble() %>%
#     mutate_all(funs(if_else(.==1, "HOME", "AWAY"))) %>% setDT() %>% as.matrix()
#
#   for(i in 1:n) {
#     test_pickem <- tibble(trials[]
#   }
#
#     new_picks <- list()
#
#   new_points <- calculate_points(new_picks, outcomes)
#   wins <- calculate_wins(rbindlist(list(base_points, new_points),use.names = T))
#   wins
# }
#
#
#
# # substitute_picks <- function(pickem, week, new_picks) {
# #   pickem <- pickem[NAME!="Tyler"]
# #   pickem <- rbindlist(list(pickem, new_picks),use.names  =T)
# # }
#
# losers <- probs_538[WEEK==12][!is.na(SCORE)][order(GAME_ID, SCORE)][,.SD[1],by = GAME_ID][,TEAM]
#
# calculate_wins(pickem_probs, week, probs_538)
# pickem_probs[NAME == "Tyler", .SD = optimal_picks] %>% calculate_wins(pickem_probs, 11, probs_538)







# pickem_probs <- pickem %>% left_join(probs_538 %>% select(-HOME_OR_AWAY), c("WEEK","GAME_ID","PICK"="TEAM")) %>% setDT()
# optimal_picks <- pickem_probs[,.(OPPONENT_NAME = NAME, GAME_ID, HOME_OR_AWAY, POINTS)]
# underdogs <- pickem_probs[,.(UNDERDOG = sum(WIN_PROB < 0.5)),by = NAME]
#
# optimal_picks[pickem_probs, on = c("GAME_ID"), allow.cartesian = T] %>%
#   .[OPPONENT_NAME != NAME] %>%
#   .[,.(NAME,POINTS_DIFF = POINTS - if_else(HOME_OR_AWAY == i.HOME_OR_AWAY, 1, -1)*i.POINTS)] %>%
#   .[,.(HAMMOND_STAT = sum(POINTS_DIFF^2)), by = "NAME"] %>%
#   .[order(-HAMMOND_STAT)] %>%
#   .[calculate_points(pickem, outcomes) %>% calculate_wins(), on = "NAME"] %>%
#   .[underdogs, on = "NAME"] %>%
#   .[order(-WIN_COUNT)]
