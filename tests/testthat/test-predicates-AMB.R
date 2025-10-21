library(DBI)
library(dplyr)
library(dbplyr)

# Set up datasets
APDC <- setDT(tibble::tribble(
  ~RL_ID, ~PPN,     ~episode_start,       ~episode_end, ~mode_of_separation_recode,
  'A1',      1, '2022-01-01 13:00', '2022-02-01 12:00',                        '5',
  'A2',      2, '2022-03-01 23:01', '2022-03-05 09:00',                        '1'
))
APDC[, c('episode_start', 'episode_end') :=
       lapply(.(episode_start, episode_end), as.POSIXct)]

AMB <- setDT(tibble::tribble(
  ~RL_ID, ~PPN,  ~RT_Response_Date, ~Time_ArrivedAtScene, ~Time_Depart_Scene, ~Time_Arrive_Destination,
  'M1',      1,      NA_character_,   '2022-01-01 12:00', '2022-01-01 12:05',       '2022-01-01 12:30',
  'M2',      1,      NA_character_,   '2022-02-01 12:05', '2022-02-01 12:08',       '2022-02-01 13:45',
  'M3',      2, '2022-03-01 22:00',        NA_character_,      NA_character_,            NA_character_
))
AMB[, c('RT_Response_Date', 'Time_ArrivedAtScene', 'Time_Depart_Scene', 'Time_Arrive_Destination') :=
      lapply(.SD, as.POSIXct),
    .SDcols = c('RT_Response_Date', 'Time_ArrivedAtScene', 'Time_Depart_Scene', 'Time_Arrive_Destination')]
AMB[, start := pmin(RT_Response_Date, Time_ArrivedAtScene, Time_Depart_Scene, Time_Arrive_Destination, na.rm = TRUE)]
AMB[, end := pmax(RT_Response_Date, Time_ArrivedAtScene, Time_Depart_Scene, Time_Arrive_Destination, na.rm = TRUE)]

# Set up connections
cons <- vector('list')

if (requireNamespace('duckdb')) {
  cons[['duckdb']] <- dbConnect(duckdb::duckdb())
  withr::defer(dbDisconnect(cons[['duckdb']], shutdown = TRUE))
}

for (pkg in names(cons)) {
  con <- cons[[pkg]]

  DBI::dbWriteTable(con, 'APDC', APDC)
  DBI::dbWriteTable(con, 'AMB', AMB)

  test_that("amb_arrival identifies ambulance arrivals correctly", {
    expected <- setDT(tibble::tribble(
      ~RL_ID_parent, ~RL_ID_child,
      'M1', 'A1'
    ))

    APDC <- tbl(con, "APDC")
    AMB <- tbl(con, 'AMB')

    observed <- inner_join(
      AMB, APDC,
      by = 'PPN',
      suffix = c("_parent", "_child")
    ) %>%
      filter(RL_ID_parent != RL_ID_child) %>%
      find_links(!!amb_arrival(start, end, episode_start)) %>%
      select(RL_ID_parent, RL_ID_child)

    expect_equal(
      setDT(dplyr::collect(observed)),
      expected
    )
  })

  test_that("amb_transfer_out identifies ambulance transfers correctly", {
    expected <- setDT(tibble::tribble(
      ~RL_ID_parent, ~RL_ID_child,
      'M2', 'A1'
    ))

    APDC <- tbl(con, "APDC")
    AMB <- tbl(con, 'AMB')

    observed <- inner_join(
      AMB, APDC,
      by = 'PPN',
      suffix = c("_parent", "_child")
    ) %>%
      filter(RL_ID_parent != RL_ID_child) %>%
      find_links(!!amb_transfer_out(start, end, episode_end, mode_of_separation_recode)) %>%
      select(RL_ID_parent, RL_ID_child)

    expect_equal(
      setDT(dplyr::collect(observed)),
      expected
    )
  })
}
