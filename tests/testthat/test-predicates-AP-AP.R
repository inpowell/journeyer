library(DBI)
library(dplyr)
library(dbplyr)

# Set up connections
cons <- vector('list')

if (requireNamespace('duckdb')) {
  cons[['duckdb']] <- dbConnect(duckdb::duckdb())
  withr::defer(dbDisconnect(cons[['duckdb']], shutdown = TRUE))
}

for (pkg in names(cons)) {
  con <- cons[[pkg]]

  test_that(sprintf("overlap_episodes correctly assigns transfers (%s)", pkg), {

    # Set up transaction to always start with clean slate
    dbBegin(con)
    withr::defer(dbRollback(con))

    data <- tibble::tribble(
      ~RL_ID,    ~PPN,        ~episode_start,          ~episode_end,
      # Person 1 has two overlapping episodes
      1L    ,      1L, '2022-01-01 12:00:00', '2022-02-01 09:00:00',
      2L    ,      1L, '2022-01-15 12:00:00', '2022-02-15 09:15:00',
      # Person 2 has two non-overlapping episodes
      3L    ,      2L, '2022-01-01 12:00:00', '2022-02-03 12:00:00',
      4L    ,      2L, '2021-01-01 12:00:00', '2021-02-03 12:00:00'
    )
    setDT(data)
    data[, c('episode_start', 'episode_end') := lapply(.SD, as.POSIXct),
         .SDcols = c('episode_start', 'episode_end')]

    expected <- tibble::tribble(
      ~child, ~parent,
      2L, 1L
    )
    setDT(expected)

    dbWriteTable(con, 'input', data)

    din <- tbl(con, 'input')
    observed <- din %>%
      inner_join(., ., suffix = c('_parent', '_child'), by = 'PPN') %>%
      filter(RL_ID_child != RL_ID_parent) %>%
      find_links(!!overlap_episodes(episode_start_parent, episode_end_parent, episode_start_child)) %>%
      select(child = RL_ID_child, parent = RL_ID_parent) %>%
      arrange(child, parent)

    expect_identical(
      setDT(dplyr::collect(observed)),
      expected
    )
  })

  test_that(sprintf("sameday_transfer correctly assigns transfers (%s)", pkg), {
    # Set up transaction to always start with clean slate
    dbBegin(con)
    withr::defer(dbRollback(con))

    data <- tibble::tribble(
      ~RL_ID,    ~PPN,          ~episode_start,          ~episode_end, ~modesep,
      # Person 1 has no other records, so no link
      1L    ,      1L,   '2022-01-01 09:00:00', '2022-02-01 09:00:00',      '1',
      # Person 2 has one transfer, and another POHC
      2L    ,      2L,   '2022-01-01 09:00:00', '2022-02-01 09:00:00',      '5',
      3L    ,      2L,   '2022-02-01 09:00:00', '2022-02-03 09:00:00',      '1',
      4L    ,      2L,   '2022-03-01 09:00:00', '2022-03-03 09:00:00',      '1',
      # Person 3 has a transfer of each type
      5L    ,      3L,   '2022-02-01 09:00:00', '2022-02-03 09:00:00',      '5', # Transfer
      6L    ,      3L,   '2022-02-03 09:00:00', '2022-02-04 09:00:00',      '9', # Type change
      7L    ,      3L,   '2022-02-04 09:00:00', '2022-02-05 09:00:00',      '6',
      # Person 4 looks like they have a transfer, but destination later - no link
      8L    ,      4L,   '2022-02-03 09:00:00', '2022-02-04 09:00:00',      '9', # Type change
      9L    ,      4L,   '2022-02-06 09:00:00', '2022-02-04 09:00:00',      '1'
    )
    setDT(data)
    data[, c('episode_start', 'episode_end') := lapply(.SD, as.POSIXct),
         .SDcols = c('episode_start', 'episode_end')]

    expected <- setDT(tibble::tribble(
      ~child, ~parent,
      3L, 2L,
      6L, 5L,
      7L, 6L
    ))

    dbWriteTable(con, 'input', data)

    din <- tbl(con, 'input')
    observed <- din %>%
      inner_join(., ., suffix = c('_parent', '_child'), by = 'PPN') %>%
      filter(RL_ID_child != RL_ID_parent) %>%
      find_links(!!sameday_transfer(
        episode_start_parent = episode_start_parent,
        episode_end_parent = episode_end_parent,
        modesep_parent = modesep_parent,
        episode_start_child = episode_start_child
      )) %>%
      select(child = RL_ID_child, parent = RL_ID_parent) %>%
      arrange(child, parent)

    expect_identical(
      setDT(dplyr::collect(observed)),
      expected
    )
  })

  test_that(sprintf("samestart correctly assigns transfers (%s)", pkg), {
    # Set up transaction to always start with clean slate
    dbBegin(con)
    withr::defer(dbRollback(con))

    data <- tibble::tribble(
      ~RL_ID,    ~PPN,        ~episode_start,          ~episode_end,
      1L    ,      1L, '2022-01-01 12:00:00', '2022-02-01 09:00:00',
      2L    ,      1L, '2022-01-01 12:00:00', '2022-02-15 09:15:00',
      # Person 2 has an episode starting at same time as Person 1
      3L    ,      2L, '2022-01-01 12:00:00', '2022-02-03 12:00:00'
    )
    setDT(data)
    data[, c('episode_start', 'episode_end') := lapply(.SD, as.POSIXct),
         .SDcols = c('episode_start', 'episode_end')]

    expected <- setDT(tibble::tribble(
      ~child, ~nest,
      1L, 2L
    ))

    dbWriteTable(con, 'input', data)

    din <- tbl(con, 'input')
    observed <- din %>%
      inner_join(., ., suffix = c('_parent', '_child'), by = 'PPN') %>%
      filter(RL_ID_child != RL_ID_parent) %>%
      find_links(!!samestart(episode_start_parent, episode_end_parent, episode_start_child, episode_end_child)) %>%
      select(child = RL_ID_child, nest = RL_ID_parent)

    expect_identical(
      setDT(dplyr::collect(observed)),
      expected
    )
  })

  test_that(sprintf("transfers_in correctly assigns transfers (%s)", pkg), {
    # Set up transaction to always start with clean slate
    dbBegin(con)
    withr::defer(dbRollback(con))

    data <- tibble::tribble(
      ~RL_ID,    ~PPN,        ~episode_start,          ~episode_end, ~facility, ~facility_from, ~source_referral,
      # Person 1 has "perfect" transfer
      1L    ,      1L, '2022-01-01 12:00:00', '2022-02-01 09:00:00',    'A111',             '',             '01',
      2L    ,      1L, '2022-02-01 12:00:00', '2022-02-15 09:15:00',    'B222',         'A111',             '05',
      # Person 2 has same characteristics as person 1, but facilities don't match
      3L    ,      2L, '2022-01-01 12:00:00', '2022-02-01 09:00:00',    'A111',             '',             '01',
      4L    ,      2L, '2022-02-01 12:00:00', '2022-02-15 09:15:00',    'B222',         'A222',             '04'
    )
    setDT(data)
    data[, c('episode_start', 'episode_end') := lapply(.SD, as.POSIXct),
         .SDcols = c('episode_start', 'episode_end')]

    expected_facility <- setDT(tibble::tribble(
      ~child, ~parent,
      2L, 1L
    ))
    expected_nofacility <- setDT(tibble::tribble(
      ~child, ~parent,
      2L, 1L,
      4L, 3L
    ))

    dbWriteTable(con, 'input', data)

    din <- tbl(con, 'input')
    observed_base <- din %>%
      inner_join(., ., suffix = c('_parent', '_child'), by = 'PPN') %>%
      filter(RL_ID_child != RL_ID_parent)

    observed_facility <- observed_base %>%
      find_links(!!transfers_in(
        episode_end_parent = episode_end_parent,
        facility_parent = facility_parent,
        episode_start_child = episode_start_child,
        source_referral_child = source_referral_child,
        facility_from_child = facility_from_child,
        facilities_must_match = TRUE
      )) %>%
      select(child = RL_ID_child, parent = RL_ID_parent) %>%
      arrange(child, parent)

    observed_nofacility <- observed_base %>%
      find_links(!!transfers_in(
        episode_end_parent = episode_end_parent,
        facility_parent = facility_parent,
        episode_start_child = episode_start_child,
        source_referral_child = source_referral_child,
        facility_from_child = facility_from_child,
        facilities_must_match = FALSE
      )) %>%
      select(child = RL_ID_child, parent = RL_ID_parent) %>%
      arrange(child, parent)

    expect_identical(
      setDT(dplyr::collect(observed_facility)),
      expected_facility
    )

    expect_identical(
      setDT(dplyr::collect(observed_nofacility)),
      expected_nofacility
    )
  })

  test_that(sprintf("transfers_out correctly assigns transfers (%s)", pkg), {
    # Set up transaction to always start with clean slate
    dbBegin(con)
    withr::defer(dbRollback(con))

    data <- tibble::tribble(
      ~RL_ID,    ~PPN,        ~episode_start,          ~episode_end, ~facility, ~facility_to, ~modesep,
      # Person 1 has "perfect" transfer
      1L    ,      1L, '2022-01-01 12:00:00', '2022-02-01 09:00:00',    'A111',       'B222',      '5',
      2L    ,      1L, '2022-02-01 12:00:00', '2022-02-15 09:15:00',    'B222',           '',      '1',
      # Person 2 has same characteristics as person 1, but facilities don't match
      3L    ,      2L, '2022-01-01 12:00:00', '2022-02-01 09:00:00',    'A111',       'A222',      '5',
      4L    ,      2L, '2022-02-01 12:00:00', '2022-02-15 09:15:00',    'B222',           '',      '1'
    )
    setDT(data)
    data[, c('episode_start', 'episode_end') := lapply(.SD, as.POSIXct, format = '%F %X'),
         .SDcols = c('episode_start', 'episode_end')]

    expected_facility <- setDT(tibble::tribble(
      ~child, ~parent,
      2L, 1L
    ))
    expected_nofacility <- setDT(tibble::tribble(
      ~child, ~parent,
      2L, 1L,
      4L, 3L
    ))

    dbWriteTable(con, 'input', data)

    din <- tbl(con, 'input')
    observed_base <- din %>%
      inner_join(., ., suffix = c('_parent', '_child'), by = 'PPN') %>%
      filter(RL_ID_child != RL_ID_parent)

    observed_facility <- observed_base %>%
      find_links(!!transfers_out(
        episode_end_parent = episode_end_parent,
        modesep_parent = modesep_parent,
        facility_to_parent = facility_to_parent,
        episode_start_child = episode_start_child,
        facility_child = facility_child,
        facilities_must_match = TRUE
      )) %>%
      select(child = RL_ID_child, parent = RL_ID_parent) %>%
      arrange(child, parent)

    observed_nofacility <- observed_base %>%
      find_links(!!transfers_out(
        episode_end_parent = episode_end_parent,
        modesep_parent = modesep_parent,
        facility_to_parent = facility_to_parent,
        episode_start_child = episode_start_child,
        facility_child = facility_child,
        facilities_must_match = FALSE
      )) %>%
      select(child = RL_ID_child, parent = RL_ID_parent) %>%
      arrange(child, parent)

    expect_identical(
      setDT(dplyr::collect(observed_facility)),
      expected_facility
    )

    expect_identical(
      setDT(dplyr::collect(observed_nofacility)),
      expected_nofacility
    )
  })

  test_that(sprintf("transfers_both correctly assigns transfers (%s)", pkg), {
    # Set up transaction to always start with clean slate
    dbBegin(con)
    withr::defer(dbRollback(con))

    data <- tibble::tribble(
      ~RL_ID,    ~PPN,        ~episode_start,          ~episode_end, ~facility, ~facility_from, ~facility_to, ~modesep, ~source_referral,
      # Person 1 has "perfect" transfer
      1L    ,      1L, '2022-01-01 12:00:00', '2022-02-01 09:00:00',    'A111',             '',       'B222',      '5',             '01',
      2L    ,      1L, '2022-02-01 12:00:00', '2022-02-15 09:15:00',    'B222',         'A111',           '',      '1',             '04',
      # Person 2 has same characteristics as person 1, but facilities don't match
      3L    ,      2L, '2022-01-01 12:00:00', '2022-02-01 09:00:00',    'A111',             '',       'A222',      '5',             '01',
      4L    ,      2L, '2022-02-01 12:00:00', '2022-02-15 09:15:00',    'B222',         'A111',           '',      '1',             '05'
    )
    setDT(data)
    data[, c('episode_start', 'episode_end') := lapply(.SD, as.POSIXct, format = '%F %X'),
         .SDcols = c('episode_start', 'episode_end')]

    expected_facility <- setDT(tibble::tribble(
      ~child, ~parent,
      2L, 1L
    ))
    expected_nofacility <- setDT(tibble::tribble(
      ~child, ~parent,
      2L, 1L,
      4L, 3L
    ))

    dbWriteTable(con, 'input', data)

    din <- tbl(con, 'input')
    observed_base <- din %>%
      inner_join(., ., suffix = c('_parent', '_child'), by = 'PPN') %>%
      filter(RL_ID_child != RL_ID_parent)

    observed_facility <- observed_base %>%
      find_links(!!transfers_both(
        episode_end_parent = episode_end_parent,
        modesep_parent = modesep_parent,
        facility_parent = facility_parent,
        facility_to_parent = facility_to_parent,
        episode_start_child = episode_start_child,
        source_referral_child = source_referral_child,
        facility_child = facility_child,
        facility_from_child = facility_from_child,
        facilities_must_match = TRUE
      )) %>%
      select(child = RL_ID_child, parent = RL_ID_parent) %>%
      arrange(child, parent)

    observed_nofacility <- observed_base %>%
      find_links(!!transfers_both(
        episode_end_parent = episode_end_parent,
        modesep_parent = modesep_parent,
        facility_parent = facility_parent,
        facility_to_parent = facility_to_parent,
        episode_start_child = episode_start_child,
        source_referral_child = source_referral_child,
        facility_child = facility_child,
        facility_from_child = facility_from_child,
        facilities_must_match = FALSE
      )) %>%
      select(child = RL_ID_child, parent = RL_ID_parent) %>%
      arrange(child, parent)

    expect_identical(
      setDT(dplyr::collect(observed_facility)),
      expected_facility
    )

    expect_identical(
      setDT(dplyr::collect(observed_nofacility)),
      expected_nofacility
    )
  })

  test_that(sprintf("type_change correctly assigns transfers (%s)", pkg), {
    dbBegin(con)
    withr::defer(dbRollback(con))

    data <- tibble::tribble(
      ~ID, ~PPN,        ~episode_start,          ~episode_end, ~src_ref, ~modesep, ~facility,
      # Perfect type change
      1L,    1L, '2022-01-01 09:00:00', '2022-01-02 13:00:00',      '1',      '9',       'A',
      2L,    1L, '2022-01-02 13:01:00', '2022-01-02 17:00:00',      '9',      '1',       'A',
      # TCS, but not TCA -- child start slightly before
      3L,    1L, '2022-07-01 09:00:00', '2022-07-02 13:00:00',      '1',      '9',       'A',
      4L,    1L, '2022-07-02 12:59:00', '2022-07-02 17:00:00',      '1',      '1',       'A',
      # TCA, but not TCS -- child start exactly the same
      5L,    1L, '2023-01-01 09:00:00', '2023-01-02 13:00:00',      '1',      '1',       'A',
      6L,    1L, '2023-01-02 13:00:00', '2023-01-02 17:00:00',      '9',      '1',       'A',
      # Perfect, except mismatched facilities
      7L,    1L, '2023-07-01 09:00:00', '2023-07-02 13:00:00',      '1',      '9',       'A',
      8L,    1L, '2023-07-02 13:03:00', '2023-07-02 17:00:00',      '9',      '1',       'B'
    )
    setDT(data)
    data[, c('episode_start', 'episode_end') := lapply(.SD, as.POSIXct, format = '%F %X'),
         .SDcols = c('episode_start', 'episode_end')]

    expected_perfect <- setDT(tibble::tribble(
      ~child, ~parent,
      2L, 1L
    ))
    expected_tcs <- setDT(tibble::tribble(
      ~child, ~parent,
      2L, 1L,
      4L, 3L
    ))
    expected_tca <- setDT(tibble::tribble(
      ~child, ~parent,
      2L, 1L,
      6L, 5L
    ))
    expected_nofac <- setDT(tibble::tribble(
      ~child, ~parent,
      2L, 1L,
      8L, 7L
    ))

    dbWriteTable(con, 'input', data)

    din <- tbl(con, 'input')
    obs_base <- din %>%
      inner_join(., ., suffix = c('_parent', '_child'), by = 'PPN') %>%
      filter(ID_child != ID_parent)

    obs_perfect <- obs_base %>%
      find_links(!!type_change(
        episode_end_parent, modesep_parent, facility_parent,
        episode_start_child, src_ref_child, facility_child,
        require = 'both'
      )) %>%
      select(child = ID_child, parent = ID_parent) %>%
      arrange(child, parent)

    obs_tcs <- obs_base %>%
      find_links(!!type_change(
        episode_end_parent, modesep_parent, facility_parent,
        episode_start_child, src_ref_child, facility_child,
        require = 'separation'
      )) %>%
      select(child = ID_child, parent = ID_parent) %>%
      arrange(child, parent)

    obs_tca <- obs_base %>%
      find_links(!!type_change(
        episode_end_parent, modesep_parent, facility_parent,
        episode_start_child, src_ref_child, facility_child,
        require = 'referral'
      )) %>%
      select(child = ID_child, parent = ID_parent) %>%
      arrange(child, parent)

    obs_nofac <- obs_base %>%
      find_links(!!type_change(
        episode_end_parent, modesep_parent, facility_parent,
        episode_start_child, src_ref_child, facility_child,
        require = 'both', facilities_must_match = FALSE
      )) %>%
      select(child = ID_child, parent = ID_parent) %>%
      arrange(child, parent)

    expect_identical(setDT(collect(obs_perfect)), expected_perfect)
    expect_identical(setDT(collect(obs_tcs)), expected_tcs)
    expect_identical(setDT(collect(obs_tca)), expected_tca)
    expect_identical(setDT(collect(obs_nofac)), expected_nofac)
  })
}

test_that("sameday_transfer does not assign child preceding parent", {
  rexp <- sameday_transfer(
    episode_start_parent = as.POSIXct('2022-01-01 12:00:00'),
    episode_end_parent = as.POSIXct('2022-01-01 15:00:00'),
    modesep_parent = '9', # Type change
    episode_start_child = as.POSIXct('2022-01-01 09:00:00')
  )

  expect_identical(rlang::eval_tidy(rexp), FALSE)
})

test_that("transfers_out returns false with missing facility requiring match", {
  rexp <- transfers_out(
    episode_end_parent = as.POSIXct('2022-01-01 12:00:00'),
    modesep_parent =  '5', # Transfer
    facility_to_parent = NA_character_,
    episode_start_child = as.POSIXct('2022-01-01 15:00:00'),
    facility_child = 'A123',
    facilities_must_match = TRUE,
    transfer_time = lubridate::hours(9L)
  )

  expect_identical(rlang::eval_tidy(rexp), FALSE)
})

