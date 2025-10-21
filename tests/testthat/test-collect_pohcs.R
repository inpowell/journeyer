test_that("collect_journeys assigns links correctly", {
  data <- data.table(RL_ID = seq_len(10),
                     PPN = rep(1L:4L, times = c(1, 2, 3, 4)))
  links <- data.table(
    child = c(3L, 5L, 6L, 8L, 10L),
    nest  = c(2L, 4L, 5L, 7L,  9L)
  )

  output <- data.table(
    data,
    journey = c(1L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 5L, 5L)
  )

  expect_identical(
    collect_journeys(data, links, identifier = 'RL_ID'),
    output
  )
})

test_that("collect_journeys works with multiple datasets", {
  data <- list(
    A = data.table(RL_ID = seq_len(4L)),
    B = data.table(RL_ID = seq_len(3L))
  )

  links <- list(
    'A to A' = data.table(from = c(1L, 2L), to = c(2L, 3L)),
    'A to B' = data.table(from = 2L, to = 1L),
    'B to B' = data.table(from = 1L, to = 2L)
  )

  expected <- list(
    A = data.table(RL_ID = seq_len(4L), journey = c(1L, 1L, 1L, 2L)),
    B = data.table(RL_ID = seq_len(3L), journey = c(1L, 1L, 3L))
  )

  expect_identical(
    collect_journeys(
      data, links, identifier = 'RL_ID',
      links_from = c('A', 'A', 'B'),
      links_to = c('A', 'B', 'B')
    ),
    expected
  )
})
