#' Collect patient journeys from links
#'
#' `collect_journeys` takes a dataset and the links between records, and adds a
#' column for the patient journey number. This number is unique for every
#' journey, not just journeys within the same person.
#'
#' For longer-running collections, `collect_journeys` will display a progress
#' bar. If this does not appear, then set the RSTUDIO environment variable to
#' '1' with `Sys.setenv(RSTUDIO = '1')`. If you want to disable this, set the
#' `progress_enabled` option to FALSE with `options(progress_enabled = FALSE)`.
#'
#' @param data The data to link together, optionally as a list of `data.table`s.
#' @param links A two-column table of links, or a list of such tables if `data`
#'   is a list. The first column should indicate parent episodes, and the second
#'   should indicate child episodes.
#' @param identifier The name of the column with episode identifiers.
#' @param journey The name of the journey column to create.
#' @param links_from (If `data` is a list.) Which item of `data` corresponds to
#'   the parent columns in each item in `links`.
#' @param links_to (If `data` is a list.) Which item of `data` corresponds to
#'   the child columns in each item in `links`.
#'
#' @return `collect_journeys` adds the column `journey` to the original data for
#'   the journey sequence number. It then returns the updated data, invisibly.
#' @export
#' @md
#' @importFrom stats setNames
collect_journeys <- function(
    data,
    links,
    identifier,
    links_from = character(),
    links_to = character(),
    journey = 'journey') {
  if (is.data.table(data)) {
    graph <- igraph::make_empty_graph(directed = TRUE) +
      igraph::vertices(data[[identifier]]) +
      igraph::edges(t(as.matrix(links)))

    comp <- igraph::components(graph, mode = 'weak')
    mem <- as.integer(comp$membership[data[[identifier]]])

    set(data, j = journey, value = mem)
  } else if (all(purrr::map_lgl(data, is.data.table))) {
    nodes <- rbindlist(purrr::imap(data, function(DT, name)
        DT[, identifier, with = FALSE][, 'dataset' := name]
    ))
    nodes[, ('node') := .I]

    linkl <- rbindlist(purrr::pmap(
      list(links, links_from, links_to), function(L, .fr, .to)
        copy(L)[, 'fromdata' := .fr][, 'todata' := .to]
    ))
    setnames(linkl, 1:2, c('parent', 'child'))

    from <- nodes[
      linkl, on = setNames(nm = c('dataset', identifier), c('fromdata', 'parent')),
      'node', with = FALSE
    ]$node
    to <- nodes[
      linkl, on = setNames(nm = c('dataset', identifier), c('todata', 'child')),
      'node', with = FALSE
    ]$node

    graph <- igraph::make_empty_graph(directed = TRUE) +
      igraph::vertices(nodes$node) +
      igraph::edges(rbind(from, to)) # Require edges in order (f1, t1), (f2, e2), ...

    comp <- igraph::components(graph, mode = 'weak')
    mem <- as.integer(comp$membership[nodes$node])

    set(nodes, j = journey, value = mem)

    for (dname in names(data)) {
      DT <- data[[dname]]
      DT[
        , (journey) := nodes[list(dname), on = 'dataset'][
          .SD, journey, with = FALSE, on = 'RL_ID']
      ]
    }
  }

  invisible(data)
}
