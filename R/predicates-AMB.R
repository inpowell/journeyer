#' Ambulance arrivals at emergency departments or hospitalisations
#'
#' `amb_arrival` finds links between ambulance calls and either emergency
#' department presentations or admitted patient episodes. By default, an hour is
#' allowed after the ambulance call for the presentation or episode to start.
#'
#' @param ambulance_start,ambulance_end Start and end timestamps of the
#'   candidate parent ambulance call, respectively.
#' @param child_start Start timestamp of the candidate child presentation or
#'   admission.
#' @param buffer_time Time allowed between the end of an ambulance call and the
#'   start of a presentation/admission.
#'
#' @return `amb_arrival` returns a call to be used in `dplyr`/`dbplyr` filters.
#'
#' @export
#' @md
#'
#' @family link predicates
amb_arrival <- function(
    ambulance_start,
    ambulance_end,
    child_start,
    buffer_time = !!getOption('jnyr.amb.time.buffer')) {
  rlang::expr(
    {{ambulance_start}} <= {{child_start}} &
      {{child_start}} <= {{ambulance_end}} + {{buffer_time}}
  )
}

#' Transfers out of hospital using an ambulance
#'
#' `amb_transfer_out` finds ambulance calls which follow the end of an admitted
#' patient episode, where the patient is recorded as being transferred to
#' another location. By default, an ambulance call can start up to an hour
#' before the end of an admission.
#'
#' @param ambulance_start,ambulance_end Start and end timestamps of the
#'   candidate ambulance call, respectively.
#' @param episode_end End timestamp of the candidate hospitalisation
#'   transferring the patient out.
#' @param modesep Recorded mode of separation of the candidate admission.
#' @param buffer_time Time allowed before the end of an admission that an
#'   ambulance call qualifies.
#' @param transfer_codes Mode of separation codes corresponding to a transfer.
#'
#' @return `amb_transfer_out` returns a call to be used in `dplyr`/`dbplyr`
#'   filters.
#'
#' @export
#' @md
#'
#' @family link predicates
amb_transfer_out <- function(
    ambulance_start,
    ambulance_end,
    episode_end,
    modesep,
    buffer_time = !!getOption('jnyr.amb.time.buffer'),
    transfer_codes = !!getOption('jnyr.apdc.modesep.transfers')) {
  rlang::expr(
    {{ambulance_start}} - {{buffer_time}} <= {{episode_end}} &
      {{episode_end}} <= {{ambulance_end}} &
      {{modesep}} %in% {{transfer_codes}}
  )
}
