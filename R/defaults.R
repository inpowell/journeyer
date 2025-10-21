#' Default options for predicates by jurisdiction
#'
#' `default_options` is a list that contains parameters for various predicates
#' by jurisdiction. These can be used to define default parameters in a flexible
#' manner.
#'
#' @export
#'
#' @format A list with two levels. The first level defines a jurisdiction, and
#'   the second level defines the options for the package. Currently, it
#'   contains options defined for:
#'
default_pred_options <- list(
  #' - NSW
  NSW = list(
    # APDC
    jnyr.apdc.modesep.sameday = quote(c('5', '9')),
    jnyr.apdc.modesep.edtrans = quote(c('4', '5', '9')),
    jnyr.apdc.modesep.transfers = quote(c('3', '4', '5', '8', '11')),
    jnyr.apdc.modesep.tcs = quote('9'),
    jnyr.apdc.time.transfer = quote(lubridate::hours(9L)),
    jnyr.apdc.time.tcs = quote(lubridate::minutes(30L)),
    jnyr.apdc.refer.transfers = quote(c('4', '04', '5', '05')),
    jnyr.apdc.refer.tcs = quote(c('9', '09')),
    jnyr.apdc.refer.ed = quote(c('1', '01')),
    jnyr.apdc.involve.ed = quote(c('1', '2', '4', '5')),

    # EDDC
    jnyr.eddc.modesep.admission = quote(c('1', '01', '5', '05', '10', '11', '12')),
    jnyr.eddc.modesep.transfer = quote(c('5', '05', '9', '09', '12')),
    jnyr.eddc.refer.transfer = quote(c('06', '07', '08', '13')),
    jnyr.eddc.time.admission = quote(lubridate::hours(6L)),
    jnyr.eddc.time.transfer = quote(lubridate::hours(9L)),
    jnyr.eddc.time.buffer = quote(lubridate::minutes(15L)),

    # AMB
    jnyr.amb.time.buffer = quote(lubridate::minutes(60L))
  )
  #' @examples
  #' options(default_pred_options$NSW)
  #' transfers_both(
  #'   episode_end_parent, mode_of_separation_recode_parent,
  #'   facility_identifier_recode_parent, facility_trans_to_recode_parent,
  #'   episode_start_child,
  #'   facility_identifier_recode_child, facility_trans_from_child
  #' )
)

# Define default predicates -----------------------------------------------

#' Default predicates for period of care links by jurisdiction
#'
#' @description `default_predicates` is a list that contains default predicates
#'   for a variety of datasets within jurisdictions.
#'
#' @export
#' @importFrom lubridate hours
#'
#' @format A `list` with three levels:
#'
#'   1. The first level defines a jurisdiction.
#'
#'   2. The second level defines the datasets which the predicates find links
#'   between.
#'
#'   3. The third level contains the predicates as expressions.
#'
#'   In each case, the predicates assume that the datasets are joined using an
#'   inner join, and where there are column name collisions, the first dataset
#'   has suffix `_parent`, and the second has suffix `_child`. They also assume
#'   that the correct options have been set, for example with
#'   `default_pred_options`.
default_predicates <- list()
# NB edit defaults in R/zzz.R to ensure predicate functions are defined
