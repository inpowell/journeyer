#' Toy dataset simulating the NSW APDC
#'
#' A simulated dataset that imitates a some patterns of patient journeys in the
#' NSW APDC. This subset only contains variables relating to the patient
#' journey, and does not reflect any real individual's healthcare journey.
#'
#' @format A data frame with 21,832 rows and 9 columns: \describe{
#'
#'   \item{RL_ID}{Primary key for the dataset, as a 32-digit hexadecimal
#'   string.}
#'
#'   \item{PPN}{Integer project person number that uniquely identifies
#'   individuals.}
#'
#'   \item{source_of_referral_recode}{Patient source of referral; codes "04" and
#'   "05" correspond to inter-hospital transfers, while "09" corresponds to a
#'   tpye change admission. Other codes are documented in the linked data
#'   dictionary.}
#'
#'   \item{mode_of_separation_recode}{Patient mode of separation; codes "3",
#'   "4", "5", "8" and "11" correspond to possible inter-facility transfers,
#'   while "9" corresponds to a type change separation. Other codes correspond
#'   to discharge or death, and are documented in the data dictionary.}
#'
#'   \item{episode_start, episode_end}{Start and end timestamps for the episode
#'   of care.}
#'
#'   \item{facility_identifier_recode}{A masked facility identifier for the
#'   episode of care.}
#'
#'   \item{facility_trans_from_recode, facility_trans_to_recode}{For some
#'   episodes beginning or ending with a transfer, these columns represent the
#'   faciliy of the previous or next episode respectively.}
#'
#'   }
#'
#' @source Artificial data developed in-house by the NSW Ministry of Health. The
#'   data dictionary for the NSW APDC, which this dataset imitates, is available
#'   on the Centre for Health Record Linkage website:
#'   [https://www.cherel.org.au/data-dictionaries](https://www.cherel.org.au/data-dictionaries).
#'
"toy_apdc"
