
#' Beards data
#'
#' This data refers to 8 types of beard.
#' Each beard was evaluated, in a few words, by 62 panelists (except beard 8 which only had 60 evaluations) in a sensometrics experiment.
#'
#' @docType data
#'
#' @format ## `beard`
#' A contingency table (data frame) with 8 rows and 337 columns: rows are the types of beard, columns are the words used to describe them at least once.
#' @source Applied mathematics department, Institut Agro Rennes-Angers
#' @examples
#' data(beard)
#' FactoMineR::descfreq(beard)
#'
"beard"
