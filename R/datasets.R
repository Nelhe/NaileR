
#' Beards data
#'
#' This data refers to 8 types of beard.
#' Each beard was evaluated, in a few words, by 62 panelists (except beard 8 which only had 60 evaluations) in a sensometrics experiment.
#'
#' @docType data
#'
#' @format ## `beard`
#' A data frame with 494 rows and 2 columns: columns are the type of beard and the words used to describe them.
#' @source Applied mathematics department, Institut Agro Rennes-Angers
#' @examples
#' data(beard)
#' beard[1:8,]
#'
"beard"


#' Beards data
#'
#' This data refers to 8 types of beard.
#' Each beard was evaluated, in a few words, by 62 panelists (except beard 8 which only had 60 evaluations) in a sensometrics experiment.
#'
#' @docType data
#'
#' @format ## `beard_cont`
#' A contingency table (data frame) with 8 rows and 337 columns: rows are the types of beard, columns are the words used to describe them at least once.
#' @source Applied mathematics department, Institut Agro Rennes-Angers
#' @examples
#' data(beard_cont)
#' FactoMineR::descfreq(beard_cont)
#'
"beard_cont"


#' Beards data
#'
#' This data refers to 8 types of beard. It comes from a subset of the original "beard" dataset.
#' Each beard was evaluated, in a few words, by 24 panelists in a sensometrics experiment.
#'
#' @docType data
#'
#' @format ## `beard_wide`
#' A data frame with 8 rows and 24 columns: rows are the types of beards, columns are the panelists' opinions.
#' @source Applied mathematics department, Institut Agro Rennes-Angers
#' @examples
#'
#' data(beard_wide)
#' beard_wide
#'
"beard_wide"

#' Q method data
#'
#' This data was collected after a Q-method-like survey on sustainable food systems.
#' Participants had to rank how acceptable they found 45 statements about a sustainable food system; then, they were asked if they agreed with 14 statements.
#'
#' @docType data
#'
#' @format ## `local_food`
#' A data frame with 573 rows and 63 columns:
#' \describe{
#'  \item{columns 1-45}{statements about food systems}
#'  \item{columns 46-59}{opinions}
#'  \item{columns 60-63}{personal information}
#'  }
#' @source Applied mathematics department, Institut Agro Rennes-Angers
#' @examples
#' data(local_food)
#' local_food[1:5, c(1,2,46,47,60)]
#'
"local_food"
