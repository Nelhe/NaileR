
#' Beard descriptions (data)
#'
#' These data refer to 8 types of beards.
#' Each beard was evaluated by 62 assessors (except beard 8 which only had 60 evaluations).
#'
#' @docType data
#'
#' @format
#' A data frame with 494 rows and 2 columns:
#' * the types of beards;
#' * the words used to describe them.
#' @source Applied mathematics department, Institut Agro Rennes-Angers
#' @examples
#' data(beard)
#' beard[1:8,]
"beard"


#' Beard descriptions (data)
#'
#' These data refer to 8 types of beards.
#' Each beard was evaluated by 62 assessors (except beard 8 which only had 60 evaluations).
#'
#' @docType data
#'
#' @format
#' A contingency table (data frame) with 8 rows and 337 columns:
#' * rows are the types of beards;
#' * columns are the words used at least once to describe them.
#' @source Applied mathematics department, Institut Agro Rennes-Angers
#' @examples
#' data(beard_cont)
#' FactoMineR::descfreq(beard_cont)
"beard_cont"


#' Beard descriptions (data)
#'
#' These data refer to 8 types of beards. They come from a subset of the original "beard" dataset.
#' Each beard was evaluated by 62 assessors (except beard 8 which only had 60 evaluations).
#'
#' @docType data
#'
#' @format
#' A data frame with 8 rows and 24 columns:
#' * rows are the types of beards;
#' * columns are the assessors' opinions.
#' @source Applied mathematics department, Institut Agro Rennes-Angers
#' @examples
#'
#' data(beard_wide)
#' beard_wide
"beard_wide"

#' Q method data
#'
#' These data were collected after a Q-method-like survey on sustainable food systems.
#' Participants had to rank how acceptable they found 45 statements about a sustainable food system; then, they were asked if they agreed with 14 other statements.
#'
#' @docType data
#'
#' @format
#' A data frame with 573 rows (participants) and 63 columns (questions):
#' \describe{
#'  \item{columns 1-45}{statements about food systems}
#'  \item{columns 46-59}{opinions}
#'  \item{columns 60-63}{personal information}
#'  }
#' @source Applied mathematics department, Institut Agro Rennes-Angers
#' @examples
#' data(local_food)
#' local_food[1:5, c(1,2,46,47,60)]
"local_food"
