
#' Beard descriptions
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


#' Beard descriptions
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
#' library(NaileR)
#' data(beard_cont)
#' FactoMineR::descfreq(beard_cont)
#' res_beard <- nail_descfreq(beard_cont, introduction = 'A survey was conducted about beards and 8 types of beards were described. In the data that follows, beards are named B1 to B8.', request = 'Please give a name to each beard and summarise what makes this beard unique.')
#' cat(res_beard$response)
"beard_cont"


#' Beard descriptions
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


#' Local food systems survey
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


#' Ideal boss survey
#'
#' These data were collected after a Q-method-like survey on participants' perception of an "ideal boss".
#' Participants had to rank how much they agreed with 30 statements about an ideal boss; then, they were asked personal questions.
#'
#' @docType data
#'
#' @format
#' A data frame with 73 rows (participants) and 39 columns (questions):
#' \describe{
#'  \item{columns 1-30}{statements about the ideal boss}
#'  \item{columns 31-39}{personal information}
#'  }
#' @source Florian LECLERE and Marianne ANDRE, students at l'Institut Agro Rennes-Angers
#' @examples
#' data(boss)
#' boss[1:5, c(1,2,31,33,34)]
"boss"


#' Agribusiness studies survey
#'
#' These data were collected after a Q-method-like survey on students' expectations of agribusiness studies.
#' Participants had to rank how much they agreed with 38 statements about possible benefits from agribusiness studies; then, they were asked personal questions.
#'
#' @docType data
#'
#' @format
#' A data frame with 53 rows (participants) and 42 columns (questions):
#' \describe{
#'  \item{columns 1-38}{statements about agribusiness studies}
#'  \item{columns 39-42}{personal information}
#'  }
#' @source Juliette LE COLLONNIER and Lou ROBERT, students at l'Institut Agro Rennes-Angers
#' @examples
#' data(agri_studies)
#' agri_studies[1:5, c(1,2,40,41)]
"agri_studies"


#' Glossophobia survey
#'
#' These data were collected after a Q-method-like survey on participants' feelings about speaking in public.
#' Participants had to rank how much they agreed with 25 descriptions of speaking in public; then, they were asked personal questions.
#'
#' @docType data
#'
#' @format
#' A data frame with 139 rows (participants) and 41 columns (questions):
#' \describe{
#'  \item{columns 1-25}{descriptions of speaking in public}
#'  \item{columns 26-41}{personal information}
#'  }
#' @source Elina BIAU and Théo LEDAIN, students at l'Institut Agro Rennes-Angers
#' @examples
#' data(glossophobia)
#' glossophobia[1:5, c(1,2,26,31,37)]
"glossophobia"


#' Food waste survey
#'
#' These data were collected after a survey on food waste, with participants describing their habits.
#'
#' @docType data
#'
#' @format
#' A data frame with 180 rows (participants) and 77 columns (questions).
#'
#' @source Héloïse BILLES and Amélie RATEAU, students at l'Institut Agro Rennes-Angers
#' @examples
#' data(waste)
#' waste[1:5, 1:5]
"waste"


#' Perception of food quality
#'
#' These data were collected after a study on the perception of food quality.
#' Participants were given 9 French logos; they had to rate, on a scale from 0 (not at all) to 10 (absolutely), how much a product bearing them aligned with their own perception of quality.
#'
#' @docType data
#'
#' @format
#' A data frame with 55 rows and 9 columns. Here is the list of logos:
#' * AB: organic;
#' * Label Rouge: superior quality (from the taste, process, packaging...);
#' * FairTrade: decent wages and working conditions for the producers;
#' * Bleu Blanc Coeur: diverse and balanced diet for the livestock;
#' * AOC: controlled designation of origin;
#' * Produit en Bretagne: processed in Brittany;
#' * Viandes de France: livestock bred, grown and slaughtered in France, with respectful living conditions;
#' * Nourri sans OGM: no GMOs in livestock food;
#' * Médailles Agro: a prize won at a yearly contest based on taste.
#' @source Sébastien Lê, applied mathematics department, Institut Agro Rennes-Angers
#' @examples
#' data(quality)
#' quality[1:5,]
"quality"


#' Nutri-score survey
#'
#' These data were collected after a survey on the nutri-score. Participants were asked various questions about their views on the nutri-score, and about their eating habits.
#'
#' @docType data
#'
#' @format
#' A data frame with 112 rows (participants) and 36 columns (questions).
#'
#' @source Anaëlle YANNIC and Jessie PICOT, students at l'Institut Agro Rennes-Angers
#' @examples
#' data(nutriscore)
#' nutriscore[1:5, c(1,8,21,30)]
"nutriscore"
