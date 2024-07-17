
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
#' library(NaileR)
#' data(beard_wide)
#' res <- nail_sort(beard_wide[,1:5], name_size = 3, stimulus_id = "beard", introduction = "As a barber, you make recommendations based on consumers comments. Examples of consumers descriptions of beards are as follows.", measure = 'the description was')
#' res$dta_sort
#' cat(res$prompt_llm[[1]])
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
#' * columns 1-45 statements about food systems
#' * columns 46-59 opinions
#' * columns 60-63 personal information
#' @source Applied mathematics department, Institut Agro Rennes-Angers
#' @examples
#' library(FactoMineR)
#' library(NaileR)
#' data(local_food)
#' res_mca_food <- MCA(local_food, quali.sup = 46:63, ncp = 100, level.ventil = 0.05, graph = FALSE)
#' res_hcpc_food <- HCPC(res_mca_food, nb.clust = 3, graph = FALSE)
#' don_clust_food <- res_hcpc_food$data.clust
#' res_food <- nail_catdes(don_clust_food, num.var = ncol(don_clust_food), introduction = 'A study on sustainable food systems was led on several French participants. This study had 2 parts. In the first part, participants had to rate how acceptable "a food system that..." (e.g, "a food system that only uses renewable energy") was to them. In the second part, they had to say if they agreed or disagreed with some statements.', request = 'I will give you the answers from one group. Please explain who the individuals of this group are, what their beliefs are. Then, give this group a new name, and explain why you chose this name. Do not use 1st person ("I", "my"...) in your answer.', isolate.groups = TRUE, drop.negative = TRUE)
#' res_food[[1]]$response |> cat()
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
#' * columns 1-30: statements about the ideal boss
#' * columns 31-39: personal information
#' @source Florian LECLERE and Marianne ANDRE, students at l'Institut Agro Rennes-Angers
#' @examples
#' library(FactoMineR)
#' library(NaileR)
#' data(boss)
#' res_mca_boss <- MCA(boss, quali.sup = 31:39, ncp = 30, level.ventil = 0.05, graph = FALSE)
#' res_hcpc_boss <- HCPC(res_mca_boss, nb.clust = 4, graph = FALSE)
#' don_clust_boss <- res_hcpc_boss$data.clust
#' intro = 'A study on "the ideal boss" was led on 73 participants. The study had 2 parts. In the first part, participants were given statements about the ideal boss (starting with "My ideal boss..."). They had to rate, on a scale from 1 to 5, how much they agreed with the statements; 1 being "Strongly disagree", 3 being "neutral" and 5 being "Strongly agree". In the second part, they were asked for personal information: work experience, age, etc. Participants were then split into groups based on their answers.'
#' requ = "Please describe, for each group, their ideal boss. Then, give each group a new name, based on your conclusions."
#' res_boss <- nail_catdes(don_clust_boss, num.var = 40, introduction = intro, request = requ, isolate.groups = FALSE, drop.negative = TRUE)
#' res_boss$response |> cat()
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
#' * columns 1-38: statements about agribusiness studies
#' * columns 39-42: personal information
#' @source Juliette LE COLLONNIER and Lou ROBERT, students at l'Institut Agro Rennes-Angers
#' @examples
#' library(NaileR)
#' data(agri_studies)
#' res_mca_agri <- FactoMineR::MCA(agri_studies, quali.sup = 39:42, level.ventil = 0.05, graph = FALSE)
#' agri_work <- res_mca_agri$ind$coord |> as.data.frame()
#' agri_work <- agri_work[,1] |> cbind(agri_studies)
#' res_agri <- nail_condes(agri_work, num.var = 1, introduction = "These data were collected after a survey on students' expectations of agribusiness studies. Participants had to rank how much they agreed with 38 statements about possible benefits from agribusiness studies; then, they were asked personal questions.")
#' cat(res_agri$response)
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
#' * columns 1-25: descriptions of speaking in public
#' * columns 26-41: personal information
#' @source Elina BIAU and Théo LEDAIN, students at l'Institut Agro Rennes-Angers
#' @examples
#' library(NaileR)
#' data(glossophobia)
#' res_mca_phobia <- FactoMineR::MCA(glossophobia, quali.sup = 26:41, level.ventil = 0.05, graph = FALSE)
#' phobia_work <- res_mca_phobia$ind$coord |> as.data.frame()
#' phobia_work <- phobia_work[,1] |> cbind(glossophobia)
#' res_phobia <- nail_condes(phobia_work, num.var = 1, introduction = "These data were collected after a survey on participants' feelings about speaking in public. Participants had to rank how much they agreed with 25 descriptions of speaking in public; then, they were asked personal questions.")
#' cat(res_phobia$response)
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
#' library(NaileR)
#' library(FactoMineR)
#' data(waste)
#' waste <- waste[-14]
#' res_mca_waste <- MCA(waste, quali.sup = c(1,2,50:76), ncp = 35, level.ventil = 0.05, graph = FALSE)
#' res_hcpc_waste <- HCPC(res_mca_waste, nb.clust = 3, graph = FALSE)
#' don_clust_waste <- res_hcpc_waste$data.clust
#' res_waste <- nail_catdes(don_clust_waste, num.var = ncol(don_clust_waste), introduction = 'These data were collected after a survey on food waste, with participants describing their habits.', request = 'Please summarize the characteristics of each group. Then, give each group a new name, based on your conclusions. Finally, give each group a grade between 0 and 10, based on how wasteful they are with food: 0 being "not at all", 10 being "absolutely".', drop.negative = TRUE)
#' cat(res_waste$response)
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
#' library(NaileR)
#' library(FactoMineR)
#' data(quality)
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
#' library(NaileR)
#' library(FactoMineR)
#' data(nutriscore)
#' res_mca_nutriscore <- MCA(nutriscore, quali.sup = 17:36, ncp = 15, level.ventil = 0.05, graph = FALSE)
#' res_hcpc_nutriscore <- HCPC(res_mca_nutriscore, nb.clust = 3, graph = FALSE)
#' don_clust_nutriscore <- res_hcpc_nutriscore$data.clust
#' res_waste <- nail_catdes(don_clust_nutriscore, num.var = ncol(don_clust_nutriscore), introduction = 'These data were collected after a survey on the nutri-score. Participants were asked various questions about their views on the nutri-score, and about their eating habits. Participants were split into groups according to their answers.', request = 'Please summarize the characteristics of each group. Then, give each group a new name, based on your conclusions.', drop.negative = TRUE)
#' cat(res_waste$response)
"nutriscore"
