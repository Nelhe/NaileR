
#' Atomic habits survey
#'
#' People think they need to make big changes to change
#' the course of their lives. But in James Clear's book,
#' Atomic Habits, they will discover that the smallest of changes,
#' coupled with a good knowledge of psychology and neuroscience,
#' can have a revolutionary effect on their lives and relationships.
#' To understand this concept of atomic habits, we interviewed 167 people
#' and asked them if they were able to never take their car alone again,
#' to buy local products...
#' We also asked them how restrictive they found this and why.
#'
#' @docType data
#'
#' @format
#' A data frame with 167 rows and 50 columns:
#' * columns 1-10, do you feel able to...
#' * columns 11-20, from 0 to 5 how restrictive...
#' * columns 21-30, is it restrictive, yes or no...
#' * columns 31-40, justify your answers
#' * columns 41-50, a combination of able and restrictive
#'
#' @source Applied mathematics department, Institut Agro Rennes-Angers
#' @examples
#' \dontrun{
#' # Processing time is often longer than ten seconds
#' # because the function uses a large language model.
#'
#'library(FactoMineR)
#'library(NaileR)
#'data(atomic_habit)
#'
#'res_mfa <- MFA(atomic_habit[,1:30],
#'               group = c(10,10,10),
#'               type = c("n","s","n"),
#'               num.group.sup = 3,
#'               name.group = c("capable","restrictive", "restrictive binary"),
#'               graph = FALSE)
#'
#'plot.MFA(res_mfa, choix = "ind", invisible = c("quali","quali.sup"),
#'         lab.ind = FALSE,
#'         title = "MFA based on being capable and restrictiveness data")
#'
#'res_hcpc <- HCPC(res_mfa, nb.clust = 3, graph = FALSE)
#'plot.HCPC(res_hcpc, choice = "map",
#'          draw.tree = FALSE,
#'          ind.names = FALSE,
#'          title = "Atomic habits - typology")
#'summary(res_hcpc$data.clust)
#' }
"atomic_habit"


#' Atomic habits survey
#'
#' People think they need to make big changes to change
#' the course of their lives. But in James Clear's book,
#' Atomic Habits, they will discover that the smallest of changes,
#' coupled with a good knowledge of psychology and neuroscience,
#' can have a revolutionary effect on their lives and relationships.
#' To understand this concept of atomic habits, we interviewed 167 people
#' and asked them if they were able to never take their car alone again,
#' to buy local products...
#' We also asked them how restrictive they found this and why.
#'
#' @docType data
#'
#' @format
#' * column 1, a combination of being able and feeling restrictive
#' * column 2, justify your answer
#'
#' @source Applied mathematics department, Institut Agro Rennes-Angers
#' @examples
#' \dontrun{
#' # Processing time is often longer than ten seconds
#' # because the function uses a large language model.
#'
#'library(FactoMineR)
#'library(NaileR)
#'library(dplyr)
#'data(car_alone)
#'sampled_car_alone <- car_alone %>%
#'group_by(car_alone_capable_restrictive) %>%
#'sample_frac(0.5)
#'sampled_car_alone <- as.data.frame(sampled_car_alone)
#'
#'intro_car <- "Knowing the impact on the climate,
#'I have made these choices based on
#'the following benefits and constraints..."
#'intro_car <- gsub('\n', ' ', intro_car) |>
#'stringr::str_squish()
#'res_nail_textual <- nail_textual(sampled_car_alone, num.var = 1,
#'                                 num.text = 2,
#'                                 introduction = intro_car,
#'                                 request = NULL,
#'                                 model = 'llama3', isolate.groups = TRUE,
#'                                 generate = TRUE)
#' res_nail_textual[[1]]$response |> cat()
#' res_nail_textual[[3]]$response |> cat()
#' res_nail_textual[[2]]$response |> cat()
#' res_nail_textual[[4]]$response |> cat()
#' }
"car_alone"

#' Atomic habits survey
#'
#' People think they need to make big changes to change
#' the course of their lives. But in James Clear's book,
#' Atomic Habits, they will discover that the smallest of changes,
#' coupled with a good knowledge of psychology and neuroscience,
#' can have a revolutionary effect on their lives and relationships.
#' To understand this concept of atomic habits, we interviewed 167 people
#' and asked them if they were able to never take their car alone again,
#' to buy local products...
#' We also asked them how restrictive they found this and why.
#'
#' @docType data
#'
#' @format
#' A data frame with 167 rows and 51 columns:
#' * columns 1-10, do you feel able to...
#' * columns 11-20, from 0 to 5 how restrictive...
#' * columns 21-30, is it restrictive, yes or no...
#' * columns 31-40, justify your answers
#' * columns 41-50, a combination of able and restrictive
#' * column 51, cluster variable based on MFA (20 first variables)
#'
#' @source Applied mathematics department, Institut Agro Rennes-Angers
#' @examples
#' \dontrun{
#' # Processing time is often longer than ten seconds
#' # because the function uses a large language model.
#'
#'library(FactoMineR)
#'library(NaileR)
#'data(atomic_habit_clust)
#'
#'catdes(atomic_habit_clust, num.var = 51)
#' }
"atomic_habit_clust"


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
#' \dontrun{
#' # Processing time is often longer than ten seconds
#' # because the function uses a large language model.
#'
#' data(beard)
#' beard[1:8,]
#' }
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
#' \dontrun{
#' # Processing time is often longer than ten seconds
#' # because the function uses a large language model.
#'
#' library(NaileR)
#' data(beard_cont)
#'
#' FactoMineR::descfreq(beard_cont)
#'
#' intro_beard <- 'A survey was conducted about beards
#' and 8 types of beards were described.
#' In the data that follow, beards are named B1 to B8.'
#' intro_beard <- gsub('\n', ' ', intro_beard) |>
#' stringr::str_squish()
#'
#' req_beard <- 'Please give a name to each beard
#' and summarize what makes this beard unique.'
#' req_beard <- gsub('\n', ' ', req_beard) |>
#' stringr::str_squish()
#'
#' res_beard <- nail_descfreq(beard_cont,
#' introduction = intro_beard,
#' request = req_beard)
#' cat(res_beard$response)
#' }
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
#' \dontrun{
#' # Processing time is often longer than ten seconds
#' # because the function uses a large language model.
#'
#' library(NaileR)
#' data(beard_wide)
#'
#' intro_beard <- "As a barber, you make
#' recommendations based on consumers comments.
#' Examples of consumers descriptions of beards
#' are as follows."
#' intro_beard <- gsub('\n', ' ', intro_beard) |>
#' stringr::str_squish()
#'
#' res <- nail_sort(beard_wide[,1:5], name_size = 3,
#' stimulus_id = "beard", introduction = intro_beard,
#' measure = 'the description was')
#'
#' res$dta_sort
#' cat(res$prompt_llm[[1]])
#' }
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
#' \dontrun{
#' # Processing time is often longer than ten seconds
#' # because the function uses a large language model.
#'
#' library(FactoMineR)
#' library(NaileR)
#' data(local_food)
#'
#' res_mca_food <- MCA(local_food, quali.sup = 46:63,
#' ncp = 100, level.ventil = 0.05, graph = FALSE)
#' res_hcpc_food <- HCPC(res_mca_food, nb.clust = 3, graph = FALSE)
#' don_clust_food <- res_hcpc_food$data.clust
#'
#' intro_food <- 'A study on sustainable food systems
#' was led on several French participants.
#' This study had 2 parts. In the first part,
#' participants had to rate how acceptable
#' "a food system that..." (e.g, "a food system that
#' only uses renewable energy") was to them.
#' In the second part, they had to say
#' if they agreed or disagreed with some statements.'
#' intro_food <- gsub('\n', ' ', intro_food) |>
#' stringr::str_squish()
#'
#' req_food <- 'I will give you the answers from one group.
#' Please explain who the individuals of this group are,
#' what their beliefs are.
#' Then, give this group a new name,
#' and explain why you chose this name.
#' Do not use 1st person ("I", "my"...) in your answer.'
#' req_food <- gsub('\n', ' ', req_food) |>
#' stringr::str_squish()
#'
#' res_food <- nail_catdes(don_clust_food, num.var = 64,
#' introduction = intro_food,
#' request = req_food,
#' isolate.groups = TRUE, drop.negative = TRUE)
#' res_food[[1]]$response |> cat()
#' }
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
#' \dontrun{
#' # Processing time is often longer than ten seconds
#' # because the function uses a large language model.
#'
#' library(FactoMineR)
#' library(NaileR)
#' data(boss)
#' res_mca_boss <- MCA(boss, quali.sup = 31:39,
#' ncp = 30, level.ventil = 0.05, graph = FALSE)
#' res_hcpc_boss <- HCPC(res_mca_boss, nb.clust = 4, graph = FALSE)
#' don_clust_boss <- res_hcpc_boss$data.clust
#'
#' intro_boss <- 'A study on "the ideal boss" was led on 73 participants.
#' The study had 2 parts. In the first part,
#' participants were given statements about the ideal boss
#' (starting with "My ideal boss...").
#' They had to rate, on a scale from 1 to 5,
#' how much they agreed with the statements;
#' 1 being "Strongly disagree", 3 being "neutral"
#' and 5 being "Strongly agree".
#' In the second part, they were asked for personal information:
#' work experience, age, etc.
#' Participants were then split into groups based on their answers.'
#' intro_boss <- gsub('\n', ' ', intro_boss) |>
#' stringr::str_squish()
#'
#' req_boss <- "Please describe, for each group, their ideal boss.
#' Then, give each group a new name, based on your conclusions."
#' req_boss <- gsub('\n', ' ', req_boss) |>
#' stringr::str_squish()
#'
#'
#' res_boss <- nail_catdes(don_clust_boss, num.var = 40,
#' introduction = intro_boss, request = req_boss,
#' isolate.groups = FALSE, drop.negative = TRUE)
#' res_boss$response |> cat()
#' }
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
#' \dontrun{
#' # Processing time is often longer than ten seconds
#' # because the function uses a large language model.
#'
#' library(NaileR)
#' data(agri_studies)
#'
#' res_mca_agri <- FactoMineR::MCA(agri_studies, quali.sup = 39:42,
#' level.ventil = 0.05, graph = FALSE)
#' agri_work <- res_mca_agri$ind$coord |> as.data.frame()
#' agri_work <- agri_work[,1] |> cbind(agri_studies)
#'
#' intro_agri <- "These data were collected after a survey
#' on students' expectations of agribusiness studies.
#' Participants had to rank how much they agreed with 38 statements
#' about possible benefits from agribusiness studies;
#' then, they were asked personal questions."
#' intro_agri <- gsub('\n', ' ', intro_agri) |>
#' stringr::str_squish()
#'
#' res_agri <- nail_condes(agri_work, num.var = 1,
#' introduction = intro_agri)
#' cat(res_agri$response)
#' }
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
#' \dontrun{
#' # Processing time is often longer than ten seconds
#' # because the function uses a large language model.
#'
#' library(NaileR)
#' data(glossophobia)
#'
#' res_mca_phobia <- FactoMineR::MCA(glossophobia, quali.sup = 26:41,
#' level.ventil = 0.05, graph = FALSE)
#' phobia_work <- res_mca_phobia$ind$coord |> as.data.frame()
#' phobia_work <- phobia_work[,1] |> cbind(glossophobia)
#'
#' intro_phobia <- "These data were collected after a survey
#' on participants' feelings about speaking in public.
#' Participants had to rank how much they agreed with
#' 25 descriptions of speaking in public;
#' then, they were asked personal questions."
#' intro_phobia <- gsub('\n', ' ', intro_phobia) |>
#' stringr::str_squish()
#'
#' res_phobia <- nail_condes(phobia_work, num.var = 1,
#' introduction = intro_phobia)
#' cat(res_phobia$response)
#' }
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
#' \dontrun{
#' # Processing time is often longer than ten seconds
#' # because the function uses a large language model.
#'
#' library(NaileR)
#' library(FactoMineR)
#' data(waste)
#' waste <- waste[-14]
#'
#' res_mca_waste <- MCA(waste, quali.sup = c(1,2,50:76),
#' ncp = 35, level.ventil = 0.05, graph = FALSE)
#' res_hcpc_waste <- HCPC(res_mca_waste, nb.clust = 3, graph = FALSE)
#' don_clust_waste <- res_hcpc_waste$data.clust
#'
#' intro_waste <- 'These data were collected
#' after a survey on food waste,
#' with participants describing their habits.'
#' intro_waste <- gsub('\n', ' ', intro_waste) |>
#' stringr::str_squish()
#'
#' req_waste <- 'Please summarize the characteristics of each group.
#' Then, give each group a new name, based on your conclusions.
#' Finally, give each group a grade between 0 and 10,
#' based on how wasteful they are with food:
#' 0 being "not at all", 10 being "absolutely".'
#' req_waste <- gsub('\n', ' ', req_waste) |>
#' stringr::str_squish()
#'
#' res_waste <- nail_catdes(don_clust_waste,
#' num.var = ncol(don_clust_waste),
#' introduction = intro_waste, request = req_waste,
#' drop.negative = TRUE)
#' cat(res_waste$response)
#' }
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
#' \dontrun{
#' # Processing time is often longer than ten seconds
#' # because the function uses a large language model.
#'
#' library(NaileR)
#' data(quality)
#'
#' colnames(quality) <- c("Agriculture biologique",
#' "Label Rouge",
#' "FairTrade",
#' "Bleu Blanc Coeur",
#' "Appelation d'origine contrôlée",
#' "Produit en Bretagne",
#' "Viandes de France",
#' "Nourri sans OGM",
#' "Médailles Agro")
#'
#' res_pca_quality <- FactoMineR::PCA(quality, graph = FALSE)
#' quali_work <- res_pca_quality$ind$coord |> as.data.frame()
#' quali_work <- quali_work[,1] |> cbind(quality)
#'
#' intro_quali <- "These data were collected after a study
#' on the perception of food quality.
#' Participants were given 9 French logos;
#' they had to rate, on a scale from 0 (not at all)
#' to 10 (absolutely), how much a product bearing them
#' aligned with their own perception of quality."
#' intro_quali <- gsub('\n', ' ', intro_quali) |>
#' stringr::str_squish()
#'
#' res_quality <- nail_condes(quali_work, num.var = 1,
#' quanti.cat = c('Higher quality', 'Lower quality', 'Neutral'),
#' introduction = intro_quali, generate = FALSE)
#'
#' ppt <- gsub('characteristics', 'opinions', res_quality$prompt)
#'
#' res_quality <- ollamar::generate('llama3', ppt, output = 'df')
#'
#' cat(res_quality$response)
#' }
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
#' \dontrun{
#' # Processing time is often longer than ten seconds
#' # because the function uses a large language model.
#'
#' library(NaileR)
#' library(FactoMineR)
#'
#' data(nutriscore)
#'
#' res_mca_nutriscore <- MCA(nutriscore, quali.sup = 17:36,
#' ncp = 15, level.ventil = 0.05, graph = FALSE)
#'
#' res_hcpc_nutriscore <- HCPC(res_mca_nutriscore, nb.clust = 3,
#' graph = FALSE)
#' don_clust_nutriscore <- res_hcpc_nutriscore$data.clust
#'
#' intro_nutri <- 'These data were collected after a survey
#' on the nutri-score. Participants were asked
#' various questions about their views on the nutri-score,
#' and about their eating habits.
#' Participants were split into groups according to their answers.'
#' intro_nutri <- gsub('\n', ' ', intro_nutri) |>
#' stringr::str_squish()
#'
#' req_nutri <- 'Please summarize the characteristics
#' of each group. Then, give each group a new name,
#' based on your conclusions.'
#' req_nutri <- gsub('\n', ' ', req_nutri)|>
#' stringr::str_squish()
#'
#' res_nutriscore <- nail_catdes(don_clust_nutriscore, num.var = 37,
#' introduction = intro_nutri, request = req_nutri,
#' drop.negative = TRUE)
#'
#' cat(res_nutriscore$response)
#' }
"nutriscore"
