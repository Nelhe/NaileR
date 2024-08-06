
#' @importFrom dplyr mutate
#' @importFrom glue glue

get_sentences = function(dataset, num.var, num.text){

  ppts = list()
  for (i in 1:nlevels(dataset[,num.var])){

    dataset_work = dataset[dataset[,num.var]==levels(dataset[,num.var])[i],] |>
      as.data.frame() |>
      mutate(Text_clean = dataset[dataset[,num.var]==levels(dataset[,num.var])[i],num.text]) |>
      mutate(Text_clean = glue('* {Text_clean}'))

    corpus = dataset_work$Text_clean |>
      paste(collapse = '\n')

    ppts[[levels(dataset[,num.var])[i]]] = paste(corpus,sep="\n")
  }
  return(ppts)
}


get_prompt_textual = function(dataset, num.var, num.text, introduction, request, isolate.groups){

  sentences = list()
  sentences = get_sentences(dataset, num.var = num.var, num.text = num.text)

  all_groups = names(sentences)

  stces = c()

  for (grp in all_groups){
    sent = ifelse(is.null(sentences[[grp]]), '', sentences[[grp]])

    ppt_grp = glue('## Group "{grp}":

                   {sent}')

    stces = c(stces, ppt_grp)
  }

  if (!isolate.groups) stces = paste(stces, collapse = '\n\n')

  deb = glue('# Introduction

             {introduction}

             # Task

             {request}

             # Data

             ')

  return(paste(deb, stces, sep = ''))
}

#' Interpret a group based on answers to open-ended questions
#'
#' Generate an LLM response to analyze a categorical latent variable, based on answers to open-ended questions.
#'
#' @param dataset a data frame made up of at least one categorical variable and a textual variable.
#' @param num.var the index of the categorical variable to be characterized.
#' @param num.text the index of the textual variable that characterizes the categorical variable of interest.
#' @param introduction the introduction for the LLM prompt.
#' @param request the request made to the LLM.
#' @param model the model name ('llama3' by default).
#' @param isolate.groups a boolean that indicates whether to give the LLM a single prompt, or one prompt per category. Recommended with long catdes results.
#' @param generate a boolean that indicates whether to generate the LLM response. If FALSE, the function only returns the prompt.
#'
#' @return A data frame, or a list of data frames, containing the LLM's prompt and response (if generate = TRUE).
#'
#' @details This function directly sends a prompt to an LLM. Therefore, to get a consistent answer, we highly recommend to customize the parameters introduction and request and add all relevant information on your data for the LLM. We also recommend renaming the columns with clear, unshortened and unambiguous names.
#'
#' Additionally, if isolate.groups = TRUE, you will need an introduction and a request that take into account the fact that only one group is analyzed at a time.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Processing time is often longer than ten seconds
#' # because the function uses a large language model.
#'
#' ### Example 1: Car alone survey ###
#' library(NaileR)
#' library(dplyr)
#' data(car_alone)
#'
#' sampled_car_alone <- car_alone %>%
#' group_by(car_alone_capable_restrictive) %>%
#' dplyr::sample_frac(0.5)
#' sampled_car_alone <- as.data.frame(sampled_car_alone)
#'
#' intro_car <- "Knowing the impact on the climate,
#' I have made these choices based on
#' the following benefits and constraints..."
#' intro_car <- gsub('\n', ' ', intro_car) |>
#' stringr::str_squish()
#'
#' res_nail_textual <- nail_textual(sampled_car_alone, num.var = 1,
#'                                  num.text = 2,
#'                                  introduction = intro_car,
#'                                  request = NULL,
#'                                  model = 'llama3', isolate.groups = TRUE,
#'                                  generate = TRUE)
#' res_nail_textual[[1]]$response |> cat()
#' res_nail_textual[[3]]$response |> cat()
#' res_nail_textual[[2]]$response |> cat()
#' res_nail_textual[[4]]$response |> cat()
#'
#' ### Example 2: Atomic habits survey ###
#' library(NaileR)
#' library(dplyr)
#' data(atomic_habit_clust)
#'
#' intro_atomic <- "These data were collected
#' after a survey on atomic habits: we asked
#' what people were prepared to change about their daily habits
#' to make the world a better place,
#' what habits they felt able to adopt,
#' what habits were restrictive."
#'intro_atomic <- gsub('\n', ' ', intro_atomic) |>
#'stringr::str_squish()
#'
#'dta_plane <- atomic_habit_clust[,c(32,51)] %>%
#'             filter(never_plane_text != 'THAT')
#'
#'sampled_dta_plane <- dta_plane %>%
#'                     group_by(clust) %>%
#'                     dplyr::sample_frac(0.75)
#'
#'sampled_dta_plane <- as.data.frame(sampled_dta_plane)
#'summary(sampled_dta_plane)
#'
#'res_nail_textual_plane <- nail_textual(sampled_dta_plane, num.var = 2,
#'                                       num.text = 1,
#'                                       introduction = intro_atomic,
#'                                       request = NULL,
#'                                       model = 'llama3',
#'                                       isolate.groups = TRUE,
#'                                       generate = TRUE)
#'cat(res_nail_textual_plane[[1]]$prompt)
#'cat(res_nail_textual_plane[[1]]$response)
#'
#'cat(res_nail_textual_plane[[2]]$prompt)
#'cat(res_nail_textual_plane[[2]]$response)
#'
#'cat(res_nail_textual_plane[[3]]$prompt)
#'cat(res_nail_textual_plane[[3]]$response)
#'
#'res_nail_textual_plane <- nail_textual(sampled_dta_plane, num.var = 2,
#'                                       num.text = 1,
#'                                       introduction = intro_atomic,
#'                                       request = NULL,
#'                                       model = 'llama3',
#'                                       isolate.groups = FALSE,
#'                                       generate = TRUE)
#'cat(res_nail_textual_plane$prompt)
#'cat(res_nail_textual_plane$response)
#'
#' ### Example 3: Car seat fabrics ###
#'
#' # Drivers of liking and disliking
#' # isolate.groups = F
#'
#' intro_car <- "In this consumer study, a number of car seat fabrics
#' were rated by consumers who gave their reasons
#' for liking or disliking the fabrics.
#' Reasons for disliking the fabrics were reported in group '0',
#' while reasons for liking the fabrics were reported in group '1'."
#' intro_car <- gsub('\n', ' ', intro_car) |>
#' stringr::str_squish()
#'
#' request_car <- "Based on the comments provided by the consumers,
#' please explain the reasons why
#' the fabrics were not appreciated (group '0'),
#' and the reasons why fabrics were appreciated (group '1').
#' In other words, what are the drivers for disliking
#' and liking the fabrics."
#' request_car <- gsub('\n', ' ', request_car) |>
#' stringr::str_squish()
#' res_nail_textual_fabric <- nail_textual(fabric, num.var = 4,
#'                                         num.text = 3,
#'                                         introduction = intro_car,
#'                                         request = request_car,
#'                                         model = 'llama3',
#'                                         isolate.groups = FALSE,
#'                                         generate = TRUE)
#'
#' cat(res_nail_textual_fabric$response)
#'
#' # Drivers of disliking with a specific prompt
#' # isolate.groups = T
#'
#' intro_car_disliking <- "In this consumer study, a range of car seat fabrics
#' were rated by consumers who gave their reasons
#' for disliking the fabrics.
#' In these data, only the reasons for disliking the fabrics were reported."
#' intro_car_disliking <- gsub('\n', ' ', intro_car_disliking) |>
#' stringr::str_squish()
#'
#' request_car_disliking <- "Based on the comments provided by the consumers,
#' please explain the reasons why
#' the fabrics were not appreciated.
#' In other words, what are the drivers for disliking the fabrics."
#' request_car_disliking <- gsub('\n', ' ', request_car_disliking) |>
#' stringr::str_squish()
#'
#' res_nail_textual_fabric <- nail_textual(fabric, num.var = 4,
#'                                         num.text = 3,
#'                                         introduction = intro_car_disliking,
#'                                         request = request_car_disliking,
#'                                         model = 'llama3',
#'                                         isolate.groups = TRUE,
#'                                         generate = FALSE)
#'
#' ppt <- res_nail_textual_fabric$prompt[1]
#' cat(ppt)
#'
#' res_disliking <- ollamar::generate(model = 'llama3', prompt = ppt,
#'                                    output = "df")
#' cat(res_disliking$response)
#'
#' # Drivers of liking with a specific prompt
#' # isolate.groups = T
#'
#' intro_car_liking <- "In this consumer study, a range of car seat fabrics
#' were rated by consumers who gave their reasons
#' for liking the fabrics.
#' In these data, only the reasons for liking the fabrics were reported."
#' intro_car_liking <- gsub('\n', ' ', intro_car_liking) |>
#' stringr::str_squish()
#'
#' request_car_liking <- "Based on the comments provided by the consumers,
#' please explain the reasons why
#' the fabrics were appreciated.
#' In other words, what are the drivers for liking the fabrics."
#' request_car_liking <- gsub('\n', ' ', request_car_liking) |>
#' stringr::str_squish()
#'
#' res_nail_textual_fabric <- nail_textual(fabric, num.var = 4,
#'                                         num.text = 3,
#'                                         introduction = intro_car_liking,
#'                                         request = request_car_liking,
#'                                         model = 'llama3', isolate.groups = TRUE,
#'                                         generate = FALSE)
#'
#' ppt <- res_nail_textual_fabric$prompt[2]
#' cat(ppt)
#'
#' res_liking <- ollamar::generate(model = 'llama3', prompt = ppt,
#'                                 output = "df")
#' cat(res_liking$response)
#' }

nail_textual = function(dataset, num.var,
                        num.text,
                        introduction = '',
                        request = NULL,
                        model = 'llama3', isolate.groups = TRUE,
                        generate = FALSE){

  if (isolate.groups == F){
    if (is.null(request)) request <- "Based on the results, please describe what characterize the individuals of each group and what set them apart from the other groups. Then, based on these characteristics, give each group a new name."
  } else {
    if (is.null(request)) request <- "Based on the results, please describe what characterize the individuals of this group and what make them so special. Then, based on these characteristics, give the group a new name."
  }

  ppt = get_prompt_textual(dataset, num.var = num.var, num.text = num.text, isolate.groups = isolate.groups,
                           introduction = introduction, request = request)

  if (!generate) return(data.frame(prompt = ppt))

  if (isolate.groups == F){
    res_llm = ollamar::generate(model = model, prompt = ppt, output = 'df')
    res_llm$prompt = ppt
    return(res_llm)
  } else {
    list_rep = list()
    for (prpt in ppt){
      res_llm = ollamar::generate(model = model, prompt = prpt, output = 'df')
      res_llm$prompt = prpt
      list_rep[[length(list_rep) + 1]] = res_llm
    }
    return(list_rep)
  }
}
